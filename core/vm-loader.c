/*
 * Copyright (c) 2012-2017, RISE SICS AB
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the copyright holder nor the names of its
 *    contributors may be used to endorse or promote products derived
 *    from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * Author: Nicolas Tsiftes <nvt@acm.org>
 */

#include "vm.h"
#include "vm-control.h"
#include "vm-log.h"
#include "vm-native.h"

/*
 * The next unique identifier, which will be assigned to the next loaded
 * program. It will not be used for any subsequent program loading during
 * the current VM instance's runtime.
 */
static unsigned next_program_id;

/* A linked list of all loaded programs. */
static vm_program_t *loaded_programs;

#define READ_CHECK(handle, buf, size)		                              \
  do {					                                      \
    if(VM_LOADER_READ((handle), (buf), (size)) != (size)) {                   \
      VM_DEBUG(VM_DEBUG_MEDIUM, "Failed to read %u bytes", (unsigned)(size)); \
      return 0;                                                               \
    }                                                                         \
  } while(0)

static void
destroy_program_threads(vm_program_t *program)
{
  int i;
  vm_thread_t *thread;

  for(i = 0; i < VM_THREAD_AMOUNT; i++) {
    thread = vm_thread_get(i);
    if(thread != NULL) {
      vm_thread_destroy(thread);
    }
  }
}

static void
free_program(vm_program_t *program)
{
#if VM_INSTRUCTION_PROFILING
    VM_FREE(program->exec_count);
#endif

    vm_table_destroy(&program->strings);
    vm_table_destroy(&program->symbols);
    vm_table_destroy(&program->exprv);

    if(program->captures != NULL) {
      unsigned i;
      for(i = 0; i < program->captures_size; i++) {
        if(program->captures[i] != NULL) {
          vm_free(program->captures[i]->symbols);
          vm_free(program->captures[i]);
        }
      }
      vm_free(program->captures);
      program->captures = NULL;
      program->captures_size = 0;
    }

    VM_FREE(program->symbol_bindings);

    vm_free(program->name);
    program->name = NULL;
    vm_free(program);
}

static int
extract_program_name(char **name, const char *filename)
{
  const char *start;
  const char *end;
  size_t length;

  start = strrchr(filename, '/');
  if(start == NULL) {
    start = filename;
  } else {
    start++;
  }

  end = strstr(start, ".vm");
  if(end == NULL) {
    end = start + strlen(start);
  }
  length = (size_t)(end - start);

  *name = VM_MALLOC(length + 1);
  if(*name == NULL) {
    return 0;
  }

  memcpy(*name, start, length);
  (*name)[length] = '\0';

  return 1;
}

#if VM_DEBUG_LEVEL >= VM_DEBUG_MEDIUM
static void
print_bytecode(uint8_t *bytes, unsigned length)
{
  unsigned i;

  for(i = 0; i < length; i++) {
    VM_PRINTF("%u ", bytes[i]);
  }

  VM_PRINTF("\n");
}
#endif /* VM_DEBUG_LEVEL >= VM_DEBUG_MEDIUM */

static int
read_table(vm_table_t *table, int handle)
{
  char buf[VM_TABLE_MAX_ITEM_SIZE]; /* The buffer size is configurable per platform. */
  uint16_t item_count;
  uint16_t item_length;
  int i;
  uint32_t table_size;
  vm_loader_offset_t saved_offset;

  item_count = 0;
  READ_CHECK(handle, &item_count, sizeof(item_count));
  VM_DEBUG(VM_DEBUG_MEDIUM, "Reading a table containing %u item%s",
         item_count, item_count == 1 ? "" : "s");

  saved_offset = VM_LOADER_SEEK_RELATIVE(handle, 0);
  if(saved_offset == (vm_loader_offset_t)-1) {
    VM_DEBUG(VM_DEBUG_LOW, "loader:seek failed");
    return 0;
  }

  /* Calculate the raw table size. */
  table_size = 0;
  for(i = 0; i < item_count; i++) {
    item_length = 0;
    READ_CHECK(handle, &item_length, sizeof(item_length));

    if(item_length > VM_TABLE_MAX_ITEM_SIZE) {
      VM_DEBUG(VM_DEBUG_LOW, "Item size (%u) exceeds maximum (%u)",
               item_length, VM_TABLE_MAX_ITEM_SIZE);
      return 0;
    }

    if(VM_LOADER_SEEK_RELATIVE(handle, item_length) == (vm_loader_offset_t)-1) {
      VM_DEBUG(VM_DEBUG_LOW, "loader:seek failed");
      return 0;
    }

    if((uint32_t)item_length + 2 > VM_TABLE_MAX_BYTES - table_size) {
      VM_DEBUG(VM_DEBUG_LOW, "Maximum table size exceeded (%lu + %u > %lu)",
               (unsigned long)table_size, (unsigned)item_length + 2,
               (unsigned long)VM_TABLE_MAX_BYTES);
      return 0;
    }

    table_size += (uint32_t)item_length + 2;
  }
  VM_DEBUG(VM_DEBUG_MEDIUM, "Allocating a table of %u bytes", table_size);

  if(vm_table_create(table, item_count, table_size) == 0) {
    VM_DEBUG(VM_DEBUG_LOW, "Failed to allocate a table");
    return 0;
  }

  if(VM_LOADER_SEEK_ABSOLUTE(handle, saved_offset) == (vm_loader_offset_t)-1) {
    VM_DEBUG(VM_DEBUG_LOW, "loader:seek failed");
    return 0;
  }

  /* Fill the table with data from the file. */
  for(i = 0; i < item_count; i++) {
    READ_CHECK(handle, &item_length, sizeof(item_length));
    READ_CHECK(handle, buf, item_length);
    if(vm_table_set(table, i, buf, item_length) == 0) {
      VM_DEBUG(VM_DEBUG_LOW, "Failed to allocate a table item");
      return 0;
    }
    VM_DEBUG(VM_DEBUG_MEDIUM, "Read item of %u byte%s",
           item_length, item_length != 1 ? "s" : "");
  }

  return 1;
}

static vm_program_t *
read_program(const char *name)
{
  vm_program_t *program;
  vm_loader_handle_t handle;
  unsigned char buf[VM_HEADER_SIZE];
  unsigned i;
  unsigned code_size;

  handle = VM_LOADER_OPEN(name);
  if(handle < 0) {
    return NULL;
  }

  program = VM_MALLOC(sizeof(vm_program_t));
  if(program == NULL) {
    VM_LOADER_CLOSE(handle);
    return NULL;
  }

  /* Initialize all fields to zero or NULL in order to know what to deallocate
     if the program loading fails at some point. */
  memset(program, 0, sizeof(vm_program_t));
  program->name = NULL;
  program->symbol_bindings = NULL;
  program->captures = NULL;
  program->captures_size = 0;
#if VM_INSTRUCTION_PROFILING
  program->exec_count = NULL;
#endif

  if(extract_program_name(&program->name, name) == 0) {
    VM_DEBUG(VM_DEBUG_LOW, "Failed to extract the program name from \"%s\"",
             name);
    goto error;
  }

  VM_DEBUG(VM_DEBUG_LOW, "Loading program \"%s\"", program->name);

  if(VM_LOADER_READ(handle, buf, 3) != 3) {
    VM_DEBUG(VM_DEBUG_LOW, "Read error on program header");
    goto error;
  }

  if(buf[0] != VM_FILE_ID1 && buf[1] != VM_FILE_ID2) {
    VM_DEBUG(VM_DEBUG_LOW, "%s: invalid program header", name);
    goto error;
  }

  if(buf[2] != VM_BYTECODE_VERSION) {
    VM_DEBUG(VM_DEBUG_LOW, "%s: unsupported bytecode version %d",
             name, buf[2]);
    goto error;
  }

  if(read_table(&program->strings, handle) == 0) {
    VM_DEBUG(VM_DEBUG_LOW, "Failed to read the string table");
    goto error;
  }

  if(read_table(&program->symbols, handle) == 0) {
    VM_DEBUG(VM_DEBUG_LOW, "Failed to read the symbol table");
    goto error;
  }

  if(read_table(&program->exprv, handle) == 0) {
    VM_DEBUG(VM_DEBUG_LOW, "Failed to read the form table");
    goto error;
  }

  /* Captures section: a sparse table of {expr_id, symbol_id_0, ...}
     entries, one per lambda body that has free variables. The total
     count comes first as uint16; each entry is 16-bit length-prefixed
     by the read_table sentinel byte sequence we don't have, so it's
     emitted directly as a count + (length, bytes) entries. */
  {
    uint16_t entry_count = 0;
    READ_CHECK(handle, &entry_count, sizeof(entry_count));
    if(entry_count > 0) {
      program->captures_size = VM_TABLE_SIZE(program->exprv);
      program->captures = VM_MALLOC(program->captures_size *
                                    sizeof(vm_captures_t *));
      if(program->captures == NULL) {
        VM_DEBUG(VM_DEBUG_LOW, "Failed to allocate captures table");
        goto error;
      }
      memset(program->captures, 0,
             program->captures_size * sizeof(vm_captures_t *));

      for(i = 0; i < entry_count; i++) {
        uint16_t entry_length = 0;
        uint16_t expr_id = 0;
        unsigned capture_count;
        unsigned k;
        vm_captures_t *cap;

        READ_CHECK(handle, &entry_length, sizeof(entry_length));
        if(entry_length < 2 || (entry_length % 2) != 0) {
          VM_DEBUG(VM_DEBUG_LOW, "Captures entry length %u invalid",
                   entry_length);
          goto error;
        }
        capture_count = (entry_length - 2) / 2;
        if(capture_count > UINT8_MAX) {
          VM_DEBUG(VM_DEBUG_LOW, "Captures entry %u exceeds limit", i);
          goto error;
        }
        READ_CHECK(handle, &expr_id, sizeof(expr_id));
        if(expr_id >= program->captures_size) {
          VM_DEBUG(VM_DEBUG_LOW, "Captures expr_id %u out of range",
                   (unsigned)expr_id);
          goto error;
        }

        cap = vm_alloc(sizeof(vm_captures_t));
        if(cap == NULL) {
          goto error;
        }
        cap->count = (uint8_t)capture_count;
        cap->symbols = capture_count == 0 ? NULL :
                       vm_alloc(capture_count * sizeof(vm_symbol_id_t));
        if(capture_count > 0 && cap->symbols == NULL) {
          vm_free(cap);
          goto error;
        }
        for(k = 0; k < capture_count; k++) {
          uint16_t sym_id = 0;
          READ_CHECK(handle, &sym_id, sizeof(sym_id));
          cap->symbols[k] = sym_id;
        }
        program->captures[expr_id] = cap;
      }
    }
  }

  if(VM_TABLE_SIZE(program->symbols) > 0) {
    program->symbol_bindings = VM_MALLOC(VM_TABLE_SIZE(program->symbols) *
                                         sizeof(vm_obj_t));
    if(program->symbol_bindings == NULL) {
      VM_DEBUG(VM_DEBUG_LOW, "Failed to allocate local symbol table");
      goto error;
    }

    for(i = 0; i < VM_TABLE_SIZE(program->symbols); i++) {
      program->symbol_bindings[i].type = VM_TYPE_NONE;
      memset(&program->symbol_bindings[i].value, 0, sizeof(vm_obj_value_t));
    }
  } else {
    program->symbol_bindings = NULL;
  }

  for(code_size = i = 0; i < VM_TABLE_SIZE(program->exprv); i++) {
    code_size += VM_TABLE_LENGTH(program->exprv, i);
    VM_DEBUG(VM_DEBUG_MEDIUM, "Form %d: ", i);
#if VM_DEBUG_LEVEL >= VM_DEBUG_MEDIUM
    print_bytecode(VM_TABLE_GET(program->exprv, i),
                   VM_TABLE_LENGTH(program->exprv, i));
#endif
  }

  if(code_size < 1) {
    VM_DEBUG(VM_DEBUG_LOW, "Ignoring program %s because it lacks bytecode",
             name);
    goto error;
  }

  VM_DEBUG(VM_DEBUG_MEDIUM, "Loading bytecode consisting of %u byte%s",
           code_size, code_size != 1 ? "s" : "");

  VM_LOADER_CLOSE(handle);
  return program;

 error:
  VM_LOADER_CLOSE(handle);
  free_program(program);
  return NULL;
}

int
vm_load_program(const char *name)
{
  vm_program_t *program;

  program = read_program(name);
  if(program == NULL) {
    VM_DEBUG(VM_DEBUG_MEDIUM, "Unable to read program %s", name);
    return 0;
  }

  VM_DEBUG(VM_DEBUG_HIGH, "Object size: %u", (unsigned)sizeof(vm_obj_t));
  VM_DEBUG(VM_DEBUG_HIGH, "Frame size: %u", (unsigned)sizeof(vm_expr_t));
  VM_DEBUG(VM_DEBUG_HIGH, "Thread size: %u", (unsigned)sizeof(vm_thread_t));

  if(next_program_id  + 1 < next_program_id) {
    VM_DEBUG(VM_DEBUG_MEDIUM, "Exhausted the program IDs");
    return 0;
  }

  program->flags = 0;

#if VM_INSTRUCTION_PROFILING
  program->exec_count = VM_MALLOC(sizeof(*program->exec_count) *
                                  VM_TABLE_SIZE(program->exprv));
  if(program->exec_count == NULL) {
    VM_DEBUG(VM_DEBUG_MEDIUM, "Unable to allocate profiling data");
    return 0;
  }
  memset(program->exec_count, 0,
         sizeof(*program->exec_count) * VM_TABLE_SIZE(program->exprv));
#endif

  /* Initialize the performance attributes of the program. */
  memset(&program->perf_attr, 0, sizeof(vm_perf_attr_t));
  program->perf_attr.exec_instr_per_invocation = VM_EXEC_INSTR_PER_INVOCATION;
  vm_filter_init(&program->perf_attr.bandwidth, 2);
  vm_filter_init(&program->perf_attr.power, 2);

  if(!vm_native_time(&program->perf_attr.start_time)) {
    return 0;
  }

  VM_TIME_COPY(program->perf_attr.last_comm, program->perf_attr.start_time);

  vm_policy_init_program(program);

  /* Insert the program into the program set and create the first thread. */
  program->next = loaded_programs;
  loaded_programs = program;
  program->program_id = next_program_id++;

  /* Register an LWM2M VM app instance. */
  vm_control_register_app(program);

  return vm_thread_create(program) != NULL;
}

int
vm_unload_program(vm_program_t *program)
{
  vm_program_t *iter;
  vm_program_t *tmp;
  vm_program_t *prev;

  prev = NULL;

  for(iter = loaded_programs; iter != NULL;) {
    if(program == NULL || iter == program) {
      tmp = iter;
      if(iter == loaded_programs) {
        loaded_programs = iter->next;
      } else if(prev != NULL) {
        prev->next = iter->next;
      }

      prev = iter;
      iter = iter->next;

      vm_control_unregister_app(tmp);

      destroy_program_threads(tmp);

#if VM_INSTRUCTION_PROFILING
      VM_PRINTF("%s instruction profiling result (# <form> <executions<)\n",
                tmp->name);
      for(i = 0; i < VM_TABLE_SIZE(tmp->exprv); i++) {
        VM_PRINTF("# %u %lu\n", i, tmp->exec_count[i]);
      }
#endif
      VM_DEBUG(VM_DEBUG_LOW, "Unloaded the program \"%s\"", tmp->name);
      free_program(tmp);
    } else {
      prev = iter;
      iter = iter->next;
    }
  }

  return 1;
}

vm_program_t *
vm_find_program(const char *name)
{
  vm_program_t *program;

  for(program = loaded_programs; program != NULL; program = program->next) {
    if(strcmp(program->name, name) == 0) {
      return program;
    }
  }

  return NULL;
}

vm_program_t *
vm_get_programs(void)
{
  return loaded_programs;
}

#ifdef VM_REPL_ENABLE
/* Register a program object that was constructed outside of
   read_program (e.g. by vm_repl_program_create). The caller has
   already populated name, tables, captures, and symbol_bindings. */
int
vm_loader_register_program(vm_program_t *program)
{
  if(next_program_id + 1 < next_program_id) {
    return 0;
  }
  program->flags = 0;
  memset(&program->perf_attr, 0, sizeof(vm_perf_attr_t));
  program->perf_attr.exec_instr_per_invocation = VM_EXEC_INSTR_PER_INVOCATION;
  vm_filter_init(&program->perf_attr.bandwidth, 2);
  vm_filter_init(&program->perf_attr.power, 2);
  if(!vm_native_time(&program->perf_attr.start_time)) {
    return 0;
  }
  VM_TIME_COPY(program->perf_attr.last_comm, program->perf_attr.start_time);
  vm_policy_init_program(program);
  program->next = loaded_programs;
  loaded_programs = program;
  program->program_id = next_program_id++;
  vm_control_register_app(program);
  return 1;
}
#endif /* VM_REPL_ENABLE */
