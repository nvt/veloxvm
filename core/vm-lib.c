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

#include "vm-config.h"
#include "vm-lib.h"
#include "vm-log.h"

extern vm_lib_t vm_lib_complex;

static vm_lib_t *libs;

static vm_lib_t *
find_lib(const char *name)
{
  vm_lib_t *lib;

  for(lib = libs; lib != NULL; lib = lib->next) {
    if(strcmp(name, lib->name) == 0) {
      return lib;
    }
  }

  return NULL;
}

int
vm_lib_init(void)
{
  return vm_lib_register(&vm_lib_complex);
}

int
vm_lib_register(vm_lib_t *lib)
{
  int i;

  if(lib->name == 0 || lib->symbol_count == 0) {
    VM_DEBUG(VM_DEBUG_MEDIUM, "Reject loading of empty lib");
    return 0;
  }

  lib->next = libs;
  libs = lib;

  lib->ref_count = 0;

  if(VM_DEBUG_LEVEL >= VM_DEBUG_MEDIUM) {
    VM_PRINTF("Register lib \"%s\" with %d operators:\n",
              lib->name, (int)lib->symbol_count);
    for(i = 0; i < lib->symbol_count; i++) {
      VM_PRINTF(" %s\n", lib->symbols[i]);
    }
  }

  return 1;
}

int
vm_lib_bind_symbol(vm_program_t *program, const char *name, vm_obj_t *obj)
{
  unsigned j;
  const char *symbol;

  for(j = 0; j < VM_TABLE_SIZE(program->symbols); j++) {
    symbol = (const char *)VM_TABLE_GET(program->symbols, j);
    if(symbol != NULL && strcasecmp(name, symbol) == 0) {
      VM_DEBUG(VM_DEBUG_MEDIUM, "Found reference to library symbol %s",
               symbol);
      program->symbol_bindings[j].type = obj->type;
      program->symbol_bindings[j].value = obj->value;
      return 1;
    }
  }

  return 0;
}

int
vm_lib_load(vm_program_t *program, const char *name)
{
  vm_lib_t *lib;
  int i;
  vm_obj_t obj;

  VM_PRINTF("Load library %s\n", name);
  lib = find_lib(name);
  if(lib == NULL) {
    return 0;
  }

  /* Perform the library specific loading as well. */
  if(lib->load(program) == 0) {
    return 0;
  }

  /* Bind all program-scope symbols that are unbound and match the
     symbols provided by the library. */
  obj.type = VM_TYPE_PROCEDURE;
  for(i = 0; i < lib->symbol_count; i++) {
    VM_PRINTF(" %s\n", lib->symbols[i]);
    obj.value.procedure = &lib->operators[i];
    vm_lib_bind_symbol(program, lib->symbols[i], &obj);
  }

  lib->ref_count++;

  return 1;
}

int
vm_lib_unload(vm_program_t *program, const char *name)
{
  vm_lib_t *lib;

  lib = find_lib(name);
  if(lib == NULL) {
    return 0;
  }

  if(lib->ref_count == 0) {
    return 0;
  }

  lib->ref_count--;

  return 1;
}
