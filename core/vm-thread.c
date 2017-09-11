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

#include <stdio.h>
#include <stdlib.h>

#include "vm.h"
#include "vm-id.h"
#include "vm-log.h"
#include "vm-native.h"

#if VM_THREAD_AMOUNT < 1
#error VM_THREAD_AMOUNT must be at least 1
#endif

static vm_id_gen_t thread_idg;
static vm_thread_t *threads[VM_THREAD_AMOUNT];

static void
thread_obj_copy(vm_obj_t *dst, vm_obj_t *src)
{
  memcpy(dst, src, sizeof(vm_obj_t));
}

static void
thread_obj_deallocate(vm_obj_t *obj)
{
}

static void
thread_obj_write(vm_port_t *port, vm_obj_t *obj)
{
  vm_thread_t *thread;

  thread = obj->value.ext_object.opaque_data;
  vm_write(port, "#<thread %ld>", (long)thread->id);
}

static vm_ext_type_t ext_type_thread = {
  .copy = thread_obj_copy,
  .deallocate = thread_obj_deallocate,
  .write = thread_obj_write
};

static int
allocate_thread_id(void)
{
  int i;

  /* Find an available thread ID. */
  for(i = 0; i < VM_THREAD_AMOUNT; i++) {
    if(threads[i] == NULL) {
      return i;
    }
  }

  return -1;
}

static vm_thread_t *
create_thread(vm_thread_t *parent)
{
  int index;
  vm_id_t id;
  vm_thread_t *child;

  child = VM_MALLOC(sizeof(*parent));
  if(child == NULL) {
    VM_DEBUG(VM_DEBUG_MEDIUM, "Unable to allocate a thread");
    return NULL;
  }

  index = allocate_thread_id();
  id = vm_id_new(&thread_idg, index);

  if(index < 0 || id == VM_ID_INVALID) {
    VM_DEBUG(VM_DEBUG_MEDIUM, "Unable to allocate a thread ID");
    VM_FREE(child);
    return NULL;
  }

  memcpy(child, parent, sizeof(vm_thread_t));
  child->exprc = 0;
  child->id = id;

  VM_DEBUG(VM_DEBUG_MEDIUM, "Create a thread with index %u and ID %ld",
           (unsigned)index, (long)child->id);

  parent->program->nthreads++;
  threads[index] = child;

  return child;
}

int
vm_thread_init(void)
{
  vm_ext_type_register(&ext_type_thread);
  return vm_id_create_generator(&thread_idg, VM_THREAD_AMOUNT - 1);
}

void
thread_obj_create(vm_obj_t *obj, vm_thread_t *thread)
{
  obj->type = VM_TYPE_EXTERNAL;
  obj->value.ext_object.type = &ext_type_thread;
  obj->value.ext_object.opaque_data = thread;
}

void
vm_thread_print_ip(vm_thread_t *thread, vm_expr_t *expr)
{
  vm_ip_t base;

  if(expr->expr_id < VM_TABLE_SIZE(thread->program->exprv)) {
    base = VM_TABLE_GET(thread->program->exprv, expr->expr_id);
    VM_PRINTF("%02u:%04u", (unsigned)expr->expr_id,
              (unsigned)(expr->ip - base));
  }
}

void
vm_thread_print_expr(vm_thread_t *thread, vm_expr_t *expr)
{
  const vm_procedure_t *procedure;
  int i;

  procedure = expr->procedure;

  VM_PRINTF("=== Start frame %u ===\n", (unsigned)expr->expr_id);

  if(procedure != NULL) {
    VM_PRINTF("Lambda function (ID %u): ", (unsigned)procedure->expr_id);
  }

  VM_PRINTF("%u/%u args loaded, %u symbols bound\n",
	 (unsigned)expr->eval_arg, (unsigned)expr->argc, (unsigned)expr->bindc);

  VM_PRINTF("IP: ");
  vm_thread_print_ip(thread, expr);

  VM_PRINTF("\nArgument vector: ");
  for(i = 0; i < expr->argc; i++) {
    VM_PRINTF("%d:[", i);
    vm_write_object(NULL, &expr->argv[i]);
    VM_PRINTF("] ");
  }

  VM_PRINTF("\n=== End frame %u ===\n", (unsigned)expr->expr_id);
}

void
vm_thread_print(vm_thread_t *thread)
{
  int i;
  uint8_t bindings, objects;

  VM_PRINTF("*** Thread %u BEGIN ***\n", (unsigned)thread->id);

  VM_PRINTF("Status: %u\n", thread->status);

  for(i = bindings = objects = 0; i < thread->exprc; i++) {
    bindings += thread->exprv[i]->bindc;
    objects += thread->exprv[i]->argc;
  }

  VM_PRINTF("Symbol bindings: %u\n", (unsigned)bindings);

  VM_PRINTF("** %u stack objects\n", (unsigned)objects);

  VM_PRINTF("\n** %u expr\n", (unsigned)thread->exprc);
  for(i = 0; i < thread->exprc; i++) {
    VM_PRINTF("=== Stack context %d ===\n", i);
    vm_thread_print_expr(thread, thread->exprv[i]);
  }

  VM_PRINTF("*** Thread %u END ***\n", (unsigned)thread->id);
}

vm_thread_t *
vm_thread_spawn(vm_thread_t *parent, vm_obj_t *obj)
{
  vm_thread_t *child;

  if(!vm_policy_check_threads(parent)) {
    return NULL;
  }

  child = create_thread(parent);
  if(child == NULL) {
    return NULL;
  }

  /* Set up the thread stack so that the top level expression is
     the form contained in the obj argument. */
  vm_thread_stack_push(child);
  vm_thread_set_expr(child, obj->value.form.id);

  return child;
}

vm_thread_t *
vm_thread_fork(vm_thread_t *parent)
{
  vm_thread_t *child;
  vm_id_index_t index;

  if(!vm_policy_check_threads(parent)) {
    return NULL;
  }

  child = create_thread(parent);
  if(child == NULL) {
    return NULL;
  }

  if(!vm_thread_stack_copy(child, parent)) {
    parent->program->nthreads--;

    index = vm_id_index(&thread_idg, child->id);
    if(index != VM_ID_INDEX_INVALID) {
      threads[index] = NULL;
    }
    VM_FREE(child);
    return NULL;
  }

  return child;
}

int
vm_thread_kill(vm_id_t id)
{
  vm_id_index_t index;

  index = vm_id_index(&thread_idg, id);

  if(index == VM_ID_INDEX_INVALID || threads[index] == NULL) {
    return 0;
  }

  threads[index]->status = VM_THREAD_FINISHED;
  if(VM_GC_AGGRESSIVE) {
    vm_gc();
  }
  VM_DEBUG(VM_DEBUG_LOW, "Thread %u got killed", (unsigned)id);

  return 1;
}

vm_thread_t *
vm_thread_get(vm_id_t id)
{
  vm_id_index_t index;

  index = vm_id_index(&thread_idg, id);

  if(index == VM_ID_INDEX_INVALID) {
    return NULL;
  }

  return threads[index];
}

vm_thread_t *
vm_thread_get_by_index(unsigned index)
{
  if(index >= VM_THREAD_AMOUNT) {
    return NULL;
  }

  return threads[index];
}

vm_id_index_t
vm_thread_get_index(vm_thread_t *thread)
{
  return vm_id_index(&thread_idg, thread->id);
}

vm_thread_t *
vm_thread_create(vm_program_t *program)
{
  vm_thread_t *thread;
  int index;

  thread = VM_MALLOC(sizeof(*thread));
  if(thread != NULL) {
    memset(thread, 0, sizeof(vm_thread_t));

    index = allocate_thread_id();
    thread->id = vm_id_new(&thread_idg, index);
    if(index < 0 || thread->id == VM_ID_INVALID) {
      VM_DEBUG(VM_DEBUG_MEDIUM, "Unable to allocate a thread ID");
      VM_FREE(thread);
      return NULL;
    }

    VM_DEBUG(VM_DEBUG_MEDIUM, "Create a thread with index %u and ID %ld",
             (unsigned)index, (long)thread->id);

    thread->program = program;
    thread->status = VM_THREAD_RUNNABLE;
    thread->error.error_obj.type = VM_TYPE_NONE;
    thread->specific_obj.type = VM_TYPE_NONE;

    /* Initialize the first expr of the thread. */
    vm_thread_stack_push(thread);
    memset(thread->exprv[0], 0, sizeof(vm_expr_t));
    thread->expr->ip = VM_TABLE_GET(program->exprv, 0);
    thread->expr->end = thread->expr->ip + VM_TABLE_LENGTH(program->exprv, 0);

    thread->program->nthreads++;
    threads[index] = thread;
  } else {
    VM_DEBUG(VM_DEBUG_MEDIUM, "Unable to allocate a thread");
  }

  return thread;
}

void
vm_thread_destroy(vm_thread_t *thread)
{
  int i;
  vm_id_index_t index;

  VM_DEBUG(VM_DEBUG_MEDIUM, "Thread %u exited", (unsigned)thread->id);

  index = vm_id_index(&thread_idg, thread->id);
  if(index == VM_ID_INDEX_INVALID) {
    VM_DEBUG(VM_DEBUG_LOW, "Error: thread %lu with index %u cannot be found",
             (unsigned long)thread->id, (unsigned)index);
    return;
  }

  thread->program->nthreads--;
  threads[index] = NULL;
  for(i = 0; i < thread->exprc; i++) {
    vm_thread_stack_free(thread->exprv[i]);
  }
  VM_FREE(thread);

  if(VM_GC_AGGRESSIVE) {
    vm_gc();
  }
}

unsigned
vm_thread_running(void)
{
  int i;

  for(i = 0; i < VM_THREAD_AMOUNT; i++) {
    if(threads[i] != NULL && threads[i]->status == VM_THREAD_RUNNABLE) {
      return 1;
    }
  }

  return 0;
}
