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
#include "vm-mempool.h"

#define MAX_STACK_FRAMES (VM_FRAME_POOL_SIZE / sizeof(vm_expr_t))

static vm_mempool_t frame_pool;

static void
initialize_frame(vm_expr_t *expr)
{
  memset(expr, 0, sizeof(vm_expr_t));
  expr->procedure = NULL;
  expr->bindv = NULL;
}

int
vm_thread_stack_create(void)
{
  return vm_mempool_create(&frame_pool, sizeof(vm_expr_t), MAX_STACK_FRAMES);
}

void
vm_thread_stack_destroy(void)
{
  vm_mempool_destroy(&frame_pool);
}

vm_expr_t *
vm_thread_stack_push(vm_thread_t *thread)
{
  vm_expr_t *expr;

  if(thread->exprc == VM_CONTEXT_STACK_SIZE) {
    vm_signal_error(thread, VM_ERROR_STACK_OVERFLOW);
  } else {
    expr = vm_mempool_alloc(&frame_pool);
    if(expr == NULL) {
      vm_signal_error(thread, VM_ERROR_HEAP);
    } else {
      initialize_frame(expr);
      thread->exprv[thread->exprc++] = expr;
      thread->expr = expr;
      return expr;
    }
  }
  return NULL;
}

void
vm_thread_stack_pop(vm_thread_t *thread)
{
  if(thread->exprc == 1) {
    /* We can reach stack overflows despite executing correct bytecode,
       but stack underflows indicate that there is something
       wrong with the VM. */
    vm_signal_error(thread, VM_ERROR_INTERNAL);
  } else {
    if(IS_CLEAR(thread->expr->flags, VM_EXPR_SAVE_FRAME)) {
      if(thread->expr->bindv != NULL) {
        VM_FREE(thread->expr->bindv);
      }
      vm_mempool_free(&frame_pool, thread->expr);
    }
    thread->exprc--;
    thread->expr = thread->exprv[thread->exprc - 1];
  }
}

vm_expr_t *
vm_thread_stack_alloc(vm_thread_t *thread)
{
  vm_expr_t *expr;

  expr = vm_mempool_alloc(&frame_pool);
  if(expr != NULL) {
    initialize_frame(expr);
  } else {
    vm_signal_error(thread, VM_ERROR_HEAP);
  }
  return expr;
}

void
vm_thread_stack_free(vm_expr_t *frame)
{
  if(frame->bindv != NULL) {
    VM_FREE(frame->bindv);
  }
  vm_mempool_free(&frame_pool, frame);
}

int
vm_thread_stack_copy(vm_thread_t *dst, vm_thread_t *src)
{
  int i;

  for(i = 0; i < src->exprc; i++) {
    dst->exprv[i] = vm_mempool_alloc(&frame_pool);
    if(dst->exprv[i] == NULL) {
      /* If an allocation failed, all previous allocations should be undone. */
      while(--i >= 0) {
        vm_mempool_free(&frame_pool, dst->exprv[i]);
      }
      return 0;
    }
    memcpy(dst->exprv[i], src->exprv[i], sizeof(vm_expr_t));
  }

  dst->expr = dst->exprv[0];

  return 1;
}
