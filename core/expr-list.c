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

#include "vm-functions.h"
#include "vm-log.h"
#include "vm-pair.h"

static void
find_member_in_list(vm_thread_t *thread, vm_integer_t argc, vm_obj_t *argv,
	    int (*compare)(vm_thread_t *, vm_obj_t *, vm_obj_t *))
{
  vm_obj_t cur;

  if(argv[1].type == VM_TYPE_NIL) {
    VM_PUSH_BOOLEAN(VM_FALSE);
    return;
  }
  if(argv[1].type != VM_TYPE_PAIR) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  /* memq/memv/member return the suffix of the input list starting
     at the matching pair (R5RS §6.3.2). The pair's identity is
     preserved -- we return the current pair value directly. */
  cur = argv[1];
  while(cur.type == VM_TYPE_PAIR && cur.value.pair != NULL) {
    if(compare(thread, &argv[0], &cur.value.pair->car)) {
      thread->result = cur;
      return;
    }
    cur = cur.value.pair->cdr;
  }

  VM_PUSH_BOOLEAN(VM_FALSE);
}

static void
find_member_in_assoc_list(vm_thread_t *thread,
            vm_integer_t argc, vm_obj_t *argv,
	    int (*compare)(vm_thread_t *, vm_obj_t *, vm_obj_t *))
{
  vm_obj_t *entry;
  vm_obj_t cur;

  if(argv[1].type == VM_TYPE_NIL) {
    VM_PUSH_BOOLEAN(VM_FALSE);
    return;
  }
  if(argv[1].type != VM_TYPE_PAIR) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  /* Walk the cdr chain. Each entry is itself a pair (key . value).
     On match, return the entry (the matched pair object),
     preserving the R5RS/R7RS contract that mutation through the
     returned value is visible in the alist. */
  cur = argv[1];
  while(cur.type == VM_TYPE_PAIR && cur.value.pair != NULL) {
    entry = &cur.value.pair->car;
    if(entry->type != VM_TYPE_PAIR || entry->value.pair == NULL) {
      vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
      return;
    }
    if(compare(thread, &argv[0], &entry->value.pair->car)) {
      thread->result = *entry;
      return;
    }
    cur = cur.value.pair->cdr;
  }
  VM_PUSH_BOOLEAN(VM_FALSE);
}

VM_FUNCTION(list)
{
  vm_pair_builder_t b;
  vm_integer_t i;

  /* (list a b c) constructs a fresh pair chain (a . (b . (c . ()))).
     Uses the C-local pair builder so allocation is O(N) with N=argc
     and the result is a VM_TYPE_PAIR chain rather than the legacy
     VM_TYPE_LIST wrapper. */
  vm_gc_disable();
  vm_pair_builder_init(&b);
  for(i = 0; i < argc; i++) {
    if(!vm_pair_builder_append(&b, &argv[i])) {
      vm_gc_enable();
      vm_signal_error(thread, VM_ERROR_HEAP);
      return;
    }
  }
  vm_pair_builder_result(&b, &thread->result);
  vm_gc_enable();
}

VM_FUNCTION(cons)
{
  vm_pair_t *p;

  /* R7RS cons: O(1) pair allocation. The previous VM_TYPE_LIST
     implementation deep-copied the tail (vm_list_copy) to avoid
     aliasing with the source, producing O(N) cost per cons of an
     N-element tail and O(N^2) for N consecutive cons calls building
     a list. The pair representation shares the cdr by reference;
     set-car!/set-cdr! mutation is observable through every reference
     to the pair, which is the standard Scheme semantics that the
     wrapper representation could not satisfy. */
  p = vm_alloc(sizeof(vm_pair_t));
  if(p == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
    return;
  }
  p->car = argv[0];
  p->cdr = argv[1];
  VM_PUSH_PAIR(p);
}

VM_FUNCTION(car)
{
  if(argv[0].type != VM_TYPE_PAIR || argv[0].value.pair == NULL) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }
  VM_PUSH(&argv[0].value.pair->car);
}

VM_FUNCTION(cdr)
{
  if(argv[0].type != VM_TYPE_PAIR || argv[0].value.pair == NULL) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }
  /* R7RS pair cdr: return the cdr field's value directly, preserving
     identity. (eq? p (cdr (cons a p))) -> #t. */
  VM_PUSH(&argv[0].value.pair->cdr);
}

VM_FUNCTION(list_ref)
{
  vm_list_walker_t walker;
  vm_obj_t *car;
  vm_integer_t k;
  vm_integer_t i;
  int status;

  if(argv[1].type != VM_TYPE_INTEGER) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }
  k = argv[1].value.integer;

  if(!vm_list_walker_init(&walker, &argv[0])) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  for(i = 0; i <= k; i++) {
    status = vm_list_walker_next(&walker, &car);
    if(status != 1) {
      /* Hit the terminator before reaching index k -- out of range. */
      vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
      vm_set_error_object(thread, &argv[1]);
      return;
    }
  }
  VM_PUSH(car);
}

VM_FUNCTION(list_tail)
{
  vm_integer_t k;
  vm_obj_t cur;

  if(argv[1].type != VM_TYPE_INTEGER) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }
  k = argv[1].value.integer;
  if(k < 0) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    vm_set_error_object(thread, &argv[1]);
    return;
  }

  /* Walk the cdr chain k times; the kth cdr is the tail. R5RS
     preserves identity for (list-tail lst 0) == lst, which falls
     out naturally because we return the current value directly. */
  cur = argv[0];
  while(k > 0) {
    if(cur.type != VM_TYPE_PAIR || cur.value.pair == NULL) {
      vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
      vm_set_error_object(thread, &argv[1]);
      return;
    }
    cur = cur.value.pair->cdr;
    k--;
  }
  thread->result = cur;
}

/* Python-style normalisation: negative indices count from the end,
   then both ends are clamped to [0, length] and start to <= end. */
static void
normalize_slice_bounds(vm_integer_t length,
                       vm_integer_t *start, vm_integer_t *end)
{
  if(*start < 0) {
    *start += length;
    if(*start < 0) {
      *start = 0;
    }
  }
  if(*end < 0) {
    *end += length;
    if(*end < 0) {
      *end = 0;
    }
  }
  if(*start > length) {
    *start = length;
  }
  if(*end > length) {
    *end = length;
  }
  if(*start > *end) {
    *start = *end;
  }
}

VM_FUNCTION(slice)
{
  vm_integer_t start;
  vm_integer_t end;
  vm_integer_t length;

  if(argv[1].type != VM_TYPE_INTEGER || argv[2].type != VM_TYPE_INTEGER) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  start = argv[1].value.integer;
  end = argv[2].value.integer;

  /* Handle lists, strings, and vectors */
  if(argv[0].type == VM_TYPE_STRING) {
    /* String slicing */
    vm_string_t *string;
    vm_string_t *result_string;

    string = argv[0].value.string;
    /* String-table-backed literals carry a string_id rather than an
       inline char buffer; resolve once so ->str points at the real
       bytes before we memcpy. Without this the resulting substring
       comes from a stale or NULL pointer. */
    if(vm_string_resolve(thread, string) == NULL) {
      vm_signal_error(thread, VM_ERROR_STRING_ID);
      return;
    }
    length = string->length;
    normalize_slice_bounds(length, &start, &end);

    /* Create substring */
    result_string = vm_string_create(&thread->result, end - start, NULL);
    if(result_string == NULL) {
      vm_signal_error(thread, VM_ERROR_HEAP);
      return;
    }

    memcpy(result_string->str, string->str + start, end - start);
    result_string->str[result_string->length] = '\0';
    return;
  } else if(argv[0].type == VM_TYPE_PAIR ||
            argv[0].type == VM_TYPE_NIL) {
    /* List/pair slicing. Walks the input uniformly via the walker
       and constructs the result as a pair chain via the builder. */
    vm_pair_builder_t b;
    vm_list_walker_t walker;
    vm_obj_t *car;
    vm_integer_t i;
    int status;

    if(vm_list_length_walk(&argv[0], &length) < 0) {
      vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
      return;
    }
    normalize_slice_bounds(length, &start, &end);

    vm_gc_disable();
    vm_pair_builder_init(&b);

    if(!vm_list_walker_init(&walker, &argv[0])) {
      vm_gc_enable();
      vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
      return;
    }

    /* Skip to start position */
    for(i = 0; i < start; i++) {
      if(vm_list_walker_next(&walker, &car) != 1) {
        break;
      }
    }

    /* Copy elements from start to end */
    for(i = start; i < end; i++) {
      status = vm_list_walker_next(&walker, &car);
      if(status != 1) {
        break;
      }
      if(!vm_pair_builder_append(&b, car)) {
        vm_gc_enable();
        vm_signal_error(thread, VM_ERROR_HEAP);
        return;
      }
    }

    vm_pair_builder_result(&b, &thread->result);
    vm_gc_enable();
  } else if(argv[0].type == VM_TYPE_VECTOR) {
    /* Vector slicing. Buffer-flagged vectors (R7RS bytevectors) and
       regular element-vectors share this path but use different
       backing storage; preserve the input's flag in the result so
       (buffer? (slice b ...)) stays #t. */
    vm_vector_t *input_vector;
    vm_vector_t *result_vector;
    vm_vector_flags_t flags;
    vm_integer_t result_length;
    vm_integer_t i;

    input_vector = argv[0].value.vector;
    length = input_vector->length;
    normalize_slice_bounds(length, &start, &end);
    result_length = end - start;

    flags = VM_IS_SET(input_vector->flags, VM_VECTOR_FLAG_BUFFER)
            ? VM_VECTOR_FLAG_BUFFER : VM_VECTOR_FLAG_REGULAR;
    result_vector = vm_vector_create(&thread->result, result_length, flags);
    if(result_vector == NULL) {
      vm_signal_error(thread, VM_ERROR_HEAP);
      return;
    }

    /* Copy from whichever backing array the input uses. Buffers store
       bytes in ->bytes; regular vectors store vm_obj_t in ->elements.
       (Buffers leave ->elements NULL, so the original code segfaulted
       when this path was taken on a bytevector.) */
    if(VM_IS_SET(input_vector->flags, VM_VECTOR_FLAG_BUFFER)) {
      memcpy(result_vector->bytes,
             input_vector->bytes + start, result_length);
    } else {
      for(i = 0; i < result_length; i++) {
        result_vector->elements[i] = input_vector->elements[start + i];
      }
    }
  } else {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
  }
}

VM_FUNCTION(append)
{
  vm_pair_builder_t b;
  vm_list_walker_t walker;
  vm_obj_t *car;
  vm_integer_t i;
  int status;

  /* Concatenate every input list/pair into a fresh pair chain.
     Currently always copies every input (including the last), which
     is conservative vs. R5RS's allow-sharing-of-last semantics. */
  vm_gc_disable();
  vm_pair_builder_init(&b);

  for(i = 0; i < argc; i++) {
    if(!vm_list_walker_init(&walker, &argv[i])) {
      vm_gc_enable();
      vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
      return;
    }
    while((status = vm_list_walker_next(&walker, &car)) == 1) {
      if(!vm_pair_builder_append(&b, car)) {
        vm_gc_enable();
        vm_signal_error(thread, VM_ERROR_HEAP);
        return;
      }
    }
    if(status < 0 && i < argc - 1) {
      /* Improper-list terminator in any non-last argument is an
         error (R5RS): all but the last input must be proper lists. */
      vm_gc_enable();
      vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
      return;
    }
  }
  vm_pair_builder_result(&b, &thread->result);
  vm_gc_enable();
}

VM_FUNCTION(remove)
{
  vm_pair_builder_t b;
  vm_list_walker_t walker;
  vm_obj_t *car;
  int status;

  /* Build a fresh pair chain of elements that don't match argv[0].
     R5RS / SRFI-1 specify remove as non-destructive; the previous
     in-place mutation corrupted any other reference to the same
     list (notably quoted-list constants, which the compiler reuses
     across uses). */
  if(!vm_list_walker_init(&walker, &argv[1])) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  vm_gc_disable();
  vm_pair_builder_init(&b);

  while((status = vm_list_walker_next(&walker, &car)) == 1) {
    if(!vm_objects_deep_equal(thread, car, &argv[0])) {
      if(!vm_pair_builder_append(&b, car)) {
        vm_gc_enable();
        vm_signal_error(thread, VM_ERROR_HEAP);
        return;
      }
    }
  }

  vm_gc_enable();

  if(status < 0) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  vm_pair_builder_result(&b, &thread->result);
}

VM_FUNCTION(reverse)
{
  vm_list_walker_t walker;
  vm_obj_t *car;
  vm_obj_t result;
  vm_pair_t *p;
  int status;

  /* Builds a fresh pair chain by prepending each input car. With
     pair-based output, prepending is the natural operation -- O(1)
     per element via a fresh pair pointing at the previous result. */
  if(!vm_list_walker_init(&walker, &argv[0])) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  vm_gc_disable();
  result.type = VM_TYPE_NIL;
  /* The first iteration copies result into p->cdr as the chain
     terminator, so the union must not hold an indeterminate value. */
  result.value.pair = NULL;

  while((status = vm_list_walker_next(&walker, &car)) == 1) {
    p = vm_alloc(sizeof(vm_pair_t));
    if(p == NULL) {
      vm_gc_enable();
      vm_signal_error(thread, VM_ERROR_HEAP);
      return;
    }
    p->car = *car;
    p->cdr = result;
    result.type = VM_TYPE_PAIR;
    result.value.pair = p;
  }
  vm_gc_enable();

  if(status < 0) {
    /* Improper list: reverse is undefined on these per R5RS. */
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  thread->result = result;
}

VM_FUNCTION(length)
{
  vm_integer_t n;
  vm_obj_t obj;

  if(argv[0].type == VM_TYPE_NIL) {
    VM_PUSH_INTEGER(0);
    return;
  }
  if(argv[0].type == VM_TYPE_STRING) {
    VM_PUSH_INTEGER(argv[0].value.string->length);
    return;
  }
  if(argv[0].type == VM_TYPE_PAIR) {
    n = 0;
    obj = argv[0];
    while(obj.type == VM_TYPE_PAIR && obj.value.pair != NULL) {
      n++;
      obj = obj.value.pair->cdr;
    }
    if(obj.type == VM_TYPE_NIL) {
      VM_PUSH_INTEGER(n);
      return;
    }
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }
  vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
}

VM_FUNCTION(nullp)
{
  VM_PUSH_BOOLEAN(argv[0].type == VM_TYPE_NIL);
}

VM_FUNCTION(listp)
{
  vm_obj_t obj;

  /* R5RS §6.3.2: list? is #t for proper lists -- structures
     terminating in the empty list. The empty list itself is a
     proper list. */
  if(argv[0].type == VM_TYPE_NIL) {
    VM_PUSH_BOOLEAN(VM_TRUE);
    return;
  }
  if(argv[0].type != VM_TYPE_PAIR) {
    VM_PUSH_BOOLEAN(VM_FALSE);
    return;
  }
  obj = argv[0];
  while(obj.type == VM_TYPE_PAIR && obj.value.pair != NULL) {
    obj = obj.value.pair->cdr;
  }
  VM_PUSH_BOOLEAN(obj.type == VM_TYPE_NIL);
}

VM_FUNCTION(pairp)
{
  VM_PUSH_BOOLEAN(argv[0].type == VM_TYPE_PAIR &&
                  argv[0].value.pair != NULL);
}

VM_FUNCTION(set_car)
{
  if(argv[0].type != VM_TYPE_PAIR || argv[0].value.pair == NULL) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }
  argv[0].value.pair->car = argv[1];
}

VM_FUNCTION(set_cdr)
{
  if(argv[0].type != VM_TYPE_PAIR || argv[0].value.pair == NULL) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }
  /* R7RS set-cdr! semantics: assign directly to the pair's cdr field.
     Any other reference to this pair sees the mutation. */
  argv[0].value.pair->cdr = argv[1];
}

VM_FUNCTION(memq)
{
  find_member_in_list(thread, argc, argv, vm_objects_equal);
}

VM_FUNCTION(memv)
{
  find_member_in_list(thread, argc, argv, vm_objects_equal);
}

VM_FUNCTION(member)
{
  find_member_in_list(thread, argc, argv, vm_objects_deep_equal);
}

VM_FUNCTION(assq)
{
  if(!VM_EVAL_ARG_DONE(thread, 1)) {
    VM_EVAL_ARG(thread, 0);
    VM_EVAL_ARG(thread, 1);
  } else {
    find_member_in_assoc_list(thread, argc, argv, vm_objects_equal);
  }
}

VM_FUNCTION(assv)
{
  if(!VM_EVAL_ARG_DONE(thread, 1)) {
    VM_EVAL_ARG(thread, 0);
    VM_EVAL_ARG(thread, 1);
  } else {
    find_member_in_assoc_list(thread, argc, argv, vm_objects_equal);
  }
}

VM_FUNCTION(assoc)
{
  if(!VM_EVAL_ARG_DONE(thread, 1)) {
    VM_EVAL_ARG(thread, 0);
    VM_EVAL_ARG(thread, 1);
  } else {
    find_member_in_assoc_list(thread, argc, argv, vm_objects_deep_equal);
  }
}

VM_FUNCTION(list_enumerate)
{
  vm_pair_builder_t b;
  vm_pair_t *inner;
  vm_list_walker_t walker;
  vm_obj_t *car;
  vm_obj_t inner_obj;
  vm_integer_t index;
  int status;

  if(!vm_list_walker_init(&walker, &argv[0])) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  vm_gc_disable();
  vm_pair_builder_init(&b);

  /* Iterate through input, creating (index . element) pairs as
     R7RS-style vm_pair_t (cdr holds element directly, not wrapped
     in a one-element list). Outer result is a fresh pair chain. */
  index = 0;
  while((status = vm_list_walker_next(&walker, &car)) == 1) {
    inner = vm_alloc(sizeof(vm_pair_t));
    if(inner == NULL) {
      vm_gc_enable();
      vm_signal_error(thread, VM_ERROR_HEAP);
      return;
    }
    inner->car.type = VM_TYPE_INTEGER;
    inner->car.value.integer = index;
    inner->cdr = *car;

    inner_obj.type = VM_TYPE_PAIR;
    inner_obj.value.pair = inner;
    if(!vm_pair_builder_append(&b, &inner_obj)) {
      vm_gc_enable();
      vm_signal_error(thread, VM_ERROR_HEAP);
      return;
    }

    index++;
  }

  vm_gc_enable();

  if(status < 0) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  vm_pair_builder_result(&b, &thread->result);
}

VM_FUNCTION(list_zip)
{
  vm_pair_builder_t b;
  vm_pair_t *inner;
  vm_list_walker_t walker1, walker2;
  vm_obj_t *car1, *car2;
  vm_obj_t inner_obj;
  int status1, status2;

  if(!vm_list_walker_init(&walker1, &argv[0]) ||
     !vm_list_walker_init(&walker2, &argv[1])) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  vm_gc_disable();
  vm_pair_builder_init(&b);

  /* Iterate both inputs in lockstep; stop at the shorter. Result
     entries are R7RS-style vm_pair_t (cdr is the second element
     directly). */
  for(;;) {
    status1 = vm_list_walker_next(&walker1, &car1);
    status2 = vm_list_walker_next(&walker2, &car2);
    if(status1 != 1 || status2 != 1) {
      break;
    }

    inner = vm_alloc(sizeof(vm_pair_t));
    if(inner == NULL) {
      vm_gc_enable();
      vm_signal_error(thread, VM_ERROR_HEAP);
      return;
    }
    inner->car = *car1;
    inner->cdr = *car2;

    inner_obj.type = VM_TYPE_PAIR;
    inner_obj.value.pair = inner;
    if(!vm_pair_builder_append(&b, &inner_obj)) {
      vm_gc_enable();
      vm_signal_error(thread, VM_ERROR_HEAP);
      return;
    }
  }

  vm_pair_builder_result(&b, &thread->result);
  vm_gc_enable();
}

VM_FUNCTION(list_index)
{
  vm_list_walker_t walker;
  vm_obj_t *car;
  vm_obj_t *target;
  vm_integer_t index;

  if(!vm_list_walker_init(&walker, &argv[1])) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  target = &argv[0];
  index = 0;
  while(vm_list_walker_next(&walker, &car) == 1) {
    if(vm_objects_deep_equal(thread, target, car)) {
      VM_PUSH_INTEGER(index);
      return;
    }
    index++;
  }

  /* Not found -- return -1 */
  VM_PUSH_INTEGER(-1);
}
