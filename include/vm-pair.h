/*
 * Copyright (c) 2026, RISE Research Institutes of Sweden AB.
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
 */

#ifndef VM_PAIR_H
#define VM_PAIR_H

#include "vm-objects.h"

/*
 * Unified iteration over VM_TYPE_LIST and VM_TYPE_PAIR.
 *
 * List values flow through the primitive set in two representations:
 *   - The legacy vm_list_t wrapper (VM_TYPE_LIST) with an internal
 *     vm_list_item_t chain and a tail-cache pointer.
 *   - The R7RS-conformant cons-pair representation (VM_TYPE_PAIR) where
 *     a list is a chain of vm_pair_t ending in the empty list.
 *
 * Primitives that traverse a list (map, filter, reverse, append, memq, ...)
 * should use vm_list_walker_t instead of branching on input type
 * themselves. This localizes the LIST-vs-PAIR distinction in one file.
 *
 * Typical use:
 *
 *   vm_list_walker_t w;
 *   vm_obj_t *car;
 *   int status;
 *
 *   if(!vm_list_walker_init(&w, &argv[0])) {
 *     vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
 *     return;
 *   }
 *   while((status = vm_list_walker_next(&w, &car)) == 1) {
 *     // process *car
 *   }
 *   if(status < 0) {
 *     // improper list -- terminator value is available via
 *     // vm_list_walker_terminator if needed.
 *   }
 */

typedef struct vm_list_walker {
  /* For VM_TYPE_LIST walks: the underlying list and the current item.
     list_item == NULL means we've stepped past the end. */
  vm_list_t *list_root;
  vm_list_item_t *list_item;

  /* For VM_TYPE_PAIR walks: the current pair-or-terminator object.
     pair_cur.type != VM_TYPE_PAIR means we've stepped past the end;
     pair_cur is then the terminator (empty list for proper, anything
     else for improper). */
  vm_obj_t pair_cur;

  /* Which representation we're walking. */
  vm_obj_type_t kind;

  /* Set after walker_next yields a value so walker_current_pair can
     return the suffix-from-here for memq-style return semantics. */
  vm_obj_t suffix_obj;
} vm_list_walker_t;

/* Initialize the walker on obj. Returns 1 if obj is a list or pair
   (possibly empty -- walker_next will then return 0 immediately).
   Returns 0 if obj is some other type; in that case the walker is
   left in an unspecified state and the caller should signal a type
   error. */
int vm_list_walker_init(vm_list_walker_t *w, const vm_obj_t *obj);

/* Step the walker. On success, sets *car_out to point at the current
   car field. The pointer is stable as long as the underlying pair or
   list-item is not freed or mutated. Returns:
     1  - element yielded; another step may follow.
     0  - end of structure reached (clean proper-list terminator).
    -1  - improper-list terminator (cdr was non-nil non-empty-list);
          the terminator object is available via walker_terminator. */
int vm_list_walker_next(vm_list_walker_t *w, vm_obj_t **car_out);

/* After walker_next returns 0 or -1, copies the terminator value to
   *out. For proper lists this is the empty list (a VM_TYPE_LIST with
   length 0); for improper this is the non-list cdr value. */
void vm_list_walker_terminator(const vm_list_walker_t *w, vm_obj_t *out);

/* After walker_next has yielded a value (returned 1), copies into
   *out the list-or-pair object whose car is the most recently yielded
   element. This is the suffix-from-here that memq/assq return on a
   match. For pair walks this is the current pair; for list walks
   this is currently the input list as a whole (a known wrong-ish
   semantics that the existing memq/assq workaround by allocating a
   fresh suffix list; primitives that need exact R5RS semantics on
   VM_TYPE_LIST input should continue to do so). */
void vm_list_walker_current(const vm_list_walker_t *w, vm_obj_t *out);

/* Compute the length of obj as a proper or improper list.
   On success returns the number of car elements before the terminator
   in *length_out, and 0 if proper / 1 if improper. Returns -1 if obj
   is neither VM_TYPE_LIST nor VM_TYPE_PAIR. */
int vm_list_length_walk(const vm_obj_t *obj, vm_integer_t *length_out);

/* Predicates that hide the LIST-vs-PAIR distinction. */

/* pair? -- true for any cons cell (R5RS §6.3.2). Empty list is not
   a pair. NULL pair pointer (defensive) is not a pair. */
int vm_obj_is_pair(const vm_obj_t *obj);

/* null? -- true only for the empty list. VM_TYPE_PAIR is never null. */
int vm_obj_is_null(const vm_obj_t *obj);

/* list? -- true for the empty list and for any pair chain that
   terminates in the empty list. Cycle detection is deferred to a
   follow-up; until set-cdr! is used to build a cycle this is safe. */
int vm_obj_is_proper_list(const vm_obj_t *obj);

/*
 * Pair builder -- C-local helper for primitives that incrementally
 * construct a pair-based list left-to-right.
 *
 * Usage:
 *   vm_pair_builder_t b;
 *   vm_pair_builder_init(&b);
 *   for(...) {
 *     if(!vm_pair_builder_append(&b, &some_obj)) {
 *       // allocation failed; the partially-built chain becomes
 *       // unreferenced garbage at the next GC.
 *       vm_signal_error(thread, VM_ERROR_HEAP);
 *       return;
 *     }
 *   }
 *   vm_pair_builder_result(&b, &thread->result);
 *
 * The builder keeps the head and tail pair pointers on the C stack
 * so the tail-cache is not paid for in the resulting list's
 * lifetime. Each append is O(1). An empty builder yields the empty
 * list as a VM_TYPE_LIST of length 0 (so existing primitives that
 * test (null? ...) on the result continue to work).
 */
typedef struct vm_pair_builder {
  vm_pair_t *head;
  vm_pair_t *tail;
} vm_pair_builder_t;

void vm_pair_builder_init(vm_pair_builder_t *b);
int  vm_pair_builder_append(vm_pair_builder_t *b, const vm_obj_t *obj);
void vm_pair_builder_result(const vm_pair_builder_t *b, vm_obj_t *out);

#endif /* !VM_PAIR_H */
