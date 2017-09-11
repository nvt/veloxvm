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
 */

#ifndef VM_MACROS_H
#define VM_MACROS_H

/* Generic macros that enhance the readibility of the source code. */
#undef IS_SET
#define IS_SET(field, flag) ((field) & (flag))
#undef IS_CLEAR
#define IS_CLEAR(field, flag) (!((field) & (flag)))
#undef SET
#define SET(field, flag) ((field) |= (flag))
#undef CLEAR
#define CLEAR(field, flag) ((field) &= ~(flag))

#ifndef ARRAY_SIZE
#define ARRAY_SIZE(a) (sizeof(a) / sizeof((a)[0]))
#endif

#ifndef VM_MIN
#define VM_MIN(a, b) ((a) < (b) ? (a) : (b))
#endif

#ifndef VM_MAX
#define VM_MAX(a, b) ((a) < (b) ? (b) : (a))
#endif

#define _VM_MAKE_STRING(str) #str
#define VM_MAKE_STRING(str) _VM_MAKE_STRING(str)

#define VM_TIME_COPY(dst_time, src_time) \
  do {                                   \
    (dst_time).sec = (src_time).sec;     \
    (dst_time).msec = (src_time).msec;   \
  } while(0)

/*
 * Macros that help us control the execution order of arguments in
 * expressions. Whilst the regular order is to evaluate arguments
 * from left to right, a few core functions use different orders,
 * which are typically dependent on the values of certain arguments.
 *
 * In some of the macros, we shift by 2U instead of by 1U because they
 * are used by operators, which receive an argument vector without the
 * initial element representing the operator symbol.
 */
#define VM_EVAL_ARG(thread, arg) ((thread)->expr->eval_requested |= 2U << (arg))
#define VM_EVAL_STOP(thread) ((thread)->expr->eval_completed = ~0U)
#define VM_EVAL_ARG_DONE(thread, arg) \
  ((thread)->expr->eval_completed & (2U << (arg)))
#define VM_EVAL_REQUESTED(thread, arg) \
  ((thread)->expr->eval_requested & (1U << (arg)))
#define VM_EVAL_COMPLETED(thread, arg) \
  ((thread)->expr->eval_completed & (1U << (arg)))
#define VM_EVAL_SET_REQUESTED(thread, arg) \
  ((thread)->expr->eval_requested |= (1U << (arg)))
#define VM_EVAL_SET_REQUESTED_RANGE(thread, from, to) \
  ((thread)->expr->eval_requested = ~((1U << (from)) - 1) & ((1U << (to)) - 1))
#define VM_EVAL_SET_COMPLETED(thread, arg) \
  ((thread)->expr->eval_completed |= (1U << (arg)))
#define VM_EVAL_COMPLETED_ALL(thread) \
  ((thread->expr->eval_requested & (thread)->expr->eval_completed) == \
  (thread)->expr->eval_requested)


/* Function call result macros. */
#define VM_POP_BOOLEAN(x) thread->result.value.boolean

#define VM_PUSH(x) \
                do { \
                  memcpy(&thread->result, (x), sizeof(vm_obj_t)); \
                } while(0)

#define VM_PUSH_BOOLEAN(x) \
                do { \
                  thread->result.type = VM_TYPE_BOOLEAN; \
                  thread->result.value.boolean = (x); \
                } while (0)

#define VM_PUSH_INTEGER(x) \
                do { \
                  thread->result.type = VM_TYPE_INTEGER; \
                  thread->result.value.integer = (x); \
                } while (0)

#define VM_PUSH_RATIONAL(x) \
                do { \
                  thread->result.type = VM_TYPE_RATIONAL; \
                  thread->result.value.rational = (x); \
                } while (0)

#define VM_PUSH_REAL(x) \
                do { \
                  thread->result.type = VM_TYPE_REAL; \
                  thread->result.value.real = (x); \
                } while (0)

#define VM_PUSH_LIST(x) \
                do { \
                  thread->result.type = VM_TYPE_LIST; \
                  thread->result.value.list = (x); \
                } while (0)

#define VM_PUSH_CHARACTER(x) \
                do { \
                  thread->result.type = VM_TYPE_CHARACTER; \
                  thread->result.value.character = (x); \
                } while (0)

#define VM_PUSH_PORT(x) \
                do { \
                  thread->result.type = VM_TYPE_PORT; \
                  thread->result.value.port = (x); \
                } while (0)

/* Debugging macro. */
#define VM_DEBUG_FORCE   0
#define VM_DEBUG_LOW     1
#define VM_DEBUG_MEDIUM  5
#define VM_DEBUG_HIGH   10

#ifndef VM_DEBUG_LEVEL
#define VM_DEBUG_LEVEL VM_DEBUG_NONE
#endif

#if VM_DEBUG_LEVEL < 0 || VM_DEBUG_LEVEL > VM_DEBUG_HIGH
#error Debug level set to a value outside the valid range 0..10
#endif

#define VM_DEBUG(level, ...) \
  do { \
    if(level <= VM_DEBUG_LEVEL) \
      vm_print_debug((level), __VA_ARGS__); \
  } while(0)

#endif /* !VM_MACROS_H */
