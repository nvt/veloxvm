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
#include "vm-log.h"

#define SYM(str) {.name = (str)}

static vm_symbol_t symbol_map[] = {
  /* Mathematical functions. */
  SYM("+"), SYM("-"), SYM("*"), SYM("/"), SYM("gcd"),
  SYM("lcm"), SYM("numerator"), SYM("denominator"), SYM("quotient"),
  SYM("remainder"), SYM("modulo"), SYM("="), SYM("/="), SYM("<"),
  SYM("<="), SYM(">"), SYM(">="), SYM("zero?"),

  /* Primitive functions. */
  SYM("bind"), SYM("return"), SYM("begin"), SYM("if"), SYM("define"),
  SYM("set!"), SYM("and"), SYM("or"), SYM("apply"), SYM("quote"),
  SYM("number?"), SYM("integer?"), SYM("rational?"), SYM("real?"),
  SYM("complex?"), SYM("exact?"), SYM("inexact?"), SYM("procedure?"),
  SYM("boolean?"), SYM("port?"), SYM("not"), SYM("eq?"), SYM("eqv?"),
  SYM("equal?"),

  /* System functions. */
  SYM("system-info"), SYM("load-program"), SYM("import"),
  SYM("get-devices"), SYM("print"), SYM("random"), SYM("time"),
  SYM("get-programs"), SYM("program-info"), SYM("exit"),

  /* List functions. */
  SYM("list"), SYM("cons"), SYM("push"), SYM("POP"), SYM("car"), SYM("cdr"),
  SYM("list-ref"), SYM("list-tail"), SYM("append"), SYM("remove"),
  SYM("reverse"), SYM("length"), SYM("null?"), SYM("list?"), SYM("pair?"),
  SYM("set-car!"), SYM("set-cdr!"), SYM("memq"), SYM("memv"), SYM("member"),
  SYM("assq"), SYM("assv"), SYM("assoc"),

  /* Higher-order list functions. */
  SYM("map"), SYM("filter"), SYM("for-each"), SYM("reduce"), SYM("count"),

  /* Character functions. */
  SYM("char?"), SYM("char-compare"), SYM("char-class"), SYM("char->integer"),
  SYM("integer->char"), SYM("char-upcase"), SYM("char-downcase"),

  /* String functions. */
  SYM("make-string"), SYM("string"), SYM("string?"), SYM("string-length"),
  SYM("string-ref"), SYM("string-set!"), SYM("string->list"),
  SYM("list->string"), SYM("vector->string"), SYM("string-fill!"),
  SYM("string-compare"), SYM("substring"), SYM("string-append"),
  SYM("string-copy"), SYM("string-split"), SYM("number->string"),
  SYM("string->number"),

  /* Exception and condition functions. */
  SYM("guard"), SYM("raise"),

  /* Thread functions. */
  SYM("thread-create!"), SYM("thread-fork!"), SYM("thread-id"),
  SYM("thread-join!"), SYM("thread-sleep!"),
  SYM("thread-specific"), SYM("thread-specific-set!"),
  SYM("thread-terminate!"), SYM("thread-yield!"), SYM("thread-stats"),

  /* Mutex functions. */
  SYM("mutex?"), SYM("make-mutex"), SYM("mutex-name"), SYM("mutex-specific"),
  SYM("mutex-specific-set"), SYM("mutex-state"), SYM("mutex-lock!"),
  SYM("mutex-unlock!"),

  /* Vector functions. */
  SYM("make-vector"), SYM("vector"), SYM("vector?"), SYM("buffer?"),
  SYM("vector-merge"), SYM("vector-length"), SYM("vector-ref"),
  SYM("vector-set!"), SYM("vector->list"), SYM("list->vector"),
  SYM("vector-fill!"), SYM("make-buffer"), SYM("buffer-append"),

  /* I/O functions. */
  SYM("input-port?"), SYM("output-port?"), SYM("current-input-port"),
  SYM("current-output-port"), SYM("open-input-file"), SYM("open-output-file"),
  SYM("close-input-port"), SYM("close-output-port"), SYM("read-char"),
  SYM("read"), SYM("peek-char"), SYM("eof-object?"), SYM("char-ready?"),
  SYM("write-char"), SYM("write"), SYM("display"), SYM("with-input-from-file"),
  SYM("with-output-to-file"), SYM("make-client"), SYM("make-server"),
  SYM("peer-name"), SYM("accept-client"), SYM("incoming-client?"),
  SYM("addr->string"), SYM("resolve_hostname"),

  /* Mathematical functions using floats. */
  SYM("floor"), SYM("ceiling"), SYM("round"), SYM("truncate"), SYM("exp"),
  SYM("log"), SYM("sin"), SYM("cos"), SYM("tan"), SYM("asin"), SYM("acos"),
  SYM("atan"), SYM("sqrt"), SYM("expt"), SYM("exact-to-inexact"),
  SYM("inexact-to-exact"),

  /* Evaluation control functions. */
  SYM("call-with-current-continuation"), SYM("values"), SYM("call-with-values"),
  SYM("dynamic-wind"), SYM("eval"),

  /* Bit manipulation functions. */
  SYM("bit-and"), SYM("bit-or"), SYM("bit-invert"), SYM("bit-not"),
  SYM("bit-xor"), SYM("bit-shift"),

  /* Packet management functions. */
  SYM("construct-packet"), SYM("deconstruct-packet")
};

#define CORE_SYMBOL_COUNT ARRAY_SIZE(symbol_map)

vm_obj_t *
vm_symbol_resolve(vm_thread_t *thread, vm_symbol_ref_t *symbol_ref)
{
  int i, j;
  static vm_obj_t self_obj;

  if(symbol_ref->scope == VM_SYMBOL_SCOPE_APP) {
    /* Check whether the symbol ID is valid. */
    if(symbol_ref->symbol_id >= VM_TABLE_SIZE(thread->program->symbols)) {
      vm_signal_error(thread, VM_ERROR_SYMBOL_ID);
      return NULL;
    }

    /* First check whether the symbol binding has been shadowed by a binding on the stack. */
    for(i = thread->exprc - 1; i >= 0; i--) {
      for(j = thread->exprv[i]->bindc - 1; j >= 0; j--) {
        if(thread->exprv[i]->bindv[j].symbol_id == symbol_ref->symbol_id) {
          return &thread->exprv[i]->bindv[j].obj;
        }
      }
    }

    /* If no stack binding was found, we return the top-level definition of a symbol, or NULL if it
       doesn't exist. */
    return &thread->program->symbol_bindings[symbol_ref->symbol_id];
  } else if(symbol_ref->scope == VM_SYMBOL_SCOPE_CORE &&
     symbol_ref->symbol_id < CORE_SYMBOL_COUNT) {
    /* Return a self-reference if the symbol refers to an operator. */
    self_obj.type = VM_TYPE_SYMBOL;
    memcpy(&self_obj.value.symbol_ref, symbol_ref,
           sizeof(vm_symbol_ref_t));
    return &self_obj;
  }

  return NULL;
}

void
vm_symbol_bind(vm_thread_t *thread, vm_symbol_ref_t *symbol_ref, vm_obj_t *obj)
{
  vm_symbol_bind_t *sym_bind;

  sym_bind = &thread->expr->bindv[thread->expr->bindc++];
  sym_bind->symbol_id = symbol_ref->symbol_id;
  memcpy(&sym_bind->obj, obj, sizeof(vm_obj_t));
}

const char *
vm_symbol_lookup(vm_program_t *program, vm_symbol_ref_t *symbol_ref)
{
  if(symbol_ref->scope == VM_SYMBOL_SCOPE_APP) {
    if(program == NULL) {
      VM_DEBUG(VM_DEBUG_MEDIUM, "No program supplied to app symbol lookup");
      return NULL;
    }

    /* Check the program's locally created symbols. */
    if(symbol_ref->symbol_id >= VM_TABLE_SIZE(program->symbols)) {
      VM_DEBUG(VM_DEBUG_MEDIUM, "Symbol ID is too high; max is %d",
             (int)(VM_TABLE_SIZE(program->symbols) - 1));
      return NULL;
    }
    return (const char *)VM_TABLE_GET(program->symbols, symbol_ref->symbol_id);
  }

  if(symbol_ref->symbol_id >= CORE_SYMBOL_COUNT) {
    return NULL;
  }

  return symbol_map[symbol_ref->symbol_id].name;
}

int
vm_symbol_get_ref(vm_thread_t *thread, const char *name,
		  vm_symbol_ref_t *symbol_ref)
{
  vm_symbol_id_t i;

  for(i = 0; i < VM_TABLE_SIZE(thread->program->symbols); i++) {
    if(strcasecmp((const char *)VM_TABLE_GET(thread->program->symbols, i),
	      name) == 0) {
      symbol_ref->scope = VM_SYMBOL_SCOPE_APP;
      symbol_ref->symbol_id = i;
      return 1;
    }
  }

  return 0;
}
