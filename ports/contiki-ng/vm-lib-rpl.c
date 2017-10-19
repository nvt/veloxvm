/*
 * Copyright (c) 2012-2017, RISE SICS AB.
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
 * Author: Nicolas Tsiftes <nvt@acm.org>,
 */

#include "contiki.h"
#include "net/rpl-lite/rpl.h"
#include "net/rpl-lite/rpl-dag-root.h"

#include "vm.h"
#include "vm-lib.h"
#include "vm-log.h"

typedef int vm_libval_t;

struct sym_libval_pair {
  const char *sym_name;
  vm_libval_t libval;
};

#if 0
static const struct sym_libval_pair sym_to_libval_map[] = {
  {"RPLMesh",   RPL_MODE_MESH},
  {"RPLFeather", RPL_MODE_FEATHER},
  {"RPLLeaf",  RPL_MODE_LEAF}
};
#endif

VM_DECLARE_FUNCTION(rpl_is_rootp);
VM_DECLARE_FUNCTION(rpl_create_dag);
VM_DECLARE_FUNCTION(rpl_in_dagp);

static int load(vm_program_t *);
static int unload(vm_program_t *);

static const vm_procedure_t rpl_operators[] = {
  VM_OPERATOR(rpl_is_rootp, 0, 0, 0, 0),
  VM_OPERATOR(rpl_create_dag, 0, 0, 0, 0),
  VM_OPERATOR(rpl_in_dagp, 0, 0, 0, 0)
};

vm_lib_t vm_lib_rpl = {
  .name = "rpl",
  .load = load,
  .unload = unload,
  .operators = rpl_operators,
  .operator_count = ARRAY_SIZE(rpl_operators),
  .symbols = (const char *[]){"rpl-is-root?", "rpl-create-dag",
                              "rpl-in-dag?"},
  .symbol_count = 3
};

static int
load(vm_program_t *program)
{
  vm_obj_t obj;

  VM_DEBUG(VM_DEBUG_LOW, "Loading the RPL library");

  /* Bind all library symbols to a dummy value. */
  obj.type = VM_TYPE_BOOLEAN;
  obj.value.boolean = VM_TRUE;

  vm_lib_bind_symbol(program, "RPLMesh", &obj);
  vm_lib_bind_symbol(program, "RPLFeather", &obj);
  vm_lib_bind_symbol(program, "RPLLeaf", &obj);

  return 1;
}

static int
unload(vm_program_t *program)
{
  VM_DEBUG(VM_DEBUG_LOW, "Unloading the RPL library");
  return 1;
}

#if 0
static vm_libval_t
get_libval(vm_program_t *program, vm_symbol_ref_t *symref)
{
  const char *name;
  int i;

  name = vm_symbol_lookup(program, symref);
  if(name == NULL) {
    return 0;
  }

  for(i = 0; i < ARRAY_SIZE(sym_to_libval_map); i++) {
    if(strcasecmp(name, sym_to_libval_map[i].sym_name) == 0) {
      return sym_to_libval_map[i].libval;
    }
  }

  return 0;
}
#endif

VM_FUNCTION(rpl_is_rootp)
{
  VM_PUSH_BOOLEAN(rpl_dag_root_is_root());
}

VM_FUNCTION(rpl_create_dag)
{
  rpl_dag_root_init_dag_immediately();
}

VM_FUNCTION(rpl_in_dagp)
{
  VM_PUSH_BOOLEAN(rpl_get_any_dag() != NULL);
}
