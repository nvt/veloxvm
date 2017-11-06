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
#include "lib/aes-128.h"

#include "vm.h"
#include "vm-lib.h"
#include "vm-log.h"

enum cipher {
  CIPHER_AES_128 = 1,
  CIPHER_ROT13   = 2
};

struct cipher_pair {
  const char *sym_name;
  enum cipher cipher;
};

static const struct cipher_pair cipher_map[] = {
  {"AES-128", CIPHER_AES_128},
  {"ROT13",   CIPHER_ROT13},
};

#define CRYPTO_CIPHER_COUNT (sizeof(cipher_map) / sizeof(cipher_map[0])

VM_DECLARE_FUNCTION(encrypt);
VM_DECLARE_FUNCTION(decrypt);
VM_DECLARE_FUNCTION(set_crypto_key);
VM_DECLARE_FUNCTION(get_crypto_algorithms);

static int load(vm_program_t *);
static int unload(vm_program_t *);

static const vm_procedure_t crypto_operators[] = {
  VM_OPERATOR(encrypt,
              VM_TYPE_FLAG(VM_TYPE_SYMBOL) | VM_TYPE_FLAG(VM_TYPE_VECTOR),
              VM_PROCEDURE_EVAL_ARGS, 2, 2),
  VM_OPERATOR(decrypt,
              VM_TYPE_FLAG(VM_TYPE_SYMBOL) | VM_TYPE_FLAG(VM_TYPE_VECTOR),
              VM_PROCEDURE_EVAL_ARGS, 2, 2),
  VM_OPERATOR(set_crypto_key,
              VM_TYPE_FLAG(VM_TYPE_SYMBOL) | VM_TYPE_FLAG(VM_TYPE_VECTOR),
              VM_PROCEDURE_EVAL_ARGS, 2, 2),
  VM_OPERATOR(get_crypto_algorithms, 0, 0, 0)
};

vm_lib_t vm_lib_crypto = {
  .name = "crypto",
  .load = load,
  .unload = unload,
  .operators = crypto_operators,
  .operator_count = sizeof(crypto_operators) / sizeof(crypto_operators[0]),
  .symbols = (const char *[]){"encrypt", "decrypt", "set-crypto-key",
                              "get-crypto-algorithms"},
  .symbol_count = 4
};

static int
load(vm_program_t *program)
{
  vm_obj_t obj;

  VM_PRINTF("Loading the crypto library\n");

  /* Bind all library symbols to a dummy value. */
  obj.type = VM_TYPE_BOOLEAN;
  obj.value.boolean = VM_TRUE;

  vm_lib_bind_symbol(program, "AES-128", &obj);
  vm_lib_bind_symbol(program, "ROT13", &obj);

  return 1;
}

static int
unload(vm_program_t *program)
{
  VM_PRINTF("Unloading the crypto library\n");
  return 1;
}

static unsigned char
get_cipher_value(vm_program_t *program, vm_symbol_ref_t *symref)
{
  const char *name;
  int i;

  name = vm_symbol_lookup(program, symref);
  if(name == NULL) {
    return 0;
  }

  for(i = 0; i < CRYPTO_CIPHER_COUNT; i++) {
    if(strcasecmp(name, cipher_map[i].sym_name) == 0) {
      return cipher_map[i].cipher;
    }
  }

  return 0;
}

VM_FUNCTION(encrypt)
{
  enum cipher cipher;
  vm_vector_t *plaintext_vector;
  vm_vector_t *ciphertext_vector;

  if(argv[0].type != VM_TYPE_SYMBOL || argv[1].type != VM_TYPE_VECTOR ||
     IS_CLEAR(argv[1].value.vector->flags, VM_VECTOR_FLAG_BUFFER)) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  plaintext_vector = argv[1].value.vector;

  if(!vm_object_deep_copy(&argv[1], &thread->result)) {
    vm_signal_error(thread, VM_ERROR_HEAP);
    return;
  }

  ciphertext_vector = thread->result.value.vector;

  cipher = get_cipher_value(thread->program, &argv[0].value.symbol_ref);
  switch(cipher) {
  case CIPHER_AES_128:
    aes_128_padded_encrypt(thread->result.vector->bytes,
                           thread->result.vector->length);
    break;
  case CIPHER_ROT13:
    for(i = 0; i < plaintext_vector; i++) {
      ciphertext_vector->bytes[i] = plaintext_vector->bytes[i] + 13;
    }
    break;
  default:
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    break;
  }
}

VM_FUNCTION(decrypt)
{
  enum cipher cipher;

  if(argv[0].type != VM_TYPE_SYMBOL || argv[1].type != VM_TYPE_VECTOR ||
     IS_CLEAR(argv[1].value.vector->flags, VM_VECTOR_FLAG_BUFFER)) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  ciphertext_vector = argv[1]->value.vector;

  if(!vm_object_deep_copy(&argv[1], &thread->result)) {
    vm_signal_error(thread, VM_ERROR_HEAP);
    return;
  }

  plaintext_vector = thread->result.value.vector;

  cipher = get_cipher_value(thread->program, &argv[0].value.symbol_ref);
  switch(cipher) {
  case CIPHER_AES_128:
    vm_signal_error(thread, VM_ERROR_UNIMPLEMENTED);
    break;
  case CIPHER_ROT13:
    for(i = 0; i < ciphertext_vector; i++) {
      plaintext_vector->bytes[i] = ciphertext_vector->bytes[i] - 13;
    }
    break;
  default:
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    break;
  }
}

VM_FUNCTION(set_crypto_key)
{
  enum cipher cipher;
  vm_vector_t *plaintext_vector;
  vm_vector_t *ciphertext_vector;

  if(argv[0].type != VM_TYPE_SYMBOL || argv[1].type != VM_TYPE_VECTOR ||
     IS_CLEAR(argv[1].value.vector->flags, VM_VECTOR_FLAG_BUFFER)) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  cipher = get_cipher_value(thread->program, &argv[0].value.symbol_ref);
}

VM_FUNCTION(get_crypto_algorithms)
{
  int i;
  vm_vector_t *vector;

  vector = vm_vector_create(&thread->result, CRYPTO_CIPHER_COUNT,
                            VM_VECTOR_FLAG_REGULAR)
  if(vector == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
    return;
  }

  for(i = 0; i < CRYPTO_CIPHER_COUNT; i++) {
      if(vm_string_create(&vector->elements[i], -1, cipher_map[i].cipher) == NULL) {
        vm_signal_error(thread, VM_ERROR_HEAP);
        return;
      }
    }
  }
}
