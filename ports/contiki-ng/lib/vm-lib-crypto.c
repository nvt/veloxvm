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
 * Author: Nicolas Tsiftes <nvt@acm.org>
 *
 * Crypto library for VeloxVM using Contiki-NG crypto APIs.
 *
 * Every user-facing operation is a thin wrapper over a Contiki-NG
 * driver. Algorithms are selected at the Scheme call site via a symbol
 * so that adding a new suite is a dispatch change, not a new primitive.
 * Confidentiality is exposed only through AEAD; there is intentionally
 * no unauthenticated block-cipher surface.
 *
 * Provides:
 *   (crypto-hash 'sha-256 data)
 *                                         -> 32-byte digest vector
 *   (crypto-mac 'hmac-sha-256 key data)
 *                                         -> 32-byte MAC vector
 *   (crypto-mac-verify 'hmac-sha-256 key data tag)
 *                                         -> #t if tag matches, else #f
 *   (crypto-aead-encrypt 'aes-128-ccm key nonce aad plaintext)
 *                                         -> ciphertext || 16-byte tag
 *   (crypto-aead-decrypt 'aes-128-ccm key nonce aad ciphertext-and-tag)
 *                                         -> plaintext vector, or #f on
 *                                            authentication failure
 */

#include "contiki.h"
#include "lib/ccm-star.h"
#include "lib/sha-256.h"

#include "vm.h"
#include "vm-lib.h"
#include "vm-log.h"

#include <string.h>

#define AES_128_KEY_LENGTH    16
#define AES_CCM_NONCE_LENGTH  13
#define AES_CCM_MIC_LENGTH    16

VM_DECLARE_FUNCTION(crypto_hash);
VM_DECLARE_FUNCTION(crypto_mac);
VM_DECLARE_FUNCTION(crypto_mac_verify);
VM_DECLARE_FUNCTION(crypto_aead_encrypt);
VM_DECLARE_FUNCTION(crypto_aead_decrypt);

static int load(vm_program_t *);
static int unload(vm_program_t *);

#define CRYPTO_ARG_TYPES (VM_TYPE_FLAG(VM_TYPE_SYMBOL) | \
                          VM_TYPE_FLAG(VM_TYPE_STRING) | \
                          VM_TYPE_FLAG(VM_TYPE_VECTOR))

static const vm_procedure_t crypto_operators[] = {
  VM_OPERATOR(crypto_hash, CRYPTO_ARG_TYPES,
              VM_PROCEDURE_EVAL_ARGS, 2, 2),
  VM_OPERATOR(crypto_mac, CRYPTO_ARG_TYPES,
              VM_PROCEDURE_EVAL_ARGS, 3, 3),
  VM_OPERATOR(crypto_mac_verify, CRYPTO_ARG_TYPES,
              VM_PROCEDURE_EVAL_ARGS, 4, 4),
  VM_OPERATOR(crypto_aead_encrypt, CRYPTO_ARG_TYPES,
              VM_PROCEDURE_EVAL_ARGS, 5, 5),
  VM_OPERATOR(crypto_aead_decrypt, CRYPTO_ARG_TYPES,
              VM_PROCEDURE_EVAL_ARGS, 5, 5)
};

vm_lib_t vm_lib_crypto = {
  .name = "crypto",
  .load = load,
  .unload = unload,
  .operators = crypto_operators,
  .operator_count = sizeof(crypto_operators) / sizeof(crypto_operators[0]),
  .symbols = (const char *[]){"crypto-hash",
                              "crypto-mac", "crypto-mac-verify",
                              "crypto-aead-encrypt", "crypto-aead-decrypt"},
  .symbol_count = 5
};

static int
load(vm_program_t *program)
{
  VM_DEBUG(VM_DEBUG_LOW, "Loading the crypto library");
  return 1;
}

static int
unload(vm_program_t *program)
{
  VM_DEBUG(VM_DEBUG_LOW, "Unloading the crypto library");
  return 1;
}

/*
 * Extract a byte pointer and length from a string or buffer-vector
 * argument. Returns 1 on success, 0 on failure.
 */
static int
get_data_bytes(vm_obj_t *obj, const uint8_t **data, size_t *len)
{
  if(obj->type == VM_TYPE_STRING) {
    *data = (const uint8_t *)obj->value.string->str;
    *len = obj->value.string->length;
    return 1;
  } else if(obj->type == VM_TYPE_VECTOR &&
            VM_IS_SET(obj->value.vector->flags, VM_VECTOR_FLAG_BUFFER)) {
    *data = obj->value.vector->bytes;
    *len = obj->value.vector->length;
    return 1;
  }
  return 0;
}

/*
 * Return 1 if argv[0] is the symbol 'name', 0 otherwise.
 */
static int
algorithm_is(vm_thread_t *thread, vm_obj_t *obj, const char *name)
{
  const char *got;

  if(obj->type != VM_TYPE_SYMBOL) {
    return 0;
  }
  got = vm_symbol_lookup(thread->program, &obj->value.symbol_ref);
  return got != NULL && vm_strcasecmp(got, name) == 0;
}

/*
 * Constant-time byte-wise comparison. Returns 1 if equal, 0 otherwise.
 */
static int
ct_equal(const uint8_t *a, const uint8_t *b, size_t len)
{
  uint8_t diff = 0;
  size_t i;

  for(i = 0; i < len; i++) {
    diff |= a[i] ^ b[i];
  }
  return diff == 0;
}

/*
 * Allocate a buffer-vector of the given length as thread->result.
 * Returns the vector on success, NULL on allocation failure.
 */
static vm_vector_t *
alloc_buffer_result(vm_thread_t *thread, size_t len)
{
  vm_vector_t *v;

  v = vm_vector_create(&thread->result, len, VM_VECTOR_FLAG_BUFFER);
  if(v == NULL) {
    vm_signal_error(thread, VM_ERROR_HEAP);
  }
  return v;
}

VM_FUNCTION(crypto_hash)
{
  const uint8_t *data;
  size_t len;
  vm_vector_t *result;

  if(!algorithm_is(thread, &argv[0], "sha-256")) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
    return;
  }
  if(!get_data_bytes(&argv[1], &data, &len)) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  result = alloc_buffer_result(thread, SHA_256_DIGEST_LENGTH);
  if(result == NULL) {
    return;
  }
  sha_256_hash(data, len, result->bytes);
}

VM_FUNCTION(crypto_mac)
{
  const uint8_t *key, *data;
  size_t key_len, data_len;
  vm_vector_t *result;

  if(!algorithm_is(thread, &argv[0], "hmac-sha-256")) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
    return;
  }
  if(!get_data_bytes(&argv[1], &key, &key_len) ||
     !get_data_bytes(&argv[2], &data, &data_len)) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }

  result = alloc_buffer_result(thread, SHA_256_DIGEST_LENGTH);
  if(result == NULL) {
    return;
  }
  sha_256_hmac(key, key_len, data, data_len, result->bytes);
}

VM_FUNCTION(crypto_mac_verify)
{
  const uint8_t *key, *data, *tag;
  size_t key_len, data_len, tag_len;
  uint8_t expected[SHA_256_DIGEST_LENGTH];

  if(!algorithm_is(thread, &argv[0], "hmac-sha-256")) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
    return;
  }
  if(!get_data_bytes(&argv[1], &key, &key_len) ||
     !get_data_bytes(&argv[2], &data, &data_len) ||
     !get_data_bytes(&argv[3], &tag, &tag_len)) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }
  if(tag_len != SHA_256_DIGEST_LENGTH) {
    VM_PUSH_BOOLEAN(VM_FALSE);
    return;
  }

  sha_256_hmac(key, key_len, data, data_len, expected);
  VM_PUSH_BOOLEAN(ct_equal(expected, tag, SHA_256_DIGEST_LENGTH)
                    ? VM_TRUE : VM_FALSE);
}

VM_FUNCTION(crypto_aead_encrypt)
{
  const uint8_t *key, *nonce, *aad, *plaintext;
  size_t key_len, nonce_len, aad_len, plain_len;
  vm_vector_t *result;

  if(!algorithm_is(thread, &argv[0], "aes-128-ccm")) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
    return;
  }
  if(!get_data_bytes(&argv[1], &key, &key_len) ||
     !get_data_bytes(&argv[2], &nonce, &nonce_len) ||
     !get_data_bytes(&argv[3], &aad, &aad_len) ||
     !get_data_bytes(&argv[4], &plaintext, &plain_len)) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }
  /* The Contiki-NG CCM* driver documents ceilings of 0xffff bytes
     for the message and 0xfeff bytes for the associated data. */
  if(key_len != AES_128_KEY_LENGTH ||
     nonce_len != AES_CCM_NONCE_LENGTH ||
     plain_len > 0xffff ||
     aad_len > 0xfeff) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
    return;
  }

  result = alloc_buffer_result(thread, plain_len + AES_CCM_MIC_LENGTH);
  if(result == NULL) {
    return;
  }
  memcpy(result->bytes, plaintext, plain_len);

  CCM_STAR.set_key(key);
  CCM_STAR.aead(nonce,
                result->bytes, plain_len,
                aad, aad_len,
                result->bytes + plain_len, AES_CCM_MIC_LENGTH,
                1 /* forward = encrypt */);
}

VM_FUNCTION(crypto_aead_decrypt)
{
  const uint8_t *key, *nonce, *aad, *ct;
  size_t key_len, nonce_len, aad_len, ct_len;
  size_t plain_len;
  uint8_t expected_tag[AES_CCM_MIC_LENGTH];
  vm_vector_t *result;

  if(!algorithm_is(thread, &argv[0], "aes-128-ccm")) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
    return;
  }
  if(!get_data_bytes(&argv[1], &key, &key_len) ||
     !get_data_bytes(&argv[2], &nonce, &nonce_len) ||
     !get_data_bytes(&argv[3], &aad, &aad_len) ||
     !get_data_bytes(&argv[4], &ct, &ct_len)) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_TYPES);
    return;
  }
  if(key_len != AES_128_KEY_LENGTH ||
     nonce_len != AES_CCM_NONCE_LENGTH ||
     ct_len < AES_CCM_MIC_LENGTH ||
     aad_len > 0xfeff ||
     ct_len - AES_CCM_MIC_LENGTH > 0xffff) {
    vm_signal_error(thread, VM_ERROR_ARGUMENT_VALUE);
    return;
  }
  plain_len = ct_len - AES_CCM_MIC_LENGTH;

  result = alloc_buffer_result(thread, plain_len);
  if(result == NULL) {
    return;
  }
  memcpy(result->bytes, ct, plain_len);

  CCM_STAR.set_key(key);
  CCM_STAR.aead(nonce,
                result->bytes, plain_len,
                aad, aad_len,
                expected_tag, AES_CCM_MIC_LENGTH,
                0 /* forward = decrypt */);

  if(!ct_equal(expected_tag, ct + plain_len, AES_CCM_MIC_LENGTH)) {
    /* Authentication failed: wipe the candidate plaintext and return #f. */
    memset(result->bytes, 0, plain_len);
    VM_PUSH_BOOLEAN(VM_FALSE);
  }
}
