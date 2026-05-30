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

#ifndef VM_BYTECODE_H
#define VM_BYTECODE_H

#include <stdint.h>

/* v6 header layout (9 fixed bytes + N-byte program name):
     0x00  2  Magic (VM_FILE_ID1, VM_FILE_ID2)
     0x02  2  Bytecode version (uint16 LE)
     0x04  4  Total file length in bytes (uint32 LE)
     0x08  1  Program name length N (0..255)
     0x09  N  Program name (UTF-8, no terminator)
   The body (tables + captures + expression bytecode) follows the name. */
#define VM_HEADER_FIXED_SIZE 9
#define VM_HEADER_OFFSET_MAGIC1      0
#define VM_HEADER_OFFSET_MAGIC2      1
#define VM_HEADER_OFFSET_VERSION     2
#define VM_HEADER_OFFSET_TOTAL_LEN   4
#define VM_HEADER_OFFSET_NAME_LEN    8
#define VM_HEADER_OFFSET_NAME        9

#define VM_FILE_ID1 94
#define VM_FILE_ID2 181

#define VM_BYTECODE_VERSION 6

#define VM_TOKEN_ATOM 0
#define VM_TOKEN_FORM 1

#define VM_FORM_INLINE  0
#define VM_FORM_LAMBDA  1
#define VM_FORM_REF     2

#define VM_ATOM_MASK 0x7

#endif /* !VM_BYTECODE_H */
