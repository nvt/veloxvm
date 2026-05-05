/*
 * Copyright (c) 2026, RISE Research Institutes of Sweden AB
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
 * Author: Nicolas Tsiftes <nicolas.tsiftes@ri.se>
 */

/*
 * Append-loader: parses a wire-format delta (magic 0x5E 0xB6) and
 * appends its sections into an existing REPL program. The format is
 * specified in doc/repl-design.md.
 */

#ifdef VM_REPL_ENABLE

#include "vm.h"
#include "vm-log.h"
#include "vm-repl.h"
#include "vm-table.h"

#include <stdint.h>
#include <string.h>

#define DELTA_MAGIC_1     0x5E
#define DELTA_MAGIC_2     0xB6
#define DELTA_VERSION     0x01

#define TAG_STRINGS       0x01
#define TAG_SYMBOLS       0x02
#define TAG_EXPRS         0x03
#define TAG_CAPTURES      0x04
#define TAG_ENTRY_EXPR    0x05
#define TAG_END           0xFF

/* A small cursor abstraction so each helper can advance position
   safely without scattering bounds checks. */
typedef struct {
  const uint8_t *data;
  size_t len;
  size_t pos;
} cursor_t;

static int
take(cursor_t *c, size_t n, const uint8_t **out)
{
  if(c->pos + n > c->len) {
    return 0;
  }
  *out = c->data + c->pos;
  c->pos += n;
  return 1;
}

static int
take_u16(cursor_t *c, uint16_t *out)
{
  const uint8_t *bytes;
  if(!take(c, 2, &bytes)) {
    return 0;
  }
  *out = (uint16_t)((bytes[0] << 8) | bytes[1]);
  return 1;
}

static int
take_u8(cursor_t *c, uint8_t *out)
{
  const uint8_t *bytes;
  if(!take(c, 1, &bytes)) {
    return 0;
  }
  *out = bytes[0];
  return 1;
}

static vm_repl_status_t
apply_table_section(vm_table_t *table, cursor_t *section_cursor,
                    const char *which)
{
  uint16_t start_id;
  uint16_t count;
  uint16_t i;

  if(!take_u16(section_cursor, &start_id) ||
     !take_u16(section_cursor, &count)) {
    VM_DEBUG(VM_DEBUG_LOW, "repl: %s section truncated header", which);
    return VM_REPL_BAD_FORMAT;
  }

  if(start_id != (uint16_t)table->item_count) {
    VM_DEBUG(VM_DEBUG_LOW,
             "repl: %s start_id mismatch (got %u, expected %u)",
             which, (unsigned)start_id, (unsigned)table->item_count);
    return VM_REPL_OUT_OF_SYNC;
  }

  for(i = 0; i < count; i++) {
    uint16_t item_len;
    const uint8_t *item_bytes;
    if(!take_u16(section_cursor, &item_len)) {
      VM_DEBUG(VM_DEBUG_LOW, "repl: %s item %u length truncated",
               which, (unsigned)i);
      return VM_REPL_BAD_FORMAT;
    }
    if(!take(section_cursor, item_len, &item_bytes)) {
      VM_DEBUG(VM_DEBUG_LOW, "repl: %s item %u payload truncated",
               which, (unsigned)i);
      return VM_REPL_BAD_FORMAT;
    }
    if(vm_table_append(table, item_bytes, item_len) < 0) {
      VM_DEBUG(VM_DEBUG_LOW, "repl: %s table_append failed at %u",
               which, (unsigned)i);
      return VM_REPL_OUT_OF_MEMORY;
    }
  }

  return VM_REPL_OK;
}

static vm_repl_status_t
apply_captures_section(vm_program_t *program, cursor_t *section_cursor)
{
  uint16_t count;
  uint16_t i;

  if(!take_u16(section_cursor, &count)) {
    return VM_REPL_BAD_FORMAT;
  }

  for(i = 0; i < count; i++) {
    uint16_t entry_len;
    uint16_t expr_id;
    unsigned cap_count;
    unsigned k;
    vm_captures_t *cap;

    if(!take_u16(section_cursor, &entry_len)) {
      return VM_REPL_BAD_FORMAT;
    }
    if(entry_len < 2 || (entry_len % 2) != 0) {
      return VM_REPL_BAD_FORMAT;
    }
    cap_count = (entry_len - 2) / 2;
    if(cap_count > UINT8_MAX) {
      return VM_REPL_BAD_FORMAT;
    }
    if(!take_u16(section_cursor, &expr_id)) {
      return VM_REPL_BAD_FORMAT;
    }
    if(expr_id >= program->captures_size) {
      VM_DEBUG(VM_DEBUG_LOW, "repl: captures expr_id %u out of range",
               (unsigned)expr_id);
      return VM_REPL_BAD_FORMAT;
    }

    cap = vm_alloc(sizeof(vm_captures_t));
    if(cap == NULL) {
      return VM_REPL_OUT_OF_MEMORY;
    }
    cap->count = (uint8_t)cap_count;
    cap->symbols = NULL;
    if(cap_count > 0) {
      cap->symbols = vm_alloc(cap_count * sizeof(vm_symbol_id_t));
      if(cap->symbols == NULL) {
        vm_free(cap);
        return VM_REPL_OUT_OF_MEMORY;
      }
    }
    for(k = 0; k < cap_count; k++) {
      uint16_t sid;
      if(!take_u16(section_cursor, &sid)) {
        if(cap->symbols != NULL) {
          vm_free(cap->symbols);
        }
        vm_free(cap);
        return VM_REPL_BAD_FORMAT;
      }
      cap->symbols[k] = sid;
    }
    program->captures[expr_id] = cap;
  }

  return VM_REPL_OK;
}

/*
 * Grow program->symbol_bindings to match the current symbol count and
 * zero-initialize the new tail. Called after applying SYMBOLS_APPEND
 * so subsequent runs see VM_TYPE_NONE for newly defined symbols.
 */
static int
grow_symbol_bindings(vm_program_t *program, unsigned old_count)
{
  unsigned new_count = VM_TABLE_SIZE(program->symbols);
  vm_obj_t *new_bindings;
  unsigned i;

  if(new_count == old_count) {
    return 1;
  }

  new_bindings = VM_REALLOC(program->symbol_bindings,
                            new_count * sizeof(vm_obj_t));
  if(new_bindings == NULL) {
    return 0;
  }
  program->symbol_bindings = new_bindings;
  for(i = old_count; i < new_count; i++) {
    program->symbol_bindings[i].type = VM_TYPE_NONE;
    memset(&program->symbol_bindings[i].value, 0, sizeof(vm_obj_value_t));
  }
  return 1;
}

/*
 * Grow program->captures to match the current expr count and zero
 * the new tail. Called after applying EXPRS_APPEND.
 */
static int
grow_captures(vm_program_t *program, unsigned old_count)
{
  unsigned new_count = VM_TABLE_SIZE(program->exprv);
  vm_captures_t **new_captures;
  unsigned i;

  if(new_count == old_count) {
    return 1;
  }

  new_captures = VM_REALLOC(program->captures,
                            new_count * sizeof(vm_captures_t *));
  if(new_captures == NULL) {
    return 0;
  }
  program->captures = new_captures;
  program->captures_size = new_count;
  for(i = old_count; i < new_count; i++) {
    program->captures[i] = NULL;
  }
  return 1;
}

vm_repl_status_t
vm_repl_apply_delta(vm_program_t *program,
                    const uint8_t *delta_bytes, size_t delta_len,
                    vm_expr_id_t *out_entry_id)
{
  cursor_t cursor = { delta_bytes, delta_len, 0 };
  uint8_t magic1;
  uint8_t magic2;
  uint8_t version;
  int have_entry = 0;
  vm_expr_id_t entry_id = 0;
  unsigned old_symbols = VM_TABLE_SIZE(program->symbols);
  unsigned old_exprs = VM_TABLE_SIZE(program->exprv);

  if(out_entry_id != NULL) {
    *out_entry_id = 0;
  }

  if(!take_u8(&cursor, &magic1) || !take_u8(&cursor, &magic2)) {
    return VM_REPL_BAD_MAGIC;
  }
  if(magic1 != DELTA_MAGIC_1 || magic2 != DELTA_MAGIC_2) {
    return VM_REPL_BAD_MAGIC;
  }
  if(!take_u8(&cursor, &version)) {
    return VM_REPL_BAD_VERSION;
  }
  if(version != DELTA_VERSION) {
    return VM_REPL_BAD_VERSION;
  }

  while(cursor.pos < cursor.len) {
    uint8_t tag;
    uint16_t section_len;
    cursor_t section_cursor;

    if(!take_u8(&cursor, &tag)) {
      return VM_REPL_BAD_FORMAT;
    }
    if(!take_u16(&cursor, &section_len)) {
      return VM_REPL_BAD_FORMAT;
    }
    if(cursor.pos + section_len > cursor.len) {
      return VM_REPL_BAD_FORMAT;
    }
    section_cursor.data = cursor.data + cursor.pos;
    section_cursor.len = section_len;
    section_cursor.pos = 0;

    switch(tag) {
    case TAG_STRINGS: {
      vm_repl_status_t s = apply_table_section(&program->strings,
                                               &section_cursor, "strings");
      if(s != VM_REPL_OK) {
        return s;
      }
      break;
    }
    case TAG_SYMBOLS: {
      vm_repl_status_t s = apply_table_section(&program->symbols,
                                               &section_cursor, "symbols");
      if(s != VM_REPL_OK) {
        return s;
      }
      break;
    }
    case TAG_EXPRS: {
      vm_repl_status_t s = apply_table_section(&program->exprv,
                                               &section_cursor, "exprs");
      if(s != VM_REPL_OK) {
        return s;
      }
      break;
    }
    case TAG_CAPTURES: {
      /* Captures references expr_ids, so it must come after EXPRS in
         the same delta. We grow the captures array now in case
         CAPTURES is the only section that mentions a new expr. */
      if(!grow_captures(program, old_exprs)) {
        return VM_REPL_OUT_OF_MEMORY;
      }
      old_exprs = VM_TABLE_SIZE(program->exprv);
      vm_repl_status_t s = apply_captures_section(program, &section_cursor);
      if(s != VM_REPL_OK) {
        return s;
      }
      break;
    }
    case TAG_ENTRY_EXPR:
      if(section_len < 2) {
        return VM_REPL_BAD_FORMAT;
      } else {
        uint16_t e;
        if(!take_u16(&section_cursor, &e)) {
          return VM_REPL_BAD_FORMAT;
        }
        entry_id = e;
        have_entry = 1;
      }
      break;
    case TAG_END:
      goto done;
    default:
      VM_DEBUG(VM_DEBUG_LOW, "repl: unknown delta section tag 0x%02x", tag);
      return VM_REPL_BAD_FORMAT;
    }

    cursor.pos += section_len;
  }

 done:
  if(!have_entry) {
    VM_DEBUG(VM_DEBUG_LOW, "repl: delta missing ENTRY_EXPR section");
    return VM_REPL_BAD_FORMAT;
  }
  if(entry_id >= VM_TABLE_SIZE(program->exprv)) {
    VM_DEBUG(VM_DEBUG_LOW, "repl: entry_id %u >= expr count %u",
             (unsigned)entry_id, VM_TABLE_SIZE(program->exprv));
    return VM_REPL_BAD_FORMAT;
  }

  if(!grow_symbol_bindings(program, old_symbols)) {
    return VM_REPL_OUT_OF_MEMORY;
  }
  if(!grow_captures(program, old_exprs)) {
    return VM_REPL_OUT_OF_MEMORY;
  }

  if(out_entry_id != NULL) {
    *out_entry_id = entry_id;
  }
  return VM_REPL_OK;
}

#endif /* VM_REPL_ENABLE */
