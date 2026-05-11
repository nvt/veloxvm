# VeloxVM packet construction and deconstruction

Two layered APIs for assembling and parsing binary packets:

1. **The C primitives** — `construct-packet` and `deconstruct-packet`, an
   anonymous bit packer that takes a vector of bit-widths plus a parallel
   vector of values. Small and fast; no field names, no type information.
2. **The schema layer** — `languages/scheme-racket/runtime/packet-schema.scm`, a Scheme
   library that wraps the primitives with named, typed fields plus
   variable-length support.

For most uses, prefer the schema layer.

## Contents

1. [Wire format](#wire-format)
2. [The C primitives](#the-c-primitives)
3. [The schema layer](#the-schema-layer)
4. [Variable-length fields](#variable-length-fields)
5. [Validation rules](#validation-rules)
6. [Known limitations](#known-limitations)

## Wire format

### Bit order

Sub-byte fields are laid out **MSB-first within each byte**, matching the
"MSB 0" bit numbering used by RFCs. A 1-bit field at packet position 32
lands in bit 7 (the MSB) of byte 4. This matches RPL DIO (RFC 6550),
DNS headers (RFC 1035), and most network protocols on the wire.

```
RPL DIO flags byte (G=1, MOP=0, Prf=0) -> 0x80:

                            7  6  5  4  3  2  1  0
                          +--+--+--+--+--+--+--+--+
                  byte 4: |G | 0|  MOP   |  Prf  |
                          +--+--+--+--+--+--+--+--+
                           ^                       ^
                  first bit of field        last bit of field
                  (packet position 32)      (packet position 39)
```

### Byte order

Multi-byte integer fields are big-endian by default. The `/le` suffix
selects little-endian for the schema layer (`u16/le`, `u32/le`, `s16/le`,
`s32/le`); the C primitive is big-endian only and the schema layer pre-
swaps bytes for `/le` fields.

A 16-bit `0x1234` as `u16` → bytes `12 34`; as `u16/le` → `34 12`.

### Signed integers

`s8`, `s16`, `s32` (and `/le` variants) encode two's complement. `-1` as
`s16` → bytes `FF FF`.

## The C primitives

### `(construct-packet WIDTHS VALUES)` → byte buffer

`WIDTHS` is a vector of positive integers (field widths in bits). `VALUES`
is a parallel vector; each element is either an integer or a byte vector
of length `WIDTH/8` (only for byte-aligned multi-byte fields).

Result is a byte buffer whose length equals `(sum WIDTHS) / 8`.

Constraints:

- Sum of `WIDTHS` must be a multiple of 8.
- Multi-byte fields (width > 8 bits) must start at a byte boundary.
- Multi-byte field widths must be multiples of 8.
- Multi-byte field widths are capped at 64 bits.

### `(deconstruct-packet WIDTHS BUFFER)` → vector of values

`BUFFER` must be a byte buffer whose length matches `(sum WIDTHS) / 8`.
Returns a vector of decoded values, in the same order as `WIDTHS`. Fields
wider than `vm_integer_t` (32 bits on POSIX) come back as byte vectors;
narrower fields come back as integers.

Implemented in `core/expr-packet.c`.

## The schema layer

A schema is a list of field specs. Each spec is `(NAME TYPE [ARG])`:

```scheme
(define dio-schema
  '((rpl-instance-id u8)
    (version         u8)
    (rank            u16)
    (g               bit)
    (zero            bit)
    (mop             bits 3)
    (prf             bits 3)
    (dtsn            u8)
    (flags           u8)
    (reserved        u8)
    (dodag-id        bytes 16)))
```

### Type table

| Type                 | Width      | Range / shape           | Encoding                |
|----------------------|------------|-------------------------|-------------------------|
| `bit`                | 1 bit      | 0..1                    | unsigned, MSB-first     |
| `bits N`             | N bits (1..8) | 0..2^N-1            | unsigned, MSB-first     |
| `u8`                 | 8 bits     | 0..255                  | unsigned                |
| `u16`                | 16 bits    | 0..65535                | unsigned big-endian     |
| `u24`                | 24 bits    | 0..16777215             | unsigned big-endian     |
| `u32`                | 32 bits    | 0..0x7FFFFFFF *         | unsigned big-endian     |
| `u16/le`             | 16 bits    | 0..65535                | unsigned little-endian  |
| `u32/le`             | 32 bits    | 0..0x7FFFFFFF *         | unsigned little-endian  |
| `s8`                 | 8 bits     | -128..127               | signed two's complement |
| `s16`, `s16/le`      | 16 bits    | -32768..32767           | signed                  |
| `s32`, `s32/le`      | 32 bits    | -2^31..2^31-1           | signed                  |
| `bytes N`            | 8N bits    | byte vector of length N | raw                     |
| `rest`               | variable   | last field only         | raw                     |
| `length-prefixed PT` | variable   | `PT` length + bytes     | PT is u8/u16/u24/u32    |

`*` Unsigned 32-bit values cap at the positive `vm_integer_t` range
(int32 max). To pack the full unsigned u32 range, use `bytes 4`.

### Public API

The schema layer ships as part of the Racket compiler's runtime
library. Use it from any program with:

```scheme
(include "packet-schema.scm")
```

The reader resolves the name against `languages/scheme-racket/runtime/`
via its built-in include search path, regardless of where the program
itself lives.

It exports:

- `(schema-validate SCHEMA)` — returns `#t`, or raises `packet-schema-error`.
- `(schema-construct SCHEMA BINDINGS)` — validates schema and bindings,
  encodes values, returns a byte buffer.
- `(schema-deconstruct SCHEMA BUFFER)` — validates schema, parses the
  buffer, returns an alist of `(name . value)` pairs.
- `(schema-bit-width SCHEMA)` — fixed-portion width in bits.
- `(schema-variable? SCHEMA)` — `#t` if any field is `rest` or
  `length-prefixed`.

### Bindings

Bindings is an alist of `(field-name . value)` pairs. Order doesn't matter:

```scheme
(schema-construct dio-schema
  (list (cons 'rpl-instance-id 0)
        (cons 'version         0)
        (cons 'rank            256)
        (cons 'g               1)
        (cons 'zero            0)
        (cons 'mop             0)
        (cons 'prf             0)
        (cons 'dtsn            0)
        (cons 'flags           0)
        (cons 'reserved        0)
        (cons 'dodag-id        (some-byte-vector))))
```

For `bytes N`, `rest`, and `length-prefixed`, the binding value is a byte
vector — either a regular vector of byte-valued integers or a VM byte
buffer (made with `make-buffer`). The schema layer normalises both forms.

### Quoted-literal alists

A quoted literal like `'((a . 1) (b . 2))` does not currently round-trip
through the Racket compiler (dotted-pair literals lose their structure).
Build bindings at runtime with `(list (cons 'a 1) (cons 'b 2))` instead.

## Variable-length fields

### `rest`

Captures all remaining bytes in the buffer. Must be the last field in
the schema, and the fixed portion preceding it must be byte-aligned.

```scheme
(define dns-schema
  '((id      u16)
    (flags   u16)
    (qdcount u16)
    (ancount u16)
    (nscount u16)
    (arcount u16)
    (qr      rest)))   ;; everything after the 12-byte header
```

### `length-prefixed PT`

Emits a length field of type `PT` (`u8`, `u16`, `u24`, or `u32`, big-
endian) followed by that many raw bytes. The prefix is computed on
construct from the value's length and consumed on deconstruct; the
binding carries only the byte vector itself.

```scheme
(define frame-schema
  '((tag    u8)
    (label  length-prefixed u8)
    (crc    u16)))

(schema-construct frame-schema
  (list (cons 'tag   #xa5)
        (cons 'label (vector #x77 #x77 #x77))
        (cons 'crc   #x1234)))
;; -> 7-byte buffer: a5 03 77 77 77 12 34

(schema-deconstruct frame-schema
  ;; ...the same buffer...
  )
;; -> ((tag . 165) (label . <3-byte buffer>) (crc . 4660))
```

A schema with variable fields is processed by walking the schema as
alternating fixed runs and single variable fields, delegating each
fixed run to the C primitive and concatenating the resulting buffers.
A schema with only fixed fields uses the single-call fast path.

## Validation rules

`schema-validate` rejects:

- Empty or non-list schemas.
- Specs that aren't `(name type)` or `(name type arg)`.
- Non-symbol field names.
- Duplicate field names.
- Unknown type tags.
- `bits N` outside 1..8; `bits` or `bytes` missing the size argument;
  `bytes` with length < 1.
- A stray third element on fixed-width types (`u8`, `s16`, etc.).
- Byte-aligned fields (`u*`/`s*`/`bytes`/`rest`/`length-prefixed`) that
  don't start on a byte boundary.
- Trailing fixed-width portion that isn't a whole number of bytes.
- Any field after `rest`.
- `length-prefixed` whose prefix type isn't one of `u8`/`u16`/`u24`/`u32`.

`schema-construct` additionally rejects:

- Bindings that aren't a list of pairs.
- Duplicate binding keys.
- Binding keys that don't appear in the schema.
- Per-field range / type violations (e.g. `u8` given 300, `s8` given
  -129, `bytes 4` given a 3-element vector or a non-vector).
- `length-prefixed` values whose length exceeds the prefix width (e.g.
  more than 255 bytes with a `u8` prefix).

`schema-deconstruct` additionally rejects:

- Non-buffer input.
- Fixed-only schemas whose buffer length doesn't match the schema width.
- `length-prefixed` fields whose decoded length runs past the buffer end.
- Trailing bytes after a variable-length schema is fully consumed.

All errors raise a `packet-schema-error` tagged list and can be caught
with `guard`.

## Known limitations

- **`bits N` only covers N in 1..8.** To pack wider bit-fields, split
  along byte boundaries (e.g. a 12-bit value as `bits 4` + `u8`) or use
  one of the `uN` / `sN` types if your field is byte-aligned anyway.
- **`u32` and `u32/le` cap at 2^31 − 1.** `vm_integer_t` is signed int32,
  so the high half of the unsigned range can't be represented. Use
  `bytes 4` for full u32.
- **No checksum or CRC primitives.** Most real protocols need one; the
  schema layer doesn't generate or verify them.
- **Per-call validation cost.** Every `schema-construct` / `schema-
  deconstruct` re-walks the schema. For hot paths, profile first.
- **No compile-time schema validation.** Schemas are pure data so a typo
  surfaces only at first runtime use.
- **No interleaved TLV / repeated-structure types.** `rest` covers
  "remaining bytes" and `length-prefixed` covers a single length + bytes
  block; anything richer (lists of TLVs, optional fields, repeated
  records) must be parsed by hand from a `rest` payload.

## Reference example and tests

`apps/networking/packet3.scm` exercises the full type table on a real
DIO header plus a mixed-type schema. Schema-layer tests live with the
rest of the VM unit tests:

  - `tests/unit-tests/vm-specific/test-packet-schema.scm` — fixed-width
    types, validation, range checks, structural binding errors.
  - `tests/unit-tests/vm-specific/test-packet-varlen.scm` — `rest` and
    `length-prefixed` round-trips and error paths.

Both run under `tests/unit-tests/run-tests.sh`.
