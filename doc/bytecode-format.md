# VeloxVM Bytecode Format Specification

## Overview

VeloxVM uses a custom bytecode format designed for efficient storage and execution on resource-constrained IoT devices. The bytecode is optimized for functional programming patterns (Scheme/Cyclus) and uses high-level instructions to minimize code size for efficient over-the-air updates.

## File Format

### Magic Number and Header

VeloxVM bytecode files (.vm) begin with a 3-byte header:

```
Offset  Size  Description
------  ----  -----------
0x00    1     File ID 1: 0x5E (94 decimal)
0x01    1     File ID 2: 0xB5 (181 decimal)
0x02    1     Bytecode version (currently 1)
```

The magic number `0x5E 0xB5` (or `0xB55E` in little-endian short format) can be used to identify VeloxVM bytecode files with the `file` utility.

### File Structure

After the header, the file contains three variable-length tables:

1. **String Table** - Contains all string literals used in the program
2. **Symbol Table** - Contains symbol names for variables and procedures
3. **Expression Table** (Form Table) - Contains the actual bytecode for all forms/expressions

### Table Encoding Format

Each table is encoded as follows:

```
Offset  Size  Description
------  ----  -----------
0x00    1     Item count (N) - number of items in table (max 255)
0x01    var   Table items (N entries)
```

Each table item is encoded as:

```
Offset  Size  Description
------  ----  -----------
0x00    1     Item length (L) - length of this item in bytes (max 255)
0x01    L     Item data (raw bytes)
```

The loader performs two passes:
1. First pass: calculate total table size by scanning all item lengths
2. Second pass: allocate memory and read all item data

## Bytecode Instruction Encoding

### Instruction Byte Layout

The VM uses a sophisticated bit-packing scheme for the first byte of each instruction:

```
Bit 7: Token type (0 = ATOM, 1 = FORM)
Bits 6-0: Type-specific encoding (see below)
```

### Token Types

#### ATOM Tokens (bit 7 = 0)

For atoms (data values), the byte is encoded as:

```
Bit 7:   0 (indicates ATOM)
Bits 6-4: Embedded data (type-specific)
Bits 3:   Embedded data (type-specific)
Bits 2-0: Object type (VM_ATOM_MASK = 0x7)
```

Object types (bits 2-0):
- `0` - VM_TYPE_BOOLEAN
- `1` - VM_TYPE_INTEGER
- `2` - VM_TYPE_RATIONAL
- `3` - VM_TYPE_REAL (if enabled)
- `4` - VM_TYPE_STRING
- `5` - VM_TYPE_SYMBOL
- `6` - VM_TYPE_CHARACTER
- `7` - (reserved for VM_TYPE_FORM when bit 7 = 1)

#### FORM Tokens (bit 7 = 1)

For forms (function calls/expressions), the byte is encoded as:

```
Bit 7:   1 (indicates FORM)
Bits 6:  Reserved
Bits 5-4: Form type (2 bits)
Bits 3-0: Type-specific data
```

Form types (bits 5-4):
- `0` - VM_FORM_INLINE - Inline function call
- `1` - VM_FORM_LAMBDA - Lambda expression reference
- `2` - VM_FORM_REF - Form reference

## Type-Specific Encodings

### Boolean (VM_TYPE_BOOLEAN)

```
Byte 0, bits 6-4: unused
Byte 0, bit 3:    boolean value (0 = false, 1 = true)
Byte 0, bits 2-0: 0 (type = BOOLEAN)
```

No additional bytes follow.

### Integer (VM_TYPE_INTEGER)

```
Byte 0, bit 6-4:  unused
Byte 0, bit 3:    sign (0 = positive, 1 = negative)
Byte 0, bits 2-0: 1 (type = INTEGER)
Byte 1, bits 6-4: unused
Byte 1, bits 3:   sign (0 = positive, 1 = negative)
Byte 1, bits 2-0: size in bytes (1-4)
Bytes 2+:         integer value (1-4 bytes, big-endian)
```

Integer values are stored in big-endian format with variable length (1-4 bytes).
- The sign bit indicates if the value should be negated after reading
- Maximum value: 2,147,483,647 (INT32_MAX)
- Minimum value: -2,147,483,648 (INT32_MIN)

**Error checking**: If size = 4 bytes (maximum) and the most significant bit is set, but the sign bit indicates positive, this is an overflow error.

### Rational (VM_TYPE_RATIONAL)

```
Byte 0, bits 6-3: unused
Byte 0, bits 2-0: 2 (type = RATIONAL)
Bytes 1+:         numerator (encoded as integer)
Bytes n+:         denominator (encoded as integer)
```

Two consecutive integer encodings: numerator followed by denominator.
The denominator must not be zero (checked at load time).

### String (VM_TYPE_STRING)

```
Byte 0, bits 6-3: unused
Byte 0, bits 2-0: 4 (type = STRING)
Byte 1:           string ID (index into string table)
```

Strings are stored by reference to the string table. The string ID is an 8-bit index.
String objects are marked as immutable (VM_STRING_FLAG_IMMUTABLE) and ID-based (VM_STRING_FLAG_ID).

### Character (VM_TYPE_CHARACTER)

```
Byte 0, bits 6-3: unused
Byte 0, bits 2-0: 6 (type = CHARACTER)
Byte 1:           character value (8-bit ASCII/byte)
```

Characters are single bytes.

### Symbol (VM_TYPE_SYMBOL)

```
Byte 0, bits 6-3: unused
Byte 0, bits 2-0: 5 (type = SYMBOL)
Byte 1, bit 7:    scope (0 = core, 1 = app)
Byte 1, bit 6:    extended flag (0 = 2-byte ID, 1 = 1-byte ID)
Byte 1, bits 5-0: symbol ID (lower 6 bits)
[Byte 2]:         symbol ID (lower 8 bits) - only if bit 6 of byte 1 is set
```

Symbols reference entries in the symbol table:
- **Scope**: VM_SYMBOL_SCOPE_CORE (0) for built-in symbols, VM_SYMBOL_SCOPE_APP (1) for application-defined symbols
- **Symbol ID**: Can be 6 bits (0-63) or 14 bits (0-16383) for extended form

### Form - Inline (VM_FORM_INLINE)

```
Byte 0, bit 7:    1 (FORM token)
Byte 0, bits 6:   unused
Byte 0, bits 5-4: 0 (INLINE form type)
Byte 0, bits 3-0: unused
Byte 1, bits 7-6: unused
Byte 1, bits 5-0: argument count (0-63)
```

Inline forms represent direct function calls with the argument count specified.
The form ID is 0 for inline forms (the instruction is executed directly without a form table lookup).

### Form - Lambda/Reference (VM_FORM_LAMBDA or VM_FORM_REF)

```
Byte 0, bit 7:    1 (FORM token)
Byte 0, bit 6:    unused
Byte 0, bits 5-4: 1 (LAMBDA) or 2 (REF)
Byte 0, bits 3-0: expression ID (lower 4 bits)
Byte 1, bit 4:    extended flag (0 = extended 12-bit ID, 1 = 4-bit ID)
[Byte 2]:         expression ID (lower 8 bits) - only if bit 4 of byte 1 is 0
```

Expression IDs reference entries in the expression table:
- **Short form**: 4-bit expression ID (0-15) when bit 4 of byte 1 is set
- **Extended form**: 12-bit expression ID (0-4095) using byte 0 bits 3-0 as high nibble and byte 2 as low byte

## Instruction Pointer Management

The VM maintains an instruction pointer (IP) that advances through the bytecode:

```c
VM_IP(thread)              // Current instruction byte
VM_STEP(thread)            // Advance IP by 1 byte (with boundary check)
VM_STEP_N(thread, n)       // Advance IP by n bytes (with boundary check)
VM_CHECK_BOUNDARY(thread, n) // Check if n bytes are available
```

All instruction decoding includes boundary checking to prevent buffer overruns.

## Object Type Enumeration

Complete list of VM object types:

```
Value  Type              Description
-----  ----------------  -----------
0      VM_TYPE_BOOLEAN   True/false values
1      VM_TYPE_INTEGER   32-bit signed integers
2      VM_TYPE_RATIONAL  Fraction (numerator/denominator)
3      VM_TYPE_REAL      Double-precision floating point (optional)
4      VM_TYPE_STRING    Immutable string references
5      VM_TYPE_SYMBOL    Variable/procedure name references
6      VM_TYPE_CHARACTER Single byte characters
7      VM_TYPE_FORM      Function call expressions
8      VM_TYPE_LIST      Linked lists
9      VM_TYPE_VECTOR    Arrays/byte buffers
10     VM_TYPE_PORT      I/O ports (files, sockets, devices)
11     VM_TYPE_COMPLEX   Complex numbers
12     VM_TYPE_PROCEDURE Built-in procedures
13     VM_TYPE_EXTERNAL  External library objects
14     VM_TYPE_NONE      Unbound/empty value
```

Note: Types 8-14 are runtime types created during execution and do not have direct bytecode encodings. They are created through VM operations.

## Encoding Built-In Instruction Calls

### Symbol-Based Instruction Encoding

VeloxVM's 191 built-in instructions (see `doc/instruction-set.md`) are accessed through the symbol mechanism. Each built-in instruction corresponds to a core symbol that references an entry in the operators table defined in `core/vm-procedures.c`.

### Core vs Application Symbols

Symbols have two scopes:

1. **Core Scope (0)**: Built-in VM instructions
   - Symbol IDs 0-190 map directly to the operators table
   - Examples: `+` (add), `cons`, `car`, `string-append`, `thread-create`
   - Defined in `vm_procedure_lookup()` at core/vm-procedures.c:399

2. **Application Scope (1)**: User-defined functions and imported libraries
   - Symbol IDs reference the program's symbol table
   - Can bind to lambdas or library procedures

### Built-In Instruction Call Encoding

A call to a built-in instruction like `(+ 1 2)` is encoded in bytecode as a sequence of objects that the VM evaluates:

#### Method 1: Expression Table Entry

The most common encoding uses the expression table where the entire expression is stored:

1. **Expression table** contains the bytecode for the body:
   - Symbol reference to `+` (core scope, ID for operator `add`)
   - Integer 1
   - Integer 2

2. **Main bytecode** contains a form reference to this expression

#### Method 2: Direct Encoding in Bytecode Stream

For inline expressions, the bytecode stream directly contains:

1. Symbol for the operator (core scope)
2. Argument values
3. Form instruction with argument count

### Symbol Resolution at Runtime

When the VM encounters a symbol with core scope:

1. **Decoding** (core/vm-bytecode.c:133):
   ```c
   void get_symbol_ref(vm_thread_t *thread, vm_symbol_ref_t *symbol_ref)
   {
     // Extract scope (bit 7 of byte 1)
     symbol_ref->scope = (byte >> 7) & 0x1;

     // Extract symbol ID (6-14 bits)
     symbol_ref->symbol_id = byte & 0x3f;
     // ... potentially extended with another byte
   }
   ```

2. **Lookup** (core/vm-procedures.c:399):
   ```c
   const vm_procedure_t *
   vm_procedure_lookup(vm_program_t *program, vm_symbol_ref_t *symbol_ref)
   {
     if(symbol_ref->scope == VM_SYMBOL_SCOPE_CORE) {
       // Symbol ID indexes into operators array
       return &operators[symbol_ref->symbol_id];
     }
     // ... handle application scope
   }
   ```

3. **Execution**: The operator's function pointer is called with the evaluated arguments

### Operators Table Indices

The operators table in `core/vm-procedures.c` defines instructions in a specific order. The index of each operator in this array is its symbol ID. For example:

```c
static const vm_procedure_t operators[] = {
  VM_OPERATOR(add, ...),           // Index 0: symbol ID for '+'
  VM_OPERATOR(subtract, ...),      // Index 1: symbol ID for '-'
  VM_OPERATOR(multiply, ...),      // Index 2: symbol ID for '*'
  VM_OPERATOR(divide, ...),        // Index 3: symbol ID for '/'
  // ... 187 more operators
};
```

The Scheme compiler maps symbol names to these indices:
- `+` → symbol ID 0 (core scope)
- `-` → symbol ID 1 (core scope)
- `cons` → symbol ID (whatever index it appears at)
- etc.

### Example: Encoding `(+ 1 2)`

A Scheme expression `(+ 1 2)` compiles to bytecode that might be stored in the expression table as:

```
; Symbol for '+' (core scope, ID 0)
Byte 0: 0x05  (bit 7=0 [ATOM], bits 2-0=101 [SYMBOL])
Byte 1: 0x00  (bit 7=0 [core scope], bit 6=1 [6-bit ID], bits 5-0=000000 [ID 0])

; Integer 1
Byte 2: 0x09  (bit 7=0 [ATOM], bits 2-0=001 [INTEGER])
Byte 3: 0x01  (1 byte size, positive)
Byte 4: 0x01  (value = 1)

; Integer 2
Byte 5: 0x09  (bit 7=0 [ATOM], bits 2-0=001 [INTEGER])
Byte 6: 0x01  (1 byte size, positive)
Byte 7: 0x02  (value = 2)

; Inline form with 3 items (operator + 2 arguments)
Byte 8: 0x80  (bit 7=1 [FORM], bits 5-4=00 [INLINE])
Byte 9: 0x03  (argc = 3)
```

When this expression executes:
1. VM reads the symbol for `+` → resolves to operators[0] (the add procedure)
2. VM reads integer 1 → pushes on stack
3. VM reads integer 2 → pushes on stack
4. VM encounters inline form with argc=3
5. VM pops 3 values (symbol + 2 args), evaluates symbol to procedure
6. VM calls the add procedure with 2 integer arguments
7. Result (3) is pushed back on stack

### Example: Encoding `(cons 'a '())`

A call to `cons` with quoted arguments:

```
; Assume 'cons' has symbol ID 17 in operators table

; Symbol for 'cons' (core scope, ID 17)
Byte 0: 0x05  (SYMBOL type)
Byte 1: 0x11  (core scope, 6-bit ID=17)

; Quote form wrapping symbol 'a' (application scope)
; ... quoted symbol encoding ...

; Quote form wrapping empty list
; ... quoted empty list encoding ...

; Inline form with 3 items
Byte N: 0x80  (FORM INLINE)
Byte N+1: 0x03  (argc = 3)
```

The VM evaluates this by:
1. Resolving `cons` symbol → operators[17]
2. Evaluating quoted arguments (returns them unevaluated)
3. Calling the cons operator with 2 arguments
4. Returning the new pair

### Special Forms vs Regular Procedures

Some built-in instructions are **special forms** that don't evaluate their arguments before execution:

- `if`, `and`, `or` - Need to control which branches evaluate
- `quote` - Prevents evaluation entirely
- `define`, `set!` - Need unevaluated symbol names
- `lambda`, `begin` - Control evaluation order

These are marked with flags=0 (no VM_PROCEDURE_EVAL_ARGS) in the operators table. The VM handles their arguments specially during evaluation.

Regular procedures (like `+`, `cons`, `car`) are marked with VM_PROCEDURE_EVAL_ARGS, causing all arguments to be evaluated before the procedure is called.

## Example Bytecode Sequences

### Example 1: Integer 42

```
Byte 0: 0x09  (bit 7=0 [ATOM], bits 6-4=001, bit 3=0 [positive], bits 2-0=001 [INTEGER])
Byte 1: 0x01  (bits 2-0=001 [1 byte size])
Byte 2: 0x2A  (42 in decimal)
```

### Example 2: Integer -100

```
Byte 0: 0x09  (bit 7=0 [ATOM], bit 3=0, bits 2-0=001 [INTEGER])
Byte 1: 0x09  (bit 3=1 [negative], bits 2-0=001 [1 byte size])
Byte 2: 0x64  (100 in decimal, negated due to sign bit)
```

### Example 3: Boolean True

```
Byte 0: 0x08  (bit 7=0 [ATOM], bit 3=1 [true], bits 2-0=000 [BOOLEAN])
```

### Example 4: String "hello" (ID 5 in string table)

```
Byte 0: 0x04  (bit 7=0 [ATOM], bits 2-0=100 [STRING])
Byte 1: 0x05  (string table index)
```

### Example 5: Inline Form with 3 Arguments

```
Byte 0: 0x80  (bit 7=1 [FORM], bits 5-4=00 [INLINE])
Byte 1: 0x03  (bits 5-0=000011 [argc=3])
```

## Design Rationale

The bytecode format is optimized for:

1. **Compactness**: Variable-length encodings minimize code size for efficient over-the-air transmission
2. **Simplicity**: Direct mapping from high-level Scheme/Cyclus constructs
3. **Performance**: Bit-packed instructions reduce memory overhead on constrained devices
4. **Safety**: Boundary checks prevent buffer overruns during bytecode parsing
5. **Extensibility**: Expression tables allow lambda closures and code reuse

The format achieves a balance between execution efficiency and code density suitable for IoT devices with as little as 32 kB RAM and 256 kB ROM.

## References

- `doc/instruction-set.md` - Complete instruction set reference
- `include/vm-bytecode.h` - Bytecode format constants
- `core/vm-bytecode.c` - Bytecode parsing implementation
- `core/vm-loader.c` - Bytecode file loading
- `core/vm-procedures.c` - Operators table and instruction lookup
- `include/vm-objects.h` - Object type definitions
- `doc/magic` - File magic number for `file` utility
