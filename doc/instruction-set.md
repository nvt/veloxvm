# VeloxVM Instruction Set Reference

## Overview

VeloxVM provides 191 core instructions (procedures/operators) that implement a comprehensive Scheme-based runtime for IoT applications. Instructions are organized into functional categories and follow Scheme R5RS naming conventions with extensions for IoT-specific operations.

All instructions are implemented as procedures in the operators table defined in `core/vm-procedures.c`. Additional platform-specific instructions are available through loadable libraries on Contiki/Contiki-NG ports.

## Instruction Format

Each instruction is defined with the following properties:

- **Name**: Scheme procedure name (e.g., `+`, `car`, `make-vector`)
- **Type Requirements**: Expected argument types (VM_TYPE_FLAG)
- **Argument Count**: Minimum and maximum number of arguments (-1 = variadic)
- **Flags**: VM_PROCEDURE_EVAL_ARGS (arguments evaluated before call) or 0 (special form)

## Core Instruction Set (191 Instructions)

### Arithmetic Operations (11 instructions)

Basic mathematical operations on integers and rationals:

| Instruction | Args | Description | Returns |
|-------------|------|-------------|---------|
| `+` (add) | 0-∞ | Add numbers. Zero args returns 0. | Integer or rational (normalized) |
| `-` (subtract) | 1-∞ | Subtract numbers. One arg negates it. | Integer or rational (normalized) |
| `*` (multiply) | 0-∞ | Multiply numbers. Zero args returns 1. | Integer or rational (normalized) |
| `/` (divide) | 1-∞ | Divide numbers. | Integer or rational (normalized) |
| `gcd` | 0-∞ | Greatest common divisor. | Integer |
| `lcm` | 0-∞ | Least common multiple. | Integer |
| `numerator` | 1 | Get numerator of rational or integer. | Integer |
| `denominator` | 1 | Get denominator of rational (1 for integers). | Integer |
| `quotient` | 2 | Integer division quotient. | Integer |
| `remainder` | 2 | Integer division remainder. | Integer |
| `modulo` | 2 | Modulo operation. | Integer |

**Note**: Rational results are automatically normalized (reduced to lowest terms). If denominator is 1, an integer is returned instead.

**Implementation**: `core/expr-math.c`

### Floating Point Operations (16 instructions)

Mathematical functions using floating-point arithmetic:

| Instruction | Args | Description | Returns |
|-------------|------|-------------|---------|
| `floor` | 1 | Round down to nearest integer. | Real |
| `ceiling` | 1 | Round up to nearest integer. | Real |
| `round` | 1 | Round to nearest integer. | Real |
| `truncate` | 1 | Truncate to integer. | Real |
| `exp` | 1 | Exponential (e^x). | Real |
| `log` | 1 | Natural logarithm. | Real |
| `sin` | 1 | Sine. | Real |
| `cos` | 1 | Cosine. | Real |
| `tan` | 1 | Tangent. | Real |
| `asin` | 1 | Arcsine. | Real |
| `acos` | 1 | Arccosine. | Real |
| `atan` | 1 | Arctangent. | Real |
| `sqrt` | 1 | Square root. | Real |
| `expt` | 1 | Exponentiation. | Real |
| `exact->inexact` | 1 | Convert exact to inexact number. | Real |
| `inexact->exact` | 1 | Convert inexact to exact number. | Integer or rational |

**Note**: Real (double-precision floating point) support is optional and controlled by VM_ENABLE_REALS.

**Implementation**: `core/expr-fp.c`

### Comparison Operations (7 instructions)

Numeric and general comparisons:

| Instruction | Args | Description | Returns |
|-------------|------|-------------|---------|
| `=` (equal) | 0-∞ | Numeric equality. | Boolean (#t or #f) |
| `!=` (different) | 0-∞ | Numeric inequality. | Boolean (#t or #f) |
| `<` (less_than) | 0-∞ | Less than comparison. | Boolean (#t or #f) |
| `<=` (less_than_equal) | 0-∞ | Less than or equal. | Boolean (#t or #f) |
| `>` (greater_than) | 0-∞ | Greater than comparison. | Boolean (#t or #f) |
| `>=` (greater_than_equal) | 0-∞ | Greater than or equal. | Boolean (#t or #f) |
| `zero?` (zerop) | 1 | Test if number is zero. | Boolean (#t or #f) |

**Note**: Comparisons accept rationals by comparing numerators if denominators are equal.

**Implementation**: `core/expr-cmp.c`

### Control Flow and Special Forms (14 instructions)

Core language constructs that control evaluation:

| Instruction | Args | Eval Args | Description | Returns |
|-------------|------|-----------|-------------|---------|
| `bind` | ∞ | No | Bind variables in lambda. | Result of body expression |
| `return` | 0-1 | Yes | Return from procedure. | Exits function with value (or unspecified) |
| `begin` | ∞ | No | Sequential evaluation. | Result of last expression |
| `if` | 2-3 | No | Conditional expression. | Result of chosen branch |
| `define` | 1-2 | No | Define variable or procedure. | Unspecified (side effect only) |
| `set!` | 2 | No | Mutate variable binding. | Unspecified (side effect only) |
| `and` | ∞ | No | Logical AND (short-circuit). | #f or value of last expression |
| `or` | ∞ | No | Logical OR (short-circuit). | First true value or #f |
| `apply` | 2 | No | Apply procedure to argument list. | Result of procedure call |
| `quote` | 1 | No | Prevent evaluation. | Unevaluated argument |
| `call/cc` | 0 | No | Call with current continuation. | Result of continuation call |
| `values` | 0 | No | Return multiple values. | Multiple values |
| `call-with-values` | 0 | No | Call with multiple values. | Result of consumer procedure |
| `dynamic-wind` | 3 | No | Dynamic extent control. | Result of body thunk |
| `eval` | 0 | No | Evaluate expression. | Result of evaluation |

**Implementation**: `core/expr-primitives.c`

### Type Predicates (13 instructions)

Test object types:

| Instruction | Args | Description | Returns |
|-------------|------|-------------|---------|
| `number?` | 1 | Test if number. | Boolean (#t or #f) |
| `integer?` | 1 | Test if integer. | Boolean (#t or #f) |
| `rational?` | 1 | Test if rational. | Boolean (#t or #f) |
| `real?` | 1 | Test if real. | Boolean (#t or #f) |
| `complex?` | 1 | Test if complex. | Boolean (#t or #f) |
| `exact?` | 1 | Test if exact number. | Boolean (#t or #f) |
| `inexact?` | 1 | Test if inexact number. | Boolean (#t or #f) |
| `procedure?` | 1 | Test if procedure. | Boolean (#t or #f) |
| `boolean?` | 1 | Test if boolean. | Boolean (#t or #f) |
| `port?` | 1 | Test if I/O port. | Boolean (#t or #f) |
| `not` | 1 | Logical negation. | Boolean (#t or #f) |
| `eq?` | ∞ | Test object identity. | Boolean (#t or #f) |
| `eqv?` | ∞ | Test equivalence. | Boolean (#t or #f) |
| `equal?` | ∞ | Test structural equality. | Boolean (#t or #f) |

**Implementation**: `core/expr-primitives.c`

### List Operations (24 instructions)

Fundamental list manipulation:

| Instruction | Args | Description | Returns |
|-------------|------|-------------|---------|
| `list` | ∞ | Create list from arguments. | List |
| `cons` | 2 | Construct pair (prepend to list). | Pair (list with car and cdr) |
| `push` | 2 | Push element onto list. | New list with element prepended |
| `pop` | 1 | Pop element from list. | First element of list |
| `car` | 1 | Get first element of pair/list. | First element (any type) |
| `cdr` | 1 | Get rest of pair/list. | Rest of list or cdr value |
| `list-ref` | 2 | Get element at index. | Element at index (any type) |
| `list-tail` | 2 | Get sublist starting at index. | List starting at index |
| `append` | 2-∞ | Concatenate lists. | New concatenated list |
| `remove` | 2 | Remove element from list. | New list without element |
| `reverse` | 1 | Reverse list. | Reversed list |
| `length` | 1 | Get list length. | Integer (list length) |
| `null?` | 1 | Test if empty list. | Boolean (#t or #f) |
| `list?` | 1 | Test if proper list. | Boolean (#t or #f) |
| `pair?` | 1 | Test if pair. | Boolean (#t or #f) |
| `set-car!` | 2 | Mutate first element. | Unspecified (side effect only) |
| `set-cdr!` | 2 | Mutate rest of pair. | Unspecified (side effect only) |
| `memq` | 2 | Find member using eq?. | Sublist starting at match or #f |
| `memv` | 2 | Find member using eqv?. | Sublist starting at match or #f |
| `member` | 2 | Find member using equal?. | Sublist starting at match or #f |
| `assq` | 2 | Association list lookup using eq?. | Matching pair or #f |
| `assv` | 2 | Association list lookup using eqv?. | Matching pair or #f |
| `assoc` | 2 | Association list lookup using equal?. | Matching pair or #f |

**Implementation**: `core/expr-list.c`

### Higher-Order List Functions (5 instructions)

Functional programming primitives:

| Instruction | Args | Description | Returns |
|-------------|------|-------------|---------|
| `map` | 2-∞ | Apply procedure to each element. | List of results |
| `filter` | 2-∞ | Filter list by predicate. | List of matching elements |
| `for-each` | 2-∞ | Apply procedure for side effects. | Unspecified (side effect only) |
| `reduce` | 2-∞ | Fold list from left. | Accumulated result value |
| `count` | 2-∞ | Count elements matching predicate. | Integer (count) |

**Implementation**: `core/expr-list.c`

### Character Operations (7 instructions)

Character manipulation:

| Instruction | Args | Description | Returns |
|-------------|------|-------------|---------|
| `char?` | 1 | Test if character. | Boolean (#t or #f) |
| `char-compare` | 2 | Compare characters. | Integer (-1, 0, or 1) |
| `char-class` | 1 | Get character class (alpha, digit, etc.). | Symbol (classification) |
| `char->integer` | 1 | Convert character to integer code. | Integer (ASCII/byte value) |
| `integer->char` | 1 | Convert integer to character. | Character |
| `char-upcase` | 1 | Convert to uppercase. | Character |
| `char-downcase` | 1 | Convert to lowercase. | Character |

**Implementation**: `core/expr-char.c`

### String Operations (17 instructions)

String creation and manipulation:

| Instruction | Args | Description | Returns |
|-------------|------|-------------|---------|
| `make-string` | 1-2 | Create string of length, optionally filled. | String |
| `string` | 0-∞ | Create string from characters. | String |
| `string?` | 1 | Test if string. | Boolean (#t or #f) |
| `string-length` | 1 | Get string length. | Integer (length) |
| `string-ref` | 2 | Get character at index. | Character |
| `string-set!` | 3 | Set character at index. | Unspecified (side effect only) |
| `string->list` | 1 | Convert string to character list. | List of characters |
| `list->string` | 1 | Convert character list to string. | String |
| `vector->string` | 1 | Convert vector to string. | String |
| `string-fill!` | 1 | Fill string with character. | Unspecified (side effect only) |
| `string-compare` | 2 | Compare strings. | Integer (-1, 0, or 1) |
| `substring` | 3 | Extract substring. | String (substring) |
| `string-append` | 1-∞ | Concatenate strings. | String (concatenated) |
| `string-copy` | 1 | Copy string. | String (new copy) |
| `string-split` | 2 | Split string by delimiter. | List of strings |
| `number->string` | 1-2 | Convert number to string. | String (representation) |
| `string->number` | 1-2 | Parse number from string. | Integer, rational, or #f |

**Implementation**: `core/expr-string.c`

### Vector Operations (13 instructions)

Array/buffer operations:

| Instruction | Args | Description | Returns |
|-------------|------|-------------|---------|
| `make-vector` | 1-2 | Create vector of length, optionally filled. | Vector |
| `vector` | 1-∞ | Create vector from arguments. | Vector |
| `vector?` | 1 | Test if vector. | Boolean (#t or #f) |
| `buffer?` | 1 | Test if byte buffer. | Boolean (#t or #f) |
| `vector-merge` | 2-∞ | Merge vectors. | Vector (merged) |
| `vector-length` | 1 | Get vector length. | Integer (length) |
| `vector-ref` | 2 | Get element at index. | Element at index (any type) |
| `vector-set!` | 3 | Set element at index. | Unspecified (side effect only) |
| `vector->list` | 1 | Convert vector to list. | List |
| `list->vector` | 1 | Convert list to vector. | Vector |
| `vector-fill!` | 1 | Fill vector with value. | Unspecified (side effect only) |
| `make-buffer` | 1 | Create byte buffer. | Vector (byte buffer) |
| `buffer-append` | 3 | Append data to buffer. | Unspecified (side effect only) |

**Implementation**: `core/expr-vector.c`

### I/O Operations (17 instructions)

File and port operations:

| Instruction | Args | Description | Returns |
|-------------|------|-------------|---------|
| `input-port?` | 1 | Test if input port. | Boolean (#t or #f) |
| `output-port?` | 1 | Test if output port. | Boolean (#t or #f) |
| `current-input-port` | 0 | Get current input port. | Port (input) |
| `current-output-port` | 0 | Get current output port. | Port (output) |
| `open-input-file` | 1 | Open file for reading. | Port (input) or error |
| `open-output-file` | 1 | Open file for writing. | Port (output) or error |
| `close-port` | 1 | Close port. | Unspecified (side effect only) |
| `read-char` | 0-1 | Read character from port. | Character or EOF object |
| `read` | 0-1 | Read S-expression from port. | Object read from port |
| `peek-char` | 0-1 | Peek at next character. | Character or EOF object |
| `eof-object?` | 1 | Test if end-of-file. | Boolean (#t or #f) |
| `char-ready?` | 0-1 | Test if character available. | Boolean (#t or #f) |
| `write-char` | 1-2 | Write character to port. | Unspecified (side effect only) |
| `write` | 1-2 | Write object to port. | Unspecified (side effect only) |
| `display` | 1-2 | Display object to port. | Unspecified (side effect only) |
| `with-input-from-file` | 1-2 | Execute with redirected input. | Result of thunk |
| `with-output-to-file` | 1-2 | Execute with redirected output. | Result of thunk |

**Implementation**: `core/expr-io.c`

### Network I/O Operations (7 instructions)

TCP/UDP socket operations:

| Instruction | Args | Description | Returns |
|-------------|------|-------------|---------|
| `make-client` | 3-4 | Create client connection (proto, addr, port [, local-port]). | Port (socket) or error |
| `make-server` | 3 | Create server socket (proto, addr, port). | Port (server socket) or error |
| `peer-name` | 1 | Get peer address of connection. | Vector (address + port) |
| `accept-client` | 1 | Accept incoming connection. | Port (client socket) or #f |
| `incoming-client?` | 1 | Test if client waiting. | Boolean (#t or #f) |
| `addr->string` | 1 | Convert address vector to string. | String (IP address) |
| `resolve-hostname` | 1 | Resolve hostname to address. | Vector (address) or #f |

**Implementation**: `core/expr-ip.c`

### System Operations (10 instructions)

VM and system information:

| Instruction | Args | Description | Returns |
|-------------|------|-------------|---------|
| `system-info` | 0 | Get VM system information. | Vector [VM version, OS version] |
| `load-program` | 1 | Load VM program from file. | Unspecified or error |
| `import` | 1 | Import library. | Boolean (#t) or error |
| `get-devices` | 0 | Get available devices. | Vector of device descriptors |
| `print` | 1-∞ | Print objects to output. | Unspecified (side effect only) |
| `random` | 0-1 | Generate random number. | Integer (random value) |
| `time` | 0 | Get current time. | Vector [seconds, milliseconds] |
| `get-programs` | 0 | Get loaded programs. | List of program objects |
| `program-info` | 0-1 | Get program information. | Vector with program details |
| `exit` | 0-1 | Exit program with optional code. | Does not return |

**Implementation**: `core/expr-system.c`

### Exception Handling (2 instructions)

Error and exception control:

| Instruction | Args | Description | Returns |
|-------------|------|-------------|---------|
| `guard` | 3 | Catch exceptions (handler, test, body). | Result of body or handler |
| `raise` | 1 | Raise exception. | Does not return (throws exception) |

**Implementation**: `core/expr-exceptions.c`

### Thread Operations (10 instructions)

Preemptive multithreading:

| Instruction | Args | Description | Returns |
|-------------|------|-------------|---------|
| `thread-create` | 1 | Create new thread with thunk. | External object (thread) or error |
| `thread-fork` | 0 | Fork current thread. | External object (thread) or error |
| `thread-id` | 0 | Get current thread ID. | Integer (thread ID) |
| `thread-join` | 0 | Wait for thread completion. | Unspecified (blocks until done) |
| `thread-sleep` | 1 | Sleep for milliseconds. | Unspecified (blocks for duration) |
| `thread-specific` | 1 | Get thread-local storage. | Stored object (any type) |
| `thread-specific-set!` | 2 | Set thread-local storage. | Unspecified (side effect only) |
| `thread-terminate` | 0 | Terminate current thread. | Boolean (#t if successful) |
| `thread-yield` | 0 | Yield to scheduler. | Unspecified (side effect only) |
| `thread-stats` | 0 | Get thread statistics. | Vector of thread stats |

**Implementation**: `core/expr-thread.c`

### Mutex Operations (8 instructions)

Thread synchronization primitives:

| Instruction | Args | Description | Returns |
|-------------|------|-------------|---------|
| `mutex?` | 1 | Test if mutex. | Boolean (#t or #f) |
| `make-mutex` | 1 | Create named mutex. | External object (mutex) or error |
| `mutex-name` | 1 | Get mutex name. | String (mutex name) |
| `mutex-specific` | 1 | Get mutex-specific data. | Stored object (any type) |
| `mutex-specific-set!` | 1 | Set mutex-specific data. | Unspecified (side effect only) |
| `mutex-state` | 1 | Get mutex state. | Symbol (state: locked/unlocked) |
| `mutex-lock!` | 1 | Acquire mutex lock. | Boolean (#t if acquired) |
| `mutex-unlock!` | 1 | Release mutex lock. | Boolean (#t if released) |

**Implementation**: `core/expr-mutex.c`

### Bit Operations (6 instructions)

Bitwise manipulation:

| Instruction | Args | Description | Returns |
|-------------|------|-------------|---------|
| `bit-and` | 2 | Bitwise AND. | Integer (result) |
| `bit-or` | 2 | Bitwise OR. | Integer (result) |
| `bit-invert` | 1 | Bitwise inversion (~). | Integer (result) |
| `bit-not` | 1 | Bitwise NOT. | Integer (result) |
| `bit-xor` | 2 | Bitwise XOR. | Integer (result) |
| `bit-shift` | 2 | Bit shift (positive = left, negative = right). | Integer (result) |

**Implementation**: `core/expr-bit.c`

### Packet Operations (2 instructions)

Binary packet encoding/decoding:

| Instruction | Args | Description | Returns |
|-------------|------|-------------|---------|
| `construct-packet` | 2 | Encode data into packet (format, values). | Vector (byte buffer) |
| `deconstruct-packet` | 2 | Decode packet (format, packet). | List of decoded values |

**Note**: Format strings use type specifiers for encoding/decoding binary data structures.

**Implementation**: `core/expr-packet.c`

## Platform-Specific Instructions

### Contiki/Contiki-NG Libraries

Additional instructions available when importing platform libraries on Contiki-based systems.

#### LED Library (`(import "leds")`)

5 instructions for controlling device LEDs:

| Instruction | Args | Description | Returns |
|-------------|------|-------------|---------|
| `leds-on` | 1-∞ | Turn on LEDs (symbols: LEDRed, LEDGreen, LEDBlue, LEDAll). | Unspecified (side effect only) |
| `leds-off` | 1-∞ | Turn off LEDs. | Unspecified (side effect only) |
| `leds-set` | 1-∞ | Set LED state. | Unspecified (side effect only) |
| `leds-get` | 0 | Get current LED state. | Integer (LED bit mask) |
| `leds-toggle` | 1-∞ | Toggle LEDs. | Unspecified (side effect only) |

**Implementation**: `ports/contiki-ng/vm-lib-leds.c`, `ports/contiki/vm-lib-leds.c`

#### Radio Library (`(import "radio")`)

7 instructions for radio configuration:

| Instruction | Args | Description | Returns |
|-------------|------|-------------|---------|
| `get-cca-threshold` | 0 | Get clear channel assessment threshold. | Integer (threshold in dBm) |
| `set-cca-threshold` | 1 | Set CCA threshold. | Unspecified (side effect only) |
| `get-channel` | 0 | Get radio channel. | Integer (channel number) |
| `set-channel` | 1 | Set radio channel. | Unspecified (side effect only) |
| `get-rssi` | 0 | Get received signal strength indicator. | Integer (RSSI in dBm) |
| `get-txpower` | 0 | Get transmission power. | Integer (power in dBm) |
| `set-txpower` | 1 | Set transmission power. | Unspecified (side effect only) |

**Implementation**: `ports/contiki-ng/vm-lib-radio.c`, `ports/contiki/vm-lib-radio.c`

#### RPL Library (`(import "rpl")`)

RPL (Routing Protocol for Low-Power and Lossy Networks) operations.

Available on Contiki/Contiki-NG for IPv6 routing control. Provides functions for managing RPL routing state, parent selection, and network topology.

**Implementation**: `ports/contiki-ng/vm-lib-rpl.c`, `ports/contiki/vm-lib-rpl.c`

#### Crypto Library (`(import "crypto")`)

Cryptographic operations for secure IoT applications.

Available on Contiki/Contiki-NG for encryption and authentication. Provides functions for hashing, encryption, and message authentication codes.

**Implementation**: `ports/contiki-ng/lib/vm-lib-crypto.c`, `ports/contiki/lib/vm-lib-crypto.c`

## Instruction Execution

### Evaluation Flags

Instructions have evaluation semantics specified by flags:

- **VM_PROCEDURE_EVAL_ARGS** (0x01): Arguments are evaluated before the procedure is called (applicative order)
- **No flags (0)**: Arguments are not evaluated (special form, normal order)

Special forms like `if`, `and`, `or`, `quote`, `define`, and `set!` use normal order evaluation to control when and whether arguments are evaluated.

### Symbol Resolution

Instructions are accessed through symbol bindings:

1. **Core symbols** (scope = VM_SYMBOL_SCOPE_CORE): Built-in procedures in the operators table (0-190)
2. **App symbols** (scope = VM_SYMBOL_SCOPE_APP): User-defined lambdas and imported library procedures
3. **Lambda procedures**: User-defined functions stored as VM_TYPE_FORM objects

Symbol resolution happens in `vm_procedure_lookup()` (core/vm-procedures.c:399).

### Argument Validation

Each instruction specifies:

- **Type constraints**: Valid argument types (via type flags)
- **Arity constraints**: Minimum and maximum argument counts
  - Fixed arity: min == max
  - Variadic: max == -1
  - Optional args: min < max

Type checking occurs during procedure invocation. Invalid types trigger VM_ERROR_ARGUMENT_TYPES.

## Return Value Conventions

VeloxVM instructions follow these return value conventions:

### Value-Producing Instructions

Most instructions return a value by pushing it onto the thread's expression stack:
- **Numeric operations**: Return integers, rationals, or reals
- **Predicates**: Return boolean values (#t or #f)
- **Accessors**: Return the accessed value (any type)
- **Constructors**: Return the newly created object

### Side-Effect Instructions

Instructions marked "Unspecified (side effect only)" don't return meaningful values:
- **Mutators**: `set!`, `set-car!`, `set-cdr!`, `string-set!`, `vector-set!`
- **I/O operations**: `write`, `display`, `write-char`, `close-port`
- **Thread operations**: `thread-sleep`, `thread-yield`
- **Device control**: `leds-on`, `leds-off`, `set-channel`

These instructions are used for their side effects and their return values should not be relied upon.

### Error Handling

Instructions may signal errors instead of returning values when:
- Arguments have incorrect types (VM_ERROR_ARGUMENT_TYPES)
- Argument values are out of range (VM_ERROR_ARGUMENT_VALUE)
- Resource allocation fails (VM_ERROR_HEAP)
- I/O operations fail (VM_ERROR_IO)

Errors are propagated via the thread's status field and can be caught with the `guard` instruction.

## Instruction Profiling

When compiled with `VM_INSTRUCTION_PROFILING=1`, the VM tracks execution counts for each instruction per program. Profiling data is reported when programs are unloaded.

## References

- `include/vm-functions.h` - Function declarations
- `core/vm-procedures.c` - Operator table and lookup
- `core/expr-*.c` - Instruction implementations
- `include/vm-objects.h` - Type definitions
- `ports/*/vm-lib-*.c` - Platform-specific libraries
