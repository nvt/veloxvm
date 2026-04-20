"""
VeloxVM Bytecode Disassembler

Reads .vm bytecode files and outputs equivalent Scheme code.
"""

import sys
from pathlib import Path
from typing import List, Tuple, Optional
from .primitives import VM_PRIMITIVES


class Disassembler:
    def __init__(self, bytecode_path: Path):
        self.path = bytecode_path
        self.data = bytecode_path.read_bytes()
        self.pos = 0
        self.strings = []
        self.symbols = []
        self.expressions = []

    def read_byte(self) -> int:
        """Read a single byte."""
        if self.pos >= len(self.data):
            raise EOFError(f"Unexpected end of file at position {self.pos}")
        b = self.data[self.pos]
        self.pos += 1
        return b

    def read_u16(self) -> int:
        """Read a 16-bit unsigned integer (little-endian)."""
        low = self.read_byte()
        high = self.read_byte()
        return (high << 8) | low

    def read_bytes(self, n: int) -> bytes:
        """Read n bytes."""
        result = self.data[self.pos:self.pos + n]
        if len(result) < n:
            raise EOFError(f"Expected {n} bytes, got {len(result)}")
        self.pos += n
        return result

    def disassemble(self) -> str:
        """Disassemble the entire bytecode file."""
        # Read header
        magic1 = self.read_byte()
        magic2 = self.read_byte()
        version = self.read_byte()

        if magic1 != 0x5E or magic2 != 0xB5:
            raise ValueError(f"Invalid magic bytes: 0x{magic1:02X}{magic2:02X}")

        output = [f"; VeloxVM Bytecode Disassembly"]
        output.append(f"; Magic: 0x{magic1:02X}{magic2:02X}, Version: {version}")
        output.append("")

        # Read string table
        string_count = self.read_u16()
        output.append(f"; String table ({string_count} entries):")
        for i in range(string_count):
            length = self.read_u16()
            string_data = self.read_bytes(length)
            string_val = string_data.decode('utf-8', errors='replace')
            self.strings.append(string_val)
            output.append(f";   [{i}] {repr(string_val)}")
        output.append("")

        # Read symbol table
        symbol_count = self.read_u16()
        output.append(f"; Symbol table ({symbol_count} entries):")
        for i in range(symbol_count):
            length = self.read_u16()
            symbol_data = self.read_bytes(length)
            symbol_val = symbol_data.decode('utf-8', errors='replace')
            self.symbols.append(symbol_val)
            output.append(f";   [{i}] {symbol_val}")
        output.append("")

        # Read expression table
        expr_count = self.read_u16()
        output.append(f"; Expression table ({expr_count} entries):")
        for i in range(expr_count):
            length = self.read_u16()
            expr_data = self.read_bytes(length)
            self.expressions.append(expr_data)
        output.append("")

        # Disassemble each expression
        for i, expr_data in enumerate(self.expressions):
            output.append(f"; Expression {i} ({len(expr_data)} bytes): {expr_data.hex()[:80]}")
            try:
                expr_pos = 0
                forms = []

                # For expression 0 (main program), parse all forms until exhausted
                # For other expressions, parse single form
                while expr_pos < len(expr_data):
                    scheme_code, expr_pos = self.disassemble_expr(expr_data, expr_pos)
                    forms.append(scheme_code)

                    # For non-zero expressions, only parse one form
                    if i != 0:
                        break

                if i == 0 and len(forms) > 1:
                    # Multiple forms in main expression - show as sequence
                    for form in forms:
                        output.append(form)
                elif forms:
                    output.append(forms[0])

                # Check for remaining bytes (usually shouldn't happen now)
                if expr_pos < len(expr_data):
                    remaining = expr_data[expr_pos:]
                    output.append(f";   WARNING: {len(remaining)} bytes remaining: {remaining.hex()}")
            except Exception as e:
                import traceback
                output.append(f";   ERROR: {e}")
                output.append(f";   {traceback.format_exc()}")
            output.append("")

        return "\n".join(output)

    def disassemble_expr(self, data: bytes, pos: int) -> Tuple[str, int]:
        """
        Disassemble a single expression from bytecode.
        Returns (scheme_string, new_position).
        """
        if pos >= len(data):
            return "#<empty>", pos

        byte = data[pos]
        pos += 1

        # Check if it's a FORM (bit 7 = 1)
        if byte & 0x80:
            return self.disassemble_form(byte, data, pos)
        else:
            return self.disassemble_atom(byte, data, pos)

    def disassemble_atom(self, header: int, data: bytes, pos: int) -> Tuple[str, int]:
        """Disassemble an ATOM token."""
        atom_type = header & 0x07  # Bits 2-0 contain the object type

        if atom_type == 0:  # Boolean
            value = (header >> 3) & 0x01  # Bit 3 contains the boolean value
            return "#t" if value else "#f", pos

        elif atom_type == 1:  # Integer
            # Integer encoding: sign and size are in the header's embedded field (bits 6-3)
            # embedded = (header >> 3) & 0x0F
            # sign = (embedded >> 3) & 1
            # byte_count = embedded & 7
            embedded = (header >> 3) & 0x0F
            is_negative = (embedded >> 3) & 1
            byte_count = embedded & 0x07

            if byte_count < 1 or byte_count > 4:
                return f"#<int-invalid-size-{byte_count}>", pos

            if pos + byte_count > len(data):
                return f"#<int-truncated-need-{byte_count}-bytes>", pos

            value = 0
            for i in range(byte_count):
                value = (value << 8) | data[pos]
                pos += 1

            if is_negative:
                value = -value

            return str(value), pos

        elif atom_type == 2:  # Rational
            # Rational = numerator (integer encoding) + denominator (integer encoding)
            # Each integer is encoded with header byte containing embedded field
            if pos >= len(data):
                return "#<rational-incomplete>", pos

            # Read numerator (full integer encoding)
            num_header = data[pos]
            pos += 1
            num_embedded = (num_header >> 3) & 0x0F
            num_neg = (num_embedded >> 3) & 1
            num_bytes = num_embedded & 0x07

            if pos + num_bytes > len(data):
                return f"#<rational-num-truncated>", pos

            numerator = 0
            for i in range(num_bytes):
                numerator = (numerator << 8) | data[pos]
                pos += 1
            if num_neg:
                numerator = -numerator

            # Read denominator (full integer encoding)
            if pos >= len(data):
                return f"{numerator}/#<denom-missing>", pos

            den_header = data[pos]
            pos += 1
            den_embedded = (den_header >> 3) & 0x0F
            den_neg = (den_embedded >> 3) & 1
            den_bytes = den_embedded & 0x07

            if pos + den_bytes > len(data):
                return f"{numerator}/#<denom-truncated>", pos

            denominator = 0
            for i in range(den_bytes):
                denominator = (denominator << 8) | data[pos]
                pos += 1
            if den_neg:
                denominator = -denominator

            return f"{numerator}/{denominator}", pos

        elif atom_type == 4:  # String
            str_id = header & 0x0F
            if header & 0x10:  # Extended
                if pos >= len(data):
                    return f"#<str-ext-incomplete>", pos
                str_id = (str_id << 8) | data[pos]
                pos += 1

            if str_id >= len(self.strings):
                return f"#<str-{str_id}-out-of-bounds>", pos

            return f'"{self.strings[str_id]}"', pos

        elif atom_type == 5:  # Symbol
            # Symbol encoding: atom header (already read), then symbol data byte
            if pos >= len(data):
                return "#<sym-incomplete>", pos

            sym_byte = data[pos]
            pos += 1

            scope = (sym_byte >> 7) & 0x01
            extended = (sym_byte >> 6) & 0x01
            sym_id = sym_byte & 0x3F

            if extended:  # Extended 14-bit ID
                if pos >= len(data):
                    return f"#<sym-ext-incomplete>", pos
                high_bits = sym_id  # bits 13-8
                low_byte = data[pos]
                pos += 1
                sym_id = (high_bits << 8) | low_byte

            if scope == 0:  # Core symbol (primitive)
                if sym_id < len(VM_PRIMITIVES):
                    return VM_PRIMITIVES[sym_id], pos
                else:
                    return f"<prim-{sym_id}>", pos
            else:  # Application symbol
                if sym_id < len(self.symbols):
                    return self.symbols[sym_id], pos
                else:
                    return f"<sym-{sym_id}>", pos

        elif atom_type == 6:  # Character
            char_val = data[pos]
            pos += 1
            if 32 <= char_val <= 126:
                return f"#\\{chr(char_val)}", pos
            else:
                return f"#\\x{char_val:02x}", pos

        return f"#<unknown-atom-{atom_type}>", pos

    def disassemble_form(self, header: int, data: bytes, pos: int) -> Tuple[str, int]:
        """Disassemble a FORM token."""
        form_type = (header >> 5) & 0x03

        if form_type == 0:  # INLINE
            argc = header & 0x3F
            elements = []
            for i in range(argc):
                elem, pos = self.disassemble_expr(data, pos)
                elements.append(elem)

            if not elements:
                return "()", pos

            return f"({' '.join(elements)})", pos

        elif form_type == 1:  # LAMBDA
            expr_id = header & 0x0F
            if not (header & 0x10):  # Extended
                high = header & 0x0F
                low = data[pos]
                pos += 1
                expr_id = (high << 8) | low

            return f"(<lambda form {expr_id}>)", pos

        elif form_type == 2:  # REF
            expr_id = header & 0x0F
            if not (header & 0x10):  # Extended
                high = header & 0x0F
                low = data[pos]
                pos += 1
                expr_id = (high << 8) | low

            return f"(<form {expr_id}>)", pos

        return f"#<unknown-form-{form_type}>", pos


def disassemble_file(path: Path) -> str:
    """Disassemble a VeloxVM bytecode file."""
    dis = Disassembler(path)
    return dis.disassemble()


def main():
    """Command-line interface for disassembler."""
    import argparse

    parser = argparse.ArgumentParser(
        description='VeloxVM Bytecode Disassembler',
        formatter_class=argparse.RawDescriptionHelpFormatter
    )

    parser.add_argument('input', help='Input .vm bytecode file')
    parser.add_argument('-o', '--output', help='Output file (default: stdout)')

    args = parser.parse_args()

    try:
        result = disassemble_file(Path(args.input))

        if args.output:
            Path(args.output).write_text(result)
            print(f"Disassembled to {args.output}")
        else:
            print(result)

        return 0
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        import traceback
        traceback.print_exc()
        return 1


if __name__ == '__main__':
    sys.exit(main())
