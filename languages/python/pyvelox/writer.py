"""
VeloxVM Bytecode File Writer

This module handles writing compiled bytecode to .vm binary files
following the VeloxVM bytecode format specification.
"""

import struct
from pathlib import Path
from typing import Union, List
from .bytecode import Bytecode


def write_bytecode_file(path: Union[str, Path], bc: Bytecode):
    """
    Write bytecode to a .vm file.

    File format:
    - Header (3 bytes): 0x5E, 0xB5, version
    - String table: count (16-bit) + items (16-bit length + data)
    - Symbol table: count (16-bit) + items (16-bit length + data)
    - Expression table: count (16-bit) + items (16-bit length + data)

    Args:
        path: Output file path
        bc: Bytecode container to write
    """
    with open(path, 'wb') as f:
        # Write header (3 bytes)
        f.write(bytes([0x5E, 0xB5, bc.version]))

        # Write string table
        _write_table(f, bc.symbol_table.strings, _encode_string_item)

        # Write symbol table
        _write_table(f, bc.symbol_table.symbols, _encode_string_item)

        # Write expression table
        _write_table(f, bc.expressions, _encode_bytes_item)


def _write_table(f, items: List, encode_fn):
    """
    Write a table to the file.

    Format:
    - Count (16-bit, little-endian)
    - For each item:
      - Length (16-bit, little-endian)
      - Data (raw bytes)

    Args:
        f: File handle
        items: List of items to write
        encode_fn: Function to encode each item to bytes
    """
    count = len(items)

    # Write count (16-bit little-endian)
    f.write(struct.pack('<H', count))

    # Write each item
    for item in items:
        data = encode_fn(item)
        length = len(data)

        if length > 65535:
            raise ValueError(f"Table item too large: {length} bytes (max 65535)")

        # Write length (16-bit little-endian)
        f.write(struct.pack('<H', length))

        # Write data
        f.write(data)


def _encode_string_item(s: str) -> bytes:
    """Encode a string item as UTF-8 bytes."""
    return s.encode('utf-8')


def _encode_bytes_item(b: bytes) -> bytes:
    """Encode a bytes item (identity function for bytes)."""
    return b


def read_bytecode_file(path: Union[str, Path]) -> Bytecode:
    """
    Read bytecode from a .vm file (for debugging/testing).

    Args:
        path: Input file path

    Returns:
        Bytecode container

    Note: This is a simplified reader for testing. The VM itself
    has the authoritative bytecode loader.
    """
    with open(path, 'rb') as f:
        # Read header
        magic_bytes = f.read(2)
        if magic_bytes != bytes([0x5E, 0xB5]):
            raise ValueError(f"Invalid magic number: {magic_bytes.hex()}")

        version = f.read(1)[0]

        # Create bytecode container
        bc = Bytecode()
        bc.version = version

        # Read string table
        strings = _read_table(f)
        for s in strings:
            bc.symbol_table.strings.append(s.decode('utf-8'))

        # Read symbol table
        symbols = _read_table(f)
        for s in symbols:
            bc.symbol_table.symbols.append(s.decode('utf-8'))

        # Read expression table
        bc.expressions = _read_table(f)

        return bc


def _read_table(f) -> List[bytes]:
    """
    Read a table from the file.

    Args:
        f: File handle

    Returns:
        List of items (as bytes)
    """
    # Read count (16-bit little-endian)
    count_bytes = f.read(2)
    if len(count_bytes) < 2:
        raise ValueError("Unexpected end of file reading table count")

    count = struct.unpack('<H', count_bytes)[0]

    items = []
    for _ in range(count):
        # Read length (16-bit little-endian)
        length_bytes = f.read(2)
        if len(length_bytes) < 2:
            raise ValueError("Unexpected end of file reading item length")

        length = struct.unpack('<H', length_bytes)[0]

        # Read data
        data = f.read(length)
        if len(data) < length:
            raise ValueError(f"Unexpected end of file reading item data (expected {length}, got {len(data)})")

        items.append(data)

    return items


def dump_bytecode(bc: Bytecode, verbose: bool = False):
    """
    Print a human-readable dump of bytecode (for debugging).

    Args:
        bc: Bytecode container
        verbose: If True, show detailed hex dumps
    """
    print(f"VeloxVM Bytecode Dump")
    print(f"{'='*60}")
    print(f"Magic: 0x{bc.magic:04X}")
    print(f"Version: {bc.version}")
    print()

    print(f"String Table ({len(bc.symbol_table.strings)} entries):")
    for i, s in enumerate(bc.symbol_table.strings):
        preview = s if len(s) <= 40 else s[:37] + '...'
        print(f"  [{i}] {repr(preview)}")
    print()

    print(f"Symbol Table ({len(bc.symbol_table.symbols)} entries):")
    for i, s in enumerate(bc.symbol_table.symbols):
        print(f"  [{i}] {s}")
    print()

    print(f"Expression Table ({len(bc.expressions)} entries):")
    for i, expr in enumerate(bc.expressions):
        size = len(expr)
        print(f"  [{i}] {size} bytes", end='')

        if verbose and size > 0:
            hex_str = expr[:min(16, size)].hex(' ')
            if size > 16:
                hex_str += ' ...'
            print(f" - {hex_str}")
        else:
            print()
