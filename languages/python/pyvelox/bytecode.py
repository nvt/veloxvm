"""
Bytecode Container and Symbol Table Management

This module defines the data structures for managing VeloxVM bytecode compilation:
- Bytecode: Main container for compiled bytecode
- SymbolTable: Manages string and symbol tables with deduplication
"""

from typing import List, Dict, Optional


class SymbolTable:
    """Manages string and symbol tables for bytecode generation."""

    def __init__(self):
        self.strings: List[str] = []
        self.symbols: List[str] = []
        self._string_map: Dict[str, int] = {}
        self._symbol_map: Dict[str, int] = {}

    def add_string(self, s: str) -> int:
        """
        Add a string to the string table (deduplicated).

        Args:
            s: The string to add

        Returns:
            The string table index
        """
        if s in self._string_map:
            return self._string_map[s]

        idx = len(self.strings)
        self.strings.append(s)
        self._string_map[s] = idx
        return idx

    def add_symbol(self, sym: str) -> int:
        """
        Add a symbol to the symbol table (deduplicated).

        This is for application-scope symbols (user-defined variables/functions).
        Core symbols (VM primitives) are not added here.

        Args:
            sym: The symbol name to add

        Returns:
            The symbol table index
        """
        if sym in self._symbol_map:
            return self._symbol_map[sym]

        idx = len(self.symbols)
        self.symbols.append(sym)
        self._symbol_map[sym] = idx
        return idx

    def get_string_id(self, s: str) -> Optional[int]:
        """Get the ID of a string if it exists in the table."""
        return self._string_map.get(s)

    def get_symbol_id(self, sym: str) -> Optional[int]:
        """Get the ID of a symbol if it exists in the table."""
        return self._symbol_map.get(sym)


class Bytecode:
    """
    Main bytecode container for VeloxVM compilation.

    Manages:
    - Magic number and version
    - String and symbol tables
    - Expression table (compiled bytecode)
    - Various compilation state
    """

    def __init__(self):
        # File header
        self.magic = 0x5EB5  # 0x5E, 0xB5 in little-endian
        self.version = 3  # VM_BYTECODE_VERSION from include/vm-bytecode.h

        # Symbol table management
        self.symbol_table = SymbolTable()

        # Expression table: list of byte arrays
        # Expression 0 is reserved as the entry point (pre-allocated)
        self.expressions: List[bytes] = []

        # Compilation state
        self.loop_counter = 0  # For generating unique loop names
        self.temp_counter = 0  # For generating temporary variable names
        self.var_counter = 0   # For generating unique variable names

    def add_string(self, s: str) -> int:
        """Add a string to the string table."""
        return self.symbol_table.add_string(s)

    def add_symbol(self, sym: str) -> int:
        """Add an application-scope symbol to the symbol table."""
        return self.symbol_table.add_symbol(sym)

    def add_expression(self, bytecode: bytes) -> int:
        """
        Add an expression to the expression table.

        Args:
            bytecode: The compiled bytecode for this expression

        Returns:
            The expression ID (index in expression table)
        """
        expr_id = len(self.expressions)
        self.expressions.append(bytecode)
        return expr_id

    def replace_expression(self, expr_id: int, bytecode: bytes):
        """
        Replace an existing expression in the expression table.

        This is used for the CL-style compilation where expression 0
        is pre-allocated and later replaced with accumulated bytecode.

        Args:
            expr_id: The expression ID to replace
            bytecode: The new bytecode
        """
        if expr_id < len(self.expressions):
            self.expressions[expr_id] = bytecode
        else:
            raise IndexError(f"Expression ID {expr_id} out of range")

    def get_unique_loop_name(self) -> str:
        """Generate a unique loop variable name."""
        name = f"_loop_{self.loop_counter}"
        self.loop_counter += 1
        return name

    def get_unique_temp_name(self) -> str:
        """Generate a unique temporary variable name."""
        name = f"_temp_{self.temp_counter}"
        self.temp_counter += 1
        return name

    def get_unique_var_name(self, prefix: str = "_var") -> str:
        """Generate a unique variable name with custom prefix."""
        name = f"{prefix}_{self.var_counter}"
        self.var_counter += 1
        return name

    def __repr__(self) -> str:
        return (f"Bytecode(magic=0x{self.magic:04X}, version={self.version}, "
                f"strings={len(self.symbol_table.strings)}, "
                f"symbols={len(self.symbol_table.symbols)}, "
                f"expressions={len(self.expressions)})")
