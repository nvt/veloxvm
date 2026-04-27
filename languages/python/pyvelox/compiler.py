"""
PyVelox Compiler Main Entry Point

This module provides the main compilation interface for converting
Python source code to VeloxVM bytecode.
"""

import sys
from pathlib import Path
from typing import Union, Optional
from .bytecode import Bytecode
from .errors import PyveloxCompileError
from .translator import translate_python_to_bytecode
from .writer import write_bytecode_file, dump_bytecode


def compile_string(source_code: str) -> Bytecode:
    """
    Compile Python source code string to bytecode.

    Args:
        source_code: Python source code

    Returns:
        Compiled bytecode container

    Raises:
        SyntaxError: If Python code has syntax errors
        PyveloxCompileError: If code uses unsupported features
    """
    return translate_python_to_bytecode(source_code)


def compile_file(source_path: Union[str, Path],
                 output_path: Optional[Union[str, Path]] = None,
                 verbose: bool = False,
                 dump: bool = False) -> Path:
    """
    Compile a Python source file to VeloxVM bytecode.

    Args:
        source_path: Path to Python source file
        output_path: Path to output .vm file (default: source_path with .vm extension)
        verbose: If True, print compilation progress
        dump: If True, print bytecode dump after compilation

    Returns:
        Path to output file

    Raises:
        FileNotFoundError: If source file doesn't exist
        SyntaxError: If Python code has syntax errors
        PyveloxCompileError: If code uses unsupported features
    """
    source_path = Path(source_path)

    if not source_path.exists():
        raise FileNotFoundError(f"Source file not found: {source_path}")

    if output_path is None:
        output_path = source_path.with_suffix('.vm')
    else:
        output_path = Path(output_path)

    if verbose:
        print(f"Compiling {source_path} -> {output_path}")

    # Read source code
    try:
        source_code = source_path.read_text(encoding='utf-8')
    except Exception as e:
        print(f"Error reading source file: {e}", file=sys.stderr)
        raise

    # Compile to bytecode
    try:
        bc = compile_string(source_code)
    except PyveloxCompileError as e:
        # Re-render the error with the file path so users get a clickable
        # location like `prog.py:14:5: <message>` instead of just the
        # message body.
        print(e.format(source_path=source_path), file=sys.stderr)
        raise
    except SyntaxError as e:
        print(f"{source_path}:{e.lineno}: syntax error: {e.msg}",
              file=sys.stderr)
        raise

    # Write bytecode file
    try:
        write_bytecode_file(output_path, bc)
    except Exception as e:
        print(f"Error writing output file: {e}", file=sys.stderr)
        raise

    if verbose:
        print(f"Successfully compiled to {output_path}")
        print(f"  Strings: {len(bc.symbol_table.strings)}")
        print(f"  Symbols: {len(bc.symbol_table.symbols)}")
        print(f"  Expressions: {len(bc.expressions)}")

    if dump:
        print("\nBytecode Dump:")
        print("=" * 60)
        dump_bytecode(bc, verbose=verbose)

    return output_path


def main():
    """
    Command-line interface for PyVelox compiler.

    Usage:
        pyvelox-compile [options] <source.py>

    Options:
        -o, --output <file>  Output file path (default: <source>.vm)
        -v, --verbose        Verbose output
        -d, --dump           Dump bytecode after compilation
        -h, --help           Show help message
    """
    import argparse

    parser = argparse.ArgumentParser(
        description='PyVelox - Python to VeloxVM Bytecode Compiler',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog='''
Examples:
  pyvelox-compile hello.py
  pyvelox-compile -o output.vm -v program.py
  pyvelox-compile --dump fibonacci.py
        '''
    )

    parser.add_argument('source', help='Python source file')
    parser.add_argument('-o', '--output', help='Output file path')
    parser.add_argument('-v', '--verbose', action='store_true', help='Verbose output')
    parser.add_argument('-d', '--dump', action='store_true', help='Dump bytecode')
    parser.add_argument('--version', action='version', version='PyVelox 0.1.0')

    args = parser.parse_args()

    try:
        compile_file(
            args.source,
            args.output,
            verbose=args.verbose,
            dump=args.dump
        )
        return 0
    except (PyveloxCompileError, SyntaxError):
        # compile_file has already printed a located error; just exit non-zero.
        return 1
    except Exception as e:
        print(f"Fatal error: {e}", file=sys.stderr)
        return 1


if __name__ == '__main__':
    sys.exit(main())
