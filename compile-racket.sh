#!/bin/bash
# VeloxVM Racket Compiler Wrapper Script
# Copyright (c) 2025, RISE Research Institutes of Sweden AB

set -e

# Configuration
RACKET_COMPILER="languages/scheme-racket/main.rkt"
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Check if racket is installed
if ! command -v racket &> /dev/null; then
    echo "Error: Racket is not installed"
    echo "Please install Racket from https://racket-lang.org/"
    exit 1
fi

# Check if source file provided
if [ $# -lt 1 ]; then
    echo "Usage: $0 <source.scm> [output.vm]"
    echo ""
    echo "Compile Scheme source to VeloxVM bytecode using Racket compiler"
    echo ""
    echo "Options:"
    echo "  -v, --verbose    Verbose output"
    echo "  --debug          Debug mode"
    echo ""
    echo "Examples:"
    echo "  $0 apps/hello.scm"
    echo "  $0 apps/math.scm apps/math.vm"
    echo "  $0 --verbose apps/test.scm"
    exit 1
fi

# Parse arguments
VERBOSE=""
DEBUG=""
SOURCE=""
OUTPUT=""

while [[ $# -gt 0 ]]; do
    case $1 in
        -v|--verbose)
            VERBOSE="--verbose"
            shift
            ;;
        --debug)
            DEBUG="--debug"
            shift
            ;;
        -o|--output)
            OUTPUT="--output $2"
            shift 2
            ;;
        *)
            if [ -z "$SOURCE" ]; then
                SOURCE="$1"
            else
                OUTPUT="--output $1"
            fi
            shift
            ;;
    esac
done

# Check source file exists
if [ ! -f "$SOURCE" ]; then
    echo "Error: Source file not found: $SOURCE"
    exit 1
fi

# Compile
cd "$SCRIPT_DIR"
echo "Compiling $SOURCE with Racket compiler..."
racket "$RACKET_COMPILER" $VERBOSE $DEBUG $OUTPUT "$SOURCE"

echo "Compilation successful!"
