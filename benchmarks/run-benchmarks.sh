#!/bin/bash
# VeloxVM Benchmark Runner
# Compiles and runs every benchmark, reports PASS/FAIL and elapsed time.

set -e

if [ -t 1 ]; then
    GREEN='\033[0;32m'
    RED='\033[0;31m'
    BLUE='\033[0;34m'
    NC='\033[0m'
else
    GREEN=''
    RED=''
    BLUE=''
    NC=''
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$PROJECT_ROOT"

if [ ! -x ./bin/vm ]; then
    echo "bin/vm not found. Run 'make' first." >&2
    exit 1
fi

echo "========================================"
echo "VeloxVM Benchmark Runner"
echo "========================================"
echo ""

# Compile all benchmarks (skips up-to-date ones).
./compile.sh > /dev/null 2>&1 || {
    echo "Compilation step failed. Re-run './compile.sh' for details." >&2
    exit 1
}

TOTAL=0
PASSED=0
FAILED=0
SOURCES=$(ls benchmarks/*.scm 2>/dev/null | sort)

for src in $SOURCES; do
    name=$(basename "$src" .scm)
    vm_file="benchmarks/bin/${name}.vm"

    if [ ! -f "$vm_file" ]; then
        printf "%-22s ${RED}MISSING .vm${NC}\n" "$name"
        FAILED=$((FAILED + 1))
        TOTAL=$((TOTAL + 1))
        continue
    fi

    output=$(./bin/vm "$vm_file" 2>&1)
    status=$(echo "$output" | grep -E "^Status:" | tail -1 | awk '{print $2}')
    elapsed=$(echo "$output" | grep -E "elapsed:" | tail -1 | awk '{print $2}')

    TOTAL=$((TOTAL + 1))
    printf "${BLUE}%-22s${NC} " "$name"
    if [ "$status" = "PASS" ]; then
        PASSED=$((PASSED + 1))
        if [ -n "$elapsed" ]; then
            printf "${GREEN}PASS${NC}  (%s ms)\n" "$elapsed"
        else
            printf "${GREEN}PASS${NC}\n"
        fi
    else
        FAILED=$((FAILED + 1))
        printf "${RED}FAIL${NC}  (status: %s)\n" "${status:-none}"
    fi
done

echo ""
echo "========================================"
printf "Total: %d  ${GREEN}Passed: %d${NC}  ${RED}Failed: %d${NC}\n" \
    "$TOTAL" "$PASSED" "$FAILED"
echo "========================================"

if [ "$FAILED" -eq 0 ]; then
    exit 0
else
    exit 1
fi
