#!/bin/bash
# Runs both kinds of Python tests:
#   compiler/  - pyvelox unit tests (Python unittest framework)
#   programs/  - end-to-end tests that compile .py to bytecode and run on bin/vm

set -e

if [ -t 1 ]; then
    GREEN='\033[0;32m'
    RED='\033[0;31m'
    BLUE='\033[0;34m'
    NC='\033[0m'
else
    GREEN='' ; RED='' ; BLUE='' ; NC=''
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
cd "$PROJECT_ROOT"

echo "========================================"
echo "Python compiler unit tests"
echo "========================================"
UNIT_STATUS=0
PYTHONPATH="$PROJECT_ROOT/languages/python:$PYTHONPATH" \
    python3 -m unittest discover -s tests/python-tests/compiler -v 2>&1 \
    || UNIT_STATUS=$?

echo ""
echo "========================================"
echo "End-to-end Python program tests"
echo "========================================"
E2E_RUN=0
E2E_PASSED=0
E2E_FAILED=0

TEST_FILES=$(find tests/python-tests/programs -name "test_*.py" -type f | sort)
for test_file in $TEST_FILES; do
    test_name=$(basename "$test_file" .py)
    vm_file="${test_file%.py}.vm"

    printf "Running ${BLUE}%s${NC}... " "$test_name"

    if ! ./languages/python/pyvelox-compile "$test_file" > /dev/null 2>&1; then
        printf "${RED}COMPILE FAILED${NC}\n"
        E2E_FAILED=$((E2E_FAILED + 1))
    elif timeout 10 ./bin/vm "$vm_file" > /dev/null 2>&1; then
        printf "${GREEN}PASSED${NC}\n"
        E2E_PASSED=$((E2E_PASSED + 1))
    else
        exit_code=$?
        if [ $exit_code -eq 124 ]; then
            printf "${RED}TIMEOUT${NC}\n"
        else
            printf "${RED}FAILED (exit %s)${NC}\n" "$exit_code"
        fi
        E2E_FAILED=$((E2E_FAILED + 1))
    fi
    E2E_RUN=$((E2E_RUN + 1))
done

echo ""
echo "========================================"
echo "Summary"
echo "========================================"
printf "Program tests:  %s run, ${GREEN}%s passed${NC}, ${RED}%s failed${NC}\n" \
    "$E2E_RUN" "$E2E_PASSED" "$E2E_FAILED"
if [ $UNIT_STATUS -eq 0 ]; then
    printf "Compiler tests: ${GREEN}OK${NC}\n"
else
    printf "Compiler tests: ${RED}FAILED${NC}\n"
fi

if [ $UNIT_STATUS -eq 0 ] && [ "$E2E_FAILED" -eq 0 ]; then
    exit 0
fi
exit 1
