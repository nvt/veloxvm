#!/bin/bash
# VeloxVM Unit Test Runner
# Compiles and runs all unit tests, aggregating results

set -e

# Colors for output (optional, can be disabled)
if [ -t 1 ]; then
    GREEN='\033[0;32m'
    RED='\033[0;31m'
    YELLOW='\033[1;33m'
    BLUE='\033[0;34m'
    NC='\033[0m' # No Color
else
    GREEN=''
    RED=''
    YELLOW=''
    BLUE=''
    NC=''
fi

# Get script directory and project root
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

cd "$PROJECT_ROOT"

echo "========================================"
echo "VeloxVM Unit Test Runner"
echo "========================================"
echo ""

# Counters
TOTAL_TESTS=0
TOTAL_PASSED=0
TOTAL_FAILED=0
TOTAL_SKIPPED=0
SUITES_RUN=0
SUITES_PASSED=0
SUITES_FAILED=0

# Create build directory for test binaries
BUILD_DIR="tests/unit-tests/build"
mkdir -p "$BUILD_DIR"

# Find all test files in subdirectories (r5rs, r6rs, vm-specific)
TEST_FILES=$(find tests/unit-tests/r5rs tests/unit-tests/r6rs tests/unit-tests/vm-specific -name "test-*.scm" -type f 2>/dev/null | sort)

if [ -z "$TEST_FILES" ]; then
    echo "No test files found!"
    exit 1
fi

# Run each test suite
for test_file in $TEST_FILES; do
    test_name=$(basename "$test_file" .scm)
    vm_file="$BUILD_DIR/${test_name}.vm"

    printf "Running ${BLUE}%s${NC}... " "$test_name"

    # Compile to build directory
    if ! ./compile-racket.sh "$test_file" "$vm_file" > /dev/null 2>&1; then
        printf "${RED}COMPILE FAILED${NC}\n"
        SUITES_FAILED=$((SUITES_FAILED + 1))
        continue
    fi

    # Run and capture output
    output=$(./bin/vm "$vm_file" 2>&1)

    # Extract statistics from output
    passed=$(echo "$output" | grep "^Passed:" | awk '{print $2}')
    failed=$(echo "$output" | grep "^Failed:" | awk '{print $2}')
    skipped=$(echo "$output" | grep "^Skipped:" | awk '{print $2}')
    total=$(echo "$output" | grep "^Total:" | awk '{print $2}')

    # Handle missing values (old format compatibility)
    passed=${passed:-0}
    failed=${failed:-0}
    skipped=${skipped:-0}
    total=${total:-0}

    # Update counters
    SUITES_RUN=$((SUITES_RUN + 1))
    TOTAL_TESTS=$((TOTAL_TESTS + total))
    TOTAL_PASSED=$((TOTAL_PASSED + passed))
    TOTAL_FAILED=$((TOTAL_FAILED + failed))
    TOTAL_SKIPPED=$((TOTAL_SKIPPED + skipped))

    # Display result
    if [ "$failed" -eq 0 ]; then
        printf "${GREEN}✓${NC} %s/%s\n" "$passed" "$total"
        SUITES_PASSED=$((SUITES_PASSED + 1))
    else
        printf "${RED}✗${NC} %s/%s (%s failed)\n" "$passed" "$total" "$failed"
        SUITES_FAILED=$((SUITES_FAILED + 1))
    fi

    # Show skipped if any
    if [ "$skipped" -gt 0 ]; then
        printf "  ${YELLOW}↷${NC} %s skipped\n" "$skipped"
    fi
done

echo ""
echo "========================================"
echo "Overall Results"
echo "========================================"
printf "Suites:  %s total (%s passed, %s failed)\n" "$SUITES_RUN" "$SUITES_PASSED" "$SUITES_FAILED"
printf "Tests:   %s total\n" "$TOTAL_TESTS"
printf "Passed:  ${GREEN}%s${NC}\n" "$TOTAL_PASSED"
printf "Failed:  ${RED}%s${NC}\n" "$TOTAL_FAILED"
if [ "$TOTAL_SKIPPED" -gt 0 ]; then
    printf "Skipped: ${YELLOW}%s${NC}\n" "$TOTAL_SKIPPED"
fi
echo "========================================"

if [ "$TOTAL_FAILED" -eq 0 ]; then
    printf "${GREEN}ALL TESTS PASSED ✓${NC}\n"
    exit 0
else
    printf "${RED}SOME TESTS FAILED ✗${NC}\n"
    exit 1
fi
