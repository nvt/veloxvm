#!/bin/bash
#
# Runs the config-sensitive test suites across the memory tiers the VM is
# deployed at, rebuilding the VM for each one.
#
# The default POSIX build sets VM_GC_MIN_ALLOCATED to half of a 10 MB
# heap, so the collector barely runs during a suite pass. From the same
# bytecode, test-gc-deep-spine allocates ~11 MB and drives 3 sweeps at
# the default tier against 705 at 32 kB. 32 kB is also the tier the
# Contiki-NG port ships at for non-Zoul targets.
#
# The mid tier additionally shrinks the GC mark work list to 64 slots so
# the expand-in-place path taken when it saturates gets exercised; at the
# default 4096 that path never runs.
#
# The suites alone are a weak instrument for GC defects, having passed at
# both tiers with every GC fix in this history reverted. The third
# configuration below is the one that catches them: it builds with
# VM_GC_VALIDATE, which checks every marked pointer against the set of
# live allocations, and it is what found the vm_box_create ordering bug.
# Violations do not fail a suite, so the output is scanned for them
# explicitly, and the sweep covers apps/ and benchmarks/ too, since the
# unit suites alone report nothing with that bug reintroduced.
#
# Not included: an 8 kB / 6 kB tier, which is the closest match to a Zoul
# deployment but cannot complete the unit suite today, as
# test-numeric-r7rs does not finish when list construction allocates
# proportionally to list length (see doc/gc-analysis.md 9.5). Add the
# tier here once that is fixed.
#
# Usage: tests/run-config-matrix.sh
# Exits non-zero if any suite fails in any configuration.

set -u

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$PROJECT_ROOT"

if [ -t 1 ]; then
    GREEN='\033[0;32m'; RED='\033[0;31m'; BLUE='\033[0;34m'; NC='\033[0m'
else
    GREEN=''; RED=''; BLUE=''; NC=''
fi

# name | DEFINES passed to make
CONFIGS=(
    "default|"
    "mid-tier-32k|-DVM_HEAP_SIZE=32768 -DVM_OBJECT_POOL_SIZE=16384 -DVM_MARK_STACK_SIZE=64"
)

# Built and run separately: the check is not "did the suite pass" but
# "did the collector touch a pointer it does not own", which needs the
# output scanned rather than the exit status read.
VALIDATE_DEFINES="-DVM_GC_VALIDATE=1 -DVM_HEAP_SIZE=32768 -DVM_OBJECT_POOL_SIZE=16384"

# Suites whose outcome depends on the memory configuration. The
# primitives suite compares source tables against doc/primitives.md and
# is configuration-independent, so it runs once at the end instead.
SUITES=(
    "unit-tests|./tests/unit-tests/run-tests.sh"
    "multi-app|./tests/multi-app/run-tests.sh"
)

FAILURES=0
declare -a RESULTS

for config in "${CONFIGS[@]}"; do
    name="${config%%|*}"
    defines="${config#*|}"

    echo ""
    echo -e "${BLUE}========================================${NC}"
    echo -e "${BLUE}Configuration: $name${NC}"
    echo "  DEFINES: ${defines:-(port defaults)}"
    echo -e "${BLUE}========================================${NC}"

    # obj/ is shared across configurations, so a stale object built with
    # different sizes would silently mix into the next build.
    make clean >/dev/null 2>&1
    if ! make -j"$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 4)" \
              DEFINES="$defines" >/dev/null 2>&1; then
        echo -e "${RED}  BUILD FAILED${NC}"
        RESULTS+=("$name|build|FAIL")
        FAILURES=$((FAILURES + 1))
        continue
    fi

    for suite in "${SUITES[@]}"; do
        suite_name="${suite%%|*}"
        suite_cmd="${suite#*|}"
        printf '  %-12s ' "$suite_name"
        if $suite_cmd >/dev/null 2>&1; then
            echo -e "${GREEN}PASS${NC}"
            RESULTS+=("$name|$suite_name|PASS")
        else
            echo -e "${RED}FAIL${NC}"
            echo "    re-run to see details: $suite_cmd"
            RESULTS+=("$name|$suite_name|FAIL")
            FAILURES=$((FAILURES + 1))
        fi
    done
done

echo ""
echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}Configuration: gc-validate${NC}"
echo "  DEFINES: $VALIDATE_DEFINES"
echo -e "${BLUE}========================================${NC}"
make clean >/dev/null 2>&1
if make -j"$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 4)" \
        DEFINES="$VALIDATE_DEFINES" >/dev/null 2>&1; then
    VALIDATE_LOG="$(mktemp -t veloxvm-gc-validate.XXXXXX)"
    ./tests/unit-tests/run-tests.sh >"$VALIDATE_LOG" 2>&1
    suite_rc=$?

    # The unit suites are not sufficient here. With the vm_box_create
    # ordering bug reintroduced they report no violation at all, because
    # nothing in them boxes a value at a moment when the object pool is
    # full. apps/ and benchmarks/ do, which is where that bug actually
    # surfaced, so the corpus is swept as well.
    corpus=0
    for prog in apps/*/bin/*.vm benchmarks/bin/*.vm; do
        [ -f "$prog" ] || continue
        corpus=$((corpus + 1))
        timeout 30 ./bin/vm "$prog" >>"$VALIDATE_LOG" 2>&1
    done
    echo "  swept $corpus compiled programs under validation"

    violations=$(grep -c "GC VALIDATION" "$VALIDATE_LOG" || true)
    printf '  %-12s ' "mark-ownership"
    if [ "$suite_rc" -eq 0 ] && [ "$violations" -eq 0 ]; then
        echo -e "${GREEN}PASS${NC} (no unowned pointers reached the mark phase)"
        RESULTS+=("gc-validate|mark-ownership|PASS")
    else
        echo -e "${RED}FAIL${NC} ($violations unowned-pointer violations)"
        grep "GC VALIDATION" "$VALIDATE_LOG" | sort -u | head -5
        echo "    full log: $VALIDATE_LOG"
        RESULTS+=("gc-validate|mark-ownership|FAIL")
        FAILURES=$((FAILURES + 1))
    fi
    [ "$violations" -eq 0 ] && rm -f "$VALIDATE_LOG"
else
    echo -e "${RED}  BUILD FAILED${NC}"
    RESULTS+=("gc-validate|build|FAIL")
    FAILURES=$((FAILURES + 1))
fi

echo ""
echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}Configuration-independent suites${NC}"
echo -e "${BLUE}========================================${NC}"
printf '  %-12s ' "primitives"
if ./tests/primitives/run-tests.sh >/dev/null 2>&1; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
    RESULTS+=("-|primitives|FAIL")
    FAILURES=$((FAILURES + 1))
fi

# Leave the tree holding a default build rather than whichever tier ran
# last, so a later ./bin/vm invocation behaves the way it usually does.
echo ""
echo "Restoring the default build..."
make clean >/dev/null 2>&1
make -j"$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 4)" >/dev/null 2>&1

echo ""
echo "========================================"
echo "Summary"
echo "========================================"
for result in "${RESULTS[@]}"; do
    IFS='|' read -r r_config r_suite r_status <<< "$result"
    printf '  %-14s %-12s %s\n' "$r_config" "$r_suite" "$r_status"
done
echo ""

if [ "$FAILURES" -eq 0 ]; then
    echo -e "${GREEN}ALL CONFIGURATIONS PASSED${NC}"
    exit 0
fi

echo -e "${RED}$FAILURES failure(s)${NC}"
exit 1
