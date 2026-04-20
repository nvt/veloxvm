# Algorithm examples

Classic algorithms implemented in more than one language so that you can
compare how each front-end expresses the same program.

## Sieve of Eratosthenes

| File | Language | Style |
|------|----------|-------|
| `sieve.scm`  | Scheme  | Functional, vector-based, tail-recursive helpers |
| `sieve2.cyl` | Cyclus  | Imperative, C-like loops, direct array manipulation |
| `sieve.py`   | Python  | List-based, `for`/`while` loops |

All three compute the same primes up to a common bound. Build any of them
with `./compile.sh algorithms/<file>` from the repository root and run
with `./run.sh algorithms/sieve`.
