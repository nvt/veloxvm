# Sieve of Eratosthenes - Find all prime numbers up to a given limit
#
# This implementation demonstrates:
# - Function definitions with returns
# - Nested loops (while loops)
# - List operations and concatenation
# - Conditionals and comparisons
# - Arithmetic operations (multiplication, modulo, comparison)

def is_prime(n):
    # Check if n is a prime number using trial division
    if n < 2:
        return 0

    # Check if n is divisible by any number from 2 to sqrt(n)
    divisor = 2
    while divisor * divisor <= n:
        if n % divisor == 0:
            return 0
        divisor = divisor + 1

    return 1


def find_primes(limit):
    # Find all prime numbers up to and including limit
    primes = []
    n = 2

    while n <= limit:
        if is_prime(n) == 1:
            primes = primes + [n]
        n = n + 1

    return primes


def count_primes(primes):
    # Count the number of primes in a list
    count = 0
    for p in primes:
        count = count + 1
    return count


def print_primes(primes):
    # Print a list of prime numbers, one per line
    for p in primes:
        print(p)


# Main program
print("========================================")
print("Sieve of Eratosthenes (Trial Division)")
print("========================================")
print()

print("Finding all prime numbers up to 10000...")
print()

primes = find_primes(10000)

print("Prime numbers:")
print_primes(primes)
print()

print("Total count:", count_primes(primes))
print()

print("Primes up to 10:")
small_primes = find_primes(10)
print(small_primes)
print()

print("Primes up to 20:")
medium_primes = find_primes(20)
print(medium_primes)
print()

print("Individual prime tests:")
print("is_prime(17):", is_prime(17))
print("is_prime(18):", is_prime(18))
print("is_prime(2):", is_prime(2))
print("is_prime(1):", is_prime(1))
print()

print("Done!")
