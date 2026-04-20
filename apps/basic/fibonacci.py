# Fibonacci sequence example

def fib(n):
    if n <= 1:
        return n
    else:
        return fib(n - 1) + fib(n - 2)

# Print first 10 Fibonacci numbers
for i in range(10):
    print(fib(i))
