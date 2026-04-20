# Python built-in functions available in VeloxVM.

print("=== Built-in Functions ===")
print("abs(-10):", abs(-10))
print("abs(5):", abs(5))
print()

print("min(5, 2, 8, 1):", min(5, 2, 8, 1))
print("max(5, 2, 8, 1):", max(5, 2, 8, 1))
print()

numbers = [1, 2, 3, 4, 5]
print("numbers:", numbers)
print("sum(numbers):", sum(numbers))
print()

# Reversed
original = [10, 20, 30]
print("original:", original)
rev = reversed(original)
print("reversed:", rev)
print()

# Tuples
print("=== Tuples ===")
point = (10, 20)
print("point:", point)
triple = (1, 2, 3)
print("triple:", triple)
print()

# String concatenation (literals only)
print("=== String Concatenation ===")
greeting = "Hello" + "World"
print(greeting)
print()

# List slicing
print("=== List Slicing ===")
data = [0, 1, 2, 3, 4, 5]
print("data:", data)
print("data[2:]:", data[2:])
print("data[:3]:", data[:3])
print("data[-2:]:", data[-2:])
