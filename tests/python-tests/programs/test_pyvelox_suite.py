#!/usr/bin/env python3
"""
PyVelox Compiler Test Suite
Tests Python-to-VeloxVM bytecode compilation and execution
"""

print("==========================================================")
print("PyVelox Compiler Test Suite")
print("==========================================================")

# Test counters
passed = 0
failed = 0
total = 0

def test_section(name):
    print("\n===", name, "===")

# Track results without list mutation (not fully supported)
def test(condition, description):
    if condition:
        print("  PASS:", description)
        return 1
    else:
        print("  FAIL:", description)
        return 0

# ============================================================================
# Basic Data Types
# ============================================================================

test_section("Basic Data Types")

x = 42
test(x == 42, "Integer assignment")
test(10 + 5 == 15, "Integer addition")
test(10 - 3 == 7, "Integer subtraction")
test(6 * 7 == 42, "Integer multiplication")
test(20 // 4 == 5, "Integer division")
test(17 % 5 == 2, "Modulo operation")

test(True, "Boolean True")
test(not False, "Boolean not False")

s = "hello"
test(s == "hello", "String assignment")
test(len("world") == 5, "String length")

# None value (comparison not supported with ==)

# ============================================================================
# Lists
# ============================================================================

test_section("Lists")

lst = [1, 2, 3, 4, 5]
test(len(lst) == 5, "List length")
test(lst[0] == 1, "List indexing first")
test(lst[4] == 5, "List indexing last")
test(lst[-1] == 5, "List negative indexing -1")
test(lst[-2] == 4, "List negative indexing -2")

test(lst[1:3] == [2, 3], "List slicing [1:3]")
test(lst[:2] == [1, 2], "List slicing [:2]")
test(lst[3:] == [4, 5], "List slicing [3:]")
test(lst[-2:] == [4, 5], "List slicing negative [-2:]")

test([1, 2] + [3, 4] == [1, 2, 3, 4], "List concatenation")

# ============================================================================
# Dictionaries
# ============================================================================

test_section("Dictionaries")

d = {"a": 1, "b": 2, "c": 3}
test(d["a"] == 1, "Dictionary lookup a")
test(d["c"] == 3, "Dictionary lookup c")

# ============================================================================
# Variables and Assignment
# ============================================================================

test_section("Variables and Assignment")

a = 10
b = 20
test(a == 10 and b == 20, "Multiple assignment")

a += 5
test(a == 15, "Augmented assignment +=")

c = 100
c -= 30
test(c == 70, "Augmented assignment -=")

d = 5
d *= 3
test(d == 15, "Augmented assignment *=")

x, y = [1, 2]
test(x == 1 and y == 2, "Tuple unpacking two values")

p, q, r = [10, 20, 30]
test(p == 10 and q == 20 and r == 30, "Tuple unpacking three values")

# ============================================================================
# Functions
# ============================================================================

test_section("Functions")

def add(a, b):
    return a + b

test(add(3, 4) == 7, "Function definition and call")

def multiply(x, y):
    result = x * y
    return result

test(multiply(6, 7) == 42, "Function with local variable")

double = lambda x: x * 2
test(double(21) == 42, "Lambda expression")

# ============================================================================
# Comparison Operators
# ============================================================================

test_section("Comparison Operators")

test(5 == 5, "Equality ==")
test(5 != 4, "Inequality !=")
test(10 > 5, "Greater than >")
test(10 >= 10, "Greater than or equal >=")
test(5 < 10, "Less than <")
test(5 <= 5, "Less than or equal <=")

# ============================================================================
# Logical Operators
# ============================================================================

test_section("Logical Operators")

test(True and True, "Logical and True True")
test(not (True and False), "Logical and True False")
test(True or False, "Logical or True False")
test(not (False or False), "Logical or False False")
test(not False, "Logical not False")
test(not not True, "Double negation")

# Note: Boolean comparisons with == not supported in VM

# ============================================================================
# Bitwise Operators
# ============================================================================

test_section("Bitwise Operators")

test((12 & 10) == 8, "Bitwise AND")
test((12 | 10) == 14, "Bitwise OR")
test((12 ^ 10) == 6, "Bitwise XOR")
test((5 << 2) == 20, "Left shift")
test((20 >> 2) == 5, "Right shift")

# ============================================================================
# Control Flow - If Statements
# ============================================================================

test_section("Control Flow - If Statements")

x = 10
if x > 5:
    result1 = "greater"
else:
    result1 = "not greater"
test(result1 == "greater", "If statement true branch")

y = 2
if y > 5:
    result2 = "greater"
else:
    result2 = "not greater"
test(result2 == "not greater", "If statement false branch")

z = 10
if z < 5:
    result3 = "small"
elif z < 15:
    result3 = "medium"
else:
    result3 = "large"
test(result3 == "medium", "If-elif-else statement")

result4 = "yes" if True else "no"
test(result4 == "yes", "Ternary expression true")

result5 = "yes" if False else "no"
test(result5 == "no", "Ternary expression false")

# ============================================================================
# Control Flow - For Loops
# ============================================================================

test_section("Control Flow - For Loops")

sum1 = 0
for i in [1, 2, 3, 4, 5]:
    sum1 += i
test(sum1 == 15, "For loop with list")

numbers = [10, 20, 30]
total = 0
for n in numbers:
    total += n
test(total == 60, "For loop iteration")

# ============================================================================
# Control Flow - While Loops
# ============================================================================

test_section("Control Flow - While Loops")

count = 0
while count < 5:
    count += 1
test(count == 5, "While loop")

# ============================================================================
# Built-in Functions
# ============================================================================

test_section("Built-in Functions")

test(len([1, 2, 3, 4]) == 4, "len on list")
test(len("hello") == 5, "len on string")

test(int("42") == 42, "int conversion")
test(str(42) == "42", "str conversion")

test(abs(-10) == 10, "abs negative")
test(abs(15) == 15, "abs positive")

test(min(3, 1, 4, 1, 5) == 1, "min function")
test(max(3, 1, 4, 1, 5) == 5, "max function")

test(sum([1, 2, 3, 4]) == 10, "sum function")

# ============================================================================
# Higher-Order Functions
# ============================================================================

test_section("Higher-Order Functions")

def square(x):
    return x * x

squares = map(square, [1, 2, 3, 4])
test(squares == [1, 4, 9, 16], "map function")

def is_even(x):
    return x % 2 == 0

evens = filter(is_even, [1, 2, 3, 4, 5, 6])
test(evens == [2, 4, 6], "filter function")

# ============================================================================
# List Functions
# ============================================================================

test_section("List Functions")

test(reversed([1, 2, 3]) == [3, 2, 1], "reversed function")

# ============================================================================
# Enumerate and Zip
# ============================================================================

test_section("Enumerate and Zip")

items = ["a", "b", "c"]
indexed = enumerate(items)
test(indexed == [[0, "a"], [1, "b"], [2, "c"]], "enumerate function")

list1 = [1, 2, 3]
list2 = [4, 5, 6]
pairs = zip(list1, list2)
test(pairs == [[1, 4], [2, 5], [3, 6]], "zip function")

# ============================================================================
# String Slicing
# ============================================================================

test_section("String Slicing")

s = "hello world"
test(s[0:5] == "hello", "String slicing [0:5]")
test(s[6:] == "world", "String slicing [6:]")
test(s[:5] == "hello", "String slicing [:5]")
test(s[-5:] == "world", "String slicing negative [-5:]")

# ============================================================================
# Exception Handling
# ============================================================================

test_section("Exception Handling")

exception_caught = False
try:
    raise ValueError
except Exception as e:
    exception_caught = True

test(exception_caught, "Try except catches raised exception")

# ============================================================================
# Complex Expressions
# ============================================================================

test_section("Complex Expressions")

def inc(x):
    return x + 1

def dec(x):
    return x - 1

test(inc(dec(inc(5))) == 5, "Nested function calls")

result = (10 + 5) * 2 - 8 // 2
test(result == 26, "Complex arithmetic expression")

numbers = [1, 2, 3, 4, 5]
doubled = map(lambda x: x * 2, numbers)
test(doubled == [2, 4, 6, 8, 10], "Map with lambda")

# ============================================================================
# Test Summary
# ============================================================================

print("")
print("==========================================================")
print("PyVelox Test Suite Complete")
print("==========================================================")
print("Check output above for PASS/FAIL results")
print("==========================================================")
print("")
