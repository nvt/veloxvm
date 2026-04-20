# Control Flow Demonstration (Python)
# Shows: if/elif/else, loops, list comprehensions

print("=== Control Flow Examples ===")
print("")

# 1. If/elif/else - Multi-way conditional
print("1. Testing if/elif/else with score grading:")
def grade(score):
    if score >= 90:
        return 'A'
    elif score >= 80:
        return 'B'
    elif score >= 70:
        return 'C'
    elif score >= 60:
        return 'D'
    else:
        return 'F'

print(f"   Score 95: {grade(95)}")
print(f"   Score 75: {grade(75)}")
print(f"   Score 55: {grade(55)}")
print("")

# 2. For loops with lists
print("2. For loop - partition numbers:")
def partition_numbers(numbers):
    positive = []
    negative = []
    for num in numbers:
        if num >= 0:
            positive.append(num)
        else:
            negative.append(num)
    return (positive, negative)

test_list = [3, -2, 8, -5, 0, 7, -1, 4]
pos, neg = partition_numbers(test_list)
print(f"   Input: {test_list}")
print(f"   Positive: {pos}")
print(f"   Negative: {neg}")
print("")

# 3. While loop - find maximum
print("3. Finding maximum using while loop:")
def find_max(numbers):
    if not numbers:
        return None
    max_val = numbers[0]
    i = 1
    while i < len(numbers):
        if numbers[i] > max_val:
            max_val = numbers[i]
        i = i + 1
    return max_val

print(f"   Max of {test_list}: {find_max(test_list)}")
print("")

# 4. List comprehensions (functional style)
print("4. List comprehensions:")
numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
squares = [x * x for x in numbers]
evens = [x for x in numbers if x % 2 == 0]
print(f"   Numbers: {numbers}")
print(f"   Squares: {squares}")
print(f"   Evens: {evens}")
