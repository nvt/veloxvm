# Exception handling example

def divide(a, b):
    try:
        result = a / b
        print("Result:", result)
        return result
    except Exception as e:
        print("Error: Division failed")
        return 0

# Test exception handling
divide(10, 2)
divide(10, 0)  # This should trigger the exception handler

# Raise an exception
def validate_positive(n):
    if n < 0:
        raise ValueError
    return n

validate_positive(5)
