# Process a stream of sensor readings using map/filter/reduce.

print("=== Sensor Data Processing Pipeline ===")
print()

# Simulated sensor readings, in degrees Celsius. Some are out of range.
raw_readings = [22, 105, 18, 200, 25, -5, 23, 150, 19, 24, 300, 21]

print("Raw readings:", raw_readings)
print("Count:", len(raw_readings))
print()

# Drop readings outside a sensible temperature range.
def is_valid(temp):
    return temp >= 0 and temp <= 100

valid_readings = filter(is_valid, raw_readings)
print("Valid readings (0-100C):", valid_readings)
print("Valid count:", len(valid_readings))
print()

# Convert the remaining readings to Fahrenheit.
def to_fahrenheit(c):
    return (c * 9 / 5) + 32

readings_f = map(to_fahrenheit, valid_readings)
print("Fahrenheit:", readings_f)
print()

# Summary statistics.
total_f = sum(readings_f)
average_f = total_f / len(readings_f)
print("Total:", total_f)
print("Average:", average_f)
print()

# Alert on any reading above a threshold.
ALERT_THRESHOLD = 80  # Fahrenheit

def above_threshold(t):
    return t > ALERT_THRESHOLD

high_temps = filter(above_threshold, readings_f)
print("Alert threshold:", ALERT_THRESHOLD, "F")
print("High temps:", high_temps)
print("Alerts triggered:", len(high_temps) > 0)
