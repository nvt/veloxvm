# Enumerate every sensor registered with Contiki-NG's sensors library,
# read a sample from each, maintain a rolling window of recent
# readings per sensor, and report summary statistics on every scan.
# Intended for the Contiki-NG port - the sensors library is a platform
# capability and is imported rather than provided as a core builtin.

import sensors

WINDOW = 8

# Per-sensor rolling windows, lazily populated as new sensors appear.
history = {}

def compute_stats(readings):
    n = len(readings)
    if n == 0:
        return [0, 0, 0]
    return [min(readings), max(readings), sum(readings) // n]

def record(name, value):
    window = history.get(name, [])
    window = window + [value]
    if len(window) > WINDOW:
        window = window[1:]
    history[name] = window

printf("Starting the sensor readings in ten seconds!")
thread_sleep(10000)

while True:
    names = get_sensors()
    print("=== Scan (", len(names), "sensors) ===")
    for name in names:
        value = sensor_value(name)
        record(name, value)
        stats = compute_stats(history.get(name, []))
        print(name, ": value=", value,
              " min=", stats[0], " max=", stats[1], " avg=", stats[2])
#    thread_sleep(5000)
