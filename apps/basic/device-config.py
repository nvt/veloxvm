# Manage an IoT device configuration with a dictionary.

config = {
    'device_id': 'sensor-001',
    'location': 'warehouse-A',
    'sample_rate': 60,
    'enabled': True,
    'threshold': 25,
}

print("=== Current configuration ===")
print("device_id:   ", config['device_id'])
print("location:    ", config['location'])
print("sample_rate: ", config['sample_rate'], "seconds")
print("enabled:     ", config['enabled'])
print("threshold:   ", config['threshold'])
print("setting count:", len(config))
print()

# Update and add settings.
config['sample_rate'] = 30
config['last_update'] = 12345
print("After update:")
print("  sample_rate:", config['sample_rate'])
print("  last_update:", config['last_update'])
print()

# Validate required settings with .get() and a default.
print("=== Required settings ===")
required = ['device_id', 'location', 'sample_rate']
for setting in required:
    value = config.get(setting, None)
    if value:
        print("  present:", setting, "=", value)
    else:
        print("  MISSING:", setting)
print()

# Safe access with fallback defaults.
print("=== Runtime parameters (with defaults) ===")
print("sample_rate:", config.get('sample_rate', 60))
print("timeout:    ", config.get('timeout', 300))
print("max_retries:", config.get('max_retries', 3))
print()

# Enforce an alert range.
reading = {'timestamp': 98765, 'temperature': 23, 'humidity': 65}
thresholds = {'temp_min': 15, 'temp_max': 30, 'humidity_max': 80}

temp = reading['temperature']
humidity = reading['humidity']

if temp < thresholds['temp_min']:
    print("ALERT: temperature too low:", temp)
elif temp > thresholds['temp_max']:
    print("ALERT: temperature too high:", temp)
else:
    print("OK: temperature in range:", temp)

if humidity > thresholds['humidity_max']:
    print("ALERT: humidity too high:", humidity)
else:
    print("OK: humidity in range:", humidity)
