#!/usr/bin/env python3

import pexpect

vm_path = "../../bin/vm"
app_dir = "../../apps/"

vm_test_suite = { 'name': "VM app tests", 'tests': [] }

def add_test(test_suite, app_name, expected_output):
    test = app_name, expected_output
    test_suite['tests'].append(test)

def run_test_suite(test_suite):
    test_count = len(test_suite['tests'])
    print("Running test suite '{}'".format(test_suite['name']))

    ok_tests = 0
    for app_name, expected_output in test_suite['tests']:
       ok_tests += run_test(app_name, expected_output)

    if ok_tests == test_count:
        print("Test suite SUCCEEDED ({}/{} tests succeeded)".format(ok_tests, test_count))
    else:
        print("Test suite FAILED ({}/{} tests succeeded)".format(ok_tests, test_count))

def run_test(app_name, expected_output):
    try:
        vm_proc = pexpect.spawn(vm_path + " " + app_dir + app_name + ".vm")
        vm_proc.expect(expected_output, timeout = 5)
        print("Test OK: {}".format(app_name))
        return 1
    except:
        print("Test FAILURE: {}".format(app_name))
    return 0

add_test(vm_test_suite, "math", "Result: 210")
add_test(vm_test_suite, "sieve", "Number of primes: 1229")
add_test(vm_test_suite, "sieve2", "Number of primes: 1229")

run_test_suite(vm_test_suite)
