"""VeloxVM threading example for the Python frontend.

Two named threads increment a shared counter under a mutex. Each thread
stores a label in its thread-specific slot, yields between increments,
and returns its label and work count to the main thread.
"""

ITERATIONS = 5

counter = 0
counter_lock = make_mutex("counter")


def worker(label):
    global counter

    current = current_thread()
    thread_specific_set(current, label)

    for _ in range(ITERATIONS):
        mutex_lock(counter_lock)
        counter += 1
        mutex_unlock(counter_lock)
        thread_yield()

    return [thread_specific(current), ITERATIONS]


# A lambda is used as the thread thunk so a return from worker() returns
# to the thunk; the thunk's value then becomes the result of thread_join().
first = make_thread(lambda: worker("first"), "worker-1")
second = make_thread(lambda: worker("second"), "worker-2")

print("Python threading demo")
print("created: ", thread_name(first), " and ", thread_name(second))
print("counter before start: ", counter)

thread_start(first)
thread_start(second)

first_result = thread_join(first)
second_result = thread_join(second)

print("first result: ", first_result)
print("second result: ", second_result)
print("counter after joins: ", counter)

if (counter == ITERATIONS * 2
        and first_result[0] == "first"
        and first_result[1] == ITERATIONS
        and second_result[0] == "second"
        and second_result[1] == ITERATIONS):
    print("PASS: Python threads completed correctly")
else:
    print("FAIL: Python threading result mismatch")
