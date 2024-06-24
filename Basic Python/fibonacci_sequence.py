# Program to generate a Fibonacci sequence of length n
def fibonacci_sequence(n):
    fib_sequence = []
    a, b = 0, 1
    for _ in range(n):
        fib_sequence.append(a)
        a, b = b, a + b
    return fib_sequence

# Example usage:
length = 8
print(f"Fibonacci sequence of length {length}: {fibonacci_sequence(length)}")
