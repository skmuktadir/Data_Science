# Program to find the sum of all even numbers between 1 and n
def sum_of_evens(n):
    sum_even = 0
    i = 2
    while i <= n:
        sum_even += i
        i += 2
    return sum_even

# Example usage:
limit = 10
print(f"Sum of even numbers between 1 and {limit} is: {sum_of_evens(limit)}")
