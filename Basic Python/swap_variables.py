# Program to swap values of two variables
def swap_variables(a, b):
    print(f"Before swapping: a = {a}, b = {b}")
    a, b = b, a
    print(f"After swapping: a = {a}, b = {b}")

# Example usage:
swap_variables(3, 5)
