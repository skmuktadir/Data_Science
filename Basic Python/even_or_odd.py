# Program to determine if a number is even or odd
def even_or_odd(num):
    if num % 2 == 0:
        return f"{num} is even"
    else:
        return f"{num} is odd"

# Example usage:
number = 15
print(even_or_odd(number))
