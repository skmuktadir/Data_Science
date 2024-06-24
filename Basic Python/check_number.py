# Program to determine if a number is positive, negative, or zero
def check_number(num):
    if num > 0:
        return f"{num} is positive"
    elif num < 0:
        return f"{num} is negative"
    else:
        return "Number is zero"

# Example usage:
number = -8
print(check_number(number))
