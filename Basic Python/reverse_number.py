# Program to reverse a given number
def reverse_number(num):
    reversed_num = 0
    while num > 0:
        remainder = num % 10
        reversed_num = reversed_num * 10 + remainder
        num = num // 10
    return reversed_num

# Example usage:
number = 12345
print(f"Reverse of {number} is: {reverse_number(number)}")
