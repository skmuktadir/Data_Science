# Program to check if a given string is a palindrome
def is_palindrome(s):
    s = s.lower()  # Convert to lowercase for case insensitivity
    return s == s[::-1]

# Example usage:
string = "Madam"
if is_palindrome(string):
    print(f"{string} is a palindrome")
else:
    print(f"{string} is not a palindrome")
