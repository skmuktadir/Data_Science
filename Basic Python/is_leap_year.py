# Program to determine if a year is a leap year or not
def is_leap_year(year):
    if (year % 4 == 0 and year % 100 != 0) or (year % 400 == 0):
        return f"{year} is a leap year"
    else:
        return f"{year} is not a leap year"

# Example usage:
year = 2024
print(is_leap_year(year))
