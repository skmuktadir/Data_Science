# Program to calculate the grade based on a given percentage
def calculate_grade(percentage):
    if percentage >= 90:
        return "A"
    elif percentage >= 80:
        return "B"
    elif percentage >= 70:
        return "C"
    elif percentage >= 60:
        return "D"
    else:
        return "F"

# Example usage:
percentage = 78
print(f"Grade for {percentage}% is: {calculate_grade(percentage)}")
