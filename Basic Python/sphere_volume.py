import math

# Program to calculate the volume of a sphere
def sphere_volume(radius):
    volume = (4/3) * math.pi * radius**3
    return volume

# Example usage:
radius = 5
print(f"Volume of sphere with radius {radius} is: {sphere_volume(radius):.2f}")
