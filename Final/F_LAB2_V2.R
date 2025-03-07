# Uncomment the following lines if you need to install the required packages:
# install.packages("ggplot2")  # ggplot2 is used for creating advanced visualizations.
# install.packages("moments")  # moments is required for calculating statistical measures like skewness.

# Load necessary libraries
library(ggplot2)  # Provides functions to create elegant data visualizations.
library(moments)  # Used to calculate skewness and other statistical moments.

# Define the file path to the dataset (update the path to your local machine)
file_path <- "/Users/skmuktadir/Downloads/HeartDiseaseTrain-Test.csv"  # Replace with the correct path to load your data.

# Read the dataset from the file
heart_data <- read.csv(file_path)  # Imports the dataset into R as a data frame.

# Check the structure and summary of the dataset
str(heart_data)  # Displays the structure of the dataset (data types, column names, etc.).
summary(heart_data)  # Provides summary statistics for each column to understand the data distribution.

# Ensure 'age' column is numeric and remove rows with missing values
heart_data$age <- as.numeric(heart_data$age)  # Ensures 'age' is treated as a numeric variable for statistical calculations and plotting.
heart_data <- na.omit(heart_data)  # Removes rows with missing values to avoid errors in analysis.

# Plot a histogram of the 'age' column
ggplot(heart_data, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Age", x = "Age", y = "Frequency")
# Why: A histogram helps visualize the distribution of ages in the dataset.
# Calculate mean, median, and mode of the 'age' column
mean_age <- mean(heart_data$age)  # Calculates the mean
median_age <- median(heart_data$age)  # Calculates the median

# Calculate mode (mode function for a numeric vector)
mode_age <- as.numeric(names(sort(table(heart_data$age), decreasing = TRUE)[1]))  # Finds the most frequent age value

# Create the histogram with mean, median, and mode
ggplot(heart_data, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Age with Mean, Median, and Mode", 
       x = "Age", 
       y = "Frequency") +
  # Add vertical lines for mean, median, and mode
  geom_vline(xintercept = mean_age, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = median_age, color = "green", linetype = "dotted", size = 1) +
  geom_vline(xintercept = mode_age, color = "purple", linetype = "longdash", size = 1) +
  # Add text annotations for each line
  annotate("text", x = mean_age + 5, y = 100, label = paste("Mean:", round(mean_age, 2)), color = "red", hjust = 0) +
  annotate("text", x = median_age + 5, y = 50, label = paste("Median:", round(median_age, 2)), color = "green", hjust = 0) +
  annotate("text", x = mode_age + 5, y = 10, label = paste("Mode:", round(mode_age, 2)), color = "purple", hjust = 0) +
  theme_minimal()




# Plot a density plot of the 'age' column
ggplot(heart_data, aes(x = age)) +
  geom_density(color = "red", size = 1) +
  labs(title = "Density Plot of Age", x = "Age", y = "Density")
# Why: A density plot gives a smoother representation of the age distribution compared to a histogram.

# Calculate and print the skewness of the 'age' column
age_skewness <- skewness(heart_data$age)  # Calculates the asymmetry of the age distribution.
print(paste("Skewness of Age: ", age_skewness))  # Displays the skewness value.
# Why: Skewness provides an understanding of whether the data is symmetric or skewed to one side.

# Plot a histogram of 'age' with skewness annotation
ggplot(heart_data, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Age with Skewness",
       x = "Age",
       y = "Frequency") +
  geom_vline(xintercept = mean(heart_data$age), color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = mean(heart_data$age) + 5, y = 10, 
           label = paste("Skewness:", round(age_skewness, 2)), 
           color = "red", hjust = 0) +
  theme_minimal()
# Why: Adding the skewness value and mean line helps to interpret the distribution visually.

# Scatter plot of 'age' vs 'resting_blood_pressure' by 'target'
ggplot(heart_data, aes(x = age, y = resting_blood_pressure, color = factor(target))) +
  geom_point(alpha = 0.7) +
  labs(title = "Scatter Plot of Age vs Resting Blood Pressure by Target",
       x = "Age",
       y = "Resting Blood Pressure",
       color = "Target") +
  theme_minimal()
# Why: A scatter plot shows the relationship between age and resting blood pressure, segmented by target (e.g., presence or absence of heart disease).

# Violin plot of 'age' by 'chest_pain_type' and 'sex'
ggplot(heart_data, aes(x = factor(chest_pain_type), y = age, fill = sex)) +
  geom_violin(trim = FALSE) +
  labs(title = "Violin Plot of Age by Chest Pain Type and Sex",
       x = "Chest Pain Type",
       y = "Age") +
  theme_minimal()
# Why: A violin plot visualizes the distribution of age for different chest pain types and sexes, including its spread and density.

# Create age groups based on the 'age' column
heart_data$age_group <- cut(heart_data$age, breaks = seq(0, 80, by = 10), right = FALSE)
# Why: Grouping ages into intervals helps analyze trends and patterns across different age ranges.

# Calculate the survival rate by age group
survival_rate <- aggregate(target ~ age_group, data = heart_data, FUN = mean)
# Why: Aggregating survival rates by age group provides insights into how survival varies with age.

# Line plot of survival rate by age group
ggplot(survival_rate, aes(x = age_group, y = target, group = 1)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Survival Rate by Age Group",
       x = "Age Group",
       y = "Survival Rate") +
  theme_minimal()
# Why: A line plot is useful for visualizing trends in survival rates across different age groups.
