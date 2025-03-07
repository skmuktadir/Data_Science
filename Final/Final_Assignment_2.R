#install.packages("ggplot2")
#install.packages("moments")
library(ggplot2)
library(moments)

file_path <- "/Users/skmuktadir/Downloads/HeartDiseaseTrain-Test.csv"  
heart_data <- read.csv(file_path)

str(heart_data)
summary(heart_data)

heart_data$age <- as.numeric(heart_data$age)
heart_data <- na.omit(heart_data) 

ggplot(heart_data, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Age", x = "Age", y = "Frequency")

ggplot(heart_data, aes(x = age)) +
  geom_density(color = "red", size = 1) +
  labs(title = "Density Plot of Age", x = "Age", y = "Density")

age_skewness <- skewness(heart_data$age)
print(paste("Skewness of Age: ", age_skewness))

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

ggplot(heart_data, aes(x = age, y = resting_blood_pressure, color = factor(target))) +
  geom_point(alpha = 0.7) +
  labs(title = "Scatter Plot of Age vs Resting Blood Pressure by Target",
       x = "Age",
       y = "Resting Blood Pressure",
       color = "Target") +
  theme_minimal()

correlation <- cor(heart_data$age, heart_data$resting_blood_pressure, method = "pearson")
print(paste("Correlation:", correlation))



ggplot(heart_data, aes(x = factor(chest_pain_type), y = age, fill = sex)) +
  geom_violin(trim = FALSE) +
  labs(title = "Violin Plot of Age by Chest Pain Type and Sex",
       x = "Chest Pain Type",
       y = "Age") +
  theme_minimal()


heart_data$age_group <- cut(heart_data$age, breaks = seq(0, 80, by = 10), right = FALSE)

survival_rate <- aggregate(target ~ age_group, data = heart_data, FUN = mean)

ggplot(survival_rate, aes(x = age_group, y = target, group = 1)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Survival Rate by Age Group",
       x = "Age Group",
       y = "Survival Rate") +
  theme_minimal()
