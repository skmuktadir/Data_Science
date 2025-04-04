library(dplyr)
library(ggplot2)

data <- read.csv("/Users/skmuktadir/Downloads/heart.csv", stringsAsFactors = TRUE)

str(data)
summary(data)

table_sex_hd <- table(data$Sex, data$HeartDisease)

chi_test <- chisq.test(table_sex_hd)

print(chi_test)

data$Age <- as.numeric(data$Age)
data$MaxHR <- as.numeric(data$MaxHR)

kendall_test <- cor.test(data$Age, data$MaxHR, method = "kendall", use = "pairwise.complete.obs")

print(kendall_test)

data$Cholesterol <- as.numeric(data$Cholesterol)

anova_test <- aov(Cholesterol ~ ChestPainType, data = data)

summary(anova_test)

ggplot(data, aes(x = Sex, fill = as.factor(HeartDisease))) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Sex and Heart Disease", x = "Sex", fill = "Heart Disease") +
  theme_minimal()

ggplot(data, aes(x = Age, y = MaxHR)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Scatter Plot of Age vs MaxHR", x = "Age", y = "MaxHR") +
  theme_minimal()

ggplot(data, aes(x = ChestPainType, y = Cholesterol, fill = ChestPainType)) +
  geom_boxplot() +
  labs(title = "Boxplot of Cholesterol by Chest Pain Type", x = "Chest Pain Type", y = "Cholesterol") +
  theme_minimal() +
  theme(legend.position = "none")
