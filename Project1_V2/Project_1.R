library(dplyr)
library(ggplot2)
library(tidyr)
library(ROSE)
library(VIM)

file_path <- "/Users/skmuktadir/Desktop/Semester11/Data Science/Project_Depression_V2/Midterm_Dataset_Section(B).csv"
df <- read.csv(file_path, stringsAsFactors = FALSE)

names(df)
str(df)

df[df == ""] <- NA

missing_counts <- colSums(is.na(df))
missing_counts

if(any(missing_counts > 0)) {
  barplot(missing_counts[missing_counts > 0],
          main = "Missing Values in Each Column",
          xlab = "Columns", ylab = "Number of Missing Values",
          col = "steelblue")
} else {
  cat("No missing values.\n")
}

if("Have.you.ever.had.suicidal.thoughts." %in% names(df)) {
  df$Have.you.ever.had.suicidal.thoughts. <- gsub("Yess", "Yes", df$Have.you.ever.had.suicidal.thoughts.)
  df$Have.you.ever.had.suicidal.thoughts. <- gsub("Noo", "No", df$Have.you.ever.had.suicidal.thoughts.)
}

colSums(is.na(df))

df_original <- df

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

df_mean_mode <- df_original
for (col_name in names(df_mean_mode)) {
  if (is.numeric(df_mean_mode[[col_name]])) {
    df_mean_mode[[col_name]][is.na(df_mean_mode[[col_name]])] <- mean(df_mean_mode[[col_name]], na.rm = TRUE)
  } else {
    df_mean_mode[[col_name]][is.na(df_mean_mode[[col_name]])] <- Mode(df_mean_mode[[col_name]])
  }
}
print(df_mean_mode)
missing_counts1 <- colSums(is.na(df_mean_mode))
missing_counts1
nrow(df_mean_mode)

df_median_mode <- df_original
for (col_name in names(df_median_mode)) {
  if (is.numeric(df_median_mode[[col_name]])) {
    df_median_mode[[col_name]][is.na(df_median_mode[[col_name]])] <- median(df_median_mode[[col_name]], na.rm = TRUE)
  } else {
    df_median_mode[[col_name]][is.na(df_median_mode[[col_name]])] <- Mode(df_median_mode[[col_name]])
  }
}

df_no_na_rows <- na.omit(df_original)

df_no_na_cols <- df_original[, colSums(is.na(df_original)) == 0]

df <- df_median_mode
colSums(is.na(df))

x <- nrow(df) - nrow(distinct(df))
print(x)
y <- nrow(distinct(df))
print(y)

df <- df %>% filter(Age < 120 & Age > 0)

boxplot(df_original$Academic.Pressure, main = "Academic Pressure (Original)",
        ylab = "Values", col = "lightblue")

Q1 <- quantile(df$Academic.Pressure, 0.25)
Q3 <- quantile(df$Academic.Pressure, 0.75)
IQR_val <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR_val
upper_bound <- Q3 + 1.5 * IQR_val

df <- df %>% filter(Academic.Pressure >= lower_bound & Academic.Pressure <= upper_bound)
nrow(df)

if("Depression" %in% names(df)) {
  filtered_data <- df %>% filter(Depression == "Yes" & Age > 30)
} else {
  filtered_data <- df
}
filtered_data

df$Dietary.Habits <- factor(df$Dietary.Habits, 
                            levels = c("Healthy", "Moderate", "Unhealthy"))
df$Dietary.Habits_num <- as.numeric(df$Dietary.Habits)

df$Gender <- factor(df$Gender, 
                    levels = c("Male", "Female"))
df$Gender_num <- as.numeric(df$Gender)

df$Depression <- factor(df$Depression, 
                        levels = c("Yes", "No"))
df$Depression_num <- as.numeric(df$Depression)

df$Sleep.Duration <- factor(df$Sleep.Duration, 
                            levels = c("5-6 hours", "7-8 hours", "Less than 5 hours","More than 8 hours"))
df$Sleep.Duration_num <- as.numeric(df$Sleep.Duration)

df$Financial.Stress_catagorical <- factor(df$Financial.Stress, 
                                          levels = c(1, 2, 3, 4, 5),
                                          labels = c("Very Low", "Low", "Moderate", "High", "Very High"))

if("Age" %in% names(df)) {
  df$AgeGroup <- cut(df$Age,
                     breaks = c(0, 18, 30, 45, 60, Inf),
                     labels = c("0-18", "19-30", "31-45", "46-60", "60+"),
                     right = FALSE)
  df$AgeGroup <- as.factor(df$AgeGroup)
}

if("Study.Hours" %in% names(df)) {
  df$Normalized_Study_Hours <- (df$Study.Hours - min(df$Study.Hours)) /
    (max(df$Study.Hours) - min(df$Study.Hours))
}

No_count <- df %>% filter(Depression == "No")
print(nrow(No_count))

Yes_count <- df %>% filter(Depression == "Yes")
print(nrow(Yes_count))

if("Depression" %in% names(df)) {
  table(df$Depression)
  prop.table(table(df$Depression))
}

df[df == ""] <- NA
colSums(is.na(df))

df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], as.factor)
df[sapply(df, is.logical)] <- lapply(df[sapply(df, is.logical)], as.factor)

str(df)

if("Depression" %in% names(df)) {
  df_balanced <- ROSE(Depression ~ ., data = df, seed = 1)$data
  table(df_balanced$Depression)
}

descriptive_stats <- summary(df)
descriptive_stats

write.csv(df, "/Users/skmuktadir/Desktop/Semester11/Data Science/Project_Depression_V2/cleaned_dataset.csv", row.names = FALSE)
if(exists("filtered_data")) {
  write.csv(filtered_data, "/Users/skmuktadir/Desktop/Semester11/Data Science/Project_Depression_V2/filtered_data.csv", row.names = FALSE)
}
write.csv(descriptive_stats, "/Users/skmuktadir/Desktop/Semester11/Data Science/Project_Depression_V2/descriptive_statistics.csv")
