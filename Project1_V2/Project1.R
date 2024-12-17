# Load necessary libraries
library(dplyr) #dplyr: Helps with data manipulation (e.g., filtering, summarizing).
library(ggplot2) #ggplot2: For data visualization (creating charts and graphs).
library(tidyr) #tidyr: Assists in tidying data (e.g., handling missing data).
library(ROSE)    # For balancing datasets
library(VIM)     # Optional for missing data visualization

#-----------------------------------
# Step 1: Load the dataset
#-----------------------------------
file_path <- "/Users/skmuktadir/Desktop/Semester11/Data Science/Project1_V2/Midterm_Dataset_Section(B).csv"  # Replace with your actual file path
df <- read.csv(file_path, stringsAsFactors = FALSE)

# View column names and structure
names(df) # Displays the column names in the dataset.
str(df) #Displays the structure of the dataset (data types, number of rows/columns).

#-----------------------------------
# Convert empty strings to NA
#-----------------------------------
df[df == ""] <- NA

#-----------------------------------
# Step 2: Identify and visualize missing values
#-----------------------------------
missing_counts <- colSums(is.na(df))
missing_counts

# Visualize missing values if present
if(any(missing_counts > 0)) {
  barplot(missing_counts[missing_counts > 0],
          main = "Missing Values in Each Column",
          xlab = "Columns", ylab = "Number of Missing Values",
          col = "steelblue")
} else {
  cat("No missing values.\n")
}

#-----------------------------------
# Data Cleaning Preparations
#-----------------------------------
# Fix inconsistent categorical entries in the suicidal thoughts column if it exists
if("Have.you.ever.had.suicidal.thoughts." %in% names(df)) {
  df$Have.you.ever.had.suicidal.thoughts. <- gsub("Yess", "Yes", df$Have.you.ever.had.suicidal.thoughts.)
  df$Have.you.ever.had.suicidal.thoughts. <- gsub("Noo", "No", df$Have.you.ever.had.suicidal.thoughts.)
}

colSums(is.na(df))

#-----------------------------------
# Step 3: Demonstrate different methods to handle missing values
#-----------------------------------
df_original <- df

# Define a Mode function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Method 1: Numeric NAs with mean, categorical NAs with mode
df_mean_mode <- df_original
for (col_name in names(df_mean_mode)) {
  if (is.numeric(df_mean_mode[[col_name]])) {
    df_mean_mode[[col_name]][is.na(df_mean_mode[[col_name]])] <- mean(df_mean_mode[[col_name]], na.rm = TRUE)
  } else {
    df_mean_mode[[col_name]][is.na(df_mean_mode[[col_name]])] <- Mode(df_mean_mode[[col_name]])
  }
}
print(df_mean_mode)
missing_counts1<-colSums(is.na(df_mean_mode))
missing_counts1
nrow(df_mean_mode)

# Method 2: Numeric NAs with median, categorical NAs with mode
df_median_mode <- df_original
for (col_name in names(df_median_mode)) {
  if (is.numeric(df_median_mode[[col_name]])) {
    df_median_mode[[col_name]][is.na(df_median_mode[[col_name]])] <- median(df_median_mode[[col_name]], na.rm = TRUE)
  } else {
    df_median_mode[[col_name]][is.na(df_median_mode[[col_name]])] <- Mode(df_median_mode[[col_name]])
  }
}

# Method 3: Remove rows with missing values
df_no_na_rows <- na.omit(df_original)

# Method 4: Remove columns with missing values
df_no_na_cols <- df_original[, colSums(is.na(df_original)) == 0]

# For final dataset, let's proceed with the median-mode imputation:
df <- df_median_mode
colSums(is.na(df))

#-----------------------------------
# Step 4: Remove duplicate rows
#-----------------------------------
z<- nrow(df)
z
x <- nrow(df) - nrow(distinct(df))
print(x)
y<-nrow(distinct(df))
print(y)
#-----------------------------------
# Step 5: Handle invalid values / Outliers
# Remove unrealistic ages, e.g., keep only 0 < Age < 120
df <- df %>% filter(Age < 120 & Age > 0)

# Assuming df_original is your dataset before outlier removal
boxplot(df_original$Academic.Pressure, main = "Academic Pressure (Original)",
        ylab = "Values", col = "lightblue")

Q1 <- quantile(df$Academic.Pressure, 0.25)
Q3 <- quantile(df$Academic.Pressure, 0.75)
IQR_val <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR_val
upper_bound <- Q3 + 1.5 * IQR_val

df <- df %>% filter(Academic.Pressure >= lower_bound & Academic.Pressure <= upper_bound)
nrow(df)


#-----------------------------------
# Step 6: Filter the data (example)
# Filter those with Depression == "Yes" and Age > 30
if("Depression" %in% names(df)) {
  filtered_data <- df %>% filter(Depression == "Yes" & Age > 30)
} else {
  filtered_data <- df  # If no Depression column, just keep df
}
filtered_data

#-----------------------------------
# Step 7: Convert categorical columns to factor, numeric columns to numeric
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

# Numeric to Categorical
df$Financial.Stress_catagorical <- factor(df$Financial.Stress, 
                           levels = c(1, 2, 3, 4, 5),
                           labels = c("Very Low", "Low", "Moderate", "High", "Very High"))


# Create AgeGroup if Age column is present
if("Age" %in% names(df)) {
  df$AgeGroup <- cut(df$Age,
                     breaks = c(0, 18, 30, 45, 60, Inf),
                     labels = c("0-18", "19-30", "31-45", "46-60", "60+"),
                     right = FALSE)
  df$AgeGroup <- as.factor(df$AgeGroup)
}

#-----------------------------------
# Step 8: Normalize one attribute (Study.Hours as an example)
if("Study.Hours" %in% names(df)) {
  df$Normalized_Study_Hours <- (df$Study.Hours - min(df$Study.Hours)) /
    (max(df$Study.Hours) - min(df$Study.Hours))
}

#-----------------------------------
# Step 9: Check for class imbalance in the target variable "Depression"
No_count <- df %>% filter(Depression == "No")
print(nrow(No_count))

Yes_count <- df %>% filter(Depression == "Yes")
print(nrow(Yes_count))

if("Depression" %in% names(df)) {
  table(df$Depression)
  prop.table(table(df$Depression))
}

# Before balancing, ensure no empty or NA values remain:
df[df == ""] <- NA  # Convert any new empty strings to NA
colSums(is.na(df))

# Ensure all columns are numeric or factor (no characters, no logical)
df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], as.factor)
df[sapply(df, is.logical)] <- lapply(df[sapply(df, is.logical)], as.factor)

# Double-check structure
str(df)

# If "Depression" is a factor and the dataset is imbalanced, apply ROSE
if("Depression" %in% names(df)) {
  df_balanced <- ROSE(Depression ~ ., data = df, seed = 1)$data
  table(df_balanced$Depression)
}

#-----------------------------------
# Step 10: Final descriptive statistics on the cleaned and imputed dataset
descriptive_stats <- summary(df)
descriptive_stats

# Save results if needed
write.csv(df, "/Users/skmuktadir/Desktop/Semester11/Data Science/Project_Depression_V2/cleaned_dataset.csv", row.names = FALSE)
if(exists("filtered_data")) {
  write.csv(filtered_data, "/Users/skmuktadir/Desktop/Semester11/Data Science/Project_Depression_V2/filtered_data.csv", row.names = FALSE)
}
write.csv(descriptive_stats, "/Users/skmuktadir/Desktop/Semester11/Data Science/Project_Depression_V2/descriptive_statistics.csv")

#-----------------------------------
# End of the code
#-----------------------------------
