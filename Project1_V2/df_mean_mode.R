# Sample dataset
df_original <- data.frame(
  Name = c("Alice", "Bob", "Charlie", "Dave", "Eve"),
  Age = c(25, NA, 30, 22, NA),
  Gender = c("Female", "Male", NA, "Male", "Female"),
  Income = c(50000, 60000, 70000, NA, 55000)
)

# View the original data
print("Original Data:")
print(df_original)

# Function to calculate Mode (most frequent value) for categorical columns
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Copy the original dataframe
df_mean_mode <- df_original

# Loop through all columns
for (col_name in names(df_mean_mode)) {
  
  # If the column is numeric, replace NA with the mean of that column
  if (is.numeric(df_mean_mode[[col_name]])) {
    df_mean_mode[[col_name]][is.na(df_mean_mode[[col_name]])] <- mean(df_mean_mode[[col_name]], na.rm = TRUE)
  } 
  
  # If the column is not numeric (i.e., categorical), replace NA with the mode of that column
  else {
    df_mean_mode[[col_name]][is.na(df_mean_mode[[col_name]])] <- Mode(df_mean_mode[[col_name]])
  }
}

# View the updated data
print("Data after handling missing values:")
print(df_mean_mode)
