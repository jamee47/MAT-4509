library(readxl)

# URL of the Excel file in the GitHub repository
github_url <- "https://github.com/jamee47/MAT-4509/raw/refs/heads/main/United%20Airlines%20Aircraft%20Operating%20Statistics-%20Cost%20Per%20Block%20Hour%20(Unadjusted).xls"

# Download the Excel file temporarily
temp_file <- tempfile(fileext = ".xls")
download.file(github_url, temp_file, mode = "wb")

# Verify if the file exists
if (file.exists(temp_file)) {
  cat("File downloaded successfully.\n")
} else {
  stop("Failed to download the file from GitHub.")
}

# Read the Excel file with the specified range
all_data <- read_excel(temp_file, range = "B2:W158")
cat("Data loaded successfully.\n")
print(head(all_data))

# Helper function to extract salary data by row
get_salary_wages <- function(row_num, data = all_data) {
  return(na.omit(as.numeric(data[row_num, -1])))
}

# Extract salary data from the dataset using get_salary_wages()
salary_wages_snbodies <- get_salary_wages(6)    # For small narrowbodies
salary_wages_lnbodies <- get_salary_wages(45)   # For large narrowbodies
salary_wages_wbodies <- get_salary_wages(84)    # For widebodies
salary_wages_tfleet <- get_salary_wages(123)    # For total fleet

# Combine the extracted salary data into one sample
salary_wages_sample <- c(salary_wages_snbodies, salary_wages_lnbodies, salary_wages_wbodies, salary_wages_tfleet)
cat("Combined Salary Data:\n")
print(salary_wages_sample)

# Randomly select 15 observations
set.seed(123)  # For reproducibility
salary_wages_sample_15 <- sample(salary_wages_sample, 15, replace = FALSE)
cat("Sample of 15 Observations:\n")
print(salary_wages_sample_15)

# Define utility functions
get_modes <- function(data) {
  freq_table <- table(data)
  max_freq <- max(freq_table)
  modes <- as.numeric(names(freq_table[freq_table == max_freq]))
  if (length(modes) == length(data)) {
    return(NULL)
  }
  return(modes)
}

get_frequency_distribution <- function(wage_data) {
  n <- length(wage_data)
  k <- ceiling(log2(n))
  min_salary <- min(wage_data)
  max_salary <- max(wage_data)
  class_interval <- ceiling((max_salary - min_salary) / k)
  break_points <- seq(min_salary - class_interval / 2, max_salary + class_interval / 2, by = class_interval)
  salary_bins <- cut(wage_data, breaks = break_points, right = TRUE)
  return(table(salary_bins))
}

# Get frequency distribution for the sample data
frequency_distribution_sample <- get_frequency_distribution(salary_wages_sample_15)
cat("Frequency Distribution for Sample of 15 Observations:\n")
print(frequency_distribution_sample)

# Analysis function
print_analysis <- function(wage_data, title) {
  cat("Analysis of", title, "::\n")
  cat("Mean:", mean(wage_data), "\n")
  cat("Median:", median(wage_data), "\n")
  modes <- get_modes(wage_data)
  if (is.null(modes)) {
    cat("Modes: None\n")
  } else {
    cat("Modes:", paste(modes, collapse = ", "), "\n")
  }
  cat("Sample Standard Deviation:", sd(wage_data), "\n")
  cat("Sample Variance:", var(wage_data), "\n")
  cat("Quartiles:", quantile(wage_data, probs = c(0.25, 0.5, 0.75)), "\n")
  cat("10th Percentile:", quantile(wage_data, probs = 0.10), "\n")
  cat("90th Percentile:", quantile(wage_data, probs = 0.90), "\n")
  cat("Range:", diff(range(wage_data)), "\n\n")
}

# Perform analysis
print_analysis(salary_wages_sample_15, "Salary Wages Sample of 15 Observations")

# Histogram plot function
plot_histogram <- function(frequency_distribution, title) {
  barplot(
    frequency_distribution,
    xlab = "Salary Ranges",
    ylab = "Frequency",
    col = "lightblue",
    main = title
  )
}

# Plot histogram for the sample frequency distribution
plot_histogram(frequency_distribution_sample, "Histogram of Salary Wages Sample of 15 Observations")