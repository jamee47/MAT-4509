library(readxl)
library(RColorBrewer)

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

# Maintenance categories and years
maintenance_categories <- c("labor", "materials", "third party", "burden")
years <- 1995:2015

# Function to extract data for a given row number (Maintenance/Load Factor)
get_data_by_row <- function(row_num) {
  return(na.omit(as.numeric(all_data[row_num, -1])))
}

get_maintenace_category <- function(row_num) {
  labor <- get_data_by_row(row_num + 1)
  materials <- get_data_by_row(row_num + 2)
  third_party <- get_data_by_row(row_num + 3)
  burden <- get_data_by_row(row_num + 5)
  return(setNames(
    c(sum(labor), sum(materials), sum(third_party), sum(burden)),
    maintenance_categories
  ))
}

# Function to plot bar charts for load factor
plot_bar <- function(data, title) {
  barplot(data,
          main = title,
          xlab = "Years",
          ylab = "Load Factor (%)",
          col = "lightblue",
          border = "red"
  )
}

# Maintenance and Load Factor row numbers
maintenance_rows <- c(16, 55, 94, 133)
load_factor_rows <- c(34, 73, 112, 151)

fleet_category <- c(
  "small narrowbodies",
  "large narrowbodies",
  "widebodies",
  "total fleet"
)

# Pie charts for maintenanc
windows(width = 1920 / 100, height = 1080 / 100) # Set window size
par(mfrow = c(2, 2), oma = c(0, 0, 3, 0))

# Define a color palette
colors <- brewer.pal(4, "Set3")  # Using RColorBrewer for a set of 4 distinct colors

# Create pie charts for each maintenance category
lapply(1:4, function(i) {
  data <- get_maintenace_category(maintenance_rows[i])
  
  # Calculate percentages
  percentages <- round(100 * data / sum(data), 1)
  
  # Create labels with category names and percentages
  labels <- paste0(names(data), ": ", percentages, "%")
  
  # Create pie chart
  pie(data, 
      labels = labels,        # Use labels with percentages
      main = paste("Maintenance Costs for", fleet_category[i]),  # Descriptive title
      col = colors,           # Set colors for slices
      border = "white")       # Add border to slices
})

# Add an outer title for all pie charts
mtext("Maintenance Cost Distribution", outer = TRUE, cex = 1.5)

# Reset plotting parameters to default
par(mfrow = c(1, 1))

# Bar charts for load factor
windows(width = 1920 / 100, height = 1080 / 100) # Set window size
par(mfrow = c(2, 2), oma = c(0, 0, 3, 0))
lapply(1:4, function(i) {
  data <- setNames(get_data_by_row(load_factor_rows[i]), years)
  plot_bar(data, fleet_category[i])
})
mtext("Load Factor", outer = TRUE, cex = 1.5)
par(mfrow = c(1, 1))