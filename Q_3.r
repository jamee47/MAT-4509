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

# Define categories
daily_utilization_categories <- c("Block hours", "Airborne hours", "Departures")
ownership_categories <- c("Rental", "Depreciation and Amortization")
purchased_goods_categories <- c("Fuel/Oil", "Insurance", "Other (inc. Tax)")
fleet_category <- c(
  "small narrowbodies",
  "large narrowbodies",
  "widebodies",
  "total fleet"
)

# Row numbers
purchased_goods_rows <- c(16, 55, 94, 133) - 5
ownership_rows <- purchased_goods_rows + 12
daily_utilization_rows <- ownership_rows + 13

# Function to extract data by row
get_data_by_row <- function(row_num) {
  if (row_num > nrow(all_data)) {
    stop("Row number exceeds data range.")
  }
  return(na.omit(as.numeric(all_data[row_num, -1])))
}

# Function to extract category data
get_category_data <- function(row_num, categories) {
  rows_data <- lapply(
    seq_along(categories),
    function(i) get_data_by_row(row_num + i)
  )
  costs <- unlist(rows_data)
  category <- factor(rep(categories, sapply(rows_data, length)))
  return(data.frame(costs = costs, category = category))
}

# Function to create box plots
box_plot <- function(data, title, ylab) {
  boxplot(costs ~ category,
          data = data,
          main = title,
          col = "orange",
          ylab = ylab,
          border = "red"
  )
}

# Function to plot data by category
plot_category <- function(rows, categories, title, ylab) {
  windows(width = 1920 / 100, height = 1080 / 100) # Set window size
  par(mfrow = c(2, 2), oma = c(0, 0, 3, 0))
  lapply(
    seq_along(rows),
    function(i) {
      box_plot(
        get_category_data(
          rows[i], categories
        ), fleet_category[i], ylab
      )
    }
  )
  mtext(title, outer = TRUE, cex = 1.5)
  par(mfrow = c(1, 1))
}

# Plot purchased goods
plot_category(
  purchased_goods_rows,
  purchased_goods_categories,
  "Purchased Goods",
  "Cost ($)"
)

# Plot aircraft ownership
plot_category(
  ownership_rows,
  ownership_categories,
  "Aircraft Ownership",
  "Cost ($)"
)

# Plot daily utilization
plot_category(
  daily_utilization_rows,
  daily_utilization_categories,
  "Daily Utilization",
  "Hours"
)