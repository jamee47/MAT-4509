# MAT-4509
# United Airlines Aircraft Operating Statistics Analysis

This repository contains scripts and data for analyzing the aircraft operating statistics of United Airlines. The analysis is focused on extracting and visualizing information from an Excel file related to **Cost Per Block Hour (Unadjusted)**.
This project is part of the **MAT 4509 Assignment** and demonstrates practical applications of statistical analysis and data visualization.

## Contents

- **Dataset**: 
  - `United Airlines Aircraft Operating Statistics - Cost Per Block Hour (Unadjusted).xls`
    - Contains operational and cost data for various aircraft categories.
- **Scripts**: 
  - `Q_1.R`: Performs statistical analysis and visualizations for maintenance costs and load factors.
  - `Q_2.R`: Analyzes purchased goods, aircraft ownership, and daily utilization costs using boxplots.
  - `Q_3.R`: Extracts salary/wage data for different fleet categories, performs statistical analysis, and plots a histogram of selected sample observations.

## Features

1. **Data Extraction**: 
   - Reads specific ranges of data from the Excel file.
   - Processes data for different aircraft categories: 
     - Small Narrowbodies
     - Large Narrowbodies
     - Widebodies
     - Total Fleet

2. **Statistical Analysis**:
   - Calculation of mean, median, mode, variance, standard deviation, and quartiles.
   - Construction of frequency distributions for salary/wage samples.

3. **Visualization**:
   - Pie charts for maintenance cost distribution.
   - Bar plots for load factor by fleet category.
   - Boxplots for costs of purchased goods, aircraft ownership, and daily utilization.
   - Histogram for salary/wage sample distributions.

## Requirements

### R Packages
The following R packages are required:
- `readxl` - For reading Excel files.
- `RColorBrewer` - For enhanced color palettes in visualizations.
- `rstudioapi` - For setting up the working directory dynamically.

Install these packages using the command:
  install.packages(c("readxl", "RColorBrewer", "rstudioapi"))
## How to Use
**Clone this repository:**
   '''' https://github.com/jamee47/MAT-4509.git ''''

Open the desired R script in RStudio or any R IDE.

## Run the script:

    Q_1.R: Visualizes maintenance and load factor statistics.
    Q_2.R: Creates boxplots for costs across fleet categories.
    Q_3.R: Analyzes salary/wage data and visualizes frequency distributions.
View the generated visualizations and statistical results in the RStudio plots window.
