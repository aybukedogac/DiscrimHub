# DiscrimHub

DiscrimHub is an R Shiny application designed to calculate and visualize various discrimination indices for test items. It provides a user-friendly interface for educators and researchers to analyze test data and evaluate item performance.

## Features

- Data upload and preview
- Calculation of multiple discrimination indices:
  - A Special Case of Brennan's Index
  - Brennan's Index (1972)
  - Discrimination Index
  - Item-Total Correlation
  - Item-Rest Correlation
- Interactive visualizations of discrimination indices
- Item Response Curves
- Descriptive statistics for items and total scores

## Installation

To run DiscrimHub locally, you need to have R and several R packages installed. Follow these steps:

1. Clone this repository:
   ```
   git clone https://github.com/yourusername/discrimhub.git
   ```

2. Install the required R packages. You can do this by running the following R code:

   ```R
   packages <- c(
     "shiny", "shinydashboard", "shinydashboardPlus", "dplyr", "plyr",
     "readxl", "shinythemes", "ggplot2", "plotly", "gt", "tidyr", "DT", "writexl"
   )

   install_if_missing <- function(pkg) {
     if (!require(pkg, character.only = TRUE)) {
       install.packages(pkg, dependencies = TRUE)
     }
   }

   lapply(packages, install_if_missing)
   ```

## Usage

To run the application:

1. Open R or RStudio
2. Set your working directory to the cloned repository
3. Run the following command:
   ```R
   shiny::runApp()
   ```

The application should open in your default web browser.

Alternatively, you can access the online version of DiscrimHub at: https://shiny.eptlab.com/discrimhub/

## Data Format

DiscrimHub expects data in the following format:

- Excel file (.xlsx or .xls)
- Each row represents a student's responses
- Each column represents an item
- The first row should contain item names as column headings
- Use 0 for wrong answers and 1 for correct answers
- Include a "TotalScore" column or ensure the app can calculate it based on item responses

## Contributing

We welcome contributions to DiscrimHub! Please feel free to submit issues, feature requests, or pull requests.

## Citation

If you use DiscrimHub in your research, please cite it as follows:

Doğaç, A., Aybek, E. C., Arıkan, S., & Coşkun, S. (2024). *DiscrimHub* [R Shiny application]. https://shiny.eptlab.com/discrimhub/

## Contact

For questions or feedback, please contact:
- Aybüke Doğaç: [dogacaybuke@gmail.com](mailto:dogacaybuke@gmail.com)

## Acknowledgments

This project is based on research by Arikan, S., & Aybek, E. C. (2022). A Special Case of Brennan's Index for Tests That Aim to Select a Limited Number of Students: A Monte Carlo Simulation Study. *Educational Measurement: Issues and Practice, 41*(4), 35-49.
