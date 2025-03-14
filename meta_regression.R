required_packages <- c("readxl", "metafor", "dplyr", "tidyr", "ggplot2", "knitr", "gridExtra")

for (package in required_packages){
  if (!requireNamespace(package, quietly = TRUE)){
    install.packages(package)
  }
}

library(readxl)      # For reading Excel files
library(metafor)     # For meta-analysis and meta-regression
library(dplyr)       # For data manipulation
library(tidyr)       # For data reshaping
library(ggplot2)     # For visualization
library(knitr)       # For nice tables
library(gridExtra)   # For arranging multiple plots

source("xlsx_reading.R")

binary_outcomes <- read_sheets_in_xlsx("updated_bin_outcomes.xlsx")
continuous_outcomes <- read_sheets_in_xlsx("updated_cont_outcomes.xlsx")

print(binary_outcomes)
