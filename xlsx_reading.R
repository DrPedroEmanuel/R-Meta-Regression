# ---------------------------------------------------------------------------- #
# This function just return the sheets as dataframes in a list --------------- #
# ---------------------------------------------------------------------------- #

# It is called in meta_regression.R before the loop
read_sheets_in_xlsx <- function(xlsx_file){
  load_packages_helper()

  sheets <- excel_sheets(xlsx_file)
  dataframes <- c()
  for (sheet in sheets){
    dataframes <- create_dataframe_from_sheet(xlsx_file, sheet)
  }
  names(dataframes) <- sheets
  print("Sheets extracted")

  dataframes
}

# Calls the function in meta_regression.R that loads the required packages
load_packages_helper <- function(){
  required_packages <- c("readxl", "dplyr", "tidyr")
  source("meta_regression.R")
  load_packages(required_packages)
}
