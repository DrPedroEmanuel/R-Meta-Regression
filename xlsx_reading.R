# ---------------------------------------------------------------------------- #
# This function just return the sheets as dataframes in a list --------------- #
# ---------------------------------------------------------------------------- #

# It is called in main.R before the loop
read_sheets_in_xlsx <- function(xlsx_file){
  xlsx_file <- as.character(xlsx_file)
  
  document <- read_excel(xlsx_file)

  sheets <- excel_sheets(xlsx_file)

  dataframes <- list()
  
  for (sheet in sheets){
    dataframes[[sheet]] <- read_excel(xlsx_file, sheet)
  }
  names(dataframes) <- sheets
  
  print("All sheets extracted")

  dataframes
}
