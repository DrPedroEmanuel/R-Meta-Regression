required_packages <- c("readxl",
                         "metafor",
                         "dplyr",
                         "tidyr",
                         "ggplot2",
                         "knitr",
                         "gridExtra")

load_packages(required_packages)

experimental_group <- "TCAR"
control_group <- "TFCAS"

dataframes <- read_sheets_in_xlsx("outcomes.xlsx")

results_handler <- list()

for (dataframe_name in names(dataframes)){
  
  
  dataframe <- dataframes[[dataframe_name]]

  print(paste("Preparing data for:", dataframe_name))
  total_data <- get_prepared_data(dataframe)
  
  prepared_data <- total_data[["dataframe"]]
  measure <- total_data[["measure"]]

  meta_analysis <- perform_meta_analysis(prepared_data, dataframe_name, measure)
  
  if (!is.null(meta_analysis)) {
    results_handler[[dataframe_name]][["Meta Analysis"]] <- meta_analysis
  } else {
    stop("NULL meta-analysis")
  }
  
  create_forest_plot(meta_analysis, dataframe, dataframe_name, measure, 
                     experimental_group, control_group)
  

}

# ---------------------------------------------------------------------------- #
# HELPER FUNCTIONS ----------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# Load required packages
load_packages <- function(required_packages){

  for (package in required_packages){
    if (!requireNamespace(package, quietly = TRUE)){
      install.packages(package)
    }
    if(!(package %in% loadedNamespaces())){
      library(package, character.only = TRUE)
    }
  }
  
  source("xlsx_reading.R")
  source("data_preparation.R")
  source("perform_metas.R")
  source("plotters.R")
  
  # Cria pastas plots e plots/sensitivity se não existirem na pasta de trabalho
  if (!dir.exists("plots")) {
    dir.create("plots")
  }
  if (!dir.exists("plots/sensitivity")) {
    dir.create("plots/sensitivity")
  }
  if (!dir.exists("plots/regression")) {
    dir.create("plots/regression")
  }
}

analyze_outcomes <- function(outcomes_list, data_type) {
  results <- list()
  plots <- list()

  for(outcome_name in names(outcomes_list)) {
    print(paste("Analyzing", outcome_name))

    # Perform standard meta-analysis
    ma_model <- try(rma(yi, vi, data = data, method = "REML"), silent = TRUE)

    if(!inherits(ma_model, "try-error")) {
      results[[outcome_name]][["meta_analysis"]] <- ma_model

      # Create forest plot for the outcome
      create_forest_plot(ma_model, outcome_name, measure_type)

      # Get moderator variables
      moderators <- grep("^mod_", names(data), value = TRUE)

      # Perform meta-regression for each moderator
      for(mod in moderators) {
        print(paste("  Testing moderator:", mod))
        mr_result <- perform_metareg(data, mod)

        if(!is.null(mr_result$model)) {
          results[[outcome_name]][[mod]] <- mr_result$model

          # Create and save bubble plot
          bubble_plot <- create_bubble_plot(data, mod, mr_result$model, outcome_name)
          if(!is.null(bubble_plot)) {
            plot_filename <- paste0("bubble_", outcome_name, "_", gsub("mod_", "", mod), ".pdf")
            ggsave(plot_filename, bubble_plot, width = 8, height = 6)
            plots[[paste(outcome_name, mod, sep = "_")]] <- bubble_plot
          }
        } else {
          print(paste("    ", mr_result$message))
        }
      }
    } else {
      print(paste("  Error in meta-analysis for", outcome_name))
    }
  }

  return(list(results = results, plots = plots))
}
