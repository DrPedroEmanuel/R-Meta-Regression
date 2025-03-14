perform_meta_regression <- function(data, moderator) {
  library(metafor)
  # Perform meta-regression
  meta_regression <- try(rma(yi, vi, mods = ~ get(moderator),
                   data = data, method = "REML"), silent = TRUE)

  if (inherits(meta_regression, "try-error")) {
    print(paste("Error in meta-regression for", moderator))
    meta_regression <- NULL
  }
  meta_regression
}

perform_meta_analysis <- function(data, outcome_name, measure) {
  
  print(data)
  
  if (measure == "OR") {
    meta_analysis <- try(metabin(data = data, 
                                 method = "MH", 
                                 sm = measure,
                                 n.e = number_exp,
                                 event.e = events_exp,
                                 n.c = number_ctrl,
                                 event.c = events_ctrl,
                                 common = FALSE,
                                 random = FALSE))
  } else {
    meta_analysis <- try(metagen(data = data,
                                 studlab = study,
                                 sm = measure,
                                 common = FALSE,
                                 random = TRUE))
  }

  if (inherits(meta_analysis, "try-error")) {
    print(paste("Error in meta-analysis for", outcome_name))
    meta_analysis <- NULL
  }
  
  meta_analysis
}

perform_metacont <- function(data) {
}

perform_metabin <- function(data) {
}

perform_metagen <- function(data) {
}
