# ---------------------------------------------------------------------------- #
# These functions get data from sheets and add effect sizes and modifiers ---- #
# ---------------------------------------------------------------------------- #

# Main function called in the loop and that calls the others
# It is called in main.R
get_prepared_data <- function(data) {
  data_type <- get_data_type(data)
  prepared_data <- list()
  if (data_type$is_binary) {
    prepared_data[["dataframe"]] <- prepare_binary_data(data)
    prepared_data[["measure"]] <- "OR"
  } else if(data_type$is_mean) {
    prepared_data[["dataframe"]] <- prepare_mean_data(data)
    prepared_data[["measure"]] <- "SMD"
  } else if(data_type$is_hr) {
    prepared_data[["dataframe"]] <- prepare_hr_data(data)
    prepared_data[["measure"]] <- "HR"
  } else {
    stop("Unknown data type")
  }
  prepared_data
}

# Returns a list with the data types in the sheet
# It should return a list with just 1 TRUE value and 2 FALSE
# todo -> add better error handling
get_data_type <- function(data) {

  # Check for binary data indicators
  binary_cols <- c("events_exp", "number_exp", "events_ctrl", "number_ctrl")
  is_binary <- any(binary_cols %in% names(data))
  
  mean_cols <- c("mean_exp", "sd_exp", "mean_ctrl", "sd_ctrl",
                 "median_exp", "iqr_exp", "median_ctrl", "iqr_ctrl")
  is_mean <- any(mean_cols %in% names(data))
  
  is_hr <- any("hr" %in% names(data))
  
  if(is_hr || is_mean){
    is_binary <- FALSE
  }

  data_type <- list(is_binary = is_binary,
                    is_mean = is_mean,
                    is_hr = is_hr)
  data_type
}

# Gets data from binary outcomes and adds effect sizes and modifiers
prepare_binary_data <- function(data) {
  # Ensure events and total patients are numeric
  data$events_exp <- as.numeric(data$events_exp)
  data$number_exp <- as.numeric(data$number_exp)
  data$events_ctrl <- as.numeric(data$events_ctrl)
  data$number_ctrl <- as.numeric(data$number_ctrl)

  # Convert moderator variables to numeric
  moderator_cols <- grep("^mod_", names(data), value = TRUE)
  
  for (col in moderator_cols) {
    data[[col]] <- as.numeric(data[[col]])
  }

  # Calculate effect sizes (log odds ratios)
  effect_sizes <- escalc(measure = "OR",
                         ai = data$events_exp,
                         n1i = data$number_exp,
                         ci = data$events_ctrl,
                         n2i = data$number_ctrl)

  prepared_data <- data
  prepared_data$yi <- effect_sizes$yi
  prepared_data$vi <- effect_sizes$vi
  data <- get_modifier_columns(data)

  prepared_data
}

# Gets data from mean or median outcomes and adds effect sizes and modifiers
prepare_mean_data <- function(data) {
  meta_mean <- metacont(data = data,
                        n.e       = number_exp,
                        mean.e    = mean_exp,
                        sd.e      = sd_exp,
                        n.c       = number_ctrl,
                        mean.c    = mean_ctrl,
                        sd.c      = sd_ctrl,
                        median.e  = median_exp,
                        median.c  = median_ctrl,
                        min.e     = min_exp,
                        min.c     = min_ctrl,
                        max.e     = max_exp,
                        max.c     = max_ctrl,
                        q1.e      = q1_exp,
                        q3.e      = q3_exp,
                        q1.c      = q1_ctrl,
                        q3.c      = q3_ctrl,
                        studlab   = study,
                        common    = FALSE,
                        random    = TRUE,
                        sm        = "MD")

  data$yi <- meta_mean$TE
  data$vi <- meta_mean$seTE^2
  data <- get_modifier_columns(data)

  prepared_data <- data

  prepared_data
}

# Gets data from hazard ratio outcomes and adds effect sizes and modifiers
prepare_hr_data <- function(data) {
  # Hazard ratio data
  data$hr <- as.numeric(data$hr)
  data$lower_ci <- as.numeric(data$lower_ci)
  data$upper_ci <- as.numeric(data$upper_ci)

  # Calculate log HR and its variance
  data$yi <- log(data$hr)
  data$vi <- ((log(data$upper_ci) - log(data$lower_ci)) / (2 * 1.96))^2
  data <- get_modifier_columns(data)

  prepared_data <- data

  prepared_data
}

# Gets the modifier columns and converts them to numeric
get_modifier_columns <- function(data) {
  mod_cols <- grep("^mod_", names(data), value = TRUE)

  for (col in mod_cols) {
    data[[col]] <- as.numeric(data[[col]])
  }

  data
}

