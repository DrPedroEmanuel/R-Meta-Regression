get_prepared_data <- function(data) {
  data_type <- get_data_type(data)

  if (data_type$is_binary) {
    prepared_data <- prepare_binary_data(data)
  } else if(data_type$is_mean) {
    prepared_data <- prepare_mean_data(data)
  } else if(data_type$is_hr) {
    prepared_data <- prepare_hr_data(data)
  } else {
    stop("Unknown data type")
  }
  prepared_data
}

get_data_type <- function(data) {
  # Check for binary data indicators
  binary_cols <- c("events_exp", "total_exp", "events_ctrl", "total_ctrl")
  is_binary <- any(binary_cols %in% names(data))

  mean_cols <- c("mean_exp", "sd_exp", "mean_ctrl", "sd_ctrl",
                 "median_exp", "iqr_exp", "median_ctrl", "iqr_ctrl")
  is_mean <- any(mean_cols %in% names(data))

  is_hr <- any("hr" %in% names(data))

  data_type <- list(is_binary = is_binary,
                    is_mean = is_mean,
                    is_hr = is_hr)
  data_type
}

prepare_binary_data <- function(data) {
  # Ensure events and total patients are numeric
  data$events_exp <- as.numeric(data$events_exp)
  data$total_exp <- as.numeric(data$total_exp)
  data$events_ctrl <- as.numeric(data$events_ctrl)
  data$total_ctrl <- as.numeric(data$total_ctrl)

  # Convert moderator variables to numeric
  moderator_cols <- grep("^mod_", names(data), value = TRUE)
  for (col in moderator_cols) {
    data[[col]] <- as.numeric(data[[col]])
  }

  # Calculate effect sizes (log odds ratios)
  effect_sizes <- escalc(measure = "OR",
                         ai = data$events_exp,
                         n1i = data$otal_exp,
                         ci = data$events_ctrl,
                         n2i = data$total_ctrl)

  prepared_data <- data
  prepared_data$yi <- effect_sizes$yi
  prepared_data$vi <- effect_sizes$vi

  prepared_data
}

# Prepare continuous outcome data for meta-analysis
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

  # Convert moderator variables to numeric
  mod_cols <- grep("^mod_", names(data), value = TRUE)
  for(col in mod_cols) {
    data[[col]] <- as.numeric(data[[col]])
  }
  prepared_data <- data

  prepared_data
}

prepare_hr_data <- function(data) {
  # Hazard ratio data
  data$hr <- as.numeric(data$hr)
  data$ci_lower <- as.numeric(data$ci_lower)
  data$ci_upper <- as.numeric(data$ci_upper)

  # Calculate log HR and its variance
  data$yi <- log(data$hr)
  data$vi <- ((log(data$ci_upper) - log(data$ci_lower)) / (2 * 1.96))^2

  prepared_data <- data

  prepared_data
}
