

impute_missing <- function(df, vars, cutoff) {
  
  # For reproducibility reasons
  set.seed(1000)
  
  # Subset the data.frame
  names(df) <- tolower(names(df))
  df <- df[c(vars, "percent_missing")]
  
  # Create identifier for a join after analysis
  df$imp_id <- 1:nrow(df)
  
  # Impute missing values
  nvars <- ncol(df[vars])
  bounds <- matrix(c(1:nvars, rep(0, nvars), rep(100, nvars)), ncol=3)
  imp_data <- df[df$percent_missing <= cutoff, ]
  
  # Capture output due to print usage in Amelia-package
  junk <- capture.output(
    imp_data <- Amelia::amelia(imp_data, 5, bounds = bounds, boot.type="none", idvars = c("percent_missing", "imp_id")))
  
  # Get the imputed dataset
  if (!stringi::stri_detect(imp_data$message, regex = "Normal")) {
    warning("Missing values could not be imputed. Cannot calculate PLS-latents.", call. = FALSE)
  } else {
    
    # Get the imputed data
    imp_data <- imp_data$imputations$imp5
    
    # Merge imputed rows with the data and return it
    df[df$imp_id %in% imp_data$imp_id, vars] <- imp_data[vars]
    warning("Imputed missing values.", call. = FALSE)
  }
  
  df[vars]
  
}