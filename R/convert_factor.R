#' Convert columns to factor
#'
#' This function lets you convert columns to \code{factor}.
#' 
#' @param survey A survey object.
#' @param ... Columns to convert to factors. Of the format \code{column_name = factor_levels}.
#' @author Kristian D. Olsen
#' @export
#' @examples 
#' x %>% convert_factor("q1" = "reporttool")

convert_factor <- function(survey, ...) {
  
  # Check class
  if (!inherits(survey, "survey")) {
    stop("Argument 'survey' is not an object with the class 'survey'. See help(survey).", call. = FALSE)
  }
  
  # Measurement model must be added first
  if (!inherits(survey$mm, "survey_mm") || !nrow(survey$mm)) {
    stop("The measurement model must be added first. See help(add_mm).", call. = FALSE)
  }
  
  # Do manual renaming from named strings
  args <- list(...)
  
  # Return early if there are no additional arguments
  if (is.null(args) || !length(args)) return(survey)
  
  # Check that all arguments are strings
  is_string <- vapply(args, is.string, logical(1))
  if (!all(is_string) || is.null(names(args))) {
    stop("All input must be named strings (length 1 character vector).", call. = FALSE)
  } else {
    args <- setNames(args, names(args))
  }
  
  # Throw an error if arguments do not match the manifest
  missing_mm <- setdiff(names(args), survey$mm$manifest)
  if (length(missing_mm)) {
    missing_mm <- stri_c(missing_mm, collapse = ", ")
    stop(stri_c("Variables not found in the measurement model:\n", missing_mm), call. = FALSE)
  }
  
  # Or names in the data
  missing_df <- setdiff(names(args), names(survey$df))
  if (length(missing_df)) {
    missing_df <- stri_c(missing_df, collapse = ", ")
    stop(stri_c("Variables not found in the columnnames of the data (df):\n", missing_df), call. = FALSE)
  }
  
  # Change to date
  for (i in names(args)) {
    dt <- factor(as.character(survey$df[[i]]), levels = args[[i]])
    
    if (all(is.na(dt))) {
      warning("Wrong levels ", stri_c("(", args[[i]],")"), " for variable ", stri_c("'", i, "'"), 
              ". Original data:\n", stri_c(head(survey$df[[i]]), collapse = " "), call. = FALSE)
    } else {
      survey$df[[i]] <- dt
      survey$mm[survey$mm$manifest %in% i, c("type", "values")] <- c("factor", stri_c(args[[i]], collapse = "\n"))
    }
    
  }
  
  # Return
  survey
  
}

