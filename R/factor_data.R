#' Convert variables to factor
#'
#' A convenience function to convert variables in the data to (ordered) factor variables,
#' based on the type and values columns in the measurement model. 
#'
#' @param survey  A survey object.
#' @param vars The variables to convert. If \code{NULL}, variables specified as
#' \code{factor} in the measurement model will be converted. Specify if you want
#' to convert \code{scale} variables.
#' @author Kristian D. Olsen
#' @note See \code{rescale_score} and \code{clean_score} to convert scales from
#' factor to numeric again.
#' @export
#' @examples 
#' x %>% factor_data(vars = c("q1", "q17"))

factor_data <- function(survey, vars = NULL) {
  
  # Check the input
  if (!inherits(survey, "survey")) {
    stop("Argument 'survey' is not an object with the class 'survey'. See help(survey).", call. = FALSE)
  }
  
  # Measurement model must be added first
  if (!inherits(survey$mm, "survey_mm") || !nrow(survey$mm)) {
    stop("The measurement model must be added first. See help(add_mm).", call. = FALSE)
  }
  
  # Only convert factors if not otherwise specified
  if (is.null(vars)) {
    vars <- survey$mm$manifest[survey$mm$type == "factor"]
  }
  
  # Check existence of specified vars
  missing_mm <- setdiff(vars, survey$mm$manifest)
  missing_df <- setdiff(vars, names(survey$df))
  
  if (length(missing_mm) || length(missing_df)) {
    stop(stri_c("Columns not found in model and/or data. Check lower and uppercase.\n", 
                "mm: ", stri_c(missing_mm, sep = " "), "\n", 
                "df: ", stri_c(missing_df, sep = " ")), call. = FALSE)
  } 
  
  # Complete the scales (if they are not already factor variables)
  is_scale <- survey$mm$type == "scale" & survey$mm$manifest %in% vars
  if (any(is_scale)) {
    scale <- lapply(survey$mm$values[is_scale], split_scale)
    s_var <- survey$mm$manifest[is_scale]
    survey$df[s_var] <- Map(haven::as_factor, survey$df[s_var], scale, ordered = TRUE)
  }
  
  # Split factor variables
  is_factor <- survey$mm$type == "factor" & survey$mm$manifest %in% vars
  if (any(is_factor)) {
    scale <- lapply(survey$mm$values[is_factor], split_fctr)
    f_var <- survey$mm$manifest[is_factor]
    survey$df[f_var] <- Map(haven::as_factor, survey$df[f_var], scale, ordered = TRUE)
  }
  
  # Return
  survey
  
}

# Utilities --------------------------------------------------------------------

split_scale <- function(x) {
  
  v <- stri_split(x, regex = "\n")
  v <- lapply(v, function(x) { n <- 1:length(x); stri_c(c(1, 10, 11)[n], x, sep = " ") })
  v <- lapply(v, function(x) { if (length(x) == 2L) c(x[1], 2:9, x[2]) else c(x[1], 2:9, x[2:3])})
  
  # Retrun
  unlist(v)
  
}

split_fctr <- function(x) {
  
  unlist(stri_split(x, regex = "\n"))
  
}