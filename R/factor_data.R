#' Convert variables to factor
#'
#' A convenience function to convert variables in the data to (ordered) factor variables,
#' based on the type and values columns in the measurement model. 
#'
#' @param srv  A survey object.
#' @param vars The variables to convert. If \code{NULL}, variables specified as
#' \code{factor} in the measurement model will be converted. Specify if you want
#' to convert \code{scale} variables.
#' @author Kristian D. Olsen
#' @note See \code{rescale_score} and \code{clean_score} to convert scales from
#' factor to numeric again.
#' @export
#' @examples 
#' x %>% factor_data(vars = c("q1", "q17"))

factor_data <- function(srv, vars = NULL) {
  
  # Check the input
  if (!is.survey(srv)) {
    stop("Argument 'srv' is not an object with the class 'survey'. See help(survey).", call. = FALSE)
  }
  
  # Measurement model must be added first
  if (!is.survey_mm(srv$mm) || !nrow(srv$mm)) {
    stop("The measurement model must be added first. See help(add_mm).", call. = FALSE)
  }
  
  # Only convert factors if not otherwise specified
  if (is.null(vars)) {
    vars <- srv$mm$manifest[srv$mm$type == "factor"]
  }
  
  # Check existence of specified vars
  missing_mm <- setdiff(vars, srv$mm$manifest)
  missing_df <- setdiff(vars, names(srv$df))
  
  if (length(missing_mm) || length(missing_df)) {
    stop(stri_c("Columns not found in model and/or data. Check lower and uppercase.\n", 
                "mm: ", stri_c(missing_mm, sep = " "), "\n", 
                "df: ", stri_c(missing_df, sep = " ")), call. = FALSE)
  }
  
  # Clean whitespace characters
  srv$df[vars] <- lapply(srv$df[vars], stri_replace_all, replacement = " ", regex = "\\s")
  
  # Complete the scales (if they are not already factor variables)
  is_scale <- srv$mm$type == "scale" & srv$mm$manifest %in% vars
  if (any(is_scale)) {
    scale <- lapply(srv$mm$values[is_scale], split_scale)
    s_var <- srv$mm$manifest[is_scale]
    srv$df[s_var] <- Map(haven::as_factor, srv$df[s_var], scale, ordered = TRUE)
  }
  
  # Split factor variables
  is_factor <- srv$mm$type == "factor" & srv$mm$manifest %in% vars
  if (any(is_factor)) {
    scale <- lapply(srv$mm$values[is_factor], split_fctr)
    f_var <- srv$mm$manifest[is_factor]
    srv$df[f_var] <- Map(haven::as_factor, srv$df[f_var], scale, ordered = TRUE)
  }
  
  # Convert dates
  is_date <- srv$mm$type == "date"
  if (any(is_date)) {
    d_var <- srv$mm$manifest[is_date]
    srv$df[d_var] <- lapply(srv$df[d_var], as.character)
    srv$df[d_var] <- lapply(srv$df[d_var], as.Date)
  }
  
  # Convert numerics
  is_numeric <- srv$mm$type == "numeric"
  if (any(is_numeric)) {
    n_var <- srv$mm$manifest[is_numeric]
    srv$df[n_var] <- lapply(srv$df[n_var], as.numeric)
  }
  
  # Convert integer
  is_integer <- srv$mm$type == "integer"
  if (any(is_integer)) {
    i_var <- srv$mm$manifest[is_integer]
    srv$df[i_var] <- lapply(srv$df[i_var], as.integer)
  }
  
  # Return
  srv
  
}

# Utilities --------------------------------------------------------------------

split_scale <- function(x) {
  
  v <- stri_split(x, regex = "\n")
  v <- lapply(v, function(x) { n <- length(x); c(stri_c(c(1, 10), x[1:2], sep = " "), if (n > 2) x[3:n] else NULL) })
  v <- lapply(v, function(x) { if (length(x) == 2L) c(x[1], 2:9, x[2]) else c(x[1], 2:9, x[2:3])})
  
  # Return after cleaning whitespaces
  stri_replace_all(unlist(v), " ", regex = "\\s")
  
}

split_fctr <- function(x) {
  
  x <- unlist(stri_split(x, regex = "\n"))
  
  # Return after cleaning whitespaces
  stri_replace_all(x, " ", regex = "\\s")
  
}