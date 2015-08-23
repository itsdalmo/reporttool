#' Flatten SPSS input
#'
#' When reading SPSS files with \code{\link{read_data}}, se this function to convert 
#' \code{labelled} to \code{factor} and remove additional attributes from the data.
#' The function returns a \code{list} (mm and df) and the additional information is stored in
#' \code{mm}. The process can be returned when writing spss files with \code{\link{write_data}}
#' (if changes to the data are reflected in the mm), but it is better to not avoid using this
#' function when doing light data cleaning and writing it after.
#'
#' @param df A data.frame as returned from \code{read_data} or \code{haven::read_sav}.
#' \code{data.frame} is returned from the function.
#' @author Kristian D. Olsen
#' @note The results are error-prone. Carefully check the results.
#' @export
#' @examples 
#' read_data("test.sav") %>% from_labelled()

from_labelled <- function(df) {
  
  # Get a list of labelled variables
  is_labelled <- vapply(df, inherits, what = "labelled", logical(1))
  if (!any(is_labelled)) warning("No labelled variables found.", call. = FALSE)
  
  # Create an empty measurement model
  mm <- new_scaffold(default$structure$mm, ncol(df))
  
  # Populate mm
  mm$manifest <- names(df)
  
  # Get question text from labels
  mm$question <- lapply(df, attr, which = "label")
  mm$question <- vapply(mm$question, function(x) ifelse(is.null(x), "", as.character(x)), character(1))
  
  # Differentiate between scale and factor variables
  labels <- lapply(df, function(x) names(attr(x, "labels")))
  is_scale <- vapply(labels, function(x) {
    sum(stri_detect(x, regex = default$pattern$detect_scale)) == 10L }, logical(1))
  
  # Check labelled scales for consistency and convert to factor
  df[is_scale] <- lapply(df[is_scale], fix_labelled)
  df[is_labelled] <- lapply(df[is_labelled], haven::as_factor, drop_na = FALSE)
  
  # Insert variable type
  mm$type <- vapply(df, class, character(1))
  mm$type[is_scale] <- "scale"
  
  # Clean up the scale variable values (only endpoints)
  labels[is_scale] <- lapply(labels[is_scale], function(x) {
    scales <- stri_replace(x, "$1", regex = default$pattern$extract_scale)
    scales[scales != ""]
  })
  
  # Add values and set corresponding type
  is_null <- vapply(labels, is.null, logical(1))
  mm$values[!is_null] <- vapply(labels[!is_null], stri_c, collapse = "\n", character(1))
  
  # Remove label attribute from data
  df[] <- lapply(df, function(x) { attr(x, "label") <- NULL; x })
  
  # Return
  list("df" = df, "mm" = mm)
  
}

#' Convert to labelled
#'
#' Reverses the process from \code{\link{from_labelled}}, and uses a measurement
#' model to create labelled variables and add \code{label} to the attributes of
#' each variable in the data. Meant for use with survey objects from \code{\link{survey}}.
#'
#' @param survey A survey object, or a list with data (df) and a measurement model (mm).
#' @author Kristian D. Olsen
#' @note The results are error-prone. Carefully check the results.
#' @export
#' @examples 
#' read_data("test.sav") %>% from_labelled() %>% to_labelled()


to_labelled <- function(survey) {
  
  # Convert to factors/scales
  vars <- survey$mm$manifest[survey$mm$type %in% c("scale", "factor")]
  
  # Make sure all factor/scale variables are factors
  survey <- factor_data(survey, vars)
  
  # Convert all factors to 'labelled'
  survey$df[] <- lapply(names(df), function(nm, df, mm) {
    
    x <- df[[nm]]
    v <- levels(x)
    
    if (is.factor(x)) {
      x <- as.numeric(x); x <- haven::labelled(x, setNames(as.numeric(1:length(v)), v), is_na = NULL)
    }
    
    # Set attributes/class and return
    attr(x, "label") <- mm$question[mm$manifest %in% nm]
    x
    
  }, survey$df, survey$mm)
  
  # Return
  survey
  
}

# Utilities --------------------------------------------------------------------

fix_labelled <- function(x) {
  
  labels <- attr(x, "labels")
  values <- unique(x[!is.na(x)])
  differ <- setdiff(values, labels)
  
  # If it has a 'do not know', fix
  if (length(differ)) {
    if (!differ %in% c(11, 98)) 
      warning("Assigned ", differ, " to label:\n", labels[length(labels)], call. = FALSE)
    
    # Set last value to 'do not know'
    labels[length(labels)] <- differ
    
    # Assign the fixed labels
    attr(x, "labels") <- labels
  }
  
  # Return
  x
  
}
