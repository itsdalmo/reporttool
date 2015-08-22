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
#' @note See \code{rescale_score} and \code{clean_score} to convert factor variables
#' to numeric again.
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
  
  # Complete the scales
  is_scale <- srv$mm$type == "scale" & survey$mm$manifest %in% vars
  if (any(is_scale)) {
    scale <- lapply(srv$mm$values[is_scale], split_scale)
    s_var <- survey$mm$manifest[is_scale]
    survey$df[s_var] <- Map(haven::as_factor, survey$df[s_var], scale, ordered = TRUE, drop_na = FALSE)
  }
  
  # Split factor variables
  is_factor <- srv$mm$type == "factor" & survey$mm$manifest %in% vars
  if (any(is_factor)) {
    scale <- lapply(srv$mm$values[is_factor], split_fctr)
    f_var <- survey$mm$manifest[is_factor]
    survey$df[f_var] <- Map(haven::as_factor, survey$df[f_var], scale, ordered = TRUE, drop_na = FALSE)
  }
  
  # Return
  survey
  
}

to_sav <- function(survey) {
  
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

from_sav <- function(df, mm = TRUE) {
  
  # Create an empty measurement model
  mm <- new_scaffold(default$structure$mm, ncol(df))
  
  # Populate mm
  mm$manifest <- names(df)
  
  # Get question text from labels
  mm$question <- lapply(df, attr, which = "label")
  mm$question <- vapply(mm$question, function(x) ifelse(is.null(x), "", as.character(x)), character(1))
  
  # Bug in Haven ---------------------------------------------------------------
  
  # Differentiate between scale and factor variable
  labels <- lapply(df, attr, which = "labels")
  type <- vapply(labels, function(x) {
    if (is.null (x)) {
      "other"
    } else {
      scale <- stri_detect(names(x), regex = default$pattern$detect_scale)
      if (sum(scale) == 10L) "scale" else "factor"
    }
  }, character(1))
  
  # Convert scales to factor
  df[type == "scale"] <- lapply(df[type == "scale"], function(x) {
    
    # Fix labelled for scale vectors
    x <- fix_labelled(x)
    
    # Convert to factor
    haven::as_factor(x, drop_na = FALSE)
    
  })
  
  # Convert the remaining labelled
  df[type == "factor"] <- lapply(df[type == "factor"], function(x) {
    haven::as_factor(x, drop_na = FALSE)
  })
   
  #df[] <- lapply(df, function(x) { if (inherits(x, "labelled")) haven::as_factor(x, drop_na = FALSE) else x })
  # Bug in Haven ---------------------------------------------------------------
  
  # Insert variable type
  mm$type <- vapply(df, class, character(1))
  
  # Extract factor levels
  vars <- lapply(df, levels)
  is_scale <- unlist(lapply(vars, function(x) {
    sum(stri_detect(x, regex = default$pattern$detect_scale)) == 10L }))
  
  # Clean up the scale variable values (only endpoints)
  vars[is_scale] <- lapply(vars[is_scale], function(x) {
    scales <- stri_replace(x, "$1", regex = default$pattern$extract_scale)
    scales <- scales[scales != ""]
  })
  
  # Add values and set corresponding type
  is_null <- vapply(vars, is.null, logical(1))
  mm$values[!is_null] <- vapply(vars[!is_null], stri_c, collapse = "\n", character(1))
  mm$type[type == "scale"] <- "scale"
  
  # Remove label attribute from data
  df[] <- lapply(df, function(x) { attr(x, "label") <- NULL; x })
  
  # Return
  list("df" = df, "mm" = mm)
  
}

# Temporary fix to a bug in haven::read_sav

fix_labelled <- function(x) {
  
  labels <- attr(x, "labels")
  
  # If it has a 'do not know', fix
  if (length(labels) == 11L) {
    
    # Keep the label
    label <- attr(x, "label")
    label <- if (is.null(label)) "" else label
    
    # Fix values
    v <- as.numeric(x)
    v[!v %in% 1:10] <- 11
    
    # Fix labels and assign
    labels[length(labels)] <- 11
    x <- labelled(v, labels)
    attr(x, "label") <- label
    
  }
  
  # Return
  x
  
}

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