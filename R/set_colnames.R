#' Set latent associations for survey
#'
#' Use this function to rename columns in the data in a survey. It makes sure
#' that the manifest stays updated as well.
#' 
#' @param survey A survey object.
#' @param ... Renaming of individual columns. Of the format \code{old_name = new_name}.
#' @param nms Optional character vector with the column names to use. Either
#' a named vector, or one of the same length as the number of columns in the data.
#' @author Kristian D. Olsen
#' @export
#' @examples 
#' x %>% set_colnames(q1 = "mainentity")

set_colnames <- function(survey, ..., nms = NULL) {
  
  # Check class
  if (!inherits(survey, "survey")) {
    stop("Argument 'survey' is not an object with the class 'survey'. See help(survey).", call. = FALSE)
  }
  
  # Measurement model must be added first
  if (!inherits(survey$mm, "survey_mm") || !nrow(survey$mm)) {
    stop("The measurement model must be added first. See help(add_mm).", call. = FALSE)
  }
  
  # Make sure the existing manifest and colnames match
  if (!identical(names(survey$df), survey$mm$manifest)) {
    stop("Columnnames in the data and in the measurement model do not match.", call. = FALSE)
  }
  
  # Assign 'nms' if it is valid
  if (!is.null(nms)) {
    if(!is.character(nms)) {
      stop("nms must be a character vector.", call. = FALSE)
    }
    
    string_names <- names(nms)
    
    # If it is a named vector
    if (!is.null(string_names) && !any(is.na(string_names))) {
      missing <- setdiff(string_names, survey$ents$entity)
      if (length(missing)) {
        stop("Variables not found\n:", stri_c(missing, collapse = ", "), call. = FALSE)
      } else {
        names(survey$df)[match(string_names, names(survey$df))] <- nms
        survey$mm$manifest[match(string_names, survey$mm$manifest)] <- nms
      }
      
    # If not, only accept if it is the same length
    } else {
      if (length(nms) == length(names(survey$df))) {
        names(survey$df) <- nms
        survey$mm$manifest <- nms
      } else {
        stop("nms must either be a named vector or same length as the column names.", call. = FALSE)
      }
    }
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
    args <- setNames(names(args), args)
  }
  
  # Throw an error if arguments do not match the manifest
  missing_mm <- setdiff(args, survey$mm$manifest)
  if (length(missing_mm)) {
    missing_mm <- stri_c(missing_mm, collapse = ", ")
    stop(stri_c("Variables not found in the measurement model:\n", missing_mm), call. = FALSE)
  }
  
  # Or names in the data
  missing_df <- setdiff(args, names(survey$df))
  if (length(missing_df)) {
    missing_df <- stri_c(missing_df, collapse = ", ")
    stop(stri_c("Variables not found in the columnnames of the data (df):\n", missing_df), call. = FALSE)
  }
  
  # Update names
  names(survey$df) <- ordered_replace(names(survey$df), args)
  survey$mm$manifest <- ordered_replace(survey$mm$manifest, args)
  
  # Return
  survey
  
}

