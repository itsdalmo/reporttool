#' Add measurement model to a survey
#'
#' Second step in creating a \code{survey} that can be used with \code{generate_report}.
#' The function accepts a \code{data.frame} as input, but columns must be correctly named
#' or be dropped (see description below). If no model is provided \code{(mm = NULL)}, 
#' \code{add_mm} will generate a suggestion based on the data.
#' 
#' @section Structure/naming convention for \code{mm}:
#' 
#' \describe{
#' 
#'    \item{\code{latent}}{A column describing special significance of variables,
#'    such as latent association, complaints and so forth.}
#' 
#'    \item{\code{manifest}}{The name of variables/columns in the data.}  
#' 
#'    \item{\code{question}}{Questions as posed during interviews. Used
#'    for printed values in \code{generate_report}.} 
#'
#'    \item{\code{type}}{The type of variable in the data. Either \code{character},
#'    \code{factor}, \code{numeric}, or \code{scale}. If type is specified as factor,
#'    the values (see below) will be used to create ordered factors.}
#'
#'    \item{\code{values}}{Optional: Possible values (factor levels) for variables
#'    in the data. If \code{type} is \code{scale}, values are the outer points (1 and 10),
#'    and an optional "do not know".}
#'    
#' }  
#'
#' A structure and naming convention is also required for the survey itself, see
#' \code{help(survey) for information.}
#'
#' @param survey A survey object.
#' @param mm Optional: Specify a \code{data.frame} which contains the measurement
#' model. 
#' @author Kristian D. Olsen
#' @note For smaller surveys, using this function without specifiying a model might
#' not show all possible values for factor variables etc.
#' @export
#' @examples 
#' x <- survey(data.frame("test" = 1, stringsAsFactors = FALSE))
#' x %>% add_mm()

add_mm <- function(survey, mm = NULL, ...) {
  
  # Check the input
  if (!inherits(survey, "survey")) {
    stop("Argument 'survey' is not an object with the class 'survey'. See help(survey)\n", call. = FALSE)
  }
  
  # Generate new, or check that the provided mm is a data.frame
  if (is.null(mm)) {
    mm <- new_mm(survey$df)
  } else if (!inherits(mm, "data.frame")) {
      stop("Measurement model (if specified) should be a data.frame\n", call. = FALSE)
  }
  
  # TODO: Add checks for length/varnames in add_mm? stop if not?
  # TODO: Force DF colnames to equal mm$manifest when mm is specified and same length? or as a separate function?
  # TODO: Should instead manifest be forced to equal data, and supply a separate function to
  # change the columnnames in the data?
  
  # Warn and replace if entities contains existing data
  if (nrow(survey$mm)) {
    warning("Existing measurement model will be replaced\n", call. = FALSE)
    survey$mm <- new_scaffold(default$structure$mm)
  }
  
  # Replace mm in the survey and set class
  survey$mm <- merge_with_scaffold(survey$mm, mm)
  class(survey$mm) <- c("survey_mm", "data.frame")
  
  # Return
  survey
  
}

# Methods ----------------------------------------------------------------------

#' @rdname add_mm
#' @method print survey_mm
#' @export
print.survey_mm <- function(mm, width = getOption("width")) {
  
  cat("Measurement model\n")
  
  # Return early if it is empty
  if (is.null(mm)) {
    cat("Not specified (NULL). See help(add_mm)\n"); return()
  } 
  
  # Print the number of observations
  n <- nrow(mm); cat("Observations: ", n, "\n\n", sep = ""); if (!n) return()
  
  # Return early if it contains no columnnames (obs = 0)
  if (!ncol(mm)) {
    cat("No columns\n"); return()
  }
  
  # Lowercase for easier referencing
  names(mm) <- stringi::stri_trans_tolower(names(mm))
  
  w_name <- max(stringi::stri_length(mm$manifest), na.rm = TRUE) + 1
  w_reserved <- 8 + w_name + 3 # $ and three spaces as separation
  w_available <- width - w_reserved - 5 # in case of large font
  
  # Type
  mm$type <- vapply(mm$type, function(x) {
    x <- ifelse(is.na(x), "miss", x)
    switch(x, character = "(char)", factor = "(fctr)", numeric = "(num)", 
           scale = "(scale)", integer = "(int)", "(????)") }, character(1))
  
  mm$type <- ifelse(!is.na(mm$latent), paste0(mm$type, "*"), mm$type)
  
  # Clean manifest/type
  mm$manifest <- vapply(mm$manifest, stringi::stri_pad_right, width = w_name, character(1))
  mm$type <- vapply(mm$type, stringi::stri_pad_right, width = 8, character(1))
  
  # Shorten question-text to the remaining width
  mm$question <- vapply(mm$question, stringi::stri_sub, to = w_available-2, character(1))
  
  # Print
  for (i in 1:nrow(mm)) {
    cat("$", mm$manifest[i], mm$type[i], " ", mm$question[i], sep = "", collapse = "\n")
  }
  
  cat("Note: Associations (including latents) are marked with *\n")
  
}


# Utilities --------------------------------------------------------------------

new_mm <- function(df) {
  
  # Use variable names as manifest and question text.
  manifest <- names(df)
  question <- stringi::stri_replace(manifest, regex = "\\.", " ")
  
  # Get the variable types (error-prone, but better than nothing)
  type <- vapply(df, class, character(1))
  
  # Set type and try to determine whether it is a scale
  is_character <- manifest[type == "character"]
  
  # Assume that any variables with 1 or more observed alues starting with 1-2
  # numbers, are scale variables with end-points and/or "do not know".
  is_scale <- vapply(df[is_character], function(x) {
    x <- na.omit(unique(x))
    n <- length(x)
    l <- stringi::stri_detect(x, regex = "^[0-9]{1,2}[^0-9][[:alpha:][:punct:] ]+")
    all(sum(l) >= 1, length(x) <= 11)
  }, logical(1))
  
  # Clean up the scale variable values (only keep endpoints)
  scale_values <- lapply(df[is_scale], function(x) {
    x <- na.omit(unique(x))
    s <- stringi::stri_replace(x, "$1", regex = "^[0-9]{1,2}\\s*=?\\s*([[:alpha:]]*)")
    s[s != ""]
  })
  
  # Return if any scale variables were found
  if (length(is_scale)) {
    values <- vector("character", length(manifest))
    values[is_scale] <- vapply(scale_values, paste, collapse = "\n", character(1))
    type[is_scale] <- "scale"
  }
  
  # Create the data.frame
  mm <- data.frame("latent" = NA, "manifest" = manifest, "question" = question, 
                   "type" = type, "values" = values, stringsAsFactors = FALSE)
  
  # Return
  mm
  
}