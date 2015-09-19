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
#' @param srv A survey object.
#' @param mm Optional: Specify a \code{data.frame} which contains the measurement
#' model. 
#' @author Kristian D. Olsen
#' @note For smaller surveys, using this function without specifiying a model might
#' not show all possible values for factor variables etc.
#' @export
#' @examples 
#' x <- survey(data.frame("test" = 1, stringsAsFactors = FALSE))
#' x %>% add_mm()

add_mm <- function(srv, mm = NULL) {
  
  # Check the input
  if (!is.survey(srv)) {
    stop("Argument 'srv' is not an object with the class 'survey'. See help(survey).", call. = FALSE)
  }
  
  # Generate new, or check that the provided mm is a data.frame
  if (is.null(mm)) {
    mm <- new_mm(srv$df)
  } else if (!is.data.frame(mm)) {
      stop("Measurement model (if specified) should be a data.frame.", call. = FALSE)
  }
  
  # TODO: Add checks for length/varnames in add_mm? stop if not?
  # TODO: Force DF colnames to equal mm$manifest when mm is specified and same length? or as a separate function?
  # TODO: Should instead manifest be forced to equal data, and supply a separate function to
  # change the columnnames in the data?
  
  # Warn and replace if measurement model contains existing data
  if (nrow(srv$mm)) {
    warning("Measurement model has been replaced.", call. = FALSE)
    srv$mm <- new_scaffold(default$structure$mm)
  }
  
  # Replace mm in the srv and set class
  srv$mm <- merge_with_scaffold(srv$mm, mm)
  srv$mm <- as.survey_mm(srv$mm)
  
  # Return
  srv
  
}

# Utilities --------------------------------------------------------------------

is.survey_mm <- function(x) inherits(x, "survey_mm")
as.survey_mm <- function(x) structure(x, class = c("survey_mm", "data.frame"))

update_mm <- function(srv, cols) {
  
  # Create a new measurement model
  mm <- new_mm(srv$df[cols])
  
  # Find new columns, or old columns that have been changed
  new_cols <- setdiff(cols, srv$mm$manifest)
  old_cols <- setdiff(cols, new_cols)
  
  # Update old columns
  changed <- mm[mm$manifest %in% old_cols, c("type", "values")]
  if (nrow(changed)) {
    srv$mm[srv$mm$manifest %in% old_cols, c("type", "values")] <- changed
  }
  
  # And new columns
  srv$mm <- bind_rows(srv$mm, mm[mm$manifest %in% new_cols, ])
  srv$mm <- as.survey_mm(srv$mm)
  
  # Return
  srv
  
}

new_mm <- function(df) {
  
  # Use variable names as manifest and question text.
  manifest <- names(df)
  question <- stri_replace(manifest, regex = "\\.", " ")
  
  # Get the variable types (error-prone, but better than nothing)
  type <- lapply(df, class)
  type <- lapply(type, function(x) { if (length(x) > 1 && x[1] == "ordered") "factor" else x})
  type <- unlist(type)
  
  # See if any variables are factors
  values <- vector("character", length(manifest))
  is_factor <- type == "factor"
  
  # Get factor levels
  if (any(is_factor)) {
    values <- lapply(df[is_factor], levels)
  } 

  # See if any factors are scales
  if (any(is_factor)) {
    is_scale <- vapply(values, function(x) {
      x <- na.omit(unique(x))
      l <- stri_detect(x, regex = "^[0-9]{1,2}[^0-9][[:alpha:][:punct:] ]+")
      all(sum(l) >= 1, length(x) <= 11)
    }, logical(1))
  } else {
    is_scale <- FALSE
  }

  # If we have scales, only keep the end points and update mm
  if (any(is_scale)) {
    values[is_scale] <- lapply(values[is_scale], function(x) {
      x <- na.omit(unique(x))
      s <- stri_replace(x, "$1", regex = "^[0-9]{1,2}\\s*=?\\s*([[:alpha:]]*)")
      s[s != ""]
    })

    values[is_scale] <- vapply(values[is_scale], stri_c, collapse = "\n", character(1))
    type[is_scale] <- "scale"
  }

  # Update factor values
  only_fact <- is_factor & !is_scale
  values[only_fact] <- vapply(values[only_fact], stri_c, collapse = "\n", character(1))
  
  # Create the data.frame
  mm <- data_frame("latent" = NA, "manifest" = manifest, 
                   "question" = question, "type" = type, "values" = unlist(values))
  
  # Return
  mm
  
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
  names(mm) <- stri_trans_tolower(names(mm))
  
  w_name <- max(stri_length(mm$manifest), na.rm = TRUE) + 1
  w_reserved <- 8 + w_name + 3 # $ and three spaces as separation
  w_available <- width - w_reserved - 5 # in case of large font
  
  # Type
  mm$type <- vapply(mm$type, function(x) {
    x <- ifelse(is.na(x), "miss", x)
    switch(x, character = "(char)", factor = "(fctr)", numeric = "(num)", Date = "(date)",
           scale = "(scale)", integer = "(int)", "(????)") }, character(1))
  
  mm$type <- ifelse(!is.na(mm$latent), stri_c(mm$type, "*"), mm$type)
  
  # Clean manifest/type
  mm$manifest <- vapply(mm$manifest, stri_pad_right, width = w_name, character(1))
  mm$type <- vapply(mm$type, stri_pad_right, width = 8, character(1))
  
  # Shorten question-text to the remaining width
  mm$question <- vapply(mm$question, stri_sub, to = w_available-2, character(1))
  
  # Print
  for (i in 1:nrow(mm)) {
    cat("$", mm$manifest[i], mm$type[i], " ", mm$question[i], sep = "", collapse = "\n")
  }
  
  cat("Note: Associations (including latents) are marked with *\n")
  
}
