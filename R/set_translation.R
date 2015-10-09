#' Set translations for the survey
#'
#' Use this function to specify translations before generating reports. Use the
#' argument \code{language} to use a prespecified language (currently only Norwegian) 
#' for the most common translations. 
#' 
#' @param srv A survey object.
#' @param ... The translations you would like to set.
#' @param language Default translations for a given language (currently Norwegian).
#' @author Kristian D. Olsen
#' @export

set_translation <- function(srv, ..., language = "norwegian") {
  
  # Check class
  if (!is.survey(srv)) {
    stop("Argument 'srv' is not an object with the class 'survey'. See help(survey).", call. = FALSE)
  }
  
  # Gather dots
  args <- list(...)
  language <- stri_trans_tolower(language)
  
  # Always create a new scaffold
  tr <- new_scaffold(default$structure$tr, size = length(default$translation$required))
  tr$original <- default$translation$required
  
  # Add a default language if wanted
  if (!is.null(language)) {
    if (!language %in% names(default$translation)) stop("Language not found.", call. = FALSE)
    tr$replacement <- stri_encode(default$translation[[language]], from = "UTF-8")
  }
  
  # Merge with existing information
  if (nrow(srv$tr)) {
    nms <- intersect(tr$original, srv$tr$original)
    tr$replacement[tr$original %in% nms] <- srv$tr$replacement[srv$tr$original %in% nms]
  }
  
  # Assign the replacement
  srv$tr <- as.survey_tr(tr)
  
  # Check that all arguments are character vectors
  is_string <- vapply(args, is.string, logical(1))
  if (!all(is_string)) {
    stop("All input must be named strings.", call. = FALSE)
  }
  
  # Throw an error if arguments do not match the manifest
  missing <- setdiff(names(args), srv$tr$original)
  if (length(missing)) {
    missing <- stri_c(missing, collapse = ", ")
    warning(stri_c("Values not found in translations:\n", missing), call. = FALSE)
  }
  
  # Update with a loop for clarity
  for (i in names(args)[!names(args) %in% missing]) {
    srv$tr$replacement[srv$tr$original %in% i] <- args[[i]]
  }
  
  # Note that translations have been set
  srv$cfg$value[srv$cfg$config %in% "language"] <- language
  
  # Return
  srv
  
}

#' @rdname set_translation
#' @export
get_translation <- function(srv, trans) {
  
  trans <- stri_trans_tolower(trans)
  
  # Measurement model must be added first
  if (!is.survey_tr(srv$tr) || !nrow(srv$tr)) {
    stop("Translations must be set first. See help(set_translation).", call. = FALSE)
  }

  filter(srv$tr, stri_trans_tolower(original) %in% trans)[["replacement"]]
  
}

# Utilities --------------------------------------------------------------------

is.survey_tr <- function(x) inherits(x, "survey_tr")
as.survey_tr <- function(x) structure(x, class = c("survey_tr", "data.frame"))

use_latent_translation <- function(srv) {
  
  # Check class
  if (!is.survey(srv)) {
    stop("Argument 'srv' is not an object with the class 'survey'. See help(survey).", call. = FALSE)
  }
  
  # Set latent translations as "question"
  cols <- stri_trans_tolower(srv$mm$manifest) %in% default$latents
  srv$mm$question[cols] <- get_translation(srv, default$latents)
  
  # Return
  srv
  
}
