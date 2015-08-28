#' Set translations for the survey
#'
#' Use this function to specify translations before generating reports. Use the
#' argument \code{language} to use a prespecified language (currently only Norwegian) 
#' for the most common translations. 
#' 
#' @param survey A survey object.
#' @param ... The translations you would like to set.
#' @param language Default translations for a given language (currently Norwegian).
#' @author Kristian D. Olsen
#' @export

set_translation <- function(survey, ..., language = "norwegian") {
  
  # Check class
  if (!inherits(survey, "survey")) {
    stop("Argument 'survey' is not an object with the class 'survey'. See help(survey).", call. = FALSE)
  }
  
  # Gather dots
  args <- list(...)
  
  # Always create a new scaffold
  tr <- new_scaffold(default$structure$tr, size = length(default$translation$required))
  tr$original <- default$translation$required
  
  # Add a default language if wanted
  if (!is.null(language)) {
    if (!stri_trans_tolower(language) %in% names(default$translation)) stop("Language not found.", call. = FALSE)
    tr$replacement <- default$translation[[language]]
    #survey <- set_config(survey, language = language)
  }
  
  # Merge with existing information
  if (nrow(survey$tr)) {
    nms <- intersect(tr$original, survey$tr$original)
    tr$replacement[tr$original %in% nms] <- survey$tr$replacement[survey$tr$original %in% nms]
  }
  
  # Assign the replacement
  survey$tr <- structure(tr, class = c("survey_tr", "data.frame"))
  
  # Check that all arguments are character vectors
  is_string <- vapply(args, is.string, logical(1))
  if (!all(is_string)) {
    stop("All input must be named strings.", call. = FALSE)
  }
  
  # Throw an error if arguments do not match the manifest
  missing <- setdiff(names(args), survey$tr$original)
  if (length(missing)) {
    missing <- stri_c(missing, collapse = ", ")
    warning(stri_c("Values not found in translations:\n", missing), call. = FALSE)
  }
  
  # Update with a loop for clarity
  for (i in names(args)[!names(args) %in% missing]) {
    survey$tr$replacement[survey$tr$original %in% i] <- args[[i]]
  }
  
  # Return
  survey
  
}