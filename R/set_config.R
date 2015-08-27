#' Set configuration for the survey
#'
#' Use this function to specify config for the survey. Use it without arguments
#' to add default values.
#' 
#' @param survey A survey object.
#' @param ... The config you would like to set for the study.
#' @author Kristian D. Olsen
#' @export

set_config <- function(survey, ...) {
  
  # Check class
  if (!inherits(survey, "survey")) {
    stop("Argument 'survey' is not an object with the class 'survey'. See help(survey).", call. = FALSE)
  }
  
  # Gather dots
  args <- list(...)
  args <- vapply(args, as.character, character(1))
  
  # Always create a new scaffold and add default settings
  cfg <- new_scaffold(default$structure$cfg, size = length(default$config$setting))
  cfg$config <- default$config$setting
  cfg$value <- default$config$value
  
  # Merge with existing information
  if (nrow(survey$cfg)) {
    nms <- intersect(cfg$config, survey$cfg$config)
    cfg$value[cfg$config %in% nms] <- survey$cfg$value[survey$cfg$config %in% nms]
  }
  
  # Assign the replacement
  survey$cfg <- structure(cfg, class = c("survey_cfg", "data.frame"))
  
  # Check that all arguments are character vectors
  is_string <- vapply(args, is.string, logical(1))
  if (!all(is_string)) {
    stop("All input must be named strings.", call. = FALSE)
  }
  
  # Throw an error if arguments do not match the manifest
  missing <- setdiff(names(args), survey$cfg$config)
  if (length(missing)) {
    missing <- stri_c(missing, collapse = ", ")
    warning(stri_c("Values not found in config:\n", missing), call. = FALSE)
  }
  
  # Update with a loop for clarity
  for (i in names(args)[!names(args) %in% missing]) {
    survey$cfg$value[survey$cfg$config %in% i] <- args[[i]]
  }
  
  # Return
  survey
  
}