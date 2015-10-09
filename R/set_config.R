#' Set configuration for the survey
#'
#' Use this function to specify config for the survey. Use it without arguments
#' to add default values.
#' 
#' @param srv A survey object.
#' @param ... The config you would like to set for the study.
#' @author Kristian D. Olsen
#' @export

set_config <- function(srv, ...) {
  
  # Check class
  if (!is.survey(srv)) {
    stop("Argument 'srv' is not an object with the class 'survey'. See help(survey).", call. = FALSE)
  }
  
  # Gather dots
  args <- list(...)
  args <- vapply(args, as.character, character(1))
  
  # Always create a new scaffold and add default settings
  cfg <- new_scaffold(default$structure$cfg, size = length(default$config$setting))
  cfg$config <- default$config$setting
  cfg$value <- default$config$value
  
  # Merge with existing information
  if (nrow(srv$cfg)) {
    nms <- intersect(cfg$config, srv$cfg$config)
    cfg$value[cfg$config %in% nms] <- srv$cfg$value[srv$cfg$config %in% nms]
  }
  
  # Assign the replacement
  srv$cfg <- as.survey_cfg(cfg)
  
  # Check that all arguments are character vectors
  is_string <- vapply(args, is.string, logical(1))
  if (!all(is_string)) {
    stop("All input must be named strings.", call. = FALSE)
  }
  
  # Throw an error if arguments do not match the manifest
  missing <- setdiff(names(args), srv$cfg$config)
  if (length(missing)) {
    missing <- stri_c(missing, collapse = ", ")
    warning("Values not found in config:\n", missing, call. = FALSE)
  }
  
  # Give warning when modifying fields that are updated by other function
  updated <- intersect(names(args), c("reporttool", "language", "cutoff", "latents", "marketshares"))
  if (length(updated)) {
    updated <- stri_c(updated, collapse = ", ")
    stop("The following fields should not be set manually:\n", updated, call. = FALSE)
  }
  
  # Update with a loop for clarity
  for (i in names(args)[!names(args) %in% missing]) {
    srv$cfg$value[srv$cfg$config %in% i] <- args[[i]]
  }
  
  # Return
  srv
  
}

#' @rdname set_config
#' @export
get_config <- function(srv, cfg) {
  
  cfg <- stri_trans_tolower(cfg)
  
  # Measurement model must be added first
  if (!is.survey_cfg(srv$cfg) || !nrow(srv$cfg)) {
    stop("The config must be added first. See help(set_config).", call. = FALSE)
  }
  
  filter(srv$cfg, stri_trans_tolower(config) %in% cfg)[["value"]]
  
}

# Utilities --------------------------------------------------------------------

is.survey_cfg <- function(x) inherits(x, "survey_cfg")
as.survey_cfg <- function(x) structure(x, class = c("survey_cfg", "data.frame"))
