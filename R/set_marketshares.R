#' Set marketshares for entities
#'
#' Use this function to set marketshares for entities in Q1.
#' 
#' @param survey A survey object.
#' @param ... Marketshares for individual entities. Of the format \code{name = marketshare}.
#' @param vec Optional character vector with the marketshares to use.
#' @author Kristian D. Olsen
#' @export
#' @examples 
#' x %>% set_marketshares("reporttool" = 1)

set_marketshares <- function(survey, ..., vec = NULL) {
  
  # Check class
  if (!inherits(survey, "survey")) {
    stop("Argument 'survey' is not an object with the class 'survey'. See help(survey).", call. = FALSE)
  }
  
  if (!inherits(survey$ents, "survey_ents") || !nrow(survey$ents)) {
    stop("Entities must be added first. See help(add_entities).", call. = FALSE)
  }
  
  # Assign 'vec' if it is valid
  if (!is.null(vec)) {
    if(!is.character(vec)) {
      stop("vec must be a character vector.", call. = FALSE)
    } else if (length(vec) != length(survey$ents$entity)) {
      stop("vec must be the same length as the number of entities in the data.", call. = FALSE)
    } else if (sum(vec) != 1) {
      stop("vec must sum to 1.", call. = FALSE)
    } else {
      survey$ents$marketshare <- vec
    }
  }
  
  # Do manual renaming from named strings
  args <- list(...)
  
  # Return early if there are no additional arguments
  if (is.null(args) || !length(args)) return()
  
  # Check that all arguments are strings
  is_numeric <- vapply(args, is.numeric, logical(1))
  if (!all(is_numeric)) {
    stop("All input must be numeric (length 1).", call. = FALSE)
  } else {
    args <- setNames(args, names(args))
  }
  
  # Throw an error if arguments do not match entity names
  missing_ents <- setdiff(names(args), survey$ents$entity)
  if (length(missing_ents)) {
    missing_mm <- stri_c(missing_ents, collapse = ", ")
    stop(stri_c("Entities not found in ents:\n", missing_ents), call. = FALSE)
  }
  
  # Update with a for loop for clarity
  for (i in names(args)) {
    survey$ents$marketshare[survey$ents$entity %in% i] <- args[[i]]
  }
  
  # Return
  survey
  
}
