#' Set marketshares for entities
#'
#' Use this function to set marketshares for entities in Q1.
#' 
#' @param survey A survey object.
#' @param ... Marketshares for individual entities. Of the format \code{name = marketshare}.
#' @param ms Optional vector with the marketshares to use. Either
#' a named vector, or one of the same length as the number of entities in the data.
#' @author Kristian D. Olsen
#' @export
#' @examples 
#' x %>% set_marketshares("reporttool" = 1)

set_marketshares <- function(survey, ..., ms = NULL) {
  
  # Check class
  if (!inherits(survey, "survey")) {
    stop("Argument 'survey' is not an object with the class 'survey'. See help(survey).", call. = FALSE)
  }
  
  if (!inherits(survey$ents, "survey_ents") || !nrow(survey$ents)) {
    stop("Entities must be added first. See help(add_entities).", call. = FALSE)
  }
  
  # Assign 'ms' if it is valid
  if (!is.null(ms)) {
    nms <- names(ms)
    
    # If it is a named vector
    if (!is.null(nms) && !any(is.na(nms))) {
      missing <- setdiff(nms, survey$ents$entity)
      if (length(missing)) {
        stop("Entities not found\n:", stri_c(missing, collapse = ", "), call. = FALSE)
      } else {
        survey$ents$marketshare[match(nms, survey$ents$entity)] <- ms
      }
      
    # If not, only accept if it is the same length
    } else {
      if (length(ms) == length(survey$ents$entity)) {
        survey$ents$marketshare <- ms
      } else {
        stop("ms must either be a named vector or same length as entities.", call. = FALSE)
      }
    }
  }
  
  # Do manual renaming from named strings
  args <- list(...)
  
  # Return early if there are no additional arguments
  if (is.null(args) || !length(args)) return(survey)
  
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
