#' Set marketshares for entities
#'
#' Use this function to set marketshares for entities in Q1.
#' 
#' @param srv A survey object.
#' @param ... Marketshares for individual entities. Of the format \code{name = marketshare}.
#' @param ms Optional vector with the marketshares to use. Either
#' a named vector, or one of the same length as the number of entities in the data.
#' @author Kristian D. Olsen
#' @export
#' @examples 
#' x %>% set_marketshares("reporttool" = 1)

set_marketshare <- function(srv, ..., ms = NULL) {
  
  # Check class
  if (!is.survey(srv)) {
    stop("Argument 'srv' is not an object with the class 'survey'. See help(survey).", call. = FALSE)
  }
  
  if (!is.survey_ents(srv$ents) || !nrow(srv$ents)) {
    stop("Entities must be added first. See help(add_entities).", call. = FALSE)
  }
  
  # Assign 'ms' if it is valid
  if (!is.null(ms)) {
    nms <- names(ms)
    
    # If it is a named vector
    if (!is.null(nms) && !any(is.na(nms))) {
      missing <- setdiff(nms, srv$ents$entity)
      if (length(missing)) {
        stop("Entities not found\n:", conjunct_string(missing), call. = FALSE)
      } else {
        srv$ents$marketshare[match(nms, srv$ents$entity)] <- ms
      }
      
    # If not, only accept if it is the same length
    } else {
      if (length(ms) == length(srv$ents$entity)) {
        srv$ents$marketshare <- ms
      } else {
        stop("ms must either be a named vector or same length as entities.", call. = FALSE)
      }
    }
    # Note that marketshares have been set
    srv$cfg$value[srv$cfg$config %in% "marketshares"] <- "yes"
    
  }
  
  # Do manual renaming from named strings
  args <- list(...)
  
  # Return early if there are no additional arguments
  if (is.null(args) || !length(args)) return(srv)
  
  # Check that all arguments are strings
  is_numeric <- vapply(args, is.numeric, logical(1))
  if (!all(is_numeric)) {
    stop("All input must be numeric (length 1).", call. = FALSE)
  } else {
    args <- setNames(args, names(args))
  }
  
  # Throw an error if arguments do not match entity names
  missing_ents <- setdiff(names(args), srv$ents$entity)
  if (length(missing_ents)) {
    stop(stri_c("Entities not found:\n", conjunct_string(missing_ents)), call. = FALSE)
  }
  
  # Update with a for loop for clarity
  for (i in names(args)) {
    srv$ents$marketshare[srv$ents$entity %in% i] <- args[[i]]
  }
  
  # Note that marketshares have been set
  srv$cfg$value[srv$cfg$config %in% "marketshares"] <- "yes"
  
  # Return
  srv
  
}
