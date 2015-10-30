#' Add weights based on marketshares
#'
#' This function calculates weights for each entity based on their expected
#' marketshares and number of observations. If a \code{survey} is passed to the function, 
#' a new column \code{w} is added to the \code{df} which contains a weight for all
#' the obserations. If a vector of types \code{character} or \code{factor} is
#' passed to the function, and the entities/ents from a survey, a \code{numeric}
#' vector is returned instead. 
#' 
#' @param x A survey object, or vector of type \code{character} or \code{facotor}.
#' @param entities Optional: If a vector is passed to \code{x}, the entities/ents
#' from a survey object must be passed along with it.
#' @author Kristian D. Olsen
#' @note See \code{add_entities} for information on the entities object.
#' @export
#' @examples 
#' x <- survey(data.frame("test" = 1, stringsAsFactors = FALSE))
#' x <- x %>% add_mm() %>% add_entities() %>% add_weights()
#' x %>% mutate(w = add_weights(q1, x$ents)) # i.e. create weights for subsets

add_weights <- function(x, entities = NULL) UseMethod("add_weights")

#' @rdname add_weights
#' @export
add_weights.survey <- function(srv) {
  
  # Get the mainentity variable from data
  mainentity <- get_association(srv, "mainentity")
  mainentity <- as.character(srv$df[[mainentity]])
  marketshares <- get_marketshare(srv)
  
  if (!all(unique(mainentity) %in% names(marketshares))) {
   stop("One or more entities do not have an associated marketshare.", call. = FALSE) 
  } else if (!all(names(marketshares) %in% unique(mainentity))) {
    stop("Marketshares are set for entities that do not exist in the data.", call. = FALSE) 
  }
  
  # Add weights (w) to data.frame
  srv$df$w <- calculate_weights(mainentity, marketshares)
  
  # Return the survey
  srv
  
}

#' @rdname add_weights
#' @export
add_weights.character <- function(x, ms) {
  calculate_weights(x, ms)
}

#' @rdname add_weights
#' @export
add_weights.factor <- function(x, ms) {
  calculate_weights(as.character(x), ms)
}

# Utilities --------------------------------------------------------------------

calculate_weights <- function(x, ms) {
  
  # Check input
  if (is.null(names(ms)) || !is.numeric(ms)) {
    stop("Marketshares must be a named numeric vector.")
  } 
  
  # Get count
  count <- tapply(x, x, length)
  share <- ms[match(names(count), names(ms), nomatch = NA_integer_)]
  
  # Match count with marketshares
  if (any(is.na(names(share)))) {
    stop("The shares do not correspond to the observations (x).")
  } else if (!sum(share) == 1L) {
    stop("Marketshares do not sum to one.")
  }
  
  # Calculate the new weight
  wts <- (share * sum(count)) / count
  names(wts) <- names(count)
  
  # Return a weight of same length as mainentity
  as.numeric(wts[match(x, names(wts), nomatch = NA_integer_)])

}