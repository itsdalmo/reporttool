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
add_weights.survey <- function(survey, ...) {
  
  # Measurement model must be added first
  if (!inherits(survey$mm, "survey_mm") || !nrow(survey$mm)) {
    stop("The measurement model must be added first. See help(add_mm).", call. = FALSE)
  }
  
  # Entities must be added first
  if (!inherits(survey$ents, "survey_ents") || !nrow(survey$ents)) {
    stop("Entities must be added first. See help(add_entities).", call. = FALSE)
  }
  
  # Get the mainentity variable from data
  mainentity <- survey$mm$manifest[stri_trans_tolower(survey$mm$latent) == "mainentity"]
  mainentity <- mainentity[!is.na(mainentity)]
  
  # Add weights (w) to data.frame
  survey$df$w <- calculate_weights(survey$df[[mainentity]], survey$ents)
  
  # Return the survey
  survey
  
}

#' @rdname add_weights
#' @export
add_weights.character <- function(mainentity, entities) {
  
  # Entities must be added first
  if (!inherits(entities, "survey_ents") || !nrow(entities)) {
    stop("Entities must be a survey_ents object. See help(add_entities).", call. = FALSE)
  }
  
  # Return a vector of weights
  calculate_weights(mainentity, entities)
  
}

#' @rdname add_weights
#' @export
add_weights.factor <- function(mainentity, entities) {
  
  # Entities must be added first
  if (!inherits(entities, "survey_ents") || !nrow(entities)) {
    stop("Entities must be a survey_ents object. See help(add_entities).", call. = FALSE)
  }
  
  # Return a vector of weights
  calculate_weights(as.character(mainentity), entities)
  
}

# Utilities --------------------------------------------------------------------

calculate_weights <- function(mainentity, entities) {
  
  # Generate new information on entities
  # with updated count for n (observations)
  obs <- new_entities(mainentity)
  
  # Get existing marketshares
  obs$marketshare <- as.numeric(entities$marketshare[match(obs$entity, entities$entity)])
  
  # Calculate weight
  obs$w <- (obs$marketshare * sum(obs$n))/obs$n
  
  # Return a vector with corresponding weights
  obs$w[match(mainentity, obs$entity)]
  
}