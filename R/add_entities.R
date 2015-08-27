#' Add a summary of entities to a survey
#'
#' Third step in creating a \code{survey} that can be used with \code{generate_report}.
#' The function accepts a \code{data.frame} as input, but columns must be correctly named
#' or be dropped (see description below). If no data is provided \code{(entities = NULL)}, 
#' \code{add_entities} will generate a suggestion based on the data.
#' 
#' @section Structure/naming convention for \code{ents}:
#' 
#' \describe{
#' 
#'    \item{\code{entity}}{List of names for \code{mainentity} found in the data.}
#' 
#'    \item{\code{n}}{The number of rows/observations per entity.}  
#' 
#'    \item{\code{marketshare}}{The marketshare associated with each entity. 
#'    This is used by \code{add_weigts} when calculating weights.} 
#'
#'    \item{\code{other}}{Specifies whether the entity should be collected among
#'    "other" entities. I.e., when it is too small to publish and should be 
#'    excluded from reports.}
#'    
#' }  
#'
#' A structure and naming convention is also required for the survey itself, see
#' \code{help(survey) for information.}
#'
#' @param survey A survey object.
#' @param entity Optional: Specify a \code{data.frame} which contains the summary
#' and information for entities.
#' @author Kristian D. Olsen
#' @note This function requires that mainentity is specified in the measurement model.
#' See \code{set_association} for more information.
#' @export
#' @examples 
#' x <- survey(data.frame("test" = 1, stringsAsFactors = FALSE))
#' x %>% add_mm() %>% add_entities()

add_entities <- function(survey, entities = NULL) {
  
  # Check the input
  if (!inherits(survey, "survey")) {
    stop("Argument 'survey' is not an object with the class 'survey'. See help(survey).", call. = FALSE)
  }
  
  # Measurement model must be added first
  if (!inherits(survey$mm, "survey_mm") || !nrow(survey$mm)) {
    stop("The measurement model must be added first. See help(add_mm).", call. = FALSE)
  }
  
  # Mainentity must be specified in latents
  if (!any(stri_detect(survey$mm$latent, regex = "mainentity"))) {
    stop("'mainentity' is not specified in latents for the measurement model. See help(set_association).", call. = FALSE)
  } else {
    mainentity <- survey$mm$manifest[stri_trans_tolower(survey$mm$latent) == "mainentity"]
    mainentity <- mainentity[!is.na(mainentity)]
  }
  
  # If more than one mainentity is specified, stop and revise
  if (length(mainentity) > 1L) {
    stop("More than one 'mainentity' found. Please revise measurement model.", call. = FALSE)
  }
  
  # Generate the needed data from the mainentity vector
  if (is.null(entities)) {
    entities <- new_entities(survey$df[[mainentity]])
  }
  
  # Warn and replace if entities contains existing data
  if (nrow(survey$ents)) {
    warning("Entities have been replaced.", call. = FALSE)
    survey$ents <- new_scaffold(default$structure$ents)
  }
  
  # Replace entities in the survey and set class
  survey$ents <- merge_with_scaffold(survey$ents, entities)
  class(survey$ents) <- c("survey_ents", "data.frame")
  
  # Return
  survey
  
}

# Methods --------------------------------------------------------------------

#' @rdname add_entities
#' @method print survey_ents
#' @export
print.survey_ents <- function(ents, width = getOption("width")) {
  
  cat("Entities\n")
  
  # Return early if it is empty
  if (is.null(ents)) {
    cat("Not specified (NULL). See help(add_entities)\n"); return()
  } 
  
  # Print the number of observations
  n <- nrow(ents); cat("Observations: ", n, "\n\n", sep = ""); if (!n) return()
  
  # Return early if it contains no columnnames (obs = 0)
  if (!ncol(ents)) {
    cat("No columns\n"); return()
  } 
  
  # Extract only entity, observations, valid and marketshare
  ents <- ents[c("entity", "n", "valid", "marketshare")]

  # Add the total and format marketshare as a percentage
  ents[nrow(ents)+1,] <- c("Total*", sum(as.numeric(ents$n)), 
                           sum(as.numeric(ents$valid)), 
                           sum(as.numeric(ents$marketshare)))
  
  # Format the strings
  w_name <- max(stri_length(ents$entity), na.rm = TRUE) + 4
  w_n <- max(stri_length(ents$n), na.rm = TRUE) + 4
  
  ents$entity <- vapply(ents$entity, stri_pad_right, width = w_name, character(1))
  ents$n <- vapply(ents$n, stri_pad_right, width = w_n, character(1))
  
  ents$marketshare <- sprintf("%.2f%%", as.numeric(ents$marketshare)*100)
  ents$valid <- sprintf("%.0f%%", (as.numeric(ents$valid)/as.numeric(ents$n))*100)
  ents$valid <- vapply(ents$valid, stri_pad_right, width = 9, character(1))
  
  # Print headers for the table
  cat(stri_pad_right("Entity", width = w_name), 
      stri_pad_right("Obs", width = w_n),
      stri_pad_right("Valid", width = 9),
      "Marketshare/Weight\n", sep = "")
  
  # Print results per entity
  for (i in 1:nrow(ents)) {
    cat(ents$entity[i], ents$n[i], ents$valid[i], ents$marketshare[i], sep = "", collapse = "\n")
  }
  
}

# Utilities --------------------------------------------------------------------

new_entities <- function(mainentity) {
  
  mainentity <- as.character(mainentity)
  
  # Gather the information on entities
  entities <- tapply(mainentity, mainentity, 'length')
  entities <- data.frame("entity" = names(entities), "n" = unname(entities), "valid" = unname(entities), stringsAsFactors = FALSE)
  entities$other <- "No" # Assumption. Must be manually specified after.
  
  # Add marketshare
  entities$marketshare <- sum(entities$n)
  entities$marketshare <- entities$n/entities$marketshare
  
  # Return
  entities
  
}