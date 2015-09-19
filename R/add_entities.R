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
#' @param srv A survey object.
#' @param entity Optional: Specify a \code{data.frame} which contains the summary
#' and information for entities.
#' @author Kristian D. Olsen
#' @note This function requires that mainentity is specified in the measurement model.
#' See \code{set_association} for more information.
#' @export
#' @examples 
#' x <- survey(data.frame("test" = 1, stringsAsFactors = FALSE))
#' x %>% add_mm() %>% add_entities()

add_entities <- function(srv, entities = NULL) {
  
  # Check the input
  if (!is.survey(srv)) {
    stop("Argument 'survey' is not an object with the class 'survey'. See help(survey).", call. = FALSE)
  }
  
  # Measurement model must be added first
  if (!is.survey_mm(srv$mm) || !nrow(srv$mm)) {
    stop("The measurement model must be added first. See help(add_mm).", call. = FALSE)
  }
  
  # Mainentity must be specified in latents
  if (!any(stri_detect(srv$mm$latent, regex = "mainentity"), na.rm = TRUE)) {
    stop("'mainentity' is not specified in latents for the measurement model. See help(set_association).", call. = FALSE)
  } else {
    mainentity <- filter(srv$mm, stri_trans_tolower(latent) == "mainentity")[["manifest"]]
  }
  
  # If more than one mainentity is specified, stop and revise
  if (length(mainentity) > 1L) {
    stop("More than one 'mainentity' found. Please revise measurement model.", call. = FALSE)
  }
  
  # Check mainentities
  if (all(is.na(srv$df[[mainentity]]))) {
    stop("No observations found in mainentity column ", stri_c("(", mainentity, ")"), ".", call. = FALSE)
  } else if (any(is.na(srv$df[[mainentity]]))) {
    warning("Removed rows in data where mainentity was NA.", call. = FALSE)
    filter_call <- lazyeval::interp(quote(!is.na(var)), var = as.name(mainentity))
    srv <- filter_(srv, .dots = filter_call)
  } 
  
  # Generate a new summary if none is given
  if (is.null(entities)) {
    cutoff <- as.numeric(filter(srv$cfg, config == "cutoff")[["value"]])
    cutoff <- if (!length(cutoff) || is.na(cutoff)) NULL else cutoff
    entities <- new_entities(srv$df, mainentity, cutoff)
  }
  
  # Warn and replace if entities contains existing data
  if (nrow(srv$ents)) {
    warning("Entities have been replaced.", call. = FALSE)
    srv$ents <- new_scaffold(default$structure$ents)
  }
  
  # Replace entities in the survey and set class
  srv$ents <- merge_with_scaffold(srv$ents, entities)
  srv$ents <- as.survey_ents(srv$ents)
  
  # Return
  srv
  
}

# Utilities --------------------------------------------------------------------

is.survey_ents <- function(x) inherits(x, "survey_ents")
as.survey_ents <- function(x) structure(x, class = c("survey_ents", "data.frame"))

new_entities <- function(df, mainentity, cutoff = NULL) {
  
  # All observations
  entities <- data_frame(entity = as.character(df[[mainentity]]))
  entities <- summarise(group_by(entities, entity), n = n())
  
  # Valid observations
  if ("percent_missing" %in% names(df) && !is.null(cutoff)) {
    valid <- filter(df, percent_missing <= cutoff)
    valid <- data_frame(entity = as.character(valid[[mainentity]]))
    valid <- summarise(group_by(valid, entity), valid = n())
    entities <- left_join(entities, valid, by = c("entity" = "entity"))
  } else {
    warning("Missing percentage and/or cutoff was not found. See help(prepare_data).", call. = FALSE)
    entities <- mutate(entities, valid = n)
  }
  
  entities$valid[is.na(entities$valid)] <- 0
  entities <- mutate(entities, marketshare = valid/sum(valid))
  
  # Return
  entities
  
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
  ents <- select(ents, entity, n, valid, marketshare)
  ents <- mutate_each(ents, funs(as.numeric(.)), n, valid, marketshare)
  tot <- suppressWarnings(summarise_each(ents, funs(sum(as.numeric(.)))))
  tot <- bind_cols(data_frame("entity" = "Total*"), tot)
  ents <- bind_rows(ents, tot)
  
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
