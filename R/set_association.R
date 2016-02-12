#' Set latent associations for survey
#'
#' Use this function to specify latent associations and other variables 
#' of special significance in the data. Beside latents, typical values are:
#' \code{mainentity subentity complaint oa_positive oa_negative 
#' statement_categorical statement_metric} etc. You can also (auto) look for common
#' associations.
#' 
#' @param survey A survey object.
#' @param ... The latent associations you would like to set. See examples.
#' @param common If set to \code{TRUE}, the function sets common associations
#' automatically. Typically, this will succesfully mark latents and mainentity.
#' @author Kristian D. Olsen
#' @note Use \code{none} to remove latent association. See examples.
#' @export
#' @examples 
#' x <- survey(data.frame("test" = 1, stringsAsFactors = FALSE))
#' x <- x %>% add_mm() %>% set_association(mainentity = "q1", complaint = "q17") 
#' x %>% add_entities() %>% add_weights()
#' x %>% set_association(none = "q1") # to remove

set_association <- function(survey, ..., common = FALSE) {
  
  # Check class
  if (!inherits(survey, "survey")) {
    stop("Argument 'survey' is not an object with the class 'survey'. See help(survey).", call. = FALSE)
  }
  
  # Measurement model must be added first
  if (!inherits(survey$mm, "survey_mm") || !nrow(survey$mm)) {
    stop("The measurement model must be added first. See help(add_mm).", call. = FALSE)
  }
  
  # Gather dots
  args <- list(...)
  
  # If no latents are specified, use default associations.
  if (isTRUE(common)) {
    survey$mm <- common_latents(survey$mm)
  }
  
  # Check that all arguments are character vectors
  is_character <- vapply(args, is.character, logical(1))
  if (!all(is_character)) {
    stop("All input must be named character vectors.", call. = FALSE)
  }
  
  # Throw an error if arguments do not match the manifest
  missing <- setdiff(unlist(args), survey$mm$manifest)
  if (length(missing)) {
    stop("Variables not found in the measurement model:\n", conjunct_string(missing), call. = FALSE)
  }
  
  # Update with a loop for clarity
  for (i in names(args)) {
    # Argument named "remove" can be used to delete latent associations
    latent <- if (i == "none") NA else i
    survey$mm$latent[survey$mm$manifest %in% args[[i]]] <- latent
  }
  
  # Return
  survey
  
}

#' @rdname set_association
#' @export
get_association <- function(srv, association) {
  
  # Measurement model must be added first
  if (!is.survey_mm(srv$mm) || !nrow(srv$mm)) {
    stop("The measurement model must be added first. See help(add_mm).", call. = FALSE)
  }
  
  association <- stri_trans_tolower(association)
  latent <- stri_trans_tolower(srv$mm$latent)
  
  srv$mm$manifest[match_all(association, latent)]
  
}

# Utilities --------------------------------------------------------------------

common_latents <- function(mm) {
  
  # Add latents if variables have standard names
  for (i in names(default$associations)) {
    
    vars <- default$associations[[i]]
    
    # Match greedily if latent only has one var associated
    if (length(vars) == 1L) {
      match <- suppressWarnings(stri_c("^", vars, "[[:alpha:]]*$"))
    } else {
      match <- suppressWarnings(stri_c("^", vars, "$", collapse = "|"))
    }
    
    # Look for matches
    var_match <- stri_detect(mm$manifest, regex = match, case_insensitive = TRUE)
    end_match <- !stri_detect(mm$manifest, regex = "em$", case_insensitive = TRUE)
    
    # Set latent association
    mm$latent[var_match & end_match] <- i
  }
  
  # Suggest q1 as mainentity if it exists
  if (any(stri_trans_tolower(mm$manifest) == "q1")) {
    mm$latent[stri_trans_tolower(mm$manifest) == "q1"] <- "mainentity"
  }
  
  # Return
  mm
  
}
