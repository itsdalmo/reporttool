#' Recode entities based on open answers
#'
#' Small function to help in recoding the mainentity column based on open answers.
#'
#' @param survey A survey object.
#' @param other Optional: Name of the column containing open answers for mainentity.
#' @param ... One or more named arguments.
#' @author Kristian D. Olsen
#' @export
#' @examples 
#' x %>% recode_entites(other = "q1a", "Example" = "test")

recode_entities <- function(survey, other = NULL, ...) {
  
  # Check the input
  if (!inherits(survey, "survey")) {
    stop("Argument 'survey' is not an object with the class 'survey'. See help(survey).", call. = FALSE)
  }
  
  if (!inherits(survey$mm, "survey_mm") || !nrow(survey$mm)) {
    stop("The measurement model must be added first. See help(add_mm).", call. = FALSE)
  }
  
  # Mainentity must be specified
  if (!any(stri_detect(survey$mm$latent, regex = "mainentity"))) {
    stop("'mainentity' is not specified in latents for the measurement model. 
         See help(set_association).", call. = FALSE)
  } else {
    mainentity <- filter(survey$mm, stri_trans_tolower(latent) == "mainentity")[["manifest"]]
  }
  
  if (is.null(other)) {
    other <- stri_c(mainentity, "a")
    other <- filter(survey$mm, stri_detect(manifest, regex = stri_c("^", other), case_insensitive = TRUE))[["manifest"]]
  }
  
  # Gather dots
  dots <- list(...)
  entity <- survey$df[[mainentity]]
  others <- survey$df[[other]]
  
  # Check missing
  if (is.character(entity)) {
    missing <- setdiff(names(dots), unique(entity))
  } else if (is.factor(entity)) {
    missing <- setdiff(names(dots), levels(entity))
  } else {
    stop("mainentity column is not a factor or character vector.", call. = FALSE)
  }
  
  # Stop if missing
  if (length(missing)) {
    stop("The following values were not found in the mainentity column:\n", stri_c(missing, collapse = ", "), call. = FALSE)
  }
  
  # For factors, replace with factor values
  if (is.factor(entity)) {
    nms <- match(names(dots), unique(entity))
    val <- setNames(nms, names(dots))
  } else {
    val <- dots
  }
  
  # Lowercase and do the recode
  dots <- lapply(dots, stri_trans_tolower)
  others <- stri_trans_tolower(others)
  
  for (nm in names(dots)) {
    entity[others %in% dots[[nm]]] <- nm
    others[others %in% dots[[nm]]] <- ""
  }
  
  # Assign and return
  survey$df[[other]] <- others
  survey$df[[mainentity]] <- entity
  survey
  
}