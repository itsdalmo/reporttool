#' Create a survey object
#'
#' This function takes a list as input, replaces names (data = df etc), and checks
#' whether the necessary info is in place to generate a report. If this is the case,
#' the class 'survey' is appended to the object.
#'
#' @param x The list to convert to a survey
#' @author Kristian D. Olsen
#' @note survey objects have a print method.
#' @export
#' @examples 
#' survey <- as.survey(lst)

as.survey <- function(x) UseMethod("as.survey")

as.survey.default <- function(x) {
  stop("as.survey expects a 'list' \n", call. = FALSE)
}

as.survey.list <- function(x) {
  
  # Replace list names with shorthand versions
  item_names <- with(cfg$sheet_names, setNames(long, short))
  names(x) <- ordered_replace(names(x), item_names)
  
  # Lowercase names and make sure the necessary data is present
  if (!all(c("df", "mm", "ents") %in% names(x))) {
    stop("The required data was not supplied, or incorrectly named\n", call. = FALSE)
  }
  
  # Convert measurement model and entities to their appropriate classes
  x$mm <- as.survey_mm(x$mm)
  x$ents <- as.survey_ents(x$ents)
  
  # Make sure latents exist in the data
  latents <- cfg$latent_names
  manifest <- paste0(tolower(x$mm$manifest[tolower(x$mm$latent) %in% latents]), "em")
  
  if (!all(latents %in% names(x$df))) {
    stop("Latents were not found in the data. Make sure you have run prepare_data first\n", call. = FALSE)
  }
  
  # Model scales
  if (!all(manifest %in% names(x$df))) {
    stop("Manifest variables (em) were not found in the data\n", call. = FALSE)
  }
  
  # And weights
  if (!"w" %in% names(x$df)) {
    stop("Weights (w) were not found in the data\n", call. = FALSE)
  }
  
  structure(x, class = append("survey", class(x)))
  
}

as.survey.survey <- function(x) x

#' @rdname as.survey
#' @export
is.survey <- function(x) inherits(x, "survey")

#' @rdname as.survey
#' @method print survey
#' @export
print.survey <- function(lst, width = getOption("width")) {
  
  cat("Object:", quote(lst), "\n")
  
  # Class and dimensions of the objects
  info <- lapply(lst, function(x) {
    classes <- paste("(", class(x)[1], ")", sep = "")
    dimensions <- paste("[", paste(dim(x), collapse = "x"), "]", sep = "")
    paste(classes, dimensions, sep = "")
  })
  
  cat(paste(paste("$", names(lst), sep = ""), info, sep = "\t", collapse = "\n"))
  
}

#' Create a survey_mm object
#'
#' Convert a data.frame to a survey_mm object if the required data is present.
#'
#' @param x A data.frame
#' @author Kristian D. Olsen
#' @note survey_mm objects have a print method.
#' @export
#' @examples 
#' survey$ents <- as.survey_ents(survey$ents)

as.survey_mm <- function(x) UseMethod("as.survey_mm")

as.survey_mm.default <- function(x) {
  stop("as.survey_mm expects a 'data.frame' \n", call. = FALSE)
}

as.survey_mm.data.frame <- function(x) {
  
  # Check that the measurement model contains the correct columns
  if (all(!cfg$req_structure$mm %in% names(x))) {
    stop("The required columns were not found, or incorrectly named\n", call. = FALSE)
  }
  
  # Make sure the minimum specifications are found in the data
  if (!all(cfg$latent_names %in% tolower(x$latent))) {
    stop("Latent variables are not specified in the measurement model\n", call. = FALSE)
  }
  
  # mainentity must be specified
  if (!"mainentity" %in% tolower(x$latent)) {
    stop("The mainentity is not specified in the measurement model\n", call. = FALSE)
  }
  
  structure(x, class = append("survey_mm", class(x)))
  
}

as.survey_mm.survey_mm <- function(x) x

#' @rdname as.survey_mm
#' @export
is.survey_mm <- function(x) inherits(x, "survey_mm")

#' @rdname as.survey_mm
#' @method print survey_mm
#' @export
print.survey_mm <- function(mm, width = getOption("width")) {
  
  cat("Measurement model:\n")
  
  # Lowercase for easier referencing
  names(mm) <- tolower(names(mm))
  
  # Clean manifest to fit viewport
  mm$manifest <- vapply(mm$manifest, function(x) {
    x <- ifelse(nchar(x) > 10, paste0(substr(x, 1, 8), ".."), x)
    x <- paste0(x, paste0(rep(" ", 10-nchar(x)), collapse = "")) },
    character(1))
  
  # Type
  mm$type <- vapply(mm$type, function(x) {
    switch(x, character = " (char)", 
           factor = " (fctr)", 
           numeric = " (nmbr)", 
           scale = "(scale)",
           integer = "  (int)") }, character(1))
  
  mm$type <- ifelse(!is.na(mm$latent), paste0(mm$type, "*"), paste0(mm$type, " "))
  
  # Get max string length for questions
  max_length <- width - 8 - 11 - 5 #$, manifest, \t, (type), latent
  
  # Collate
  info <- paste(paste0("$", mm$manifest), mm$type, sep = " ")
  info <- paste(info, substr(mm$question, 1, max_length), collapse = "\n")
  
  cat(info)
  cat("\nNote: Latents are marked with *\n")
  
}

#' Create a survey_ents object
#'
#' Convert a data.frame to a survey_ents object if the required data is present.
#'
#' @param x A data.frame
#' @author Kristian D. Olsen
#' @note survey_ents objects have a print method.
#' @export
#' @examples 
#' survey$ents <- as.survey_ents(survey$ents)

as.survey_ents <- function(x) UseMethod("as.survey_ents")

as.survey_ents.default <- function(x) {
  stop("as.survey_ents expects a 'data.frame' \n", call. = FALSE)
}

as.survey_ents.data.frame <- function(x) {
  
  # Lowercase names and make sure the necessary data is present
  if (all(!cfg$req_structure$ents %in% names(x))) {
    stop("The required columns were not found, or incorrectly named\n", call. = FALSE)
  }
  
  # Check if entities have been populated
  if (nrow(x) == 0L) {
    stop("No data found in ents/entities, use prepare_data before converting to survey", call. = FALSE)
  }
  
  structure(x, class = append("survey_ents", class(x)))
  
}

as.survey_ents.survey_ents <- function(x) x

#' @rdname as.survey_ents
#' @export
is.survey_ents <- function(x) inherits(x, "survey_ents")

#' @rdname as.survey_ents
#' @method print survey_ents
#' @export
print.survey_ents <- function(ents, width = getOption("width")) {
  
  # Lowercase for easier referencing
  names(ents) <- tolower(names(ents))
  
  # Extract only entity, observations and marketshare
  info <- ents[c("entity", "n", "marketshare")]
  substr(names(info), 1, 1) <- toupper(substr(names(info), 1, 1))
  
  # Add the total and format marketshare as a percentage
  info[nrow(info)+1,] <- c("Total", sum(info$N), sum(as.numeric(info$Marketshare)))
  info$Marketshare <- paste(as.numeric(info$Marketshare)*100, "%")
  
  print.data.frame(info, right = TRUE, row.names = FALSE)
  
}