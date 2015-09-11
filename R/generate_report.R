#' Generate report
#'
#' Generate one or more reports using the templates in this package.
#' 
#' @param report Path to the report template (.Rmd) for a specific study.
#' @param entity Optional: Name of a specific entity to create a report for.
#' @param data Optional: path to input.xlsx (if NULL, assumes that this file can 
#' be found in working directory.)
#' @return Nothing. A .Rmd file and desired output is created for each entity in
#' the data, or as specified.
#' @author Kristian D. Olsen
#' @export
#' @examples 
#' generate_report("/Internal/Internal_report_2014.Rmd", entity=c("EPSI", "SKI"))
 
generate_report <- function(report=NULL, entity=NULL, data=NULL, type="pdf") {
  
  report <- clean_path(report)
  dir <- dirname(report)
  
  if (!file.exists(report)) {
    stop("File not found:\n", report, call. = FALSE)
  }
  
  # See if data is specified correctly, if not; assumes input.xlsx in same folder
  if (is.null(data)) {
    data <- file.path(dir, "input.xlsx")
  } else {
    data <- clean_path(data)
  }
  
  if (!file.exists(data)) {
    stop("File not found:\n", data, call. = FALSE)
  }

  # Read in the input-file
  input <- get_survey(data)
  
  # Check if entity is specified, if not - use mainentity and generate a report each
  if (is.null(entity)) {
    message("No entity specified, creating report for all (main) entities in:\n", data)
    entity <- unique(input$df$mainentity)
  }
  
  # Convert input to an environment to avoid loading data multiple times
  # when generating more than one report
  input_envir <- list2env(input, parent = environment())
  
  # Read in the report template
  md <- readLines(report, encoding = "UTF-8")
  
  # Replace date with current date, and data with fixed path
  md <- stringi::stri_replace(md, format(Sys.Date(), "%Y"), regex = "REPLACE_DATE")
  md <- stringi::stri_replace(md, data, regex = "REPLACE_DATA")
  
  # Make sure the Markdown and Reports directory exists
  dir.create(file.path(dir, "Reports"), showWarnings = FALSE)
  dir.create(file.path(dir, "Markdown"), showWarnings = FALSE)
  
  # Copy beamer theme files if they do not exist
  if (identical(tolower(type), "pdf")) {
    copy_beamer_theme(file.path(dir, "Markdown"))
  }
  
  # Iterate over the entities and create their individual .Rmd files.
  lapply(entity, generate_rmd, md, dir)
  
  # Generate the wanted report-type
  switch(type,
         pdf = lapply(entity, generate_beamer, dir, input_envir),
         ppt = stop("Not yet supported.\n", call. = FALSE),
         stop("Please use a supported output format.\n", call. = FALSE))
  
  invisible()
  
}

# Report functions -------------------------------------------------------------

generate_rmd <- function(entity, md, dir) {
  
  path <- file(file.path(dir, "Markdown", stri_c(entity, ".Rmd")), encoding = "UTF-8")
  on.exit(close(path), add = TRUE)
  
  writeLines(stringi::stri_replace_all(md, entity, regex = "REPLACE_ENTITY"), path)
  
}

#' Get survey data
#'
#' This function reads in and converts data to a survey. Obviously requires that
#' whatever data is read in can be converted to a survey, which means that the file-type
#' will usually be .xlsx or .Rdata.
#' 
#' @param file Path to the input file
#' @return Either a list with the
#' @author Kristian D. Olsen
#' @export
#' @examples 
#' generate_report("/Internal/Internal_report_2014.Rmd", entity=c("EPSI", "SKI"))

get_survey <- function(file) {
  
  # Validate path
  file <- clean_path(file)
  
  # Read in the data and convert it to class survey
  input <- read_data(file)
  
  # Convert to survey
  input <- survey(input)
  
  # Replace names for mainentity in the data
  me_name <- setNames(stri_trans_tolower(input$mm$manifest[input$mm$latent %in% "mainentity"]), "mainentity")
  input <- set_colnames(input, nms = setNames(names(me_name), me_name))
  input <- lapply(input, function(x, re) { names(x) <- ordered_replace(names(x), re);  x }, me_name)
  
  # And subentities (if they exist)
  if ("subentity" %in% input$mm$latent) {
    se_name <- setNames(stri_trans_tolower(input$mm$manifest[input$mm$latent %in% "subentity"]), "subentity")
    input <- set_colnames(input, nms = setNames(names(se_name), se_name))
    input <- lapply(input, function(x, re) { names(x) <- ordered_replace(names(x), re); x }, se_name)
  }
  
  # Return as factored data
  class(input) <- c("survey", "list")
  factor_data(input)
  
}
