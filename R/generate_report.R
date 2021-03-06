#' Generate report
#'
#' Generate one or more reports using the templates in this package.
#' 
#' @param srv A survey object containing the data that is being reported.
#' @param report Path to the report template (.Rmd) for the specific study.
#' @param entity Optional: Name of a specific entity to create a report for.
#' @param type The type of report. Either PPT or PDF.
#' @return Nothing. A .Rmd file and desired output is created for each entity in
#' the data, or as specified.
#' @author Kristian D. Olsen
#' @export
#' @examples 
#' generate_report(x, "/Internal/Internal_report_2014.Rmd", entity=c("EPSI", "SKI"))
 
generate_report <- function(srv, report=NULL, entity=NULL, type="pdf") {
  
  # Check the input
  if (!is.survey(srv)) {
    stop("Argument 'srv' is not an object with the class 'survey'. See help(survey).", call. = FALSE)
  }
  
  # Get report path
  report <- clean_path(report)
  dir <- dirname(report)
  
  if (!file.exists(report)) {
    stop("File not found:\n", report, call. = FALSE)
  }
  
  # Rename and prepare for reporting
  # Mainentity must be specified in latents
  if (!any(stri_detect(srv$mm$latent, regex = "mainentity"), na.rm = TRUE)) {
    stop("'mainentity' is not specified in latents for the measurement model. 
         See help(set_association).", call. = FALSE)
  } else {
    srv <- prepare_survey(srv)
  }
  
  # Check if entity is specified, if not - use mainentity and generate a report each
  if (is.null(entity)) {
    message("No entity specified, creating report for all (main) entities.")
    entity <- unique(srv$df$mainentity)
  }
  
  # Convert input to an environment to avoid loading data multiple times
  # when generating more than one report
  srv_envir <- list2env(srv, parent = environment())
  
  # Read in the report template
  md <- readLines(report, encoding = "UTF-8")
  
  # Replace date with current date, and data with fixed path
  md <- stringi::stri_replace_all(md, format(Sys.Date(), "%Y"), regex = "REPLACE_DATE")
  
  # Make sure the Markdown and Reports directory exists
  dir.create(file.path(dir, "Reports"), showWarnings = FALSE)
  dir.create(file.path(dir, "Markdown"), showWarnings = FALSE)
  dir.create(file.path(dir, "PPT"), showWarnings = FALSE)
  
  # Copy beamer theme files if they do not exist
  if (identical(stri_trans_tolower(type), "pdf")) {
    copy_beamer_theme(file.path(dir, "Markdown"))
  } else if (identical(stri_trans_tolower(type), "ppt")) {
    copy_ppt_theme(file.path(dir, "PPT"))
  }
  
  # Iterate over the entities and create their individual .Rmd files.
  lapply(entity, generate_rmd, md, dir)
  
  # Generate the wanted report-type
  switch(type,
         pdf = lapply(entity, generate_beamer, dir, srv_envir),
         ppt = lapply(entity, generate_ppt, dir, srv_envir),
         stop("Please use a supported output format.\n", call. = FALSE))
  
  invisible()
  
}

#' Prepare survey for reporting
#'
#' This function prepares a survey for reporting.
#' 
#' @param srv A survey object.
#' @author Kristian D. Olsen
#' @export
#' @examples 
#' x %>% prepare_report()

prepare_survey <- function(srv) {
  
  # Check class
  if (!is.survey(srv)) {
    stop("Argument 'srv' is not an object with the class 'survey'. See help(survey).", call. = FALSE)
  }
  
  # Lowercase names
  nms <- names(srv$df)
  srv <- rename_(srv, .dots = setNames(nms, stri_trans_tolower(nms)))
  
  if (nrow(srv$cd)) {
    nms <- names(srv$cd)
    srv$cd <- rename_(srv$cd, .dots = setNames(nms, stri_trans_tolower(nms)))
  }
  
  if (nrow(srv$hd)) {
    nms <- names(srv$hd)
    srv$hd <- rename_(srv$hd, .dots = setNames(nms, stri_trans_tolower(nms)))
  }
  
  # Replace mainentity
  mainentity <- get_association(srv, "mainentity")
  if (length(mainentity)) {
    if (mainentity %in% names(srv$df)) srv <- rename_(srv, .dots = setNames(list(mainentity), "mainentity"))
    if (mainentity %in% names(srv$cd)) names(srv$cd) <- ordered_replace(names(srv$cd), setNames(mainentity, "mainentity"))
    if (mainentity %in% names(srv$hd)) names(srv$hd) <- ordered_replace(names(srv$hd), setNames(mainentity, "mainentity"))
  }
  
  # Replace subentity
  subentity <- get_association(srv, "subentity")
  if (length(subentity)) {
    if (subentity %in% names(srv$df)) srv <- rename_(srv, .dots = setNames(list(subentity), "subentity"))
    if (subentity %in% names(srv$cd)) names(srv$cd) <- ordered_replace(names(srv$cd), setNames(subentity, "subentity"))
    if (subentity %in% names(srv$hd)) names(srv$hd) <- ordered_replace(names(srv$hd), setNames(subentity, "subentity"))
  }
  
  # Insert translations for latents as question texts  
  srv <- use_latent_translation(srv)
  
  # Include question text for EM variables
  vars <- srv$mm$manifest[srv$mm$latent %in% default$latents]
  questions <- get_question(srv, vars)
  
  srv$mm$question[match(stri_c(vars, "em"), srv$mm$manifest, nomatch = 0)] <- questions
 
  # Return
  srv
  
}

# Report functions -------------------------------------------------------------

generate_rmd <- function(entity, md, dir) {
  
  path <- file(file.path(dir, "Markdown", stri_c(entity, ".Rmd")), encoding = "UTF-8")
  on.exit(close(path), add = TRUE)
  
  writeLines(stringi::stri_replace_all(md, entity, regex = "REPLACE_ENTITY"), path)
  
}
