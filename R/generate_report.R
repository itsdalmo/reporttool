#' Generate report
#'
#' Generate one or more reports using the templates in this package.
#' 
#' @param srv A survey object containing the data that is being reported.
#' @param report Path to the report template (.Rmd) for the specific study.
#' @param entity Optional: Name of a specific entity to create a report for.
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
  
  # Check if entity is specified, if not - use mainentity and generate a report each
  if (is.null(entity)) {
    message("No entity specified, creating report for all (main) entities.")
    entity <- unique(srv$df$mainentity)
  }
  
  # Convert input to an environment to avoid loading data multiple times
  # when generating more than one report
  srv_envir <- list2env(prepare_report(srv), parent = environment())
  
  # Read in the report template
  md <- readLines(report, encoding = "UTF-8")
  
  # Replace date with current date, and data with fixed path
  md <- stringi::stri_replace(md, format(Sys.Date(), "%Y"), regex = "REPLACE_DATE")
  
  # Make sure the Markdown and Reports directory exists
  dir.create(file.path(dir, "Reports"), showWarnings = FALSE)
  dir.create(file.path(dir, "Markdown"), showWarnings = FALSE)
  dir.create(file.path(dir, "PPT"), showWarnings = FALSE)
  
  # Copy beamer theme files if they do not exist
  if (identical(tolower(type), "pdf")) {
    copy_beamer_theme(file.path(dir, "Markdown"))
  }
  
  # Iterate over the entities and create their individual .Rmd files.
  lapply(entity, generate_rmd, md, dir)
  
  # Generate the wanted report-type
  switch(type,
         pdf = lapply(entity, generate_beamer, dir, srv_envir),
         ppt = lapply(entity, generate_ppt, md, dir, srv_envir),
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

prepare_report <- function(srv) {
  
  # Check class
  if (!is.survey(srv)) {
    stop("Argument 'srv' is not an object with the class 'survey'. See help(survey).", call. = FALSE)
  }
  
  # Measurement model must be added first
  if (!is.survey_mm(srv$mm) || !nrow(srv$mm)) {
    stop("The measurement model must be added first. See help(add_mm).", call. = FALSE)
  }
  
  # Lowercase names
  nms <- names(srv$df)
  srv <- rename_(srv, .dots = setNames(nms, stri_trans_tolower(nms)))
  
  if (nrow(srv$cd) && !is.null(names(srv$cd))) {
    nms <- names(srv$cd)
    srv$cd <- rename_(srv$cd, .dots = setNames(nms, stri_trans_tolower(nms)))
  }
  
  if (nrow(srv$hd) && !is.null(names(srv$hd))) {
    nms <- names(srv$hd)
    srv$hd <- rename_(srv$hd, .dots = setNames(nms, stri_trans_tolower(nms)))
  }
  
  # Replace mainentity
  mainentity <- srv$mm$manifest[srv$mm$latent %in% "mainentity"]
  if (length(mainentity)) {
    if (mainentity %in% names(srv$df)) srv <- rename_(srv, .dots = setNames(list(mainentity), "mainentity"))
    if (mainentity %in% names(srv$cd)) names(srv$cd) <- ordered_replace(names(srv$cd), setNames(mainentity, "mainentity"))
    if (mainentity %in% names(srv$hd)) names(srv$hd) <- ordered_replace(names(srv$hd), setNames(mainentity, "mainentity"))
  }
  
  # Replace subentity
  subentity <- srv$mm$manifest[srv$mm$latent %in% "subentity"]
  if (length(subentity)) {
    if (subentity %in% names(srv$df)) srv <- rename_(srv, .dots = setNames(list(subentity), "subentity"))
    if (subentity %in% names(srv$cd)) names(srv$cd) <- ordered_replace(names(srv$cd), setNames(subentity, "subentity"))
    if (subentity %in% names(srv$hd)) names(srv$hd) <- ordered_replace(names(srv$hd), setNames(subentity, "subentity"))
  }
  
  # Set latent translations as "question"
  srv$mm$question[srv$mm$manifest %in% default$latents] <- filter(srv$tr, original %in% default$latents)[["replacement"]]
  
  # Return
  srv
  
}

# Report functions -------------------------------------------------------------

generate_rmd <- function(entity, md, dir) {
  
  path <- file(file.path(dir, "Markdown", stri_c(entity, ".Rmd")), encoding = "UTF-8")
  on.exit(close(path), add = TRUE)
  
  writeLines(stringi::stri_replace_all(md, entity, regex = "REPLACE_ENTITY"), path)
  
}
