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
  
  # Get mainentity variable
  nms <- srv$mm$manifest[srv$mm$latent %in% "mainentity"]
  rep <- "mainentity"
  
  # And subentity if it exists
  if ("subentity" %in% srv$mm$latent) {
    nms <- c(nms, srv$mm$manifest[srv$mm$latent %in% "subentity"])
    rep <- c("mainentity", "subentity")
  }
  
  # Replace name for data
  for (i in names(srv)) {
    names(srv[[i]]) <- ordered_replace(names(srv[[i]]), nms, rep)
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
  srv_envir <- list2env(srv, parent = environment())
  
  # Read in the report template
  md <- readLines(report, encoding = "UTF-8")
  
  # Replace date with current date, and data with fixed path
  md <- stringi::stri_replace(md, format(Sys.Date(), "%Y"), regex = "REPLACE_DATE")
  
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
         pdf = lapply(entity, generate_beamer, dir, srv_envir),
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
