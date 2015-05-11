#' @title Generate report
#' @description Generate one or more reports using the templates in this package.
#' @param report Path to the report template (.Rmd) for a specific study.
#' @param data Optional: path to input.xlsx (if NULL, assumes that this file can 
#' be found in working directory.)
#' @param entity Optional: character vector with one or more entities for generating reports.
#' @return A .Rmd and .pdf file for each entity (in the data, or for each specified).
#' @author Kristian D. Olsen
#' @examples
#' generate_report("/Internal/Internal_report_2014.Rmd", entity=c("EPSI", "SKI"))

#' @export
generate_report <- function(report=NULL, entity=NULL, data=NULL, type="pdf") {
  
  report <- validate_path(report)
  dir <- dirname(report)
  
  if (!file.exists(report)) {
    stop("File not found:\n", report, call. = FALSE)
  }
  
  # See if data is specified correctly, if not; assumes input.xlsx in same folder
  if (is.null(data)) {
    data <- file.path(dir, "input.xlsx")
  } else {
    data <- validate_path(data)
  }
  
  if (!file.exists(data)) {
    stop("File not found: ", data, call. = FALSE)
  }
  
  # Check if the entity is specified, if not - use the mainentity and generate a report for each.
  if (is.null(entity)) {
    message("No entity specified, creating report for all (main) entities in:\n", data)
    
    # Load the data
    mm <- read_sheets(data, sheets="measurement model", clean.missing = TRUE)
    df <- read_sheets(data, sheets="data", clean.missing = TRUE)
    
    # Lowercase variables for easier referencing
    names(mm) <- tolower(names(mm))
    names(df) <- tolower(names(df))
    
    # Get name of mainentity variable
    mainentity <- tolower(mm$manifest[tolower(mm$latent) %in% "mainentity"])
    
    entity <- unique(df[[mainentity]])
  }
  
  # Read in the report template
  md <- readLines(report, encoding="UTF-8")
  
  # Replace date with current date, and data with fixed path
  md <- sub("REPLACE_DATE", format(Sys.Date(), "%Y"), md, fixed=TRUE)
  md <- sub("REPLACE_DATA", data, md, fixed=TRUE)
  
  # Create the directory for the reports (if it does not exist) and
  # check if the required .sty files are present.
  md_dir <- file.path(dir, "Markdown")
  md_sty <- file.path(dir, reporttool$beamer_thm$files)
  
  if (!file.exists(md_dir) || !all(file.exists(md_sty))) {
    dir.create(md_dir, showWarnings = FALSE)
    copy_beamer_theme(dir = md_dir)
  }
  
  # Make sure the Reports directory exists
  dir.create(file.path(dir, "Reports"), showWarnings = FALSE)
  
  # Iterate over the entities and create their individual .Rmd files.
  lapply(entity, generate_rmd, md, dir)
  
  # Generate the wanted report-type
  switch(type,
         pdf = lapply(entity, generate_beamer, dir, environment()),
         stop("Please use a supported output format.", call. = FALSE))
  
  invisible()
}

# Report functions -------------------------------------------------------------

generate_rmd <- function(entity, md, dir) {
  
  path <- file(file.path(dir, "Markdown", paste0(entity, ".Rmd")), encoding = "UTF-8")
  on.exit(close(path), add = TRUE)
  
  writeLines(gsub("REPLACE_ENTITY", entity, md, fixed=TRUE), path)
  
}