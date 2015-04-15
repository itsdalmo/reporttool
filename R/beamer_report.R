#' Beamer template
#'
#' This is a beamer template for generating reports, and it is based on
#' mtheme. The template assumes that .sty files are in the same directory as
#' the \code{.Rmd} file that is being rendered to a beamer pdf.
#'
#' @param dev Defaults to cairo_pdf. Set to 'pdf' if you do not have the 
#' 'cairo_pdf' device.
#' @param toc Include a table of contents or not. Default is true.
#' @param keep_tex Keep the intermediate .tex files (useful for troubleshooting)
#' @param slide_level Manually set the slide level. Default is 2, which makes
#' a single hashtag a section divider in latex and double hashtag into frames.
#' @author Kristian D. Olsen
#' @note This function uses a slightly modified template_beamer.tex file which
#' is included in the package (i.e. does not rely on rmarkdown).
#' @import knitr
#' @import rmarkdown
#' @export
#' @examples 
#' render("index.Rmd", "reporttool::report_template")

beamer_template <- function(dev = "cairo_pdf", toc = TRUE, keep_tex = FALSE, slide_level = NULL) {
  
  # Set slidelevel to default
  if (is.null(slide_level)) {
    slide_level <- 2
  }
  
  dir <- "rmd/beamer/"
  
  # Locate file to include in preamble, and the report template
  preamble <- system.file(file.path(dir, "preamble.tex"), package="reporttool")
  template <- system.file(file.path(dir, "report_template_beamer.tex"), package="reporttool")
  
  # Edit beamer_presentation
  format <- rmarkdown::beamer_presentation(template = template,
                                           theme = "m", 
                                           toc = toc,
                                           keep_tex = keep_tex,
                                           slide_level = slide_level,
                                           includes = includes(in_header = preamble),
                                           pandoc_args = c("--latex-engine=xelatex"))
  
  # Change the default chunk-options
  format$knitr$opts_chunk$echo <- FALSE
  format$knitr$opts_chunk$message <- FALSE
  format$knitr$opts_chunk$warning <- FALSE
  format$knitr$opts_chunk$dev <- dev
  format$knitr$opts_chunk$dev.args <- list(bg = "transparent")
  format$knitr$opts_chunk$results <- "asis"
  
  format
  
}

#' @rdname beamer_template
#' @export

copy_beamer_theme <- function(dir=NULL) {
  
  # Specify directory
  if (is.null(dir)) {
    dir <- getwd()
  }
  
  files <- with(default$beamer_thm, file.path(dir, files))
  files <- system.file(files, package="reporttool")
  
  lapply(files, function(x, dir) { 
    file.copy(x, file.path(dir, basename(x)), overwrite=FALSE) }, dir)
  
  invisible()
}

#' @title Generate report
#' @description Generate one or more reports using the templates in this package.
#' @param report Path to the report template (.Rmd) for a specific study.
#' @param data Optional: path to input.xlsx (if NULL, assumes that this file can 
#' be found in working directory.)
#' @param entity Optional: character vector with one or more entities for generating reports.
#' @return A .Rmd and .pdf file for each entity (in the data, or for each specified).
#' @author Kristian D. Olsen
#' @import knitr
#' @import rmarkdown
#' @examples
#' generate_report("/Internal/Internal_report_2014.Rmd", entity=c("EPSI", "SKI"))

#' @export
generate_report <- function(report=NULL, type="pdf", entity=NULL, df=NULL) {
  
  report <- validate_path(report)
  dir <- dirname(report)
  
  if (!file.exists(report)) {
    stop("File not found: ", report, call. = FALSE)
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
  md <- gsub("REPLACE_DATE", format(Sys.Date(), "%Y"), md, fixed=TRUE)
  md <- gsub("REPLACE_DATA", data, md, fixed=TRUE)
  
  # Create the directory for the reports (if it does not exist) and
  # check if the required .sty files are present.
  md_dir <- file.path(dir, "Markdown")
  md_sty <- file.path(dir, default$beamer_thm$files)
  
  if (!file.exists(md_dir) || !all(file.exists(md_sty))) {
    dir.create(md_dir, showWarnings = FALSE)
    copy_beamer_theme(dir = md_dir)
  }
  
  dir.create(file.path(dir, "Reports"), showWarnings = FALSE)
  
  # Iterate over the list of entities, create a Rmd file and PDF for each one.
  
  lapply(entity, generate_rmd, md, dir)
  
  switch(type,
         pdf = lapply(entity, generate_pdf, md, dir),
         stop("Please use a supported output format."))
}

# Report functions -------------------------------------------------------------

generate_rmd <- function(entity, md, dir) {
  
  report <- gsub("REPLACE_ENTITY", entity, md, fixed=TRUE)
  writeLines(report, file.path(dir, "Markdown", paste0(i, ".Rmd")))
  
  invisible()
}

generate_pdf <- function(entity, md, dir) {
  
  rmarkdown::render(file.path(dir, "Markdown", paste0(i, ".Rmd")),
                    output_format = "all",
                    intermediates_dir = file.path(dir, "Markdown"),
                    output_dir = file.path(dir, "Reports"),
                    quiet = TRUE)
  
  invisible()
}
