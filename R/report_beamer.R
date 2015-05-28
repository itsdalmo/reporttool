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
#' @export
#' @examples 
#' render("index.Rmd", "reporttool::report_template")

beamer_template <- function(dev = "cairo_pdf", toc = TRUE, keep_tex = FALSE, slide_level = NULL) {
  
  # Set slidelevel to default
  if (is.null(slide_level)) slide_level <- 2
  
  dir <- "rmd/beamer/"
  
  # Locate file to include in preamble, and the report template
  preamble <- file.path(getwd(), "preamble.tex")
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
  if (is.null(dir)) dir <- getwd()
  
  files <- with(cfg$beamer_thm, file.path(dir, files))
  files <- system.file(files, package="reporttool")
  
  lapply(files, function(x, dir) { 
    file.copy(x, file.path(dir, basename(x)), overwrite=FALSE) 
  }, dir)
  
  invisible()
}

#' @rdname beamer_template
#' @export 

generate_beamer <- function(entity, dir, envir) {
  
  # Create the directory for the reports (if it does not exist) and
  # check if the required .sty files are present.
  md_dir <- file.path(dir, "Markdown")
  md_sty <- file.path(dir, cfg$beamer_thm$files)
  
  if (!file.exists(md_dir) || !all(file.exists(md_sty))) {
    dir.create(md_dir, showWarnings = FALSE)
    copy_beamer_theme(dir = md_dir)
  }
  
  # Render the document
  rmarkdown::render(file.path(dir, "Markdown", paste0(entity, ".Rmd")),
                    output_format = "all",
                    intermediates_dir = file.path(dir, "Markdown"),
                    output_dir = file.path(dir, "Reports"),
                    quiet = TRUE,
                    envir = envir)
}