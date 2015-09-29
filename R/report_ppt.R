#' Powerpoint template
#'
#' This function uses \code{ReporteRs} and \code{evaluate} to generate powerpoint
#' slides based on the same rmarkdown syntax that creates the pdf reports. I.e.
#' it is a dumbed down version of normal rmarkdown, where only \code{#} and \code{##}
#' are understood as being section titles and new frames/subtitles respectively.
#'
#' @param entity The name of the entity to create a report for.
#' @param rmd A loaded (\code{readLines}) rmarkdown document to use as a template.
#' @param dir The directory in which to place the PPT folder containing the reports.
#' @param envir The environment in which to evaluate the rmarkdown code.
#' @author Kristian D. Olsen
#' @note This function is used by \code{generate_report}.
#' @export
#' @examples 
#' x <- survey(data.frame("test" = 1, stringsAsFactors = FALSE))
#' x %>% add_mm()

generate_ppt <- function(entity, rmd, dir, envir, font = "Sisco Book") {
  
  # Set default font for ReporteRs
  ofont <- getOption("ReporteRs-default-font")
  options("ReporteRs-default-font" = font)
  on.exit(options("ReporteRs-default-font" = ofont), add = TRUE)
  
  # Copy theme if it does not exist
  dir.create(file.path(dir, "PPT"), showWarnings = FALSE)
  copy_ppt_theme(dir = file.path(dir, "PPT"))
  
  # Evaluate the markdown
  res <- evaluate_rmd(rmd, envir = envir)
  
  # Create report and add the first slide
  doc <- ReporteRs::pptx(template = file.path(dir, "PPT", "ppt_template.pptx"))
  doc <- ReporteRs::addSlide(doc, slide.layout = "titleslide")
  doc <- ReporteRs::addTitle(doc, value = res$title)
  doc <- ReporteRs::addParagraph(doc, value = res$date)
  doc <- ReporteRs::addParagraph(doc, value = res$author)
  doc <- ReporteRs::addParagraph(doc, value = res$subtitle)
  
  # Add title etc
  doc <- to_ppt(doc, res$code)
  
  ReporteRs::writeDoc(doc, file = stri_c(file.path(dir, "PPT", stri_c(entity, ".pptx"))))
  
  invisible()
  
}

#' @export
to_ppt <- function(doc, res) {
  
  # Get the types of data in our results
  type <- result_types(res)
    
  # Get the actual results
  title <- " "; subtitle <- " "
  
  for (i in seq_along(res)) {
    if (type[i] == "title") {
      title <- stri_replace(res[[i]], "", regex = "^#[^#]")
    } else if (type[i] == "subtitle") {
      subtitle <- stri_replace(res[[i]], "", regex = "^##[^#]")
    } else if (type[i] == "table") {
      doc <- ReporteRs::addSlide(doc, slide.layout = 'tableslide')
      doc <- ReporteRs::addTitle(doc, title)
      doc <- ReporteRs::addFlexTable(doc, format_flextable(res[[i]]))
      doc <- ReporteRs::addParagraph(doc, subtitle)
    } else if (type[i] == "markdown") {
      doc <- ReporteRs::addSlide(doc, slide.layout = 'standardslide')
      doc <- ReporteRs::addTitle(doc, title)
      doc <- ReporteRs::addMarkdown(doc, text = res[[i]])
      doc <- ReporteRs::addParagraph(doc, subtitle)
    } else if (type[i] == "recordedplot") {
      doc <- ReporteRs::addSlide(doc, slide.layout = 'standardslide')
      doc <- ReporteRs::addTitle(doc, title)
      doc <- ReporteRs::addPlot(doc, fun = print, x = res[[i]])
      doc <- ReporteRs::addParagraph(doc, subtitle)
    } else if (type[i] == "footnote") {
      warning("Footnotes are not supported and will not be included.", call. = FALSE)
    } else if (type[i] == "latextable") {
      warning("Latex tables are not supported and will not be included.", call. = FALSE)
    }
    
  }

  return(doc)
  
}

result_types <- function(res) {
  
  vapply(res, function(x) {
    
    if (length(x) == 0L || is.na(x)) return("unknown")
    
    if (is.character(x)) {
      if (stri_detect(x, regex = "^#[^#].*")) {
        "title" 
      } else if (stri_detect(x, regex = "^##[^#].*")) {
        "subtitle"
      } else if (stri_detect(x, regex = "begin\\{table\\}")) {
        "latextable"
      } else if (stri_detect(x, regex = "footnoteextra\\{")) {
        "footnote"
      } else {
        "markdown"
      }
    } else if (inherits(x, "recordedplot")) {
      "recordedplot"
    } else if (inherits(x, "source")) {
      "source"
    } else if (inherits(x, "data.frame")) {
      "table"
    } else {
      "unknown"
    }}, character(1))
  
}

copy_ppt_theme <- function(dir) {
  
  files <- system.file("ppt/ppt_template.pptx", package="reporttool")
  
  lapply(files, function(x, dir) { 
    file.copy(x, file.path(dir, basename(x)), overwrite=FALSE) 
  }, dir)
  
  invisible()
  
}
