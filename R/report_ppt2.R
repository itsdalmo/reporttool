#' @export
generate_ppt <- function(entity, rmd, dir, envir) {
  
  # Copy theme if it does not exist
  dir.create(file.path(dir, "PPT"), showWarnings = FALSE)
  copy_ppt_theme(dir = file.path(dir, "PPT"))
  
  # Create report
  doc <- ReporteRs::pptx(template = file.path(dir, "PPT", "ppt_template.pptx"))
  res <- evaluate_rmd(rmd, envir = envir)
  doc <- to_ppt(doc, res)
  
  ReporteRs::writeDoc(doc, file = stri_c(file.path(dir, "PPT", stri_c(entity, ".pptx"))))
  
}

to_ppt <- function(doc, res) {
  
  # Get the types of data in our results
  type <- vapply(res, function(x) {
    
    if (length(x) == 0L || is.na(x)) return("unknown")
    
    if (is.character(x)) {
      if (stri_detect(x, regex = "^# .*")) {
        "title"
      } else if (stri_detect(x, regex = "^## .*")) {
        "subtitle"
      } else if (stri_detect(x, regex = "begin\\{table\\}")) {
        "table"
      } else {
        "markdown"
      }
      
    } else if (inherits(x, "recordedplot")) {
      "recordedplot"
    } else {
      "other"
    }}, character(1))
    
  # Get the actual results
  title <- " "; subtitle <- " "
  
  for (i in seq_along(res)) {
    print(res[[i]])
    if (type[i] == "title") {
      title <- stri_replace(res[[i]], "$1", regex = "^# (.*)")
    } else if (type[i] == "subtitle") {
      subtitle <- stri_replace(res[[i]], "$1", regex = "^## (.*)")
    } else if (type[i] == "table") {
      doc <- ReporteRs::addSlide(doc, slide.layout = 'tableslide')
      doc <- ReporteRs::addTitle(doc, title)
      # doc <- ReporteRs::addFlexTable(doc, ReporteRs::as.FlexTable(res[[i]]))
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
    } else if (type[i] == "other") {
#       doc <- ReporteRs::addSlide(doc, slide.layout = 'textslide')
#       doc <- ReporteRs::addTitle(doc, title)
#       doc <- ReporteRs::addParagraph(doc, subtitle)
#       doc <- ReporteRs::addParagraph(doc, res[[i]])
    }
  }

  return(doc)
  
}

copy_ppt_theme <- function(dir) {
  
  files <- system.file("ppt/ppt_template.pptx", package="reporttool")
  
  lapply(files, function(x, dir) { 
    file.copy(x, file.path(dir, basename(x)), overwrite=FALSE) 
  }, dir)
  
  invisible()
  
}
