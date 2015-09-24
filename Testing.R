library(devtools); test()
library(stringi)

srv %>% generate_report("../EPSI/Lyse ladestasjoner/Lyse ladestasjoner.Rmd", type = "ppt")

# INPUT
report <- "../EPSI/Lyse ladestasjoner/Lyse ladestasjoner.Rmd"
srv <- osrv
type <- "ppt"

# STUFF THAT HAPPENS IN GENERATE REPORT
report <- clean_path(report)
dir <- dirname(report)
entity <- unique(srv$df$mainentity)
srv_envir <- list2env(prepare_report(srv), parent = environment())

# Read in the report template
md <- readLines(report, encoding = "UTF-8")
md <- stringi::stri_replace(md, format(Sys.Date(), "%Y"), regex = "REPLACE_DATE")
# dir.create(file.path(dir, "PPT"), showWarnings = FALSE)

#ppt = lapply(entity, generate_ppt, md, dir, srv_envir)
# INPUT FOR GENERATE PPT
entity <- entity[1]
rmd <- md
dir <- dir
envir <- srv_envir

# STUFF THAT HAPPENS IN GENERATE PPT
doc <- ReporteRs::pptx(template = file.path(dir, "PPT", "ppt_template.pptx"))
res <- evaluate_rmd(rmd, envir = envir)

# # STUFF THAT HAPPENS IN EVALUATE RMD
# envir <- envir
# rmd <- rmd_to_r(rmd, write = FALSE)
# 
# # Remove yaml
# yaml <- which(stri_detect(rmd, regex = stri_c("^##\\+ ---")))
# rmd <- rmd[-c(yaml[1]:yaml[2])]
# rmd <- rmd[rmd != ""]
# 
# inlines <- which(stri_detect(rmd, regex = "^##+"))
# chunks <- which(!stri_detect(rmd, regex = "^##+"))
# 
# # etc.... SEEMS TO WORK

# NEXT, TO PPT

# NOT WORKING
# doc <- to_ppt(doc, res)

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