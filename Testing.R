library(devtools); test()
library(stringi)

srv <- prepare_survey(srv)
entities <- levels(srv$df$mainentity)[1]
rmd <- "c:/Github/EPSI/Kontorstudien PM 2015/Kontorstudien PM 2015.Rmd"
options("ReporteRs-default-font" = "Arial")

report = rmd
entity = entities
type = "ppt"
font = "Arial"

# generate_report(report = rmd, entity = entities, type = "ppt")
report <- clean_path(report)
dir <- dirname(report)
srv_envir <- list2env(srv, parent = environment())

md <- readLines(report, encoding = "UTF-8")
md <- stringi::stri_replace_all(md, format(Sys.Date(), "%Y"), regex = "REPLACE_DATE")
dir.create(file.path(dir, "PPT"), showWarnings = FALSE)
copy_ppt_theme(file.path(dir, "PPT"))
lapply(entity, generate_rmd, md, dir)


# lapply(entity, generate_ppt, dir, srv_envir)
rmd <- file.path(dir, "Markdown", stri_c(entity, ".Rmd"))
rmd <- readLines(rmd, encoding = "UTF-8")

if (!any(stri_detect(rmd, regex = "##\\+"), na.rm = TRUE)) {
  rmd <- rmd_to_r(rmd, write = FALSE)
}

# Separate out YAML
envir = srv_envir

is_yaml <- which(stri_detect(rmd, regex = default$pattern$code$yaml))
if (!length(is_yaml)) {
  stop("Could not find the YAML frontmatter.", call. = FALSE)
} else {
  yaml <- extract_yaml(rmd)
  rmd <- rmd[-c(is_yaml[1]:is_yaml[2])]
}

# Evaluate the markdown
res <- evaluate_rmd(rmd, envir = envir)

res[sapply(res, class) == "recordedplot"]
