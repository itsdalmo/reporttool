library(devtools); test()
library(stringi)


# osrv from a report
rmd <- "../EPSI/Lyse ladestasjoner/Lyse ladestasjoner.Rmd"
md <- rmd_to_r(rmd, write = FALSE)


srv <- osrv
srv$entity <- "Lyse ladestasjoner"
envir <- list2env(srv)

md <- readLines(rmd, encoding = "UTF-8")
md <- stringi::stri_replace(md, format(Sys.Date(), "%Y"), regex = "REPLACE_DATE")


# ------------------------------------------------------------------------------

ev <- evaluate_rmd(md, envir = envir)
test <- ev
ev <- test$code

# ev[sapply(ev, class) == "recordedplot"]
ev[sapply(ev, class) == "character"]

# ------------------------------------------------------------------------------


rmd <- rmd_to_r(rmd, write = FALSE)

srv %>% generate_report("C:/Github/EPSI/Lyse ladestasjoner/Lyse ladestasjoner.Rmd", type = "ppt")


rmd <- md

if (!any(stri_detect(rmd, regex = "##\\+"), na.rm = TRUE)) {
  rmd <- rmd_to_r(rmd, write = FALSE)
}

# Separate out YAML
is_yaml <- which(stri_detect(rmd, regex = default$pattern$code$yaml))
if (!length(is_yaml)) stop("Could not find the YAML frontmatter.", call. = FALSE)
yaml <- extract_yaml(rmd)
rmd <- rmd[-c(is_yaml[1]:is_yaml[2])]

res <- evaluate_rmd(rmd, envir = envir)





rmd <- rmd[(length(rmd)-132):length(rmd)]


# Evaluate the markdown
res <- evaluate_rmd(rmd, envir = envir)
