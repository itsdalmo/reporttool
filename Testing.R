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

ev[sapply(ev, class) == "recordedplot"]
ev[sapply(ev, class) == "character"]

# ------------------------------------------------------------------------------



srv %>% generate_report("C:/Github/EPSI/Lyse ladestasjoner/Lyse ladestasjoner.Rmd", type = "ppt")
