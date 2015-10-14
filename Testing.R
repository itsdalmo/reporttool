library(devtools); load_all()
library(stringi)

# ------------------------------------------------------------------------------

lnk <- "../EPSI/Kronjylland BM H2015"
x <- read_data(file.path(lnk, "Data", "Data Sparekassen Kronjylland B2B 2015.sav"))


# Ready the survey with associations etc
srv <- survey(x) %>%
  rename(percent_missing = Andel_Missing, q1_org = q1, q1 = Afdeling) %>%
  set_translation(study_average = "SK afdelingsstudie", language = "Danish") %>%
  set_config(study = "Kronjylland BM") %>%
  set_association(complaint = "qc17",
                  open_complaint = "qc17a",
                  prodq = stri_c("qp7", c("f", "g", "h")),
                  servq = stri_c("qs7", c("a", "b", "c", "d", "e")),
                  oa_positive = stri_c("q30a", c("", stri_c(".OpenText", c(2, 3)))),
                  oa_negative = stri_c("q30b", c("", stri_c(".OpenText", c(2, 3)))),
                  common = TRUE) %>%
  add_entities() %>%
  lowercase_names()

# ------------------------------------------------------------------------------
srv <- ungroup(srv)

srv %>% group_by(q1) %>% 
  survey_table(loj1, wide = TRUE) %>%
  select(-question, -manifest)

srv %>% group_by(q1) %>% 
  survey_table(kon1, kon2, wide = TRUE)

srv %>% group_by(q1) %>% 
  survey_table(one_of(get_default("latents")), wide = TRUE) 

srv %>%  
  mutate_each(funs(rescale_score(clean_score(.))), q3:q4a) %>%
  group_by(q1) %>%
  survey_table(q3, q4a, wide = TRUE) 
