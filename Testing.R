library(devtools); load_all(); test()

# TOPLINE (using real data) ----------------------------------------------------

library(dplyr); library(stringi)
x <- read_data("c:/Users/krist_000/Desktop/enc39795_150909.sav")

x %>% 
  survey() %>% 
  set_translation(language = "norwegian") %>%
  set_association(common = TRUE) %>% 
  add_entities() %>% 
  topline()

x %>% 
  select(starts_with("for")) %>% 
  survey() %>%
  set_colnames(nms = stri_replace_all(names(.$df), "", regex = "^for")) %>%
  set_translation(language = "norwegian") %>%
  set_association(common = TRUE) %>% 
  add_entities() %>% 
  topline()

