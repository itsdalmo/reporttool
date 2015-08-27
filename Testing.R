# Example

library(devtools); load_all(); test()

library(dplyr)

x <- read_data("./tests/testthat/test.sav")
y <- survey(x[1:37]) %>% 
  set_association(complaint = "q17", common = TRUE) %>% 
  add_entities() %>%
  set_translation(difference = "Differanse", language = "norwegian") %>%
  set_config(cutoff = ".5") %>%
  prepare_data()

yy <- survey(x[1:37]) 
yy$df[1, 3:14] <- NA
yy <- yy %>% 
  set_association(complaint = "q17", common = TRUE) %>% 
  add_entities() %>%
  set_translation(difference = "Differanse", language = "norwegian") %>%
  set_config() %>%
  prepare_data()
