# Example

library(devtools); load_all(); test()

library(dplyr)

x <- read_data("./tests/testthat/test.sav")
y <- survey(x[1:37]) %>% 
  set_association(complaint = "q17", common = TRUE) %>% 
  add_entities() %>%
  prepare_data()

