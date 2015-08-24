# Example

library(devtools); load_all(); test()

library(dplyr)

x <- read_data("./tests/testthat/test.sav")
x <- survey(x)
x <- x %>% 
  set_association(common = TRUE) %>% 
  add_entities() %>%
  add_weights()

