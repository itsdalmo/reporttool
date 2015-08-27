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

# yy <- survey(x[1:37]) 
# yy$df[1, 3:14] <- NA
# yy <- yy %>% 
#   set_association(complaint = "q17", common = TRUE) %>% 
#   add_entities() %>%
#   set_translation(difference = "Differanse", language = "norwegian") %>%
#   set_config() %>%
#   prepare_data()
# 

y %>% write_data("test.xlsx")
yy <- read_data("test.xlsx") %>% factor_data()

yy <- survey(yy)
yy %>% write_data("test2.sav")

yyy <- read_data("test2.sav")
yyyy <- survey(yyy)

data.frame("y" = unlist(lapply(y$df, class)), "y4" = unlist(lapply(yyyy$df, class)))
