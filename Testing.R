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

write_sharepoint(y, "./test/")

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
yy <- read_data("test.xlsx")

yy <- survey(yy) 
yy <- factor_data(yy, vars = yy$mm$manifest[yy$mm$type %in% c("scale", "factor")])
yy %>% write_data("test2.sav")

yyy <- read_data("test2.sav")
yyyy <- survey(yyy)

wb <- openxlsx::createWorkbook()

# yyyy$df %>% select(q1, q3em) %>%
#             group_by(q1) %>%
#             summarise(q3 = mean(q3em, na.rm = TRUE)) %>%
#             to_sheet(wb, title = yyyy$mm$question[yyyy$mm$manifest %in% "q3"])
# 
# openxlsx::saveWorkbook(wb, "test3.xlsx", overwrite = TRUE)

#data.frame("y" = unlist(lapply(yy$df, class)), "y4" = unlist(lapply(yyyy$df, class)))
testthat::expect_identical(yyyy$df, yy$df)

yyyy$df[20, 1:5]; yy$df[20, 1:5]
yyyy$df[20, 6:10]; yy$df[20, 6:10]
yyyy$df[20, 11:15]; yy$df[20, 11:15]
yyyy$df[20, 16:20]; yy$df[20, 16:20]
yyyy$df[20, 54:60]; yy$df[20, 54:60]
yyyy$df[20, 45:50]; yy$df[20, 45:50]
yyyy$df[20, 40:45]; yy$df[20, 40:45]
yyyy$df[20, 30:35]; yy$df[20, 30:35]


library(dplyr)
x <- read_data("c:/Users/krist_000/Desktop/ene39795_150902.sav")
x %>% survey() %>% 
  set_translation(language = "norwegian") %>%
  set_association(common = TRUE) %>% 
  add_entities() %>% 
  topline() %>%
  write_data("c:/Users/krist_000/Desktop/Topline Bank BM 04-09-2015.xlsx")
