library(devtools);test()
library(stringi)

x <- read_data("../EPSI/Kronjylland H2015/Data_SK_11 septemeber.sav")


xx <- survey(x) %>%
      rename(q1_org = Q1, q1 = b_4) %>%
      mutate(q1 = stri_replace_all(q1, "", regex = "\""), 
             q1 = factor(q1, levels = unique(q1))) %>%
      set_association(common = TRUE) %>% 
      add_entities() %>%
      set_config() %>%
      set_translation() %>%
      prepare_data()


all(names(xx$df) %in% xx$mm$manifest)

write_data(xx, "SK 11 september.xlsx")
