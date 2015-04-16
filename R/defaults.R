# Default values ---------------------------------------------------------------
reporttool <- list(
  
  "input_formats" = c("xlsx", "csv", ""),
  
  "output_formats" = c("xlsx", "csv", "txt", ""),
  
  "latent_names" = c("image", "expect", "prodq", 
                     "servq", "value", "epsi", "loyal"),
  
  "missing_values" = c("NA", " ", "", "#DIV/0!", "#NULL!", "#NAVN?", "#NAME?"),
  
  "sheet_names" = list(
    "long" = c("data", "contrast data", "historic data",
               "raw data", "entities", "measurement model", "config"),
    "short" = c("df", "cd", "hd", "rd", "ents", "mm", "cf")),
  
  "required_cols" = list(
    "entities" = c("entity", "marketshare", "other"),
    "measurement model" = c("latent", "manifest", "text"),
    "config" = c("original", "replacement")
    ),
  
  "beamer_tmp" = list(
    "dir" = "rmd/beamer",
    "files" = c("beamer_preamble.tex", 
                "beamer_template.tex")),
  
  "beamer_thm" = list(
    "dir" = "rmd/beamer",
    "files" = c("beamercolorthememetropolis.sty", 
                "beamerfontthememetropolis.sty", 
                "beamerthemem.sty", "logo.eps"))
)
