# Default values ---------------------------------------------------------------
reporttool <- list(
  
  "ggcolors" = c("#F8766D", "#00BFC4", "#808080", 
                 "#00BF7D", "#9590FF", "#A3A500", "#EA8331"),
  
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
    "ents" = c("entity", "n","marketshare", "other"),
    "mm" = c("latent", "manifest", "text"),
    "cfg" = c("original", "replacement")
    ),
  
  "beamer_tmp" = list(
    "dir" = "rmd/beamer",
    "files" = c("beamer_preamble.tex", 
                "beamer_template.tex")),
  
  "beamer_thm" = list(
    "dir" = "rmd/beamer",
    "files" = c("beamercolorthememetropolis.sty", 
                "beamerfontthememetropolis.sty", 
                "beamerthemem.sty", "logo.eps")),
  

  "rmd_pat" = list(
    "chunk_start" = "^```\\{r",
    "chunk_end" = "```$",
    "chunk_eval" = ".*eval\\s*=\\s*((.[^},]+|.[^}]+\\))),?.*",
    "inline" = "`r[ [:alnum:][:punct:]][^`]+`"
     )
)
