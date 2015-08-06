#' @rdname utilities
#' @export
rt_defaults <- function(string) {
  
  y <- cfg[stringi::stri_detect(names(cfg), regex = string, list(case_insensitive = TRUE))]
  
  # Drop list if only one entry is returned
  if (length(y) == 1L) y[[1]] else y

}

# Default values ---------------------------------------------------------------
cfg <- list(
  
  "ggcolors" = c("#F8766D", "#00BFC4", "#808080", 
                 "#00BF7D", "#9590FF", "#A3A500", "#EA8331"),

  "latent_names" = c("image", "expect", "prodq", 
                     "servq", "value", "epsi", "loyal"),
  
  "missing_values" = c("NA", " ", "", "#DIV/0!", "#NULL!", "#NAVN?", "#NAME?"),
  
  "sheet_names" = list(
    "long" = c("data", "contrast data", "historic data",
               "raw data", "entities", "measurement model", "config"),
    "short" = c("df", "cd", "hd", "rd", "ents", "mm", "cf")),
  
  "req_structure" = list(
    "ents" = c("entity", "n","marketshare", "other"),
    "mm" = c("latent", "manifest", "question", "type", "values"),
    "cfg" = c("original", "replacement")
    ),
  
  "beamer_tmp" = list(
    "dir" = "rmd/beamer",
    "files" = "beamer_template.tex"),
  
  "beamer_thm" = list(
    "dir" = "rmd/beamer",
    "files" = c("beamercolorthememetropolis.sty", 
                "beamerfontthememetropolis.sty", 
                "beamerthemem.sty", "logo.eps",
                "beamer_preamble.tex")),
  

  "rmd_pat" = list(
    "chunk_start" = "^```\\{r",
    "chunk_end" = "```$",
    "chunk_eval" = ".*eval\\s*=\\s*((.[^},]+|.[^}]+\\))),?.*",
    "inline" = "`r[ [:alnum:][:punct:]][^`]+`",
    "chunk_section" = "^[`r]?(print|cat)\\s*\\(\\s*\"\\s*##[^#]",
    "chunk_slide" = "^`r.*?(^print|^cat)\\s*\\(\\s*\"\\s*##[^#]",
    "text_section" = "^#[^#]",
    "text_slide" = "^##[^#]"
     ),
  
  "code_pat" = list(
    "chunk_start" = "^#\\sCHUNK\\s[0-9]+\\s[-]+$",
    "chunk_end" = "^#\\sCHUNK\\sEND\\s[-]+$",
    "not_chunk" = "^##+\\s.*"
    ),
  
  "scale_pat" = "^[0-9]{1,2}[[:alpha:][:punct:] ]*",
  "scale_end" = "^[0-9]{1,2}\\s*=?\\s*([[:alpha:]]*)",
  
  "epsi_model" = rbind("image" = c(0,0,0,0,0,0,0),
                       "expect" = c(1,0,0,0,0,0,0),
                       "prodq" = c(1,1,0,0,0,0,0),
                       "servq" = c(1,1,1,0,0,0,0),
                       "value" = c(0,0,1,1,0,0,0),
                       "epsi" = c(1,0,1,1,1,0,0),
                       "loyal" = c(0,0,0,0,0,1,0)),
  
  "latent_association" = list("image" = "q4",
                              "expect" = "q5",
                              "prodq" = "q7p",
                              "servq" = "q7s",
                              "value" = "q8",
                              "epsi" = c("q3", "q6", "q16"),
                              "loyal" = c("q10", "q15", "q15b"))
)
