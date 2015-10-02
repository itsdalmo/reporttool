get_translation <- function(srv, tr) {
  
  value <- filter(srv$tr, original %in% stri_trans_tolower(tr))[["replacement"]]
  if (!length(value) || is.na(value)) {
    stop(stri_c("\'", tr, "\'"), " was not found in translations.", call. = FALSE)
  } 
  
  value
  
}

get_config <- function(srv, cfg) {
  
  value <- filter(srv$cfg, config %in% stri_trans_tolower(cfg))[["value"]]
  if (!length(value) || is.na(value)) {
    stop(stri_c("\'", cfg, "\'"), " was not found in config.", call. = FALSE)
  } 
  
  value
  
}

get_data <- function(srv) srv$df

#' @export
get_question <- function(srv, str) {
  
  filter(srv$mm, manifest %in% str)[["question"]]
  
}