# Functions for cleaning survey data -------------------------------------------
rescale_score <- function(var) {
  suppressWarnings(ifelse(test %in% 1:10, (as.numeric(test)-1)*(100/9), NA))
}

clean_score <- function(var) {
  gsub("([0-9]+).*$", "\\1", var)
}

# Clean missing values from a data.frame
clean_missing <- function(df) {
  
  if (all(!is.null(df), nrow(df) > 0L)) {
    df <- lapply(df, function(x) ifelse(x %in% default$missing_values, NA, x))
    df <- as.data.frame(df, stringsAsFactors=FALSE)
  }
  
  return(df)
}

# Lowercase all columnnames
tolower_cols <- function(df) {
  names(df) <- tolower(names(df))
  df
}

# Functions to validate paths --------------------------------------------------

expand_path <- function(path) {
  
  if (grepl("^(/|[A-Za-z]:|\\\\|~)", path))
    path <- normalizePath(path, "/", mustWork = FALSE)
  
  sub("/$", "", path)
}

is_valid_path <- function(...) {
  
  paths <- list(...)
  paths <- vapply(paths, expand_path, character(1))
  
  valid <- vapply(paths, function(x) {is.character(x) && length(x) == 1L}, logical(1))
  exists <- vapply(paths, file.exists, logical(1)) 
  
  all(valid, exists)

}

# Functions to validate extensions ---------------------------------------------

has_extension <- function(path, ext) {
  tolower(tools::file_ext(path)) == ext
}

is_valid_ext <- function(...) {
  
  exts <- vapply(list(...), tools::file_ext, character(1))
  all(exts %in% default$input_formats)
  
}