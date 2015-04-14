# Functions to clean data -------------------------------------------
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
  
  if(!is.null(names(df)))
    names(df) <- tolower(names(df))
  
  df
}


# Functions to validate extensions ---------------------------------------------

has_extension <- function(path, ext) {
  identical(tolower(tools::file_ext(path)), ext)
}

is_supported_ext <- function(...) {
  
  exts <- vapply(list(...), tools::file_ext, character(1))
  all(exts %in% default$input_formats)
  
}

# Misc -------------------------------------------------------------------------
isFALSE <- function(x) identical(x, FALSE)

validate_path <- function(path) {
  
  if (!all(is.character(path), length(path) == 1L)) {
    stop("Path is not in valid format (character(1)):\n", path, call. = FALSE)
  }
  
  if (!grepl("^(/|[A-Za-z]:|\\\\|~)", path)) {
    path <- normalizePath(path, "/", mustWork = FALSE)
  }
  
  # Remove trailing slashes from path
  path <- sub("/$", "", path)
  
  path
}