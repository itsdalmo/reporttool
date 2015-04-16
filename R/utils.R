#' Utilities
#'
#' A few wrappers to make a common tasks less verbose.
#' 
#' @section List of utilities:
#' 
#' \describe{
#'    \item{\code{ordered_replace}}{Replace \code{x} with \code{y} where \code{x}
#'    matches \code{by}. Matches and replacements retain the original order of
#'    \code{x}.} 
#'
#'    \item{\code{clean_missing}}{Takes a \code{data.frame} and cleans
#'    the default missing value strings. See \code{defaults.R} for these
#'    strings.}
#'
#'    \item{\code{clean_score}}{Takes vectors representing likert scales and
#'    cleans text descriptions. E.g. "10 Very happy" becomes "10".}
#'
#'    \item{\code{rescale_score}}{Takes vectors representing 10-point likert 
#'    scales and transforms them to 100-point scales. (x-1)*(100/9)}
#' }
#'
#' @param df A \code{data.frame}
#' @param na.strings Optional: Provide a list of strings to convert to NA.
#' @param var A numeric or character vector.
#' @author Kristian D. Olsen
#' @return \code{clean_missing} returns a \code{data.frame}, \code{clean_score}
#' returns a vector of the same type (typically character) and \code{rescale_score} 
#' returns a numeric vector.
#' @export
#' @examples 
#' df <- clean_missing(df)
#' df$Q3 <- clean_score(df$Q3)
#' df$Q3 <- rescale_score(df$Q3)

clean_missing <- function(df, na.strings = reporttool$missing_values) {
  
  if (all(!is.null(df), nrow(df) > 0L)) {
    df <- lapply(df, function(x) ifelse(x %in% na.strings, NA, x))
    df <- as.data.frame(df, stringsAsFactors=FALSE)
  }
  
  return(df)
}

#' @rdname clean_missing
#' @export
clean_score <- function(var) {
  gsub("([0-9]+).*$", "\\1", var)
}

#' @rdname clean_missing
#' @export
rescale_score <- function(var) {
  suppressWarnings(ifelse(test %in% 1:10, (as.numeric(test)-1)*(100/9), NA))
}

#' @rdname clean_missing
#' @export
ordered_replace <- function(x, by, y) {
  
  # Use setnames for y and by
  if (length(by) == length(y)) {
    y <- setNames(by, y)
  } else {
    stop("'y' and 'by' must have same length.", call. = FALSE)
  }
  
  # Replace x by y (based on 'by')
  if (any(x %in% y)) {
    x[x %in% y] <- names(y)[vapply(x, function(x) match(x, y), numeric(1))]
  } else {
    warning("x had no matches in y; no values replaced", call. = FALSE)
  }
  
  return(x)  
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

has_extension <- function(path, ext) {
  identical(tolower(tools::file_ext(path)), ext)
}

is_supported_ext <- function(...) {
  exts <- vapply(list(...), tools::file_ext, character(1))
  all(exts %in% reporttool$input_formats)
}

# Lowercase all columnnames
tolower_cols <- function(df) {
  
  if(!is.null(names(df))) {
    names(df) <- tolower(names(df))
  }
  
  df
}
