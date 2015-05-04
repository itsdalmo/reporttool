#' Utilities
#'
#' A few wrappers to make a common tasks less verbose.
#' 
#' @section List of utilities:
#' 
#' \describe{
#'    \item{\code{get_sheet_names}}{Takes the path to a .xlsx file and returns
#'    the sheetnames in the file if any sheets exist. Quicker than using
#'    openxlsx::loadWorkbook() and names() when you do not need to preserve
#'    styling/formatting.}  
#' 
#'    \item{\code{ordered_replace}}{Replace \code{x} with \code{y} where \code{x}
#'    matches \code{by}. Matches and replacements retain the original order of
#'    \code{x}.} 
#'
#'    \item{\code{set_missing}}{Takes a \code{data.frame} and cleans
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
#' @name Utilities
#' @author Kristian D. Olsen
#' @import tools
#' @import utils
#' @export
#' @examples 
#' df <- set_missing(df)
#' df$Q3 <- clean_score(df$Q3)
#' df$Q3 <- rescale_score(df$Q3)

#' @rdname utilities
set_missing <- function(df, na.strings = reporttool$missing_values) {
  
  if (all(!is.null(df), nrow(df) > 0L)) {
    df <- lapply(df, function(x) ifelse(x %in% na.strings, NA, x))
    df <- as.data.frame(df, stringsAsFactors=FALSE)
  }
  
  return(df)
}

#' @rdname utilities
#' @export
clean_score <- function(var) {
  gsub("([0-9]+).*$", "\\1", var)
}

#' @rdname utilities
#' @export
rescale_score <- function(var) {
  suppressWarnings(ifelse(var %in% 1:10, (as.numeric(var)-1)*(100/9), NA))
}

#' @rdname utilities
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

#  Copied from sourcecode
#' @rdname utilities
#' @export 
get_sheet_names <- function(file) {
  
  xmlDir <- file.path(tempdir(), "_excelXMLRead")
  xmlFiles <- utils::unzip(file, exdir = xmlDir)
  
  on.exit(unlink(xmlDir, recursive = TRUE), add = TRUE)
  
  worksheets <- xmlFiles[grepl("/worksheets/sheet[0-9]", xmlFiles, perl = TRUE)]
  workbook <- xmlFiles[grepl("workbook.xml$", xmlFiles, perl = TRUE)]
  
  if(length(worksheets) == 0) {
    stop("Workbook has no worksheets")
  }

  ## get workbook names
  workbook <- unlist(readLines(workbook, warn = FALSE, encoding = "UTF-8"))
  sheets <- unlist(regmatches(workbook, gregexpr("<sheet .*/sheets>", workbook, perl = TRUE)))
  
  ## make sure sheetId is 1 based
  sheetrId <- as.integer(unlist(regmatches(sheets, gregexpr('(?<=r:id="rId)[0-9]+', sheets, perl = TRUE)))) 
  sheetrId <- sheetrId - min(sheetrId) + 1L
  
  sheets <- unlist(regmatches(sheets, gregexpr('(?<=name=")[^"]+', sheets, perl = TRUE)))
  
  return(sheets)
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
  
  # Remove trailing slashes for Windows-users
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
