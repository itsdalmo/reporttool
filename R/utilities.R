#' Utilities
#'
#' A few wrappers to make a common tasks less verbose.
#' 
#' @section List of utilities:
#' 
#' \describe{
#'    \item{\code{rt_defaults}}{Get default settings from the reporttool package.
#'    \code{x} is a search-string which is not case sensitive.}
#' 
#'    \item{\code{intranet_link}}{Converts any http(s) link with a .se domain
#'    to a link for a network drive (sharepoint) on windows.}  
#' 
#'    \item{\code{get_sheet_names}}{Takes the path to a .xlsx file and returns
#'    the sheetnames in the file if any sheets exist. Quicker than using
#'    openxlsx::loadWorkbook() and names() when you do not need to preserve
#'    styling/formatting.}  
#' 
#'    \item{\code{ordered_replace}}{Replace \code{x} with \code{replacement} where \code{x}
#'    matches \code{match_by}. Matches and replacements retain the original order of
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
#'    
#'    \item{\code{lowercase_names}}{Used to lowercase columnnames in lists 
#'    (instead of an anonyomous function.)}
#' }
#' 
#' @name Utilities
#' @author Kristian D. Olsen
#' @export
#' @examples 
#' df <- set_missing(df)
#' df$Q3 <- clean_score(df$Q3)
#' df$Q3 <- rescale_score(df$Q3)
#' ordered_replace("measurement model", with(cfg$sheet_names, setNames(long, short)))
#' lst <- lapply(lst, lowercase_names)
#' rt_defaults("latent")

#' @rdname utilities
set_missing <- function(df, na.strings = cfg$missing_values) {
  
  if (is.matrix(df)) {
    df <- as.data.frame(df, stringsAsFactors = FALSE)
  }
  
  if (all(!is.null(df), nrow(df) > 0L)) {
    df <- lapply(df, function(x) ifelse(x %in% na.strings, NA_character_, x))
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
ordered_replace <- function(x, match_by, replacement = NULL) {
  
  # Make sure a named vector is used if replacement is not specified
  if (is.null(replacement)) {
    
    if (is.null(attr(match_by, "names"))) {
      stop("'match_by' must be a named vector or replacement must be specified\n", call. = FALSE)
    } else {
      y <- match_by
    }
  
  } else {
    
    if (length(match_by) == length(replacement)) {
      y <- setNames(match_by, replacement)
    } else {
      stop("'match' and 'replace' must have same length\n", call. = FALSE)
    }
  }
    
  # Replace x with values from replace (based on 'match')
  if (any(x %in% y)) {
    x[x %in% y] <- names(y)[match(x, y, nomatch = 0)]
  } 
  
  x
  
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

#' @rdname utilities
#' @export 
intranet_link <- function(https) {
  
  # If you are on windows and the link ends with .se
  if (grepl("^http[s]*://.*[^/]\\.se/.*", https) && Sys.info()["sysname"] == "Windows") {
    domain <- sub("^http[s]*://(.[^/]*)/.*", "\\1", https)
    https <- paste0("\\\\", domain, "@SSL/DavWWWRoot", sub(paste0(".*", domain, "(.*)"), "\\1", https))
  }
  
  https
  
}

#' @rdname utilities
#' @export
lowercase_names <- function(x) {
  
  if(!is.null(names(x))) {
    names(x) <- tolower(names(x))
  }
  
  x
  
}

# MISC -------------------------------------------------------------------------

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

