#' Utilities
#'
#' A few wrappers to make a common tasks less verbose.
#' 
#' @section List of utilities:
#' 
#' \describe{
#' 
#'    \item{\code{recode}}{Recode variables.} 
#' 
#'    \item{\code{intranet_link}}{Converts any http(s) link with a .se domain
#'    to a link for a network drive (sharepoint) on windows.}  
#' 
#'    \item{\code{clean_score}}{Takes vectors representing likert scales and
#'    cleans text descriptions. E.g. "10 Very happy" becomes "10", and the value
#'    is converted to \code{numeric} without warning.}
#'
#'    \item{\code{rescale_score}}{Takes vectors representing 10-point likert 
#'    scales and transforms them to 100-point scales (\code{numeric}). (x-1)*(100/9)}
#'    
#'    \item{\code{ordered_replace}}{Replace \code{x} with \code{replacement} where \code{x}
#'    matches \code{match_by}. Matches and replacements retain the original order of
#'    \code{x}.} 
#'    
#'    \item{\code{get_default}}{Get default settings from the reporttool package.
#'    \code{x} is a search-string which is not case sensitive.}
#'
#'    \item{\code{set_missing}}{Takes a \code{data.frame} and cleans
#'    the default missing value strings. See \code{defaults.R} for these
#'    strings.}
#'
#' }
#' 
#' @name Utilities
#' @author Kristian D. Olsen
#' @rdname utilities
#' @export
#' @examples 
#' get_default("palette")

recode <- function(x, ..., by = x, drop = TRUE) {
  
  funs <- lazyeval::lazy_eval(lazyeval::lazy_dots(...))
  is_logical <- vapply(funs, is.logical, logical(1))
  is_null <- vapply(funs, is.null, logical(1))
  
  # Check input
  if (any(is_null)) {
    null <- names(funs)[is_null]
    stop("Some of the arguments evaluate to NULL:\n", stri_c(null, collapse = ", "), call. = FALSE)
  } else if (length(x) != length(by)) {
    stop("Arguments 'x' and 'by' must be the same length.", call. = FALSE)
  } else if (is.factor(x)) {
    missing <- setdiff(names(funs), levels(x))
    if (length(missing)) {
      stop("Some named arguments do not match levels in the factor:\n",
           stri_c(missing, ", "), call. = FALSE)
    }
  } else if (any(is_logical) && !is.logical(by)) {
    stop("Argument 'by' is not logical, but one or more of the arguments are.", call. = FALSE)
  }
  
  
  # Lowercase and recode
  by <- stri_trans_tolower(by)
  for (nm in names(funs)) {
    
    # If drop is true, keep old values
    if (drop && is.factor(x)) {
      removed <- x[by %in% stri_trans_tolower(funs[[nm]])]
      removed <- unique(removed)
    } else {
      removed <- NULL
    }
    
    # Do the recode
    x[by %in% stri_trans_tolower(funs[[nm]])] <- nm
  }
  
  # Drop levels that have been recoded 
  if (!is.null(removed)) {
    
    # Fix whitespaces
    old <- stri_replace_all(levels(x), " ", regex = "\\s")
    removed <- stri_replace_all(removed, " ", regex = "\\s")
    
    # Replace levles
    new_levels <- setdiff(old, removed)
    x <- factor(x, levels = new_levels)
  }
  
  # Return
  x
  
}

#' @rdname utilities
#' @export
set_missing <- function(df, na_strings = get_default("na_strings")) {
  
  if (inherits(df, "matrix")) df <- as_data_frame(df)
  
  if (all(!is.null(df), nrow(df) > 0L)) {
    df <- mutate_each(df, funs(ifelse(. %in% na_strings, NA, .)))
  }
  
  df
}

#' @rdname utilities
#' @export
clean_score <- function(var) {
  if (is.factor(var)) var <- as.character(var)
  var <- stri_replace(var, replacement = "$1", regex = "([0-1]+).*$")
  suppressWarnings(as.numeric(var))
}

#' @rdname utilities
#' @export
rescale_score <- function(var) {
  stopifnot(!is.factor(var)); if (is.character(var)) var <- as.numeric(var)
  suppressWarnings(ifelse(var %in% 1:10, (as.numeric(var)-1)*(100/9), NA))
}

#' @rdname utilities
#' @export
ordered_replace <- function(x, match_by, replacement = NULL) {
  
  # Make sure a named vector is used if replacement is not specified
  if (is.null(replacement)) {
    
    if (is.null(attr(match_by, "names"))) {
      stop("'match_by' must be a named vector or replacement must be specified.", call. = FALSE)
    } else {
      y <- match_by
    }
  
  } else {
    
    if (length(match_by) == length(replacement)) {
      y <- setNames(match_by, replacement)
    } else {
      stop("'match' and 'replace' must have same length.", call. = FALSE)
    }
  }
    
  # Replace x with values from replace (based on 'match')
  if (any(x %in% y)) {
    x[x %in% y] <- names(y)[match(x, y, nomatch = 0)]
  } 
  
  x
  
}

#' @rdname utilities
#' @export 
intranet_link <- function(https) {
  
  
  if (Sys.info()["sysname"] != "Windows") {
    warning("This function only works with a network drive on windows.", call. = FALSE)
  } else {
    
    # If you are on windows and a http(s) link ends with .se
    if (stri_detect(https, regex = "^https?://.*[^/]\\.se/.*")) {
      domain <- stri_replace(https, "$1", regex = "^https?://(.[^/]*)/.*")
      folder <- stri_replace(https, "$1", regex = paste0(".*", domain, "(.*)"))
      
      https <- stri_c("\\\\", domain, "@SSL/DavWWWRoot", folder)
    }
  }
  
  https
  
}

# MISC -------------------------------------------------------------------------

clean_path <- function(path) {
  
  if (!is.string(path)) {
    stop("Path is not a string (character(1)):\n", path, call. = FALSE)
  } 
  
  # Normalize
  if (!stri_detect(path, regex = "^(/|[A-Za-z]:|\\\\|~)")) {
    path <- normalizePath(path, "/", mustWork = FALSE)
  }
    
  # Remove trailing slashes and return
  stri_replace(path, "", regex = "/$")
  
}

isFALSE <- function(x) identical(x, FALSE)
is.string <- function(x) is.character(x) && length(x) == 1
is.spss <- function(x) any(vapply(x, inherits, what = "labelled", logical(1)))
is.list2 <- function(x) inherits(x, "list")
