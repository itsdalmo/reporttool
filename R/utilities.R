#' Utilities
#'
#' A few wrappers to make a common tasks less verbose.
#' 
#' @section List of utilities:
#' 
#' \describe{
#' 
#'    \item{\code{recode}}{Recode variables without nested ifelse statements. Uses
#'    \code{\%in\%} to create a logical vector when recoding. I.e. you can recode one
#'    vector based on matches in another vector of the same length. For factors,
#'    the function only allows recoding to an existing level unless \code{add} is \code{TRUE}.
#'    You can also set \code{drop = TRUE} to drop levels that you are recoding (away) from,
#'    which can also be used in conjuction with \code{add}. You can also set \code{as_factor}
#'    to \code{TRUE} to convert other types of vectors to factor after recoding - the
#'    values you recode into (argument names) will become the levels for the new factor,
#'    and everything else becomes \code{NA}.} 
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

recode <- function(x, ..., by = x, drop = TRUE, add = FALSE, as_factor = FALSE) {
  
  dots <- lazyeval::lazy_dots(...)
  
  # x and by must be same length
  if (length(x) != length(by)) {
    stop("Arguments 'x' and 'by' must be the same length.", call. = FALSE)
  } else if (!identical(x, by)) {
    # Don't drop levels when recoding by another variable
    drop <- FALSE
  }
    
  # Replace . with by and evaluate subsets
  subsets <- lapply(dots, lazyeval::interp, .values = list(. = by))
  subsets <- lapply(subsets, lazyeval::lazy_eval)

  # Check the arguments
  is_null <- vapply(subsets, is.null, logical(1))
  if (any(is_null)) {
    null <- names(subsets)[is_null]
    stop("Some of the arguments evaluate to NULL:\n", 
         stri_c(null, collapse = ", "), call. = FALSE)
  }

  # Check which vectors do not evaluate to logical, and %in% them.
  subsets <- lapply(subsets, function(x) if (is.character(x) || is.numeric(x)) by %in% x else x)

  # Must be logical
  is_logical <- vapply(subsets, is.logical, logical(1))
  if (any(!is_logical)) {
    not_logical <- names(subsets)[!is_logical]
    stop("Some of the arguments are not boolean (TRUE/FALSE):\n", 
         stri_c(not_logical, collapse = ", "), call. = FALSE)
  }
  
  # For factors, names must match the levels
  if (is.factor(x) && !add) {
    missing <- setdiff(names(subsets), levels(x))
    if (length(missing)) {
      stop("Some named arguments do not match existing factor levels:\n",
           stri_c(missing, ", "), "\n Set add_levels to TRUE to override.", call. = FALSE)
    }
  } 
  
  # Warn if the recodes overlap
  overlap <- unlist(lapply(subsets, which))
  if (length(overlap) != length(unique(overlap))) {
    warning("Values are being recoded multiple times. Check results.", call. = FALSE)
  }

  # Factors require special attention
  old_levels <- levels(x)
  new_levels <- old_levels
  
  # Convert x to character and recode
  x <- as.character(x)
  for (nm in names(subsets)) {
    
    by_subset <- subsets[[nm]]
     
    # Store values that should be added/dropped for factors
    if (!is.null(old_levels)) {
      if (drop && add) {
        new_levels[new_levels %in% x[by_subset]] <- nm
        new_levels <- unique(new_levels)
      } else if (add) {
        new_levels <- union(new_levels, nm)
      } else if (drop) {
        new_levels <- setdiff(new_levels, x[by_subset])
      }
    }
    
    # Do the recode
    x[by_subset] <- nm
    
  }
  
  # Convert to desired output format
  if (!is.null(old_levels)) {
    # I.e., if it WAS a factor
    x <- factor(x, levels = new_levels)
  } else if (as_factor) {
    # Coerce to factor based on recodes
    x <- factor(x, levels = names(subsets))
  }
   
  # Return
  x
  
}

#' @rdname utilities
#' @export
clean_text <- function(var) {
  
  # Remove punctuation in start and trailing "
  var <- stri_replace(as.character(var), "", regex = "^[ [:punct:]]*")
  var <- stri_replace(var, "", regex = "\"$")
  
  # Set zero-length strings to NA and convert to markdown list
  var[var == ""] <- NA
  var <- stri_c("- ", var)
  
  var
  
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
# Adapted from: http://tolstoy.newcastle.edu.au/R/help/04/06/0217.html
collect_warnings <- function(expr) {
  myWarnings <- NULL
  wHandler <- function(w) {
    myWarnings <<- c(myWarnings, list(w))
    invokeRestart("muffleWarning")
  }
  val <- withCallingHandlers(expr, warning = wHandler)
  list(value = val, warnings = myWarnings)
} 

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

filename_no_ext <- function(file)  {
  stri_replace(basename(file), "$1", regex = stri_c("(.*)\\.", tools::file_ext(file), "$"))
}

isFALSE <- function(x) identical(x, FALSE)
is.string <- function(x) is.character(x) && length(x) == 1
is.spss <- function(x) any(vapply(x, inherits, what = "labelled", logical(1)))
is.list2 <- function(x) inherits(x, "list")
