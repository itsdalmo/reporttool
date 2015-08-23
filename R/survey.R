#' Create a survey object
#'
#' First step in creating a \code{survey} that can be used with \code{generate_report}.
#' The function accepts either a \code{list} or a \code{data.frame} as input, and
#' will only retain information following the structure and naming convention.
#' 
#' @section Structure/naming convention for \code{list}:
#' 
#' \describe{
#' 
#'    \item{\code{df}}{The dataset from the current study, either in it's processed or raw
#'    format. (Alias: \code{data}).}
#' 
#'    \item{\code{cd}}{Optional: A dataset to compare or contrast the current study to.
#'    Typically used when comparing a smaller (web) study to the national study.
#'    (Alias: \code{contrast data}).}  
#' 
#'    \item{\code{hd}}{Optional: Historical data used to compare the present study against.
#'    (Alias: \code{historical data}).} 
#'
#'    \item{\code{ents}}{Data regarding the entities (typically Q1), and their
#'    respecitve marketshares etc. (Alias: \code{entities}).}
#'
#'    \item{\code{mm}}{The measurement model (questionnaire) and additional information
#'    describing the variables in the data. (Alias: \code{measurement model}).}
#'
#'    \item{\code{cfg}}{Config for the study and \code{generate_report}, including
#'    translations/printed names for latent variables. (Alias: \code{config}).}
#' }  
#'
#' A structure and naming convention is also required for the measurement model,
#' entities and config. See \code{\link{add_model}}, \code{\link{add_entities}} and 
#' \code{\link{add_config}} for information.
#'
#' @param x A list or a data.frame
#' @author Kristian D. Olsen
#' @note When using \code{\link{write_data}} with a \code{survey} object, all names will
#' be converted to their longer versions (alias). E.g. "df" becomes "data" and so forth.
#' Conversely, when converting a \code{list} to \code{survey} with \code{survey} or
#' \code{as.survey}, long names are converted to their short version. This is
#' done for brevity when coding.
#' @export
#' @examples 
#' x <- survey(data.frame("test" = 1, stringsAsFactors = FALSE))

survey <- function(x) {
  
  # New scaffolding
  srv <- new_survey()
  
  # Check input
  if (inherits(x, "data.frame")) {
    x <- if(is.sav(x)) flatten_spss(x) else list("df" = x)
  } else if (!inherits(x, "list")) {
    stop("A list or data.frame was expected.", class. = FALSE)
  }
  
  # If input is a list, merge scaffolding with correctly named input
  replace_nms <- setNames(default$structure$sheet, default$structure$survey)
    
  # Replace long names to match scaffolding and look for matches
  names(x) <- ordered_replace(names(x), replace_nms)
  found <- intersect(names(srv), names(x))
  
  # Data cannot be merged or appended (scaffolding contains no names)
  data <- c("df", "hd", "cd")
    
  if (any(data %in% names(x))) {
    data <- intersect(found, data); found <- setdiff(found, data)
    srv[data] <- Map(merge_with_scaffold, srv[data], x[data])
  }
    
  # Only keep the parts that can be appended to the scaffolding
  if (length(found)) {
    srv[found] <- Map(merge_with_scaffold, srv[found], x[found])
  }
    
  
  # Set the class of underlying objects
  for (i in names(srv)) {
    if (i %in% c("mm", "ents")) {
      class(srv[[i]]) <- c(stri_c("survey_", i), "data.frame")
    } else if (inherits(srv[[i]], "data.frame")) {
      class(srv[[i]]) <- c("tbl_df", "tbl", "data.frame")
    }
  }
  
  # Return
  structure(srv, class = c("survey", "list"))
  
}

# Methods ----------------------------------------------------------------------

#' @rdname survey
#' @export
is.survey <- function(x) inherits(x, "survey")

#' @rdname survey
#' @export
as.survey <- function(x) UseMethod("as.survey")

#' @rdname survey
#' @export
as.survey.default <- function(x) survey(x)

#' @rdname survey
#' @export
as.survey.survey <- function(x) x

#' @rdname survey
#' @method print survey
#' @export
print.survey <- function(x, width = getOption("width")) {
  
  cat("Survey\n")
  
  # Class and dimensions of the objects
  info <- lapply(x, function(x) {
    classes <- stri_c("(", class(x)[1], ")", sep = "")
    dimensions <- stri_c("[", stri_c(dim(x), collapse = "x"), "]", sep = "")
    stri_c(classes, dimensions, sep = "")
  })
  
  cat(stri_c(stri_c("$", names(x), sep = ""), info, sep = "\t", collapse = "\n"))
  
}

# Utilities --------------------------------------------------------------------

new_survey <- function() {
  
  # Default scaffolding
  nms <- default$structure$survey
  srv <- vector("list", length(nms))
  
  srv <- lapply(nms, function(nm) { new_scaffold(default$structure[[nm]]) })
  
  # Set default names
  names(srv) <- nms
  
  # Return
  structure(srv, class = c("survey", "list"))
  
}

new_scaffold <- function(nms, size = 0L) {
  
  if (is.null(nms)) return(data.frame())
  
  # Allot a vector for the data.frame
  df <- replicate(length(nms), vector("character", size), simplify = FALSE)
  df <- setNames(df, nms)
  
  # Return the minimum structure
  as.data.frame(df, stringsAsFactors = FALSE)
  
}

merge_with_scaffold <- function(scaffold, x) {
  
  # Return early if scaffolding does not have names (it is data)
  if (!length(names(scaffold)) || is.null(scaffold)) {
    
    is_data <- all(!is.null(x) && is.data.frame(x) && nrow(x))
    if (is_data) return(x) else return(scaffold)
    
  }
  
  # If not, convert x to a list
  x <- as.list(x)
  names(x) <- stri_trans_tolower(names(x))
  
  # Append to existing columns
  result <- lapply(names(scaffold), function(nm, scaffold, x) {
    if (any(names(x) == nm)) x[[nm]] else rep.int(NA, length(x[[1]])) 
    }, scaffold, x)
  
  # Set correct names for result and return as data.frame
  names(result) <- names(scaffold)
  as.data.frame(result, stringsAsFactors = FALSE)
  
}