#' @export

mutate_.survey <- function(survey, ..., .dots) {
  
  # Gather dots and mutate the data
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  survey$df <- dplyr::mutate_(survey$df, .dots = dots)

  # Update mm (overhead)
  cols <- unique(names(dots))
  survey <- update_mm(survey, cols)
  
  # Return
  survey
  
}

#' @export

select_.survey <- function(survey, ..., .dots) {
  
  # Gather dots and apply select to data
  dots <- lazyeval::all_dots(.dots, ...)
  survey$df <- dplyr::select_(survey$df, .dots = dots)
  
  # Get list of renamed variables
  expr <- dots[!is.na(names(dots)) & names(dots) != ""]
  if (length(expr)) {
    nms <- lapply(names(expr), function(nm) { x <- expr[[nm]]$expr; if (x != nm) x })
    nms <- setNames(unlist(nms), names(expr))
  } else {
    nms <- NULL
  }
  
  # Update renames in manifest
  if (!is.null(nms) && length(nms)) {
    survey$mm$manifest <- ordered_replace(survey$mm$manifest, nms, names(nms))
  }
  
  # Subset measurement model
  survey$mm <- filter(survey$mm, manifest %in% names(survey$df))
  class(survey$mm) <- c("survey_mm", "data.frame")
  
  # Return
  survey
  
}

#' @export

filter_.survey <- function(survey, ..., .dots) {
  
  # Gather dots and filter data
  dots <- lazyeval::all_dots(.dots, ...)
  survey$df <- dplyr::filter_(survey$df, .dots = dots)
  
  # Return
  survey
  
}
