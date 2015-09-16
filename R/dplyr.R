#' @export
tbl_vars.survey <- function(survey) names(survey$df)

#' @export
group_by_.survey <- function(survey, ..., .dots, add = FALSE) {
  survey$df <- dplyr::group_by_(survey$df, ..., .dots = .dots, add = add)
  survey
}

#' @export
groups.survey <- function(x) NULL

#' @export
summarise_.survey <- function(survey, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  grps <- unlist(groups(survey$df))
  survey$df <- dplyr::summarise_(survey$df, .dots = dots)
  
  # Subset measurement model
  survey$mm <- filter(survey$mm, manifest %in% c(names(dots), grps))
  class(survey$mm) <- c("survey_mm", class(survey$mm))
  
  survey
}

#' @export
arrange_.survey <- function(survey, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  survey$df <- dplyr::arrange_(survey$df, .dots = dots)
  
  survey
}

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
  
  # Subset measurement model while retaining order
  row_order <- match(names(survey$df), survey$mm$manifest)
  survey$mm <- slice(survey$mm, row_order)
  class(survey$mm) <- c("survey_mm", class(survey$mm))
  
  # Return
  survey
  
}

#' @export
rename_.survey <- function(survey, ..., .dots) {
  
  # Gather dots and apply select to data
  dots <- lazyeval::all_dots(.dots, ...)
  survey$df <- dplyr::rename_(survey$df, .dots = dots)
  
  # Get list of renamed variables
  if (length(dots)) {
    nms <- lapply(dots, function(x) x$expr)
    nms <- setNames(unlist(nms), names(dots))
  } else {
    nms <- NULL
  }
  
  # Update renames in manifest
  if (!is.null(nms) && length(nms)) {
    survey$mm$manifest <- ordered_replace(survey$mm$manifest, nms, names(nms))
  }
  
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
