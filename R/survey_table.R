#' Create tables from surveys
#'
#' Function for creating summary tables of factors and numeric columns in a survey.
#' You can also group the survey by other variables before passing it to \code{survey_table} 
#' to get scores/proportions by one or more variables.
#' 
#' The function does the following:
#' 
#' \describe{
#' 
#'    \item{\code{average}}{Produces an average for the tables, which is always the 
#'    average for the first group (as set by \code{group_by}),
#'    and it is appropriately grouped for the remaining groups.}
#' 
#'    \item{\code{weight}}{Unless otherwise specified, this function will always
#'    weight the results for the average. This also means that the function requires
#'    that the weight column (\code{w}) is present in the data.}  
#' 
#'    \item{\code{filter}}{Always filters missing values \code{NA}
#'    for all groups and the variables that go into the table, as well as 
#'    \code{percent_missing} which exceeds the cutoff in the surveys config.}
#'
#'    \item{\code{count}}{For both numeric and factor variables, this function always
#'    provides a count for the number of valid observations (after filtering) in each group.}
#'    
#'    \item{\code{missing}}{Turns implicit missing values into
#'    explicit missing values. When this is the case, counts will be 0 and
#'    the aggregated values will be \code{NA} (unless it is a factor and one of the
#'    other proportions are not NA, in this case, the proportions are set to 0 instead.)}
#'
#' }
#' 
#' @param srv A survey object.
#' @param ... Columns to summarise. Accepts either numeric or factors, and a 
#' warning will be issued if it encounters text etc. When creating a table for
#' factors, they must all have identical factor levels.
#' @param wide If this is \code{TRUE} (the default), the output will be in a wide format.
#' @param weight When \code{TRUE}, the average will be weighted (assumes weights are in column 'w').
#' @param question When \code{TRUE}, the question text specified in the measurement
#' model will be included in the table (provided they are not empty strings). 
#' @param contrast Set to \code{FALSE} if a contrast exist but you want to use the study
#' average instead.
#' @author Kristian D. Olsen
#' @export
#' @examples 
#' x %>% group_by(q7_service) %>% survey_table(image:loyal)

survey_table <- function(srv, ..., wide = TRUE, weight = TRUE, question = TRUE, contrast = TRUE) {
  dots <- lazyeval::lazy_dots(...)
  survey_table_(srv, .dots = dots, wide = wide, weight = weight, question = question, contrast = contrast)
}

#' @export
#' @rdname survey_table
survey_table_ <- function(srv, ..., .dots, wide = TRUE, weight = TRUE, question = TRUE, contrast = TRUE) {
  
  # Check the input
  if (!is.survey(srv)) {
    stop("Argument 'survey' is not an object with the class 'survey'. See help(survey).", call. = FALSE)
  } else {
    mainentity <- get_association(srv, "mainentity")
    cutoff <- as.numeric(get_config(srv, "cutoff"))
  }
  
  dots <- lazyeval::all_dots(.dots, ...)
  if(!length(dots)) stop("No variables specified.", call. = FALSE)
  
  # Extract groups and ungroup
  grouping <- as.character(groups(srv))
  if (is.null(grouping) || !length(grouping)) {
    grouping <- mainentity
    warning("No preexisting groups. Grouping by '", mainentity, "'.", call. = FALSE)
  } else {
    mainentity <- grouping[1]
    srv <- ungroup(srv)
  }

  # Filter missing and subset columns
  srv <- suppressWarnings(filter_(srv, .dots = lazyeval::lazy_dots(percent_missing <= cutoff)))
  srv <- select_(srv, .dots = c(grouping, "w", dots))
  
  # Get the grouping variable
  entities <- srv$df[[mainentity]]
  entities <- if (is.factor(entities)) levels(entities) else unique(as.character(entities))
  
  # 2x length dataset to produce average as well
  vars <- select_vars_(names(srv$df), c(grouping, dots))
  if (contrast && is.data.frame(srv$cd) && nrow(srv$cd) && all(vars %in% names(srv$cd))) {
    tr <- get_translation(srv, "contrast_average")
    mt <- lazyeval::interp(~x, x = tr)
    df <- bind_rows(srv$df, mutate_(srv$cd, .dots = setNames(list(mt), mainentity)))
  } else {
    tr <- get_translation(srv, "study_average")
    mt <- lazyeval::interp(~x, x = tr)
    df <- bind_rows(srv$df, mutate_(srv$df, .dots = setNames(list(mt), mainentity)))
  }
  
  # Mainentity should be a (ordered) factor variable
  mt <- lazyeval::interp(~factor(x, levels = y, ordered = TRUE), x = as.name(mainentity), y = c(entities, tr))
  df <- mutate_(df, .dots = setNames(list(mt), mainentity))
  
  # Set w to 1 for all rows but the average
  if (weight) {
    df <- mutate_(df, .dots = lazyeval::lazy_dots(w = as.numeric(w)))
    mt <- lazyeval::interp(~ifelse(x == y, w, 1L), x = as.name(mainentity), y = tr)
    df <- mutate_(df, .dots = setNames(list(mt), "w"))
  } else {
    df <- mutate_(df, .dots = lazyeval::lazy_dots(w = 1L))
  }

  # Remove character vectors
  is_character <- names(df)[vapply(df, is.character, logical(1))]
  if (length(is_character)) {
    df <- select(df, .dots = lazyeval::lazy_dots(-one_of(is_character)))
    warning("The following columns are character vectors and will not be included:\n",
            stri_c(is_character, collapse = ", "), call. = FALSE)
  }
  
  # Either factor or numeric
  vars <- select_vars_(names(df), args = dots)

  is_factor <- all(vapply(df[vars], is.factor, logical(1)))
  is_numeric <- all(vapply(df[vars], is.numeric, logical(1)))
  
  if (!all(is_factor) && !all(is_numeric)) {
    stop("All selected columns must either be factor or numeric (no mixing).", call. = FALSE)
  } else if (all(is_factor)) {
    identical_levels <- lapply(df[vars], levels)
    identical_levels <- vapply(identical_levels, identical, y = identical_levels[[1]], logical(1))
    if (!all(identical_levels)) {
      stop("All factor levels must be identical.", call. = FALSE)
    }
  }

  # Gather all variables to a single column
  if (length(vars) == 1L) {
    df <- mutate_(df, .dots = lazyeval::lazy_dots(manifest = vars))
    df <- rename_(df, .dots = setNames(vars, "answer"))
  } else {
    df <- tidyr::gather_(df, "manifest", "answer", vars)
  }
  
  by_group <- c(grouping, "manifest")
  by_group <- if (is_factor) c(by_group, "answer") else by_group

  # Filter missing for grouping variables
  nas <- lapply(by_group, function(x) { lazyeval::interp(~!is.na(y), "y" = as.name(x)) } )
  df <- filter_(df, .dots = nas)
  
  # Expand data and get count for groups
  df_count <- select_(df, .dots = by_group)
  df_count <- ungroup(count_(ungroup(df_count), vars = names(df_count)))
  df_count <- tidyr::complete_(df_count, cols = setdiff(names(df_count), "n"), fill = list("n" = 0))

  # Join questions if desired
  if (question) {
    var_names <- filter(select(srv$mm, manifest, question), manifest %in% df_count$manifest, !question %in% c("", " "))
    missing <- setdiff(vars, var_names$manifest)
    if (length(missing)) {
      message("The following variables had empty 'questions' and will not been replaced:\n",
              stri_c(missing, collapse = ", "))
    }
    df_count <- suppressWarnings(left_join(df_count, select(srv$mm, manifest, question), by = c("manifest" = "manifest")))
    df_count <- select_(df_count, .dots = c(by_group, "question", if (is_factor) "answer" else NULL, "n"))
    df_count <- mutate(df_count, question = ifelse(question %in% c("", " "), manifest, question))
  }

  # Summarise the data and join with df_count
  if (is_numeric) {
    df <- group_by_(df, .dots = by_group)
    df <- summarise_each_(df, funs(weighted.mean(., w = w, na.rm = TRUE)), vars = "answer")
    df <- suppressMessages(left_join(df_count, df))
  } else if (is_factor) {
    df <- count_(df, vars = c(by_group, "answer"), wt = quote(w))
    df <- mutate_(df, .dots = lazyeval::lazy_dots(proportion = prop.table(n), n = sum(n)))
    df <- select_(df, .dots = lazyeval::lazy_dots(-n))
    df <- suppressMessages(left_join(df_count, df))
    df <- tidyr::replace_na(df, replace = list("proportion" = 0))
    
    # Patchwork
    df <- group_by_(df, .dots = by_group)
    mt <- lazyeval::lazy_dots(total = sum(n), proportion = ifelse(total == 0, NA_real_, proportion))
    df <- mutate_(df, .dots = mt)
    df <- select_(df, .dots = lazyeval::lazy_dots(-total))
  }
  
  # Spread 
  if (wide) {
    df <- group_by_(df, .dots = by_group)
    df <- mutate(df, n = sum(n))

    if (is_numeric) {
      if (question) {
        by_group <- setdiff(by_group, "manifest")
        df <- select_(ungroup(df), .dots = lazyeval::lazy_dots(-manifest))
        mt <- lazyeval::lazy_dots(question = factor(question, levels = unique(question), ordered = TRUE))
        df <- mutate_(df, .dots = mt)
        df <- tidyr::spread_(df, key_col = "question", value_col = "answer", drop = TRUE)
      } else {
        df <- tidyr::spread_(ungroup(df), key_col = "manifest", value_col = "answer", drop = TRUE)
      }
    } else {
      df <- tidyr::spread_(ungroup(df), key_col = "answer", value_col = "proportion", drop = TRUE)
    }
    
  }
  
  # Return
  group_by_(df, .dots = by_group)
  
}

