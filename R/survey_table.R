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
#'    \item{\code{filter}}{Filters missing values \code{NA}
#'    for all groups and the variables that go into the table, as well as 
#'    \code{percent_missing} which exceeds the cutoff in the surveys config. Setting
#'    \code{filter_missing} to \code{FALSE} overrides the filtering of \code{percent_missing},
#'    while setting \code{filter_response} to \code{FALSE} does the same for filtering of
#'    response variables.}
#'
#'    \item{\code{count}}{For both numeric and factor variables, this function always
#'    provides a count for the number of valid observations (after filtering) in each group.
#'    This count is not weighted. (Only the proportions themselves.)}
#'    
#'    \item{\code{missing}}{Turns implicit missing values into
#'    explicit missing values. When this is the case, counts will be 0 and
#'    the aggregated values will be \code{NA} (unless it is a factor and one of the
#'    other proportions are not NA, in this case, the proportions are set to 0 instead.)}
#'    
#'    \item{\code{spread}}{When \code{wide} is set to \code{TRUE}, the function 
#'    spreads the results to a wide format. For numeric, the variables are put in
#'    separate columns. For factor variables, the proportions are
#'    spread by their respective levels (e.g. "Yes", "No" etc become columns.) 
#'    An exception is made when grouping by several variables and there is only
#'    one response variable (numeric).)}
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
#' @param filter_missing Set to \code{FALSE} to NOT remove observations with \code{percent_missing}
#' above the cutoff that is set in config.
#' @param filter_response When set to \code{TRUE} (default), the function will
#' filter NA values in the response variables (i.e. \code{...}) before counting
#' observations and/or calculating mean or proportions
#' @param contrast Set to \code{FALSE} if a contrast exist but you want to use the study
#' average instead.
#' @author Kristian D. Olsen
#' @export
#' @examples 
#' x %>% group_by(q7_service) %>% survey_table(image:loyal)

survey_table <- function(srv, ..., wide = TRUE, weight = TRUE, question = TRUE, filter_missing = TRUE, filter_response = TRUE, contrast = TRUE) {
  dots <- lazyeval::lazy_dots(...)
  survey_table_(srv, dots = dots, wide = wide, weight = weight, question = question, contrast = contrast)
}

#' @export
#' @rdname survey_table
survey_table_ <- function(srv, dots, wide = TRUE, weight = TRUE, question = TRUE, filter_missing = TRUE, filter_response = TRUE, contrast = TRUE) {
  
  if(!length(dots)) stop("No variables specified.", call. = FALSE)
  
  # Check the input
  if (!is.survey(srv)) {
    stop("Argument 'survey' is not an object with the class 'survey'. See help(survey).", call. = FALSE)
  } else {
    mainentity <- get_association(srv, "mainentity")
    cutoff <- as.numeric(get_config(srv, "cutoff"))
  }
  
  # Extract groups and ungroup
  grouping <- as.character(groups(srv))
  if (is.null(grouping) || !length(grouping)) {
    grouping <- mainentity
    warning("No preexisting groups. Grouping by '", mainentity, "' (mainentity).", call. = FALSE)
  } else {
    mainentity <- grouping[1]
    srv <- ungroup(srv)
  }
  
  # Filter missing and add weights if not found
  if (filter_missing) {
    mt <- lazyeval::lazy_dots(percent_missing <= cutoff)
    srv <- suppressWarnings(filter_(srv, .dots = mt))
  }
  
  # Add weight if it does not exist
  if (!"w" %in% names(srv$df)) {
    srv <- mutate(srv, w = 1L)
  }
  
  # Subset columns
  vars <- select_vars_(names(srv$df), c(grouping, dots))
  if (length(setdiff(vars, mainentity)) == 0L && vars != mainentity) {
    stop("No variables found.", call. = FALSE)
  } else {
    srv <- suppressWarnings(select_(srv, .dots = c(as.character(vars), "w")))
  }
  
  # Get the grouping variable
  entities <- srv$df[[mainentity]]
  entities <- if (is.factor(entities)) levels(entities) else unique(as.character(entities))
  
  # 2x length dataset to produce average as well
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
  
  df <- drop_character_columns(df)
  
  # Get variables and return early if only the mainentity is specified
  vars <- select_vars_(names(df), args = dots)
  if (identical(unname(vars), mainentity)) {
    return(count_(df, vars = vars, wt = quote(w)))
  }
  
  # Make sure only factor or numeric cols were selected
  all_factor <- all(vapply(df[vars], is.factor, logical(1)))
  all_numeric <- all(vapply(df[vars], is.numeric, logical(1)))
  
  if (!all_factor && !all_numeric) {
    stop("All selected columns must either be factor or numeric (no mixing).", call. = FALSE)
  } else if (all_factor) {
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
  if (all_factor) by_group <- c(by_group, "answer")
  
  # Filter missing for grouping variables
  filter_by <- if (filter_response) c(by_group, "answer") else by_group
  nas <- lapply(filter_by, function(x) { lazyeval::interp(~!is.na(y), "y" = as.name(x)) } )
  df <- filter_(df, .dots = nas)

  # Expand data and get count for groups
  df_count <- complete_count(df, by_group)
  
  # Join questions if desired
  if (question) {
    df_count <- add_question_column(df_count, srv)
  }
  
  # Summarise the data and join with df_count
  if (all_numeric) {
    df <- group_by_(df, .dots = by_group)
    df <- summarise_each_(df, funs(weighted.mean(., w = w, na.rm = TRUE)), vars = "answer")
    df <- suppressWarnings(suppressMessages(left_join(df_count, df)))
  } else if (all_factor) {
    df <- count_(df, vars = by_group, wt = quote(w))
    df <- mutate_(df, .dots = lazyeval::lazy_dots(proportion = prop.table(n)))
    df <- select_(df, .dots = lazyeval::lazy_dots(-n))
    df <- suppressWarnings(suppressMessages(left_join(df_count, df)))
    df <- tidyr::replace_na(df, replace = list("proportion" = 0L))
  }

  # Spread 
  if (wide && all_numeric) {
    df <- spread_numeric(df, by_group, drop = TRUE)
  } else if (wide) {
    df <- spread_factor(df, by_group, drop = TRUE)
  }
  
  # Return
  df
  
}

# Utilities --------------------------------------------------------------------

spread_numeric <- function(df, grouping, drop = TRUE) {
  
  # disallow grouping by answer
  grouping <- setdiff(grouping, "answer")
  
  # Figure out the variable to spread by
  if (length(grouping) > 2L && length(unique(df$manifest)) == 1L) {
    # Spread by last group if > 1 group and only 1 numeric
    spread_by <- setdiff(grouping, "manifest")[-1]
    grouping <- setdiff(grouping, spread_by)
  } else {
    spread_by <- "manifest"
  }
  
  # Merge manifest and question (if it exists) when spreading by manifest
  if (spread_by == "manifest" && "question" %in% names(df)) {
    df <- mutate_(df, .dots = lazyeval::lazy_dots(manifest = question))
    df <- select_(df, .dots = lazyeval::lazy_dots(-question))
  } else if (spread_by != "manifest") {
    mt <- lazyeval::lazy_dots(manifest = stri_c(spread_by, " (", manifest, ")"))
    df <- suppressWarnings(mutate_(df, .dots = mt))
  }
  
  # Sum counts
  df <- group_by_(df, .dots = grouping)
  df <- mutate_(df, .dots = lazyeval::lazy_dots(n = sum(n)))
  
  # Convert to factor before spreading
  mt <- lazyeval::lazy_dots(manifest = factor(manifest, levels = unique(manifest), ordered = TRUE))
  df <- mutate_(ungroup(df), .dots = mt)
  
  tidyr::spread_(ungroup(df), key_col = spread_by, value_col = "answer", drop = drop)
  
}

spread_factor <- function(df, grouping, drop = TRUE) {
  
  # Sum counts
  grouping <- setdiff(grouping, "answer")
  df <- group_by_(df, .dots = grouping)
  df <- mutate_(df, .dots = lazyeval::lazy_dots(n = sum(n)))
  
  tidyr::spread_(ungroup(df), key_col = "answer", value_col = "proportion", drop = TRUE)
  
}

complete_count <- function(df, grouping) {
  
  df <- select_(df, .dots = grouping)
  
  # Count and create the completed dataset
  df_count <- count_(df, vars = names(df))
  df_compl <- lapply(df, function(x) { if (is.factor(x)) levels(x) else unique(x) })
  df_compl <- as_data_frame(expand.grid(df_compl))
  
  # Join and clean counts
  df_count <- suppressWarnings(suppressMessages(left_join(df_compl, df_count)))
  df_count <- tidyr::replace_na(df_count, list(n = 0L))
  df_count <- arrange_(df_count, .dots = grouping)
  
  # Return
  df_count
  
}

drop_character_columns <- function(df) {
  
  # Remove character vectors
  is_character <- names(df)[vapply(df, is.character, logical(1))]
  if (length(is_character)) {
    df <- select(df, .dots = lazyeval::lazy_dots(-one_of(is_character)))
    warning("The following columns are character vectors and will not be included:\n",
            stri_c(is_character, collapse = ", "), call. = FALSE)
  }
  
  # Return
  df
  
}

add_question_column <- function(df, srv) {
  
  # Use translations for latents
  srv <- use_latent_translation(srv)
  
  first <- names(df)[1:(which(names(df) == "manifest"))]
  last <- setdiff(names(df), first)
  mm <- select(srv$mm, manifest, question)
  
  # Join and reorder
  df <- suppressWarnings(left_join(df, mm, by = c("manifest" = "manifest")))
  mt <- lazyeval::lazy_dots(question = ifelse(question %in% c(NA, "", " "), manifest, question))
  df <- select_(mutate_(df, .dots = mt), .dots = c(first, "question", last))
  
  # Make manifest a factor and return - converting to factor caused crashes...
  # df <- mutate_(df, .dots = lazyeval::lazy_dots(manifest = as.factor(manifest)))
  df
  
}
