#' @export
survey_info <- function(srv, ent) {
  
  # Check the input
  if (!is.survey(srv)) {
    stop("Argument 'survey' is not an object with the class 'survey'. See help(survey).", call. = FALSE)
  }
  
  # Mainentity must be specified in latents
  if (!any(stri_detect(srv$mm$latent, regex = "mainentity"), na.rm = TRUE)) {
    stop("'mainentity' is not specified in latents for the measurement model. See help(set_association).", call. = FALSE)
  } else {
    srv <- prepare_survey(srv)
  }

  # Measurement model must be added first
  if (!is.survey_mm(srv$mm) || !nrow(srv$mm)) {
    stop("The measurement model must be added first. See help(add_mm).", call. = FALSE)
  }
  
  # Entities must be added first
  if (!is.survey_ents(srv$ents) || !nrow(srv$ents)) {
    stop("The entities must be added first. See help(add_entities).", call. = FALSE)
  }
  
  # Add dates if they exist
  if (any(srv$mm$type == "Date", na.rm = TRUE)) {
    
    # If more than one datevariables - use the first one
    var <- filter(srv$mm, type == "Date")[["manifest"]][1]
    var <- filter(srv$df, mainentity == ent)[[var]]
    
    if (!inherits(var, "Date")) stop("Date variable is not actually of type Date.", call. = FALSE)
    
    dates <- data_frame(start = min(var, na.rm = TRUE), end = max(var, na.rm = TRUE))
    dates <- mutate(dates, month = format(start, "%m"),
                           year = format(start, "%Y"),
                           start = format(start, "%e. %b. %Y"),
                           end = format(end, "%e. %b. %Y"))
  } else {
    dates <- NULL
  }
  
  # Add subentities if specified
  if ("subentity" %in% names(srv$df)) {
    cutoff <- as.numeric(filter(srv$cfg, config == "cutoff")[[value]])
    sub <- filter(srv$df, mainentity == ent, percent_missing <= cutoff)
    sub <- group_by(sub, subentity)
    sub <- summarise(sub, valid = n())
    sub <- mutate(valid = stri_c(subentity, valid, sep = " "))
  } else {
    sub <- NULL
  }
  
  # Response information
  resp <- filter(srv$ents, entity %in% ent)
  resp <- select(resp, n, valid)
  resp <- mutate(resp, valid_percent = valid/n)
  
  # Model questions
  questions <- nrow(filter(srv$mm, stri_trans_tolower(latent) %in% default$latents))
  
  # Return
  list(respondents = resp, questions = questions, dates = dates, subentities = sub)
  
}

#' Create tables from surveys
#'
#' Function for creating summary tables of factors and numeric columns in a survey.
#' This function will always group by the mainentity in a survey, and always add
#' the average for the study. You can also group the survey by other variables
#' before passing it to \code{survey_table} to get scores/proportions of an
#' arbitrary variable, grouped gender in addition to company for instance.
#' 
#' @param srv A survey object.
#' @param ... Columns to summarise. All columns must be of the same type, and not
#' text columns. If they are several factor variables, they must have the same levels.
#' @param drop Default is \code{FALSE}, which means unused factor levels will be
#' kept in the data and filled with \code{0} for factors, or \code{NA} for numeric.
#' @param wide If this is \code{TRUE} (the default), the output will be a wide
#' \code{data.frame}. 
#' @param weighted When \code{TRUE}, the average will be weighted.
#' @param questions When \code{TRUE}, the question text specified in the measurement
#' model will be included in the table (if they are not empty strings). 
#' @param contrast Set to false if a contrast exist but you want to use the study
#' average instead.
#' @author Kristian D. Olsen
#' @export
#' @examples 
#' x %>% group_by(q7_service) %>% survey_table(image:loyal)

survey_table <- function(srv, ..., drop = FALSE, wide = TRUE, weighted = TRUE, questions = TRUE, contrast = TRUE) {
  
  dots <- lazyeval::lazy_dots(...)
  if(!length(dots)) stop("No variables specified.", call. = FALSE)
  
  # Check the input
  if (!is.survey(srv)) {
    stop("Argument 'survey' is not an object with the class 'survey'. See help(survey).", call. = FALSE)
  }
  
  # Extract groups and ungroup
  grouping <- groups(srv)
  srv <- ungroup(srv)
  
  # Mainentity must be specified in latents
  if (!any(stri_detect(srv$mm$latent, regex = "mainentity"), na.rm = TRUE)) {
    stop("'mainentity' is not specified in latents for the measurement model. See help(set_association).", call. = FALSE)
  } else {
    srv <- prepare_survey(srv)
  }
  
  # Measurement model must be added first
  if (!is.survey_mm(srv$mm) || !nrow(srv$mm)) {
    stop("The measurement model must be added first. See help(add_mm).", call. = FALSE)
  }
  
  # w and percent missing
  if (!all(c("w", "percent_missing") %in% names(srv$df))) {
    stop("Weight (w) and percent_missing was not found in the data. See help(prepare_data).", call. = FALSE)
  } else {
    cutoff <- as.numeric(get_config(srv, "cutoff"))
    srv <- suppressWarnings(filter(srv, percent_missing <= cutoff))
  }
  
  # Get the entities
  if (is.factor(srv$df$mainentity)) {
    entities <- levels(srv$df$mainentity)
  } else {
    entities <- unique(as.character(srv$df$mainentity))
  }
  
  # 2x length dataset to produce average as well
  vars <- select_vars_(names(srv$df), dots)
  if (contrast && is.data.frame(srv$cd) && nrow(srv$cd) && all(vars %in% names(srv$cd))) {
    tr <- get_translation(srv, "contrast_average")
    df <- bind_rows(srv$df, mutate(srv$cd, mainentity = tr))
  } else {
    tr <- get_translation(srv, "study_average")
    df <- bind_rows(srv$df, mutate(srv$df, mainentity = tr))
  }
  
  # Mainentity should be a (ordered) factor variable
  entities <- entities[stri_order(entities)]
  df <- mutate(df, mainentity = factor(mainentity, levels = c(entities, tr), ordered = TRUE))
  
  # Set w to 1 for all rows but the average
  if (weighted) {
    df <- mutate(df, w = as.numeric(w))
    df <- mutate(df, w = ifelse(mainentity == tr, w, 1L))
  } else {
    df <- mutate(df, w = 1L)
  }
  
  # Subset the data
  df <- select_(df, .dots = c("mainentity", as.character(grouping), "w", dots))

  # Remove character vectors
  is_character <- names(df)[vapply(df, is.character, logical(1))]
  if (length(is_character)) {
    df <- select(df, -one_of(is_character))
    warning("The following columns are character vectors and will not be included:\n",
            stri_c(is_character, collapse = ", "), call. = FALSE)
  }
  
  # Either factor or numeric
  vars <- setdiff(names(df), c("mainentity", "w", as.character(grouping)))
  
  is_factor <- all(vapply(df[vars], is.factor, logical(1)))
  is_numeric <- all(vapply(df[vars], is.numeric, logical(1)))
  
  if (!all(is_factor) && !all(is_numeric)) {
    stop("All selected columns must either be factor or numeric. Mixing does not work.", call. = FALSE)
  } else if (all(is_factor)) {
    identical_levels <- lapply(df[vars], levels)
    identical_levels <- vapply(identical_levels, identical, y = identical_levels[[1]], logical(1))
    if (!all(identical_levels)) {
      stop("All factor variables must have identical levels (possible values).", call. = FALSE)
    }
  }
  
  # Gather all variables to a single column
  vars <- select_vars_(names(df), args = dots)
  if (length(vars) == 1L) {
    df <- mutate_(df, .dots = lazyeval::lazy_dots(manifest = vars))
    df <- rename_(df, .dots = setNames(vars, "answer"))
  } else {
    df <- tidyr::gather_(df, "manifest", "answer", vars)
  }
  
  # Filter missing (also for grouping variables)
  grouping <- as.character(grouping)
  
  groups_na <- c(grouping, "answer")
  groups_na <- lapply(groups_na, function(x) { lazyeval::interp(quote(!is.na(y)), "y" = as.name(x)) } )
  df <- filter_(df, .dots = groups_na)
  
  # Update groups and group_by_
  if (is_numeric) {
    df <- group_by_(df, .dots = c("mainentity", grouping, "manifest"))
    df <- summarise_each_(df, funs(weighted.mean(., w = w, na.rm = TRUE)), vars = "answer")
  } else if (is_factor) {
    df <- count_(df, vars = c("mainentity", grouping, "manifest", "answer"), wt = lazyeval::lazy(w))
    df <- mutate_(df, .dots = lazyeval::lazy_dots(proportion = prop.table(n), n = sum(n)))
  }
  
  # Spread if desired
  if (is_factor && wide) {
    n <- distinct_(ungroup(df), .dots = c("mainentity", grouping))
    n <- select_(n, .dots = c("mainentity", grouping, "n"))
    df <- select_(ungroup(df), .dots = setdiff(names(df), "n"))
    df <- tidyr::spread_(df, "answer", "proportion", fill = 0, drop = drop)
    df <- suppressMessages(left_join(df, n))
    df <- select(df, mainentity, manifest, one_of(grouping), n, everything())
  } else if (wide) {
    df <- tidyr::spread_(ungroup(df), "manifest", "answer", fill = NA, drop = drop)
  }
  
  # Add questions/translate if desired
  if (questions && is_numeric && wide) {
    var_names <- filter(select(srv$mm, manifest, question), manifest %in% vars, !question %in% c("", " "))
    missing <- setdiff(vars, var_names$manifest)
    if (length(missing)) {
      warning("The following variables had empty 'questions' and have not been replaced:\n",
              stri_c(missing, collapse = ", "), call. = FALSE)
    }
    new_names <- setNames(var_names$manifest, var_names$question)
    names(df) <- ordered_replace(names(df), new_names)
  } else if (questions) {
    df <- suppressWarnings(left_join(df, select(srv$mm, manifest, question), by = c("manifest" = "manifest")))
    df <- select(df, mainentity, manifest, question, everything())
  }
  
  # Return
  df
  
}

