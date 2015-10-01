#' Summaries
#'
#' A few wrappers to make a common tasks less verbose.
#' 
#' @section List of summaries:
#' 
#' \describe{
#' 
#'    \item{\code{survey_info}}{Returns a list of information about the survey
#'    and entity, including respondents, dates of first and last response etc.}
#'    
#'    \item{\code{survey_table}}{Creates a table based on survey data, in either
#'    wide or long format. The function filters missing_percentage based on cutoff,
#'    and includes a weighted average as the last row in the table. It also
#'    replaces columnnames with the question text and/or the specified translations.
#'    Note: The function suppresses warnings on left_join and filter.}
#'
#' }
#' 
#' @name Summaries
#' @author Kristian D. Olsen
#' @rdname summaries
#' @export
#' @examples 
#' survey_info(srv)

survey_info <- function(srv, entity) {
  
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
    var <- filter(mm, type == "Date")[["manifest"]][1]
    var <- filter(srv$df, mainentity == entity)[[var]]
    
    if (!inherits(var, "Date")) stop("Date variable is not actually of type Date.", call. = FALSE)
    
    dates <- data_frame(start = min(var, na.rm = TRUE), end = max(var, na.rm = TRUE))
    dates <- mutate(info$dates, month = format(start, "%m"),
                                year = format(start, "%Y"),
                                start = format(start, "%e. %b. %Y"),
                                end = format(end, "%e. %b. %Y"))
  } else {
    dates <- NULL
  }
  
  # Add subentities if specified
  if ("subentity" %in% names(srv$df)) {
    cutoff <- as.numeric(filter(srv$cfg, config == "cutoff")[[value]])
    sub <- filter(srv$df, mainentity == entity, percent_missing <= cutoff)
    sub <- group_by(sub, subentity)
    sub <- summarise(sub, valid = n())
    sub <- mutate(valid = stri_c(subentity, valid, sep = " "))
  } else {
    sub <- NULL
  }
  
  # Response information
  resp <- filter(srv$ents, entity == entity)
  resp <- select(resp, n, valid)
  resp <- mutate(resp, valid_percent = valid/n)
  
  # Model questions
  questions <- nrow(filter(mm, stri_trans_tolower(latent) %in% default$latents))
  
  # Return
  list(respondents = resp, questions = questions, dates = dates, subentities = sub)
  
}

#' @rdname summaries
#' @export

survey_table <- function(srv, ..., wide = TRUE, weighted = TRUE, questions = TRUE) {
  
  dots <- lazyeval::lazy_dots(...)
  if(!length(dots)) stop("No variables specified.", call. = FALSE)
  
  # Check the input
  if (!is.survey(srv)) {
    stop("Argument 'survey' is not an object with the class 'survey'. See help(survey).", call. = FALSE)
  }
  
  # Extract groups and ungroup
  groups <- groups(srv)
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
  if (is.data.frame(srv$cd) && nrow(srv$cd)) {
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
  df <- select_(df, .dots = c("mainentity", as.character(groups), "w", dots))

  # Remove character vectors
  is_character <- names(df)[vapply(df, is.character, logical(1))]
  if (length(is_character)) {
    df <- select(df, -one_of(is_character))
    warning("The following columns are character vectors and will not be included:\n",
            stri_c(is_character, collapse = ", "), call. = FALSE)
  }
  
  # Either factor or numeric
  vars <- setdiff(names(df), c("mainentity", "w", as.character(groups)))
  
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
  
  # Filter missing
  df <- filter_(df, .dots = lazyeval::lazy_dots(!is.na(answer)))
  
  # Update groups and group_by_
  groups <- c("mainentity", as.character(groups))
  if (is_numeric) {
    df <- group_by_(df, .dots = c(groups, "manifest"))
    df <- summarise_each_(df, funs(weighted.mean(., w = w, na.rm = TRUE)), vars = "answer")
  } else if (is_factor) {
    df <- count_(df, vars = c(groups, "manifest", "answer"), wt = lazyeval::lazy(w))
    df <- mutate_(df, .dots = lazyeval::lazy_dots(proportion = prop.table(n), n = sum(n)))
  }
  
  # Spread if desired
  if (is_factor && wide) {
    df <- tidyr::spread_(df, "answer", "proportion", fill = 0)  
  } else if (wide) {
    df <- tidyr::spread_(df, "manifest", "answer", fill = NA)
  }
  
  # Add questions/translate if desired
  if (questions && is_numeric && wide) {
    var_names <- filter(srv$mm, manifest %in% vars)[c("manifest", "question")]
    new_names <- setNames(var_names$manifest, var_names$question)
    names(df) <- ordered_replace(names(df), new_names)
  } else if (questions) {
    df <- left_join(df, select(srv$mm, manifest, question), by = c("manifest" = "manifest"))
    df <- select(df, mainentity, manifest, question, everything())
  }
  
  # Return
  df
  
}

