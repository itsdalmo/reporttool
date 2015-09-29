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
survey_table <- function(srv, ..., entities = NULL, long_format = FALSE, questions = TRUE) {
  
  dots <- lazyeval::lazy_dots(...)
  if(!length(dots)) stop("No variables specified.", call. = FALSE)
  
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
  
  # w and percent missing
  if (!all(c("w", "percent_missing") %in% names(srv$df))) {
    stop("Weight (w) and percent_missing was not found in the data. See help(prepare_data).", call. = FALSE)
  } 
  
  # Cutoff
  cutoff <- as.numeric(filter(srv$cfg, config %in% "cutoff")[["value"]])
  if (is.na(cutoff)) {
    stop("Cutoff was not found. See help(prepare_data).", call. = FALSE)
  } else {
    srv <- suppressWarnings(filter(srv, percent_missing <= cutoff))
  }
  
  # 2x length dataset to produce average as well
  if (is.data.frame(srv$cd) && nrow(srv$cd)) {
    tr <- filter(srv$tr, original == "contrast_average")[["replacement"]]
    df <- bind_rows(srv$df, mutate(srv$cd, mainentity = tr))
  } else {
    tr <- filter(srv$tr, original == "study_average")[["replacement"]]
    df <- bind_rows(srv$df, mutate(srv$df, mainentity = tr))
  }
  
  # Subset data if entities are specified
  if (is.null(entities)) {
    if (is.factor(srv$df$mainentity)) {
      entities <- levels(srv$df$mainentity)
    } else {
      entities <- unique(as.character(srv$df$mainentity))
    }
  } else {
    df <- filter(df, mainentity %in% c(entities, tr))
  }
  
  # Mainentity should be a factor variable
  entities <- entities[stri_order(entities)]
  df <- mutate(df, mainentity = factor(mainentity, levels = c(entities, tr)))
  
  # Set w to 1 for all rows but the average
  df <- mutate(df, w = as.numeric(w))
  df <- mutate(df, w = ifelse(mainentity == tr, w, 1L))

  # Select relevant columns and figure out their types
  df <- select_(df, .dots = c(dots, "mainentity", "w"))

  is_character <- names(df)[vapply(df, is.character, logical(1))]
  if (length(is_character)) {
    warning("The following columns are character vectors and will not be included:\n",
            stri_c(is_character, collapse = ", "), call. = FALSE)
    df <- select(df, -one_of(is_character))
  }
  
  # Either factor or numeric
  is_factor <- vapply(df[, !names(df) %in% c("mainentity", "w")], is.factor, logical(1))
  is_numeric <- vapply(df[, !names(df) %in% c("mainentity", "w")], is.numeric, logical(1))
  
  if (!all(is_factor) && !all(is_numeric)) {
    stop("All selected columns must either be factor or numeric. Mixing does not work.", call. = FALSE)
  } else if (all(is_factor)) {
    equal_levels <- lapply(df[!names(df) %in% c("mainentity", "w")], levels)
    equal_levels <- vapply(equal_levels, identical, y = equal_levels[[1]], logical(1))
    if (!all(equal_levels)) {
      stop("All factor variables should have equal levels (possible values).", call. = FALSE)
    }
  }
  
  # Create tables
  if (all(is_factor)) {
    df <- prop_table(df, srv$mm, long = long_format, questions = questions)
  } else if (all(is_numeric)) {
    df <- score_table(df, srv$mm, long = long_format, questions = questions)
  }
  
  names(df) <- ordered_replace(names(df), setNames(srv$mm$manifest, srv$mm$question))
  names(df) <- ordered_replace(names(df), setNames(srv$tr$original, srv$tr$replacement))
  
  # Return
  df
  
}

prop_table <- function(df, mm, dots, long, questions) {
  
  nms <- setdiff(names(df), c("mainentity", "w"))
  
  if (length(nms) == 1L) {
    df$manifest <- nms
    df$answer <- df[[nms]]
    df <- select(df, mainentity, w, manifest, answer)
  } else {
    df <- tidyr::gather(df, manifest, answer, -mainentity, -w)
  }

  df <- filter(df, !is.na(answer))
  df <- count(df, mainentity, manifest, answer, wt = w)
  df <- mutate(df, prop = prop.table(n))
  
  if (questions) {
    df <- suppressWarnings(left_join(df, mm, by = c("manifest" = "manifest")))
    df <- select(df, mainentity, question, answer, n, prop)
  }
  
  if (!long) {
    df <- mutate(df, n = sum(n))
    df <- tidyr::spread(df, answer, prop, fill = 0, drop = TRUE)
    df <- arrange(df, manifest, mainentity)
  } else {
    df <- arrange(df, manifest, mainentity)
  }
  
  # Return
  df
  
}

score_table <- function(df, mm, long, questions) {
  
  # Subset and summarise
  df <- mutate_each(df, funs(as.numeric(.)), -mainentity)
  df <- group_by(df, mainentity)
  df <- summarise_each(df, funs(weighted.mean(., w = w, na.rm = TRUE)), -w)
  
  # If long format
  if (long) {
    df <- tidyr::gather(df, manifest, score, -mainentity)
    df <- mutate(df, manifest = as.character(manifest))
    df <- left_join(df, mm, by = c("manifest" = "manifest"))
    df <- mutate(df, question = factor(question, levels = unique(question), ordered = TRUE))
    df <- select(df, mainentity, manifest, question, score)
    df <- arrange(df, question, mainentity)
  } else if (questions) {
    mm <- filter(mm, manifest %in% names(df))
    df <- arrange(df, mainentity)
  }
  
  # Return
  df
  
}