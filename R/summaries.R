#' Summaries
#'
#' A few wrappers to make a common tasks less verbose.
#' 
#' @section List of summaries:
#' 
#' \describe{
#' 
#'    \item{\code{recode}}{Recode variables.} 
#'
#' }
#' 
#' @name Summaries
#' @author Kristian D. Olsen
#' @rdname utilities
#' @export
#' @examples 
#' get_default("palette")

info_list <- function(srv, entity) {
  
  # Check the input
  if (!is.survey(srv)) {
    stop("Argument 'survey' is not an object with the class 'survey'. See help(survey).", call. = FALSE)
  } else {
    srv <- prepare_report(srv)
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

#' @export
score_table <- function(srv, vars, entities = NULL, long_format = TRUE, questions = TRUE) {
  
  # Check the input
  if (!is.survey(srv)) {
    stop("Argument 'survey' is not an object with the class 'survey'. See help(survey).", call. = FALSE)
  } else {
    srv <- prepare_report(srv)
  }
  
  # Measurement model must be added first
  if (!is.survey_mm(srv$mm) || !nrow(srv$mm)) {
    stop("The measurement model must be added first. See help(add_mm).", call. = FALSE)
  }
  
  # Subset data if entities are specified
  if (is.null(entities)) {
    if (is.factor(srv$df$mainentity)) {
      entities <- levels(srv$df$mainentity)
    } else {
      entities <- unique(srv$df$mainentity)
    }
  }
  
  df <- filter(srv$df, mainentity %in% entities)

  # Use contrast data if it exists
  if (is.data.frame(srv$cd) && nrow(srv$cd)) {
    tr <- filter(srv$tr, original == "contrast_average")[["replacement"]]
    df <- bind_rows(df, mutate(srv$cd, mainentity = tr))
  } else {
    tr <- filter(srv$tr, original == "study_average")[["replacement"]]
    df <- bind_rows(df, mutate(srv$df, mainentity = tr))
  }
  
  # Subset and summarise
  df <- select(df, mainentity, one_of(vars), w)
  df <- mutate_each(df, funs(as.numeric(.)), -mainentity)
  df <- group_by(df, mainentity)
  df <- summarise_each(df, funs(weighted.mean(., w = w, na.rm = TRUE)), -w)
  df <- mutate(df, mainentity = factor(mainentity, levels = c(entities, tr), ordered = TRUE))
  
  # If long format
  if (long_format) {
    df <- tidyr::gather(df, manifest, score, -mainentity)
    df <- mutate(df, manifest = as.character(manifest))
    df <- left_join(df, srv$mm, by = c("manifest" = "manifest"))
    df <- mutate(df, question = factor(question, levels = unique(question), ordered = TRUE))
    df <- select(df, mainentity, manifest, question, score)
    df <- arrange(df, question, mainentity)
  } else if (questions) {
    mm <- filter(srv$mm, manifest %in% names(df))
    df <- arrange(df, mainentity)
    names(df) <- ordered_replace(names(df), setNames(mm$manifest, mm$question))
  }

  # Return
  df
  
}