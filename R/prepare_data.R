#' Prepare survey data
#'
#' This function does the imputation/PLS-PM (or simple mean) for the survey data.
#' In addition, it calculates missing percentage and tallies the valid observations
#' for each entity. 
#' 
#' @param survey A survey object.
#' @param type The approach to use for latents. Either \code{mean}, \code{pls},
#' or \code{none} (no latents). If \code{pls} is used, only \code{coderesp} and
#' \code{percent_missing} are added to the data. I.e. it is a preparation for the
#' PLS-wizard.
#' @return Returns the survey with EM-variables and latent scores added using the
#' specified method.
#' @author Kristian D. Olsen
#' @export

prepare_data <- function(survey, type = "mean", cutoff = .3) {
  
  # Check the input
  if (!inherits(survey, "survey")) {
    stop("Argument 'survey' is not an object with the class 'survey'. See help(survey).", call. = FALSE)
  }
  
  if (!inherits(survey$mm, "survey_mm") || !nrow(survey$mm)) {
    stop("The measurement model must be added first. See help(add_mm).", call. = FALSE)
  }
  
  if (!inherits(survey$ents, "survey_ents") || !nrow(survey$ents)) {
    stop("Entities must be added first. See help(add_entities).", call. = FALSE)
  }
  
  if (!inherits(survey$cfg, "survey_cfg") || !nrow(survey$cfg)) {
    stop("The config must be added first. See help(set_config).", call. = FALSE)
  }
  
  # Mainentity must be specified
  if (!any(stri_detect(survey$mm$latent, regex = "mainentity"))) {
    stop("'mainentity' is not specified in latents for the measurement model. 
          See help(set_association).", call. = FALSE)
  } else {
    mainentity <- survey$mm$manifest[stri_trans_tolower(survey$mm$latent) == "mainentity"]
    mainentity <- mainentity[!is.na(mainentity)]
  }
  
  # Check type
  if (!type %in% c("none", "mean", "pls")) {
      stop("Invalid type. Please use 'none', 'mean' or 'pls'.", call. = FALSE)
  } else {
    survey$cfg$value[survey$cfg$config %in% "latents"] <- type
  }
  
  # Check cutoff
  cutoff <- as.numeric(cutoff)
  if (is.na(cutoff) || cutoff > 1 || cutoff < 0) {
    stop("Invalid cutoff. Must be a number between 0 and 1.", call. = FALSE)
  } else {
    survey$cfg$value[survey$cfg$config %in% "cutoff"] <- cutoff
  }

  # Get the model
  model <- survey$mm[stri_trans_tolower(survey$mm$latent) %in% default$latents, ]
  model$latent <- factor(stri_trans_tolower(model$latent), levels = default$latents, ordered = TRUE)
  model$EM <- stri_c(model$manifest, "em")
  
  # Order the model
  model <- model[order(model$latent), ]
  
  # Add an index to the start of the data
  if ("coderesp" %in% names(survey$df)) {
    survey <- set_colnames(survey, coderesp = "coderesp_old")
  }
  
  survey$df <- cbind("coderesp" = 1:nrow(survey$df), survey$df)
  
  # Clean and rescale scores
  survey$df[model$EM] <- lapply(survey$df[model$manifest], clean_score)
  survey$df[model$EM] <- lapply(survey$df[model$EM], rescale_score)

  # Calculate missing percentage and get the cutoff
  survey$df["percent_missing"] <- rowSums(is.na(survey$df[model$EM]))/length(model$EM)
  cutoff <- as.numeric(survey$cfg$value[survey$cfg$config %in% "cutoff"])
  
  # Tally valid observations in entities
  entities <- survey$df[survey$df$percent_missing <= cutoff,][[mainentity]]
  entities <- new_entities(entities)
  
  survey$ents$valid <- entities$valid[match(survey$ents$entity, entities$entity)]
  survey$ents$valid[is.na(survey$ents$valid)] <- 0
  
  # Add latents to the data
  if (type == "mean") {
    survey <- latents_mean(survey, model, cutoff)
  } else if (type == "pls") {
    survey$df <- survey$df[!names(survey$df) %in% model$EM]
  }
  
  # Create a updated measurement model
  vars <- setdiff(names(survey$df), survey$mm$manifest)
  mm <- new_scaffold(default$structure$mm, size = length(vars))
  mm$manifest <- vars; mm$question <- vars; mm$type <- c("int", rep("numeric", length(vars)-1)); mm$latent <- NA
  
  # Replace the measurement model
  survey$mm <- rbind(mm[1, ], survey$mm, mm[2:nrow(mm), ])
  class(survey$mm) <- c("survey_mm", "data.frame")
  
  # Set class and return
  class(survey$df) <- c("survey_df", "tbl_df", "tbl", "data.frame")
  survey
  
}

# Utilities --------------------------------------------------------------------

latents_mean <- function(survey, model, cutoff) {

  for (i in levels(model$latent)) {
    x <- survey$df[survey$df$percent_missing <= cutoff, model$EM[model$latent %in% i], drop = FALSE]
    survey$df[survey$df$percent_missing <= cutoff, i] <- rowMeans(x, na.rm = TRUE)
  }
  
  # Return
  survey
  
}
