#' Prepare survey data
#'
#' This function does the imputation/PLS-PM (or simple mean) for the survey data.
#' In addition, it calculates missing percentage and tallies the valid observations
#' for each entity. 
#' 
#' @param srv A survey object.
#' @param type The approach to use for latents. Either \code{mean}, \code{pls},
#' or \code{none} (no latents). If \code{pls} is used, only \code{coderesp} and
#' \code{percent_missing} are added to the data. I.e. it is a preparation for the
#' PLS-wizard.
#' @return Returns the survey with EM-variables and latent scores added using the
#' specified method.
#' @author Kristian D. Olsen
#' @export

prepare_data <- function(srv, type = "mean", cutoff = .3) {
  
  # Check the input
  if (!is.survey(srv)) {
    stop("Argument 'srv' is not an object with the class 'survey'. See help(survey).", call. = FALSE)
  }
  
  if (!is.survey_mm(srv$mm) || !nrow(srv$mm)) {
    stop("The measurement model must be added first. See help(add_mm).", call. = FALSE)
  }
  
  if (!is.survey_cfg(srv$cfg) || !nrow(srv$cfg)) {
    stop("The config must be added first. See help(set_config).", call. = FALSE)
  }
  
  # Mainentity must be specified
  if (!any(stri_detect(srv$mm$latent, regex = "mainentity"), na.rm = TRUE)) {
    stop("'mainentity' is not specified in latents for the measurement model. 
          See help(set_association).", call. = FALSE)
  } else {
    mainentity <- filter(srv$mm, stri_trans_tolower(latent) == "mainentity")[["manifest"]]
  }
  
  # Check type
  if (!type %in% c("none", "mean", "pls")) {
      stop("Invalid type. Please use 'none', 'mean' or 'pls'.", call. = FALSE)
  } else {
    srv$cfg$value[srv$cfg$config %in% "latents"] <- type
  }
  
  # Check cutoff
  cutoff <- as.numeric(cutoff)
  if (is.na(cutoff) || cutoff > 1 || cutoff < 0) {
    stop("Invalid cutoff. Must be a number between 0 and 1.", call. = FALSE)
  } else {
    srv$cfg$value[srv$cfg$config %in% "cutoff"] <- cutoff
  }

  # Get the model
  model <- filter(srv$mm, stri_trans_tolower(latent) %in% default$latents)
  model <- mutate(model, latent = factor(stri_trans_tolower(latent), levels = default$latents, ordered = TRUE))
  model <- mutate(model, EM = stri_c(manifest, "em"))
  model <- arrange(model, latent)
  
  # Add an index to the start of the data
  if ("coderesp" %in% names(srv$df)) {
    srv <- rename(srv, coderesp_old = coderesp)
  }
  
  srv <- select(mutate(srv, coderesp = 1:n()), coderesp, everything())

  # Clean and rescale scores
  srv$df[model$EM] <- mutate_each(srv$df[model$manifest], funs(clean_score(.)))
  srv <- mutate_each(srv, funs(rescale_score(.)), one_of(model$EM))

  # Calculate missing percentage
  perc_miss <- rowSums(is.na(srv$df[model$EM]))/length(model$EM)
  srv <- mutate(srv, percent_missing = perc_miss)
  
  # Add latents to the data
  if (type == "mean") {
    srv <- latents_mean(srv, model, cutoff)
  } else if (type == "pls") {
    srv <- select(srv, -one_of(model$EM))
  }
  
  # Always update entities
  srv <- add_entities(srv)
  
  # Set class and return
  srv$df <- as_data_frame(srv$df)
  srv
  
}

# Utilities --------------------------------------------------------------------

latents_mean <- function(srv, model, cutoff) {

  for (i in levels(model$latent)) {
    x <- select(filter(srv$df, percent_missing <= cutoff), one_of(model$EM[model$latent %in% i]))
    srv$df[srv$df$percent_missing <= cutoff, i] <- rowMeans(x, na.rm = TRUE)
  }
  
  # Update measurement model
  srv <- update_mm(srv, levels(model$latent))

  # Return
  srv
  
}

