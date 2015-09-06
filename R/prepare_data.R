#' Prepare survey data
#'
#' This function does the imputation/PLS-PM (or simple mean) for the survey data.
#' In addition, it calculates missing percentage and tallies the valid observations
#' for each entity. 
#' 
#' @param survey A survey object.
#' @param type Either \code{mean}, \code{pls} or \code{NULL} (default) in which
#' case no latents are added.
#' @return Returns the survey with EM-variables and latent scores added using the
#' specified method.
#' @author Kristian D. Olsen
#' @export

prepare_data <- function(survey, type = NULL) {
  
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
  if (!is.null(type)) {
    if (!type %in% c("mean", "pls")) stop("Invalid type. Please choose 'mean' or 'pls'.", call. = FALSE)
  } else {
    type <- "none"
  }
  
  # Update config
  survey$cfg$value[survey$cfg$config %in% "latents"] <- type

  # Get the model
  model <- survey$mm[stri_trans_tolower(survey$mm$latent) %in% default$latents, ]
  model$latent <- factor(stri_trans_tolower(model$latent), levels = default$latents, ordered = TRUE)
  model$EM <- stri_c(model$manifest, "em")
  
  # Order the model
  model <- model[order(model$latent), ]
  
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

  # Impute missing
  if (type == "pls") {
    imputed <- impute_missing(survey$df, model$EM, cutoff)
    
    if (is.character(imputed)) {
      warning(imputed, call. = FALSE)
      imputed <- FALSE
    } else {
      survey$df[model$EM] <- imputed
      imputed <- TRUE
    }
  }
  
  # Add latents to the data
  if (type == "mean") {
    survey <- latents_mean(survey, model, cutoff)
  } 
  
  if (type == "pls" && imputed) {
    survey <- latents_pls(survey, model, mainentity, cutoff)
  }
  
  # Update measurement model
  vars <- setdiff(names(survey$df), survey$mm$manifest)
  mm <- new_scaffold(default$structure$mm, size = length(vars))
  mm$manifest <- vars; mm$question <- vars; mm$type <- "numeric"; mm$latent <- NA
  survey$mm <- rbind(survey$mm, mm)
  
  # Set class and return
  class(survey$df) <- c("survey_df", "tbl_df", "tbl", "data.frame")
  survey
  
}

# Utilities --------------------------------------------------------------------

latents_mean <- function(survey, model, cutoff) {

  for (i in levels(model$latent)) {
    survey$df[survey$df$percent_missing <= cutoff, i] <- rowMeans(
      survey$df[survey$df$percent_missing <= cutoff, model$EM[model$latent %in% i]], na.rm = TRUE)
  }
  
  # Return
  survey
  
}

latents_pls <- function(survey, model, mainentity, cutoff) {
  
  # Set seed (reproducible) and subset data
  set.seed(1000)
  df <- survey$df[c(mainentity, model$EM, "percent_missing")]
  
  # Create identifier for a join after analysis
  df$imp_id <- 1:nrow(df)
  mod_data <- df[df$percent_missing <= cutoff, ]
  
  # Get latent names
  manifests <- model$EM
  latents <- default$latents
  
  model <- list("modes" = rep("A", length(latents)),
                "inner" = default$model,
                "outer" = lapply(latents, function(x, mm) {
                  o <- mm$manifest[stri_trans_tolower(mm$latent) %in% x]
                  stri_c(stri_trans_tolower(o), "em") }, model))
  
  names(model$outer) <- latents
  
  # Run the analysis for each entity
  entities <- unique(mod_data[[mainentity]])
  list_em_data <- lapply(entities, function(i, ent_var, df, model) {
    
    df <- df[df[[ent_var]] == i,]
    em <- plspm::plspm(df, model$inner, model$outer, model$modes, scaled=FALSE, boot.val=TRUE)
    
    em_df <- cbind(df, plspm::rescale(em))
    em_mm <- as.data.frame(em$path_coefs, stringsAsFactors = FALSE)
    
    em_mm <- lapply(Map('[', em_mm), function(x, nm) {
      out <- setNames(x, nm)
      out[x != 0L]
    }, colnames(em_mm))
    
    em_mm <- data.frame(as.list(unlist(em_mm)), stringsAsFactors = FALSE)
    em_mm$entity <- i
    
    # Return list
    list("data" = em_df, "weights" = em_mm)
    
  }, mainentity, mod_data, model)
  
  mod_data <- do.call('rbind', lapply(list_em_data, '[[', 1))
  mod_data <- mod_data[order(mod_data$imp_id), ]
  
  mod_coefs <- do.call('rbind', lapply(list_em_data, '[[', 2))
  names(mod_coefs) <- gsub("\\.", "-", names(mod_coefs))
  
  # Join the estimated latents with the original dataset
  input$df[latents] <- NA
  input$df[df$imp_id %in% mod_data$imp_id, c(manifests, latents)] <- mod_data[c(manifests, latents)]
  
  # Add path coefficients to entities and return results
  input$ents <- merge(input$ents, mod_coefs, by = "entity", all = TRUE)
  
  input
  
}

impute_missing <- function(df, vars, cutoff) {
  
  # Set seed (reproducible)
  set.seed(1000)
  
  # Subset the data.frame
  df <- df[c(vars, "percent_missing")]
  
  # Create identifier for a join after analysis
  df$imp_id <- 1:nrow(df)
  
  # Impute missing values
  nvars <- ncol(df[vars])
  bounds <- matrix(c(1:nvars, rep(0, nvars), rep(100, nvars)), ncol=3)
  imp_data <- df[df$percent_missing <= cutoff, ]
  
  # Capture output due to print usage in Amelia-package
  junk <- capture.output(
    imp_data <- Amelia::amelia(imp_data, 5, bounds = bounds, boot.type="none", 
                               idvars = c("percent_missing", "imp_id"))
    )
  
  # Check message for errors (Not normal)
  if (!stringi::stri_detect(imp_data$message, regex = "Normal")) {
    return(stri_c("Failed to impute missing values:\n", imp_data$message))
  } 
  
  # Get the imputed data and merge
  imp_data <- imp_data$imputations$imp5
  df[df$imp_id %in% imp_data$imp_id, vars] <- imp_data[vars]
  
  # Return
  df[vars]
  
}
