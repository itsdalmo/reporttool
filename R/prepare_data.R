#' Prepare survey data
#'
#' This function expects an input file containing raw data. Ideally the input also
#' includes a measurement model and entities w/marketshares, but it attempts to 
#' create these based on the data (to help achieve the desired structure) if they
#' are not found.
#' 
#' @param input Either a path to a file, or a list containing the raw data and/or
#' measurement model/entities.
#' @param latents Specify either 'pls' or 'mean' to calculate latents based on
#' the latent specification in the measurement model. Requires that latents are in
#' fact specified.
#' @param impute Specify whether missing values should be imputed for observations
#' that are retained.
#' @param cutoff The missing-values cutoff; defaults to .3 (30%). Any observations 
#' with a missing percent above this threshold are excluded when imputing missing
#' values and calculating latents.
#' @return A list containing the processed data, measurement model and entities.
#' @author Kristian D. Olsen
#' @export
#' @examples 
#' prepared <- prepare_data("test.xlsx", latents = "pls", impute = TRUE, cutoff = .3)

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
    stop("The config must be added first. See help(add_mm).", call. = FALSE)
  }
  
  # Check type
  if (is.null(type)) type <- "mean"
  if (!type %in% c("mean", "pls")){
    stop("Invalid type. Please chose 'mean' or 'pls'.", call. = FALSE)
  }
  
  # Mainentity must be specified in latents for PLS
  if (type == "pls") {
    if (!any(stri_detect(survey$mm$latent, regex = "mainentity"))) {
      stop("'mainentity' is not specified in latents for the measurement model. 
           See help(set_association).", call. = FALSE)
    } else {
      mainentity <- survey$mm$manifest[stri_trans_tolower(survey$mm$latent) == "mainentity"]
      mainentity <- mainentity[!is.na(mainentity)]
    }
  }

  # Get the model
  model <- survey$mm[stri_trans_tolower(survey$mm$latent) %in% default$latents, ]
  model$latent <- factor(stri_trans_tolower(model$latent), levels = default$latents, ordered = TRUE)
  model$EM <- stri_c(model$manifest, "em")
  
  # Order the model
  model <- model[order(model$latent), ]
  
  # Clean and rescale scores
  survey$df[model$EM] <- lapply(survey$df[model$manifest], clean_score)
  survey$df[model$EM] <- lapply(survey$df[model$EM], rescale_score)
  
  # Calculate missing percentage
  survey$df["percent_missing"] <- rowSums(is.na(survey$df[model$EM]))/length(model$EM)

  # Impute missing
  if (type == "pls") {
    cutoff <- survey$cfg$value[survey$cfg$config %in% "cutoff"]
    imputed <- impute_missing(survey$df, model$EM, cutoff)
    
    if (is.string(imputed)) {
      warning(imputed, call. = FALSE)
      imputed <- FALSE
    } else {
      survey$df[model$EM] <- imputed
    }
  }
  
  # Add latents to the data
  if (type == "mean") {
    survey <- latents_mean(survey, model, mainentity)
  } 
  
  if (type == "pls" && imputed) {
    survey <- latents_pls(survey, model)
  }
  
  # Return
  survey
  
}

# Utilities --------------------------------------------------------------------

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

latents_pls <- function(survey, model, mainentity) {

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

latents_mean <- function(survey, model) {
  
  for (i in levels(model$latent)) {
    survey$df[i] <- rowMeans(survey$df[names(df %in% model$EM[model$latent %in% i])], na.rm = TRUE)
  }
  
  # Return
  survey
  
}
