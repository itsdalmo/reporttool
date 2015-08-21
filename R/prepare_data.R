# #' Prepare survey data
# #'
# #' This function expects an input file containing raw data. Ideally the input also
# #' includes a measurement model and entities w/marketshares, but it attempts to 
# #' create these based on the data (to help achieve the desired structure) if they
# #' are not found.
# #' 
# #' @param input Either a path to a file, or a list containing the raw data and/or
# #' measurement model/entities.
# #' @param latents Specify either 'pls' or 'mean' to calculate latents based on
# #' the latent specification in the measurement model. Requires that latents are in
# #' fact specified.
# #' @param impute Specify whether missing values should be imputed for observations
# #' that are retained.
# #' @param cutoff The missing-values cutoff; defaults to .3 (30%). Any observations 
# #' with a missing percent above this threshold are excluded when imputing missing
# #' values and calculating latents.
# #' @return A list containing the processed data, measurement model and entities.
# #' @author Kristian D. Olsen
# #' @export
# #' @examples 
# #' prepared <- prepare_data("test.xlsx", latents = "pls", impute = TRUE, cutoff = .3)
# 
# prepare_data <- function(input = NULL, latents = NULL, impute = FALSE, cutoff = .3) {
#   
#   # READY THE DATA -------------------------------------------------------------
#   
#   if (is.character(input) && length(input) == 1L) {
#     input <- validate_path(input)
#     message("Reading data from:\n", input, "\n")
#     input <- read_data(input, codebook = TRUE)
#   } else if (inherits(input, "data.frame")) {
#     # Assumes that the data.frame contains the rawdata
#     input <- list("df" = input)
#   } else if (!inherits(input, "list")) {
#     stop ("input must be a path, data.frame (rawdata) or list\n", call. = FALSE)
#   }
#   
#   # Change familiar names to their shorthand version (data = df, etc.)
#   item_names <- with(cfg$sheet_names, setNames(long, short))
#   names(input) <- ordered_replace(names(input), item_names)
#   
#   # ADD/VALIDATE THE MEASUREMENT MODEL -----------------------------------------
#   
#   if (!all("mm" %in% names(input) && nrow(input$mm) > 0)) {
#     input[["mm"]] <- add_mm(input$df)
#     warning("Measurement model was not found in input, generated a suggestion.", call. = FALSE)
#   } else {
#     input[["mm"]] <- validate_mm(input$df, input$mm)
#   }
#   
#   if (!all(cfg$latent_names %in% tolower(input$mm$latent))) {
#     input$mm$latent <- add_latents(input$mm)
#     warning("Latent association was not specified, attempted to add suggestion.", call. = FALSE)
#   }
#   
#   # CHECK COLUMNNAMES IN DATA --------------------------------------------------
#   
#   if (!all(tolower(input$mm$manifest) %in% names(input$df))) {
#     names(input$df) <- add_modelnames(names(input$df), input$mm$manifest)
#     warning("Replaced columnnames in data.", call. = FALSE)
#   }
#   
#   # ADD EM VARIABLES AND MISSING % ---------------------------------------------
#   contains_latents <- all(cfg$latent_names %in% tolower(input$mm$latent))
#   
#   if (contains_latents) {
#     
#     # Get model scales
#     model <- input$mm[tolower(input$mm$latent) %in% cfg$latent_names, c("latent", "manifest")]
#     model$latent <- factor(tolower(model$latent), levels=cfg$latent_names, ordered = TRUE)
#     model$manifest <- tolower(model$manifest)
#     model$EM <- paste0(model$manifest, "em")
#     
#     # Clean and rescale scores
#     if (!all(model$EM %in% tolower(names(input$df)))) {
#       input$df[model$EM] <- lapply(input$df[model$manifest], clean_score)
#       input$df[model$EM] <- lapply(input$df[model$EM], rescale_score)
#       warning("Added cleaned and rescaled scores to data.", call. = FALSE)
#     }
#     
#     # Add missing-calculation to data
#     if (!"percent_missing" %in% names(input$df)) {
#       input$df["percent_missing"] <- apply(input$df[tolower(model$EM)], 1,
#                                            function(x) sum(is.na(x))/length(x))
#       warning("Added %-missing to data.", call. = FALSE)
#     }
#     
#     # Impute missing values
#     if (any(isTRUE(impute), !is.null(latents) && tolower(latents) == "pls")) {
#       input$df[model$EM] <- impute_missing(input$df, model$EM, cutoff)
#     } 
#     
#   }
#   
#   # ADD ENTITIES ---------------------------------------------------------------
#   
#   if ("mainentity" %in% tolower(input$mm$latent)) {
#     entity_var <- tolower(input$mm$manifest[input$mm$latent %in% "mainentity"])
#   } else {
#     entity_var <- character(0)
#   }
#   
#   if (length(entity_var)) {
#     
#     if (!all("ents" %in% names(input) && nrow(input$ents) > 0)) {
#       input[["ents"]] <- add_entities(input$df[[entity_var]])
#       warning("Entities were not found in input, generated a suggestion.", call. = FALSE)
#     } else {
#       input[["ents"]] <- validate_entities(input$df[[entity_var]], input$ents)
#     }
#     
#     # Add weights to the data if they do not exist
#     if (!"w" %in% names(input$df)) {
#       input$df["w"] <- add_weights(input$df[[entity_var]], input$ents)
#       warning("Added weights from entities.", call. = FALSE)
#     }
#     
#   }
#   
#   # ADD LATENT SCORES ----------------------------------------------------------
#   
#   if (!all(cfg$latent_names %in% tolower(names(input$df))) && contains_latents) {
#     
#     # Mean calculation only requires latents
#     if (!is.null(latents) && tolower(latents) == "mean") {
#       input$df[levels(model$latent)] <- latents_mean(input$df, model)
#       warning("Added latents (mean) to data.", call. = FALSE)
#     
#     } else if (!is.null(latents) && tolower(latents) == "pls" && length(entity_var)) {
#       input <- latents_pls(input, entity_var, model, cutoff)
#       warning("Added latents (pls) to data.", call. = FALSE)
#       
#     } else {
#       warning("Latents were not added (specify 'pls' or 'mean' to add)", call. = FALSE)
#     }
# 
#   }
#   
#   # Return the processed input with list/sheets in correct order
#   input <- input[names(item_names)[names(item_names) %in% names(input)]]
#   
#   input
#   
# }
# 
# # IMPUTE MISSING / CALCULATE LATENTS -------------------------------------------
# 
# impute_missing <- function(df, vars, cutoff) {
#   
#   # For reproducibility reasons
#   set.seed(1000)
#   
#   # Subset the data.frame
#   names(df) <- tolower(names(df))
#   df <- df[c(vars, "percent_missing")]
#   
#   # Create identifier for a join after analysis
#   df$imp_id <- 1:nrow(df)
#   
#   # Impute missing values
#   nvars <- ncol(df[vars])
#   bounds <- matrix(c(1:nvars, rep(0, nvars), rep(100, nvars)), ncol=3)
#   imp_data <- df[df$percent_missing <= cutoff, ]
#   
#   # Capture output due to print usage in Amelia-package
#   junk <- capture.output(
#     imp_data <- Amelia::amelia(imp_data, 5, bounds = bounds, boot.type="none", idvars = c("percent_missing", "imp_id")))
#   
#   # Get the imputed dataset
#   if (!stringi::stri_detect(imp_data$message, regex = "Normal")) {
#     warning("Missing values could not be imputed. Cannot calculate PLS-latents.", call. = FALSE)
#   } else {
#     
#     # Get the imputed data
#     imp_data <- imp_data$imputations$imp5
#     
#     # Merge imputed rows with the data and return it
#     df[df$imp_id %in% imp_data$imp_id, vars] <- imp_data[vars]
#     warning("Imputed missing values.", call. = FALSE)
#   }
# 
#   df[vars]
#   
# }
# 
# latents_pls <- function(input, ent_var, model, cutoff) {
# 
#   # For reproducibility we set the seed
#   set.seed(1000)
#   
#   # Subset the data.frame
#   df <- input$df[c(ent_var, model$EM, "percent_missing")]
#   names(df) <- tolower(names(df))
#   
#   # Create identifier for a join after analysis
#   df$imp_id <- 1:nrow(df)
#   mod_data <- df[df$percent_missing <= cutoff, ]
# 
#   # Get latent names
#   manifests <- model$EM
#   latents <- cfg$latent_names
#   
#   model <- list("modes" = rep("A", length(latents)),
#                 "inner" = cfg$epsi_model,
#                 "outer" = lapply(latents, function(x, mm) {
#                   paste0(tolower(mm$manifest[tolower(mm$latent) %in% x]), "em")}, model))
#   
#   names(model$outer) <- latents
# 
#   # Run the analysis for each entity
#   list_em_data <- lapply(unique(mod_data[[ent_var]]), function(i, ent_var, df, model) {
#     
#     df <- df[df[[ent_var]] == i,]
#     em <- plspm::plspm(df, model$inner, model$outer, model$modes, scaled=FALSE, boot.val=TRUE)
#     
#     em_df <- cbind(df, plspm::rescale(em))
#     em_mm <- as.data.frame(em$path_coefs, stringsAsFactors = FALSE)
#     
#     em_mm <- lapply(Map('[', em_mm), function(x, nm) {
#       out <- setNames(x, nm)
#       out[x != 0L]
#     }, colnames(em_mm))
#     
#     em_mm <- data.frame(as.list(unlist(em_mm)), stringsAsFactors = FALSE)
#     em_mm$entity <- i
#     
#     # Return list
#     list("data" = em_df, "weights" = em_mm)
#     
#   }, ent_var, mod_data, model)
#   
#   mod_data <- do.call('rbind', lapply(list_em_data, '[[', 1))
#   mod_data <- mod_data[order(mod_data$imp_id), ]
#   
#   mod_coefs <- do.call('rbind', lapply(list_em_data, '[[', 2))
#   names(mod_coefs) <- gsub("\\.", "-", names(mod_coefs))
#   
#   # Join the estimated latents with the original dataset
#   input$df[latents] <- NA
#   input$df[df$imp_id %in% mod_data$imp_id, c(manifests, latents)] <- mod_data[c(manifests, latents)]
#   
#   # Add path coefficients to entities and return results
#   input$ents <- merge(input$ents, mod_coefs, by = "entity", all = TRUE)
#   
#   input
#   
# }
# 
# latents_mean <- function(df, model) {
#   
#   lapply(levels(model$latent), function(i, df, mod) {
#     rowMeans(df[tolower(names(df)) %in% mod$EM[mod$latent %in% i]], na.rm = TRUE)
#   }, df, model)
#   
# }
# 
# # ADD MEASUREMENT MODEL --------------------------------------------------------
# 
# # add_mm <- function(df) {
# #   
# #   # Create an empty data.frame
# #   mm <- matrix(rep(NA, ncol(df)*length(cfg$req_structure$mm)), nrow = ncol(df))
# #   mm <- as.data.frame(mm, stringsAsFactors = FALSE)
# #   names(mm) <- cfg$req_structure$mm
# #   
# #   # Extract information from the data
# #   mm$manifest <- names(df)
# #   mm$question <- gsub("\\.", " ", mm$manifest)
# #   mm$type <- vapply(df, class, character(1))
# #   
# #   # Set type and try to determine whether it is a scale
# #   character_vars <- mm$manifest[mm$type %in% "character"]
# #   scale_vars <- unlist(lapply(df[character_vars], function(x) {
# #     n <- length(x); sum(grepl("^[0-9]{1,2}[^0-9][[:alpha:][:punct:] ]*", x)) >= n-1 }))
# #   
# #   # Clean up the scale variable values (only endpoints)
# #   values <- lapply(df[scale_vars], function(x) {
# #     scales <- gsub("^[0-9]{1,2}\\s*=?\\s*([[:alpha:]]*)", "\\1", unique(x))
# #     scales[scales != ""]
# #   })
# #   
# #   # Return if any scale variables were found
# #   if (length(values)) {
# #     mm$values[scale_vars] <- unlist(lapply(values, paste, collapse = "\n"))
# #   }
# #   
# #   # Return
# #   mm
# #   
# # }
# 
# validate_mm <- function(df, mm_org) {
#   
#   # Create a new mm and compare it to the existing
#   mm_new <- add_mm(df)
#   mm_org <- mm_org[vapply(mm_org, function(x) !all(is.na(x)), logical(1))]
#   
#   if (all(names(mm_new) %in% names(mm_org))) {
#     return(mm_org)
#   } else {
#     msg <- "Measurement model did not contain the expected data."
#   }
#   
#   # Reuse existing columns if possible
#   if (any(names(mm_org) %in% names(mm_new))) {
#     
#     existing_cols <- mm_org[names(mm_org) %in% names(mm_new)]
#     
#     if (nrow(mm_org) == nrow(mm_new)) {
#       mm_new[names(mm_new) %in% names(mm_org)] <- existing_cols
#       warning(paste(msg, "Reused existing information."), call. = FALSE)
#     } else if (nrow(mm_org) < nrow(mm_new)) {
#       mm_new[1:nrow(mm_org), names(mm_new) %in% names(mm_org)] <- existing_cols
#       warning(paste(msg, "Appended to existing information."), call. = FALSE)
#     } else if (nrow(mm_org) > nrow(mm_new)) {
#       warning(paste(msg, "Replaced existing model."), call. = FALSE)
#     }
#     
#   }
#   
#   # Return
#   mm_new
#   
# }
# 
# # ADD ENTITIES -----------------------------------------------------------------
# 
# # add_entities <- function(mainentity) {
# #   
# #   ents <- na.omit(table(mainentity))
# #   ents <- as.data.frame(ents, stringsAsFactors = FALSE)
# #   
# #   names(ents) <- c("entity", "n")
# #   ents$marketshare <- ents$n/sum(ents$n)
# #   ents$other <- rep("No", nrow(ents))
# #   
# #   ents
# #   
# # }
# 
# validate_entities <- function(mainentity, ents_org) {
#   
#   # Create a new mm and compare it to the existing
#   ents_new <- add_entities(mainentity)
#   ents_org <- ents_org[vapply(ents_org, function(x) !all(is.na(x)), logical(1))]
#   
#   if (all(names(ents_new) %in% names(ents_org))) {
#     return(ents_org)
#   } else {
#     msg <- "Entities did not contain the expected data."
#   }
#   
#   # Reuse existing columns if possible
#   if (any(names(ents_org) %in% names(ents_new))) {
#     
#     existing_cols <- ents_org[names(ents_org) %in% names(ents_new)]
#     
#     if (nrow(ents_org) == nrow(ents_new)) {
#       ents_new[names(ents_new) %in% names(ents_org)] <- existing_cols
#       warning(paste(msg, "Reused existing information."), call. = FALSE)
#     } else if (nrow(ents_org) < nrow(ents_new)) {
#       ents_new[1:nrow(ents_org), names(ents_new) %in% names(ents_org)] <- existing_cols
#       warning(paste(msg, "Appended to existing information."), call. = FALSE)
#     } else if (nrow(ents_org) > nrow(ents_new)) {
#       warning(paste(msg, "Replaced existing."), call. = FALSE)
#     }
#     
#   }
#   
#   # Return
#   ents_new
#   
# }
# 
# # ADD WEIGHTS ------------------------------------------------------------------
# # 
# # add_weights <- function(mainentity, ents) {
# #   
# #   obs <- as.data.frame(table(mainentity), stringsAsFactors = FALSE)
# #   obs$ms <- ents$marketshare[match(obs$mainentity, ents$entity)]
# #   
# #   # Calculate weight and return it
# #   obs$w <- obs$Freq/(obs$ms * sum(obs$Freq))
# #   obs$w[match(mainentity, obs$mainentity)]
# #   
# # }
# 
# # ADD MODEL NAMES --------------------------------------------------------------
# 
# add_modelnames <- function(nms, manifest) {
#   
#   extra_cols <- length(manifest) - length(nms)
#   
#   if (extra_cols > 0L) {
#     stop("Manifest contains more variables than the data", call. = FALSE)
#   } else if (extra_cols == 0L) {
#     nms <- tolower(manifest)
#   } else {
#     nms <- append(nms[1:extra_cols], tolower(manifest))
#     warning("Data has more variables than manifest, names have been appended.", call. = FALSE)
#   }
#   
#   nms
#   
# }
# 
# # # ADD LATENTS ------------------------------------------------------------------
# # 
# # add_latents <- function(mm) {
# #   
# #   mm$manifest <- tolower(mm$manifest)
# #   
# #   # Add latents if variables have standard names
# #   for (i in names(cfg$latent_association)) {
# #     
# #     vars <- cfg$latent_association[[i]]
# #     
# #     # Match greedily if latent only has one var associated
# #     if (length(vars) == 1L) {
# #       match <- paste0("^", vars, "[[:alpha:]]*$")
# #     } else {
# #       match <- paste0("^", vars, "$", collapse = "|")
# #     }
# #     
# #     mm$latent[grepl(match, mm$manifest) & !grepl("em$", mm$manifest)] <- i
# #     
# #   }
# #   
# #   # Suggest q1 as mainentity if it exists
# #   if ("q1" %in% tolower(mm$manifest)) {
# #     mm$latent[tolower(mm$manifest) %in% "q1"] <- "mainentity"
# #   }
# #   
# #   mm$latent
# #   
# # }
# 
