latents_pls <- function(input, ent_var, model, cutoff) {
  
  # For reproducibility we set the seed
  set.seed(1000)
  
  # Subset the data.frame
  df <- input$df[c(ent_var, model$EM, "percent_missing")]
  names(df) <- tolower(names(df))
  
  # Create identifier for a join after analysis
  df$imp_id <- 1:nrow(df)
  mod_data <- df[df$percent_missing <= cutoff, ]
  
  # Get latent names
  manifests <- model$EM
  latents <- cfg$latent_names
  
  model <- list("modes" = rep("A", length(latents)),
                "inner" = cfg$epsi_model,
                "outer" = lapply(latents, function(x, mm) {
                  paste0(tolower(mm$manifest[tolower(mm$latent) %in% x]), "em")}, model))
  
  names(model$outer) <- latents
  
  # Run the analysis for each entity
  list_em_data <- lapply(unique(mod_data[[ent_var]]), function(i, ent_var, df, model) {
    
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
    
  }, ent_var, mod_data, model)
  
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

latents_mean <- function(df, model) {
  
  lapply(levels(model$latent), function(i, df, mod) {
    rowMeans(df[tolower(names(df)) %in% mod$EM[mod$latent %in% i]], na.rm = TRUE)
  }, df, model)
  
}