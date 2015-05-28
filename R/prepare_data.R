#' @export
prepare_data <- function(input = NULL, rawdata = NULL, latents = NULL, impute = TRUE, write = TRUE) {
  
  # Check latents
  if (is.null(latents)) latents <- "mean"
  
  # Read in the data if input is a .xlsx file
  if (is.character(input) && has_extension(input, "xlsx")) {
    input <- validate_path(input)
    input <- read_data(input)
  } else if (is.null(input)) {
    input <- list("df" = NULL)
  } else if (!inherits(input, "list")) {
    stop("Unexpected format in argument 'input'\n", call. = FALSE)
  }
  
  # Change familiar sheetnames to their shorthand version
  item_names <- with(cfg$sheet_names, setNames(long, short))
  names(input) <- ordered_replace(names(input), item_names, names(item_names))
  
  # Get rawdata if it is given
  if (!is.null(rawdata)) {
    
    if ("df" %in% names(input) && !is.null(input$df)) {
      warning("Data exists in input and will be overwritten\n", call. = FALSE)
    } 
    
    # Rawdata can be .xlsx or a data.frame
    if (is.character(rawdata) && has_extension(rawdata, "xlsx")) {
      rawdata <- validate_path(rawdata)
      input[["df"]] <- read_data(rawdata)
    } else if (inherits(rawdata, "data.frame")) {
      input[["df"]] <- rawdata
    } else {
      stop("Unexpected format in argument 'rawdata'\n", call. = FALSE)
    }
    
  } 
  
  # Create a measurement model from data if necessary --------------------------
  missing_mm <- !all("mm" %in% names(input) && nrow(input$mm) > 0)
  missing_mm_cols <- !all(cfg$required_cols$mm %in% names(input$mm))
  
  if (missing_mm) {
    warning("Measurement model was not found in input, generating suggestion\n", call. = FALSE)
    input[["mm"]] <- add_mm(input$df)
  } else if (missing_mm_cols) {
    stop("Measurement model exists, but does not contain the expected columns\n", call. = FALSE)
  }
  
  # Check if columnnames are correct (manifest added as lower case)
  if (!all(tolower(input$mm$manifest) %in% names(input$df))) {
    warning("Replacing columnnames in data\n", call. = FALSE)
    names(input$df) <- add_modelnames(names(input$df), input$mm$manifest)
  }
  
  # Functions that require latents to be specified in measurement model --------
  contains_latents <- all(cfg$latent_names %in% tolower(input$mm$latent))
  
  if (contains_latents) {
    # Get model scales
    model <- input$mm[tolower(input$mm$latent) %in% cfg$latent_names, c("latent", "manifest")]
    model$latent <- factor(tolower(model$latent), levels=cfg$latent_names, ordered = TRUE)
    model$manifest <- tolower(model$manifest)
    
    # Clean model scales
    input$df[model$manifest] <- lapply(input$df[model$manifest], clean_score)
    
    # Add EM-variables (rescaled) if they do not already exist
    model$EM <- paste0(model$manifest, "em")
    
    if (!all(model$EM %in% tolower(names(input$df)))) {
      warning("Adding rescaled scores to data\n", call. = FALSE)
      input$df[model$EM] <- lapply(input$df[model$manifest], rescale_score)
    }
    
    # Add missing-calculation to data
    if (!"percent_missing" %in% names(input$df)) {
      warning("Adding %-missing to data\n", call. = FALSE)
      
      input$df["percent_missing"] <- apply(input$df[tolower(model$EM)], 1,
                                           function(x) sum(is.na(x))/length(x))
    }
    
    # Impute missing values
    if (any(isTRUE(impute), tolower(latents) == "pls")) {
      warning("Imputing missing values\n", call. = FALSE)
      input$df[model$EM] <- impute_missing(input$df, model$EM)
    } 
    
  }
  
  # Functions that require mainentity to specified in latents (or q1) ----------
  if ("mainentity" %in% tolower(input$mm$latent)) {
    entity_var <- tolower(input$mm$manifest[input$mm$latent %in% "mainentity"])
  } else if ("q1" %in% tolower(input$mm$manifest)) {
    warning("Using 'q1' as mainentity variable\n", call. = FALSE)
    entity_var <- "q1"
  } else {
    entity_var <- character(0)
  }
  
  if (length(entity_var)) {
    # Check if valid entities have been specified
    missing_entity <- all("ents" %in% names(input) && nrow(input$ents) > 0)
    missing_entity_cols <- all(cfg$required_cols$ents %in% names(input$ents))
    
    if (!missing_entity) {
      warning("Entities were not specified in input, generating suggestion\n", call. = FALSE)
      input[["ents"]] <- add_entities(input$df[[ent_var]])
    } else if (!missing_entity_cols) {
      stop("Entities exist, but does not contain the expected columns\n", call. = FALSE)
    }
    
    # Add weights to the data if they do not exist
    missing_w <- !"w" %in% names(input$df)
    
    if (missing_w && length(input$ents$marketshare) == nrow(input$ents)) {
      warning("Adding weights from entities\n", call. = FALSE)
      input$df["w"] <- add_weights(input$df[[ent_var]], input$ents)
    } else if (missing_w) {
      warning("Adding neutral weights to data\n", call. = FALSE)
      input$df["w"] <- 1
    }
    
    
    
  }
  
  # Calculating latents requires that latents are specified -------------------
  if (contains_latents && !all(cfg$latent_names %in% tolower(names(input$df)))) {
    
    # Mean calculation only requires latents
    if (tolower(latents) == "mean") {
      warning("Adding latents (mean) to data\n", call. = FALSE)
      input$df[levels(model$latent)] <- latents_mean(input$df, model)
    
    } else if (tolower(latents) == "pls" && length(ent_var)) {
      warning("Imputing missing and adding latents (pls) to data\n", call. = FALSE)
      input <- latents_pls(input, ent_var, model)
      
    } else {
      stop("Please specify a valid calculation for latents (mean or pls)\n", call. = FALSE)
    }

  }
  
  # Return the processed input with list/sheets in correct order
  input <- input[names(item_names)[names(item_names) %in% names(input)]]
  
  if (isTRUE(write)) {
    # Use long names when writing to .xlsx
    names(input) <- ordered_replace(names(input), names(item_names), item_names)
    write_data(input, "prepared_data.xlsx")
  } else {
    return(input)
  }

}

# Functions for preparing data -------------------------------------------------
impute_missing <- function(df, vars) {
  
  # For reproducibility reasons
  set.seed(1000)
  
  # Subset the data.frame
  names(df) <- tolower(names(df))
  df <- df[c(vars, "percent_missing")]
  
  # Create identifier for a join after analysis
  df$imp_id <- 1:nrow(df)
  
  # Impute missing values
  nvars <- ncol(df[vars])
  bounds <- matrix(c(1:nvars, rep(0, nvars), rep(100, nvars)), ncol=3)
  imp_data <- df[df$percent_missing <= .3, ]
  
  # Capture output due to print usage in Amelia-package
  junk <- capture.output(
    imp_data <- Amelia::amelia(imp_data, 5, bounds = bounds, boot.type="none", idvars = c("percent_missing", "imp_id")))
  
  # Get the imputed dataset
  imp_data <- imp_data$imputations$imp5
  
  # Merge imputed rows with the data and return it
  df[df$imp_id %in% imp_data$imp_id, vars] <- imp_data[vars]
  
  df[vars]
}

latents_pls <- function(input, ent_var, model) {

  # For reproducibility we set the seed
  set.seed(1000)
  
  # Subset the data.frame
  df <- input$df[c(ent_var, model$EM, "percent_missing")]
  names(df) <- tolower(names(df))
  
  # Create identifier for a join after analysis
  df$imp_id <- 1:nrow(df)
  mod_data <- df[df$percent_missing <= .3, ]

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

# Functions for cleaning the data ----------------------------------------------
add_mm <- function(df) {
  
  # Gather data for measurement model
  n <- length(names(df))
  
  mm <- data.frame("latent" = character(n), 
                    "manifest" = names(df),
                    "text" = gsub("\\.", " ", names(df)),
                    "values" = character(n),
                    stringsAsFactors = FALSE)
  
  # If a variable has 9 or less unique values, add possible responses to "values"
  mm$values <- unlist(lapply(df, function(x) {
    x <- unique(x); if (length(x) <= 9) paste0(" \"", x[!is.na(x)], "\"", collapse=",") else NA}))
  
  # Return measurement model
  return(mm)
  
}

add_modelnames <- function(nms, manifest) {
  
  extra_cols <- length(manifest) - length(nms)
  
  if (extra_cols > 0L) {
    stop("Manifest contains more variables than the data", call. = FALSE)
  } else if (extra_cols == 0L) {
    nms <- tolower(manifest)
  } else {
    warning("Data has more variables than manifest, names have been appended", call. = FALSE)
    nms <- append(nms[1:extra_cols], tolower(manifest))
  }
  
  return(nms)
  
}

add_entities <- function(mainentity) {
  
  ents <- na.omit(table(mainentity))
  ents <- data.frame(ents)
  
  names(ents) <- c("entity", "n")
  ents$marketshare <- ents$n/sum(ents$n)
  ents$other <- rep("No", nrow(ents))
  
  return(ents)
}

add_weights <- function(mainentity, ents) {
  
  obs <- data.frame(table(mainentity))
  obs$ms <- ents$marketshare[match(obs$mainentity, ents$entity)]
  
  # Calculate weight and return it
  obs$w <- obs$Freq/(obs$ms * sum(obs$Freq))
  obs$w[match(mainentity, obs$mainentity)]
}
