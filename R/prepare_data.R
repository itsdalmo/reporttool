#' @export
get_input <- function(file, save=TRUE, assign = TRUE, envir = parent.frame()) {
  
  # Validate path
  file <- validate_path(file)
  
  dir <- dirname(file)
  file <- basename(file)
  
  # Path to .Rdata
  rdata <- file.path(dir, gsub(".[a-zA-Z]*$", ".Rdata", file))
  
  # Read .Rdata if it exists
  if (file.exists(rdata)) {
    load(rdata, envir = envir)
  } else {
    lst <- read_data(file.path(dir, file))
    lst <- prepare_input_data(lst)
    
    # See if data should be assigned
    if (assign) {
      assign_input_data(lst, envir)
      
      # Save assigned data if wanted
      if (save) {
        save(list = names(lst), file = rdata, envir = envir)
      }
    } else if (save) {
      stop("Saving data requires that it is also assigned", call. = FALSE)
      
      # If data is not assigned, return it instead  
    } else {
      return(lst)
    }
    
  }
  
}

prepare_input_data <- function(lst) {
  
  if (!inherits(lst, "list")) {
    stop("This function only accepts a list as input", call. = FALSE)
  }
  
  # Rename sheets/listed items
  item_names <- with(reporttool$sheet_names, setNames(long, short))
  
  if (all(item_names %in% names(lst))) {
    names(lst) <- ordered_replace(names(lst), item_names, names(item_names))
  } else {
    stop("The required data is not present in the provided list", call. = FALSE)
  }
  
  survey_data <- which(names(lst) %in% c("df", "cd", "hd"))
  
  # Check if w/weights are present in the data (add if not)
  lst[survey_data] <- lapply(lst[survey_data], function(x) {
    if (nrow(x) > 0 && "w" %in% names(x))  x$w <- as.numeric(x$w)
    if (nrow(x) > 0 && !"w" %in% names(x)) x$w <- 1
    x
  })
  
  # Make groupcolumns easily identifiable
  main <- "mainentity" %in% lst$mm$latent
  sub <- "subentity" %in% lst$mm$latent
  
  if (main) {
    lst[survey_data] <- lapply(lst[survey_data], function(x, mm, rep) {
      names(x)[names(x) %in% tolower(mm$manifest[mm$latent == rep])] <- rep
      x 
    }, mm = lst$mm, rep = "mainentity")
  }
  
  if (sub) {
    lst[survey_data] <- lapply(lst[survey_data], function(x, mm, rep) {
      names(x)[names(x) %in% tolower(mm$manifest[mm$latent == rep])] <- rep 
      x 
    }, mm = lst$mm, rep = "subentity")
  }
  
  return(lst)
  
}

assign_input_data <- function(lst, envir) {
  for (i in names(lst)) {
    assign(i, lst[[i]], envir = envir)
  }
}


#' @export
prepare_data <- function(input, rawdata = NULL, latents = NULL, write=TRUE) {
  
  # Check latents
  if (is.null(latents)) latents <- "mean"
  
  # Read in the data if input is a .xlsx file
  if (is.character(input) && has_extension(input, "xlsx")) {
    input <- validate_path(input)
    input <- read_data(input)
  }
  
  # Make sure the resulting input is a list
  if (!inherits(input, "list")) {
    stop("Input has to be a list or a .xlsx file\n", call. = FALSE)
  }
  
  # Change familiar sheetnames to their shorthand version
  item_names <- with(reporttool$sheet_names, setNames(long, short))
  names(input) <- ordered_replace(names(input), item_names, names(item_names))
  
  # Get rawdata if it is given
  if (!is.null(rawdata)) {
    
    if ("df" %in% names(input)) {
      warning("Data exists in input and will be overwritten\n", call. = FALSE)
    }
    
    # Rawdata can be .xlsx or a data.frame
    if (is.character(rawdata) && has_extension(rawdata, "xlsx")) {
      rawdata <- validate_path(rawdata)
      input[["df"]] <- read_data(rawdata)
    } else if(inherits(rawdata, "data.frame")) {
      input[["df"]] <- rawdata
    } else {
      stop("Unexpected format in argument 'rawdata'\n", call. = FALSE)
    }
    
  } 
  
  # Check if a valid measurement model was supplied
  has_mm <- all("mm" %in% names(input) && nrow(input$mm) > 0)
  has_mm_cols <- all(reporttool$required_cols$mm %in% names(input$mm))
  
  if (!has_mm) {
    warning("Measurement model was not found in input, generating suggestion\n", call. = FALSE)
    input[["mm"]] <- add_mm(input$df)
  } else if (!has_mm_cols) {
    stop("Measurement model exists, but does not contain the expected columns\n", call. = FALSE)
  }
  
  # Identify the mainentity from measurement model (usually Q1)
  if ("mainentity" %in% tolower(input$mm$latent)) {
    ent_var <- tolower(input$mm$manifest[input$mm$latent %in% "mainentity"])
  } else if ("q1" %in% tolower(input$mm$manifest)) {
    warning("Using 'q1' as mainentity variable\n", call. = FALSE)
    ent_var <- "q1"
  } else {
    stop("Please specify the mainentity column in measurement model\n", call. = FALSE)
  }
  
  # Check if valid entities have been specified
  has_ents <- all("ents" %in% names(input) && nrow(input$ents) > 0)
  has_ents_cols <- all(reporttool$required_cols$ents %in% names(input$ents))
  
  if (!has_ents) {
    warning("Entities were not specified in input, generating suggestion\n", call. = FALSE)
    input[["ents"]] <- add_entities(input$df[[ent_var]])
  } else if (!has_ents) {
    stop("Entities exist, but does not contain the expected columns\n", call. = FALSE)
  }
  
  # Check if columnnames are correct
  if (!all(tolower(input$mm$manifest) %in% names(input$df))) {
    warning("Replacing columnnames in data\n", call. = FALSE)
    names(input$df) <- add_modelnames(names(input$df), input$mm$manifest)
  }
  
  # Check if data has weights
  if (!"w" %in% names(input$df)) {
    if (has_ents && length(input$ents$marketshare) == nrow(input$ents)) {
      warning("Adding weights from entities\n", call. = FALSE)
      input$df["w"] <- add_weights(input$df[[ent_var]], input$ents)
    } else {
      warning("Adding neutral weights to data\n", call. = FALSE)
      input$df["w"] <- 1
    }
  }

  # Get manifest variables for cleaning and rescaling
  model <- input$mm[tolower(input$mm$latent) %in% reporttool$latent_names, c("latent", "manifest")]
  model$latent <- factor(tolower(model$latent), levels=reporttool$latent_names, ordered = TRUE)
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
  
  # Calculate latents if they do not exist
  if (!all(reporttool$latent_names %in% tolower(names(input$df)))) {
    if (latents == "mean") {
      warning("Adding latents (mean) to data\n", call. = FALSE)
      input$df[levels(model$latent)] <- latents_mean(input$df, model)
    
    } else if (latents == "pls") {
      warning("Imputing missing and adding latents (pls) to data\n", call. = FALSE)
      input$df[c(model$EM, levels(model$latent))] <- latents_pls(input$df, ent_var, model)
      
    } else {
      stop("Please specify a valid calculation for latents (mean or pls)\n", call. = FALSE)
    }

  }
  
  return(input)
  
}

# Functions for preparing data -------------------------------------------------
latents_pls <- function(df, ent_var, model) {

  # For reproducibility we set the seed
  set.seed(1000)
  
  # Subset the data.frame
  names(df) <- tolower(names(df))
  df <- df[c(ent_var, model$EM, "percent_missing")]
  
  # Create identifier for a join after analysis
  df$imp_id <- 1:nrow(df)
  
  # Impute missing values
  nvars <- ncol(df[model$EM])
  bounds <- matrix(c(1:nvars, rep(0, nvars), rep(100, nvars)), ncol=3)
  
  imp_data <- df[df$percent_missing <= .3, ]
  imp_data <- Amelia::amelia(imp_data, 5, bounds = bounds, boot.type="none", idvars = c(ent_var, "percent_missing", "imp_id"))
  imp_data <- imp_data$imputations$imp5
  
  # Get latent names
  manifests <- model$EM
  latents <- reporttool$latent_names
  
  model <- list("modes" = rep("A", length(latents)),
                "inner" = reporttool$epsi_model,
                "outer" = lapply(latents, function(x, mm) {
                  paste0(tolower(mm$manifest[tolower(mm$latent) %in% x]), "em")}, model))
  
  names(model$outer) <- latents

  # Run the analysis for each entity
  list_em_data <- lapply(unique(imp_data[[ent_var]]), function(i, ent_var, df, model) {
    
    df <- df[df[[ent_var]] == i,]
    em <- plspm::plspm(df, model$inner, model$outer, model$modes, scaled=FALSE, boot.val=TRUE)
    cbind(df, plspm::rescale(em))
    
  }, ent_var, imp_data, model)
  
  imp_data <- do.call('rbind', list_em_data)
  
  # Join the estimated latents with the original dataset and return it
  df[latents] <- NA
  df[df$imp_id %in% imp_data$imp_id, c(manifests, latents)] <- imp_data[c(manifests, latents)]
  
  df[c(manifests, latents)]
  
}

latents_mean <- function(df, model) {
  
  lapply(levels(model$latent), function(i, df, mod) {
    rowMeans(df[tolower(names(df)) %in% mod$EM[mod$latent %in% i]], na.rm = TRUE)
  }, df, model)
  
}

add_mm <- function(df) {
  
  # Gather data for measurement model
  n <- length(names(df))
  
  mm <- data.frame("latent" = character(n), 
                    "manifest" = names(lst$df),
                    "text" = gsub("\\.", " ", names(lst$df)),
                    "values" = character(n),
                    stringsAsFactors = FALSE)
  
  # If a variable has 9 or less unique values, add possible responses to "values"
  mm$values <- lapply(rawdata, function(x) {
    x <- unique(x); if (length(x) <= 9) paste(x[!is.na(x)], collapse=" // ") else NA})
  
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
