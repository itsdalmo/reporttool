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
prepare_data <- function(input, rawdata = NULL, latents=NULL, write=TRUE) {
  
  # Read in the data if input is a .xlsx file
  if (inherits(input, "character")) {
    input <- validate_path(input)
    input <- read_data(input)
  }
  
  # Make sure the resulting input is a list
  if (!inherits(input, "list")) {
    stop("Input has to be a list or a .xlsx file", call. = FALSE)
  }
  
  # Change familiar sheetnames to their shorthand version
  item_names <- with(reporttool$sheet_names, setNames(long, short))
  names(input) <- ordered_replace(names(input), item_names, names(item_names))
  
  
  # Check if a valid measurement model was supplied
  has_mm <- all("mm" %in% names(input) && nrow(input$mm) > 0)
  has_mm_cols <- all(reporttool$required_cols$mm %in% names(input$mm))
  
  if (!has_mm) {
    stop("Measurement model was not found", call. = FALSE)
  } else if (!has_mm_cols) {
    stop("Measurement model does not contain the necessary columns", call. = FALSE)
  }
  
  # Check if valid entities have been specified
  has_ents <- all("ents" %in% names(input) && nrow(input$ents) > 0)
  has_ents_cols <- all(reporttool$required_cols$ents %in% names(input$ents))
  
  if (has_ents && !has_ents_cols) {
    stop("Entities contains data but not the correct columns", call. = FALSE)
  }
  
  # Get rawdata if it is given
  if (!is.null(rawdata)) {
    
    if ("df" %in% names(input)) {
      warning("Data already exsists in input and will be replaced", call = FALSE)
    }
    
    # Rawdata can be .xlsx or a data.frame
    if (inherits(rawdata, "character") && has_extension(rawdata, "xlsx")) {
      rawdata <- validate_path(rawdata)
      input[["df"]] <- read_data(rawdata)
    } else if(inherits(rawdata, "data.frame")) {
      input[["df"]] <- rawdata
    } else {
      stop("Unexpected format in argument 'rawdata'", call. = FALSE)
    }
    
  } 
  
  # Check if columnnames are correct
  if (!all(tolower(input$mm$manifest) %in% names(input$df))) {
    warning("Replacing columnnames in data with measurment model", call. = FALSE)
    names(input$df) <- add_modelnames(names(input$df), input$mm$manifest)
  }
  
  # Check if data has weights
  ent_var <- tolower(input$mm$manifest[input$mm$latent %in% "mainentity"])
  
  
  if (!"w" %in% names(input$df)) {
    if (has_ents && length(input$ents$marketshare) == nrow(input$ents)) {
      warning("Adding marketshare-based weights to data", call. = FALSE)
      input$df["w"] <- add_weights(input$df[[ent_var]], input$ents)
    } else {
      warning("Adding natural weights to data", call. = FALSE)
      input$df["w"] <- 1
    }
  }
  
  # Add entities if they do not exist
  if (!has_ents) {
    warning("Adding entities based on data", call. = FALSE)
    input[["ents"]] <- add_entities(input$df[[ent_var]])
  } 

  # Get manifest variables for cleaning and rescaling
  model <- input$mm[tolower(input$mm$latent) %in% reporttool$latent_names, c("latent", "manifest")]
  model$latent <- factor(tolower(model$latent), levels=reporttool$latent_names, ordered= TRUE)
  model$manifest <- tolower(model$manifest)

  # Clean model scales
  input$df[model$manifest] <- lapply(input$df[model$manifest], clean_score)

  # Add EM-variables (rescaled) if they do not already exist
  model$EM <- paste0(model$manifest, "em")

  if (!all(model$EM %in% names(input$df))) {
    warning("Adding EM-variables to data", call. = FALSE)
    input$df[model$EM] <- lapply(input$df[model$manifest], rescale_score)
  }
  
  # Calculate latents if they do not exist
  if (!all(reporttool$latent_names %in% names(input$df))) {
    warning("Adding latents to data", call. = FALSE)
    
    input$df[levels(model$latent)] <- lapply(levels(model$latent), function (i, df, mod) {
      rowMeans(df[mod$EM[mod$latent %in% i]], na.rm=TRUE)
    }, input$df, model)
    
  }
  
  # Add missing-calculation to data
  if (!"percent_missing" %in% names(input$df)) {
    warning("Adding missing-% to data", call. = FALSE)
    
    input$df["percent_missing"] <- apply(input$df[tolower(model$EM)], 1,
                                         function(x) sum(is.na(x))/length(x))
  }

  return(input)
  
}

# Functions for preparing data -------------------------------------------------

add_modelnames <- function(nms, manifest) {
  
  extra_cols <- length(manifest) - length(nms)
  
  if (extra_cols > 0) {
    stop("Manifest contains more variables than the data", call. = FALSE)
  } else if (extra_cols == 0) {
    nms <- tolower(manifest)
  } else {
    nms <- append(nms[1:extra_cols], tolower(manifest))
  }
  
  return(nms)
  
}

add_entities <- function(mainentity) {
  ents <- na.omit(table(input$df$q1))
  ents <- data.frame(ents)
  
  names(ents) <- c("entity", "n")
  ents$marketshare <- ents$n/sum(ents$n)
  ents$other <- rep("No", nrow(ents))
  
  ents
}

add_weights <- function(mainentity, ents) {
  
  obs <- data.frame(table(mainentity))
  obs$ms <- ents$marketshare[match(obs$mainentity, ents$entity)]
  
  obs$w <- obs$Freq/(obs$ms * sum(obs$Freq))
  
  return(obs$w)
}
