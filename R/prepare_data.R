
has_req_cols <- function(idx, lst) {
  all(default$required_cols[[idx]] %in% lst[[idx]])
}

has_req_data <- function(lst) {
  
  df <- any(c("data", "raw data") %in% names(lst))
  mm <- "measurement model" %in% names(lst)
  
  all(df, mm)
}

prepare_data <- function(lst, latents = NULL) {
  
  if (!is.list(lst))
    stop("This function expects an object of type 'list'", call. = FALSE)
  
  # Lowercase list names and see if required data is present
  names(lst) <- tolower(names(lst))
  
  if (!has_req_data(lst))
    stop("The required data was not found in the list", call. = FALSE)
  
  # Also lowercase name of columns in the data
  
  
  if(!is_valid_list)
    stop("This function expects a list with a specific structure. See documentation", 
         call. = FALSE)
  
  
  
}


clean_raw_data <- function(lst, latents = NULL) {
  
  # Check that input is a list
  if (is.list(lst) == FALSE)
    stop("This function expects a list as input", call. = FALSE)
  
  # Check that input is a list and if the required data is present
  req_data <- c("raw data", "measurement model")
  
  
  # Hardcode the required data for this function and see if they exist
  req_data <- c("raw data", "measurement model")
  
  if (!all(req_data %in% lst))
    stop(paste("The required data is missing\n", req_sheets), call. = FALSE)
  
  
  if (length(unique(mm$manifest)) != length(mm$manifest))
    stop("Manifest has duplicated variable names.")
  
  
  
}

prepare_data <- function(lst, latents=NULL) {
  
  # Make sure that input is a list
  if (!is.list(lst))
    stop("This function expects a list as input", call. = FALSE)
  
  # Clean default 'missing values' from all list elements
  lst <- Map(clean_missing, lst)
  
  # Check the input
  if (nrow(df) == 0 || nrow(mm) == 0)
    stop("Measurement model or data have zero rows.")
  
  # Make variable names lowercase for easier referencing.
  names(mm) <- tolower(names(mm))
  
  if (!all(c("latent", "manifest", "text") %in% names(mm)))
    stop("Measurement model does not contain the specified columns.")
  
  mm[, names(mm) %in% c("latent", "manifest")] <- 
    sapply(mm[, names(mm) %in% c("latent", "manifest")], function(x) tolower(x))
  
  # Rename the columns in the dataframe (as lower case)
  if (length(unique(mm$manifest)) != length(mm$manifest))
    stop("Manifest has duplicated variable names.")
  
  extra_vars <- ncol(df) - length(mm$manifest)
  
  if (extra_vars < 0) {
    print("Manifest has specified more variables than exist in the dataset.")
    return(invisible(list("df" = df, "mm" = mm, "entities" = ents)))
  }
  
  if (extra_vars > 0)
    names(df) <- append(names(df)[1:extra_vars], mm$manifest)
  
  if (extra_vars == 0)
    names(df) <- mm$manifest
  
  # Rename entities in data and/or add their respective marketshares
  if (!is.null(ents) && nrow(ents) > 0){
    if (!all(c("entity", "marketshare", "other") %in% names(ents)))
      stop("The entities-sheet does not contain the specified columns.")
    
    names(ents) <- tolower(names(ents))
    df <- weight_entities(df, ents)
  }
  
  # Get list of manifest variables for calculations
  model <- mm[mm$latent %in% latent_names, names(mm) %in% c("latent", "manifest")]
  model$latent <- factor(model$latent, levels=latent_names)
  model <- model[order(model$latent), ]
  
  # Remove text from answers in the scales of model questions
  df <- clean_scales(data = df, variables = model$manifest)
  
  # Convert model questions (that are between 1 and 10) to 100-point scales, 
  # text becomes NA. Looks for columns with names that END with specified 
  # variables.
  df <- rescale_variables(data = df, variables = model$manifest, newcols=T)
  
  # Add latents
  if(!is.null(latents) && tolower(latents) == "pls") message("PLS-PM is not yet implemented.")
  
  # TODO: Use sweep?
  if(!is.null(latents) && tolower(latents) == "mean"){
    for(i in latent_names){
      list_manifest <- paste0(model$manifest[model$latent == i], "EM")
      df[[tolower(i)]] <- rowMeans(df[, names(df) %in% list_manifest], na.rm=T)
    }
  }
  
  # Calculate % missing based on list of latent variables
  df$percent_missing <- apply(df[, names(df) %in% model$manifest], 1, 
                              function(x) sum(!(x %in% 1:10))/length(x))
  
  # Write or return data
  if (write == TRUE) {
    # Write the cleaned data to the sheet 'data'.
    write.xlsx(df, file = "cleaned_data.xlsx", sheet = "data", overwrite = TRUE)
    
  } else {
    
    #Return the dataframe to global environment
    return(invisible(list("df" = df, "mm" = mm, "entities" = ents)))
    
  }
}