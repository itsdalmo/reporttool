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