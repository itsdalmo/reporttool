#' @title Generate report
#' @description Generate one or more reports using the templates in this package.
#' @param report Path to the report template (.Rmd) for a specific study.
#' @param data Optional: path to input.xlsx (if NULL, assumes that this file can 
#' be found in working directory.)
#' @param entity Optional: character vector with one or more entities for generating reports.
#' @return A .Rmd and .pdf file for each entity (in the data, or for each specified).
#' @author Kristian D. Olsen
#' @examples
#' generate_report("/Internal/Internal_report_2014.Rmd", entity=c("EPSI", "SKI"))

#' @export
generate_report <- function(report=NULL, entity=NULL, data=NULL, type="pdf") {
  
  report <- validate_path(report)
  dir <- dirname(report)
  
  if (!file.exists(report)) {
    stop("File not found:\n", report, call. = FALSE)
  }
  
  # See if data is specified correctly, if not; assumes input.xlsx in same folder
  if (is.null(data)) {
    data <- file.path(dir, "input.xlsx")
  } else {
    data <- validate_path(data)
  }
  
  if (!file.exists(data)) {
    stop("File not found: ", data, call. = FALSE)
  }
  
  # Check if the entity is specified, if not - use the mainentity and generate a report for each.
  if (is.null(entity)) {
    message("No entity specified, creating report for all (main) entities in:\n", data)
    
    # Load the data
    mm <- read_sheets(data, sheets="measurement model", clean.missing = TRUE)
    df <- read_sheets(data, sheets="data", clean.missing = TRUE)
    
    # Lowercase variables for easier referencing
    names(mm) <- tolower(names(mm))
    names(df) <- tolower(names(df))
    
    # Get name of mainentity variable
    mainentity <- tolower(mm$manifest[tolower(mm$latent) %in% "mainentity"])
    
    entity <- unique(df[[mainentity]])
  }
  
  # Read in the report template
  md <- readLines(report, encoding="UTF-8")
  
  # Replace date with current date, and data with fixed path
  md <- sub("REPLACE_DATE", format(Sys.Date(), "%Y"), md, fixed=TRUE)
  md <- sub("REPLACE_DATA", data, md, fixed=TRUE)
  
  # Make sure the Reports directory exists
  dir.create(file.path(dir, "Reports"), showWarnings = FALSE)
  
  # Iterate over the entities and create their individual .Rmd files.
  lapply(entity, generate_rmd, md, dir)
  
  # Generate the wanted report-type
  switch(type,
         pdf = lapply(entity, generate_beamer, dir, environment()),
         stop("Please use a supported output format.", call. = FALSE))
  
  invisible()
}

# Report functions -------------------------------------------------------------

generate_rmd <- function(entity, md, dir) {
  
  path <- file(file.path(dir, "Markdown", paste0(entity, ".Rmd")), encoding = "UTF-8")
  on.exit(close(path), add = TRUE)
  
  writeLines(gsub("REPLACE_ENTITY", entity, md, fixed=TRUE), path)
  
}

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
  item_names <- with(cfg$sheet_names, setNames(long, short))
  
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