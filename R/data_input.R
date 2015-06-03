#' Extract from a study directory on sharepoint
#'
#' This function extracts whatever data is available in our standard file 
#' structure and tries to structure it like a input.xlsx file.
#' 
#' @param file Path to a study directory on the intranet.
#' @author Kristian D. Olsen
#' @return A list containing the EM-data, entities (observations, marketshare),
#' and a measurement model (latent association, question text, values etc.)
#' @export
#' @examples 
#' x <- from_directory("https://the.intranet.se/EPSI/example")

from_directory <- function(file) {
  
  if (!has_extension(file, "")) {
    stop("The specified path is not a directory:\n", file, call. = FALSE)
  }
  
  # Make https links compatible with windows file explorer
  file <- intranet_link(file)
  
  # Check if the specified directory contains the expected folders
  exp_folders <- c("data", "input", "output")
  dir_content <- list.files(file)
  
  # Create paths for each of the expected folders
  file_dirs <- file.path(file, dir_content[tolower(dir_content) %in% exp_folders])
  
  if (length(file_dirs) == length(exp_folders)) {
    file_dirs <- setNames(file_dirs, exp_folders)
  } else {
    stop("The required (model related) folders were not found in the directory:\n", file, call. = FALSE)
  }
  
  # Read in the dataset if it has been converted to .xlsx
  data_dir <- list.files(file_dirs["data"])
  data_files <- data_dir[grepl(".*em\\.xlsx$", tolower(data_dir))]
  
  if (length(data_files) == 1L) {
    df <- read_data(file.path(file_dirs["data"], data_files))
  } else {
    stop("Make sure that you have converted the data to (one) xlsx file ending with \"EM\"\n", call. = FALSE)
  }
  
  # Add measurement model and entities skeleton based on the data
  mm <- add_mm(df)
  ents <- add_entities(df$q1)
  
  input_exp <- c("config.txt", "measurement model.txt", "qtext.txt")
  input_dir <- list.files(file_dirs["input"])
  input_files <- file.path(file_dirs["input"], input_dir[tolower(input_dir) %in% input_exp])
  
  if (length(input_files) == length(input_exp)) {
    input_files <- setNames(input_files, c("cf", "mm", "qt"))
  } else {
    stop("The required files were not found in the input directory:\n", input_exp, call. = FALSE)
  }
  
  input <- lapply(input_files, read_data, encoding = "latin1")
  names(input) <- names(input_files)
  
  # Convert model to the appropriate format
  input$mm <- unlist(lapply(input$mm[-1], function(x, manifest) {manifest[x == -1, 1]}, input$mm[1]))
  input$mm <- data.frame("latent" = names(input$mm), "manifest" = input$mm, stringsAsFactors = FALSE, row.names = NULL)
  
  input$mm$latent <- gsub("([a-z]+)[0-9]+", "\\1", input$mm$latent)
  
  # Assign latent association to the measurement model (use match in case order differs)
  mm$latent[match(input$mm$manifest, tolower(mm$manifest))] <- input$mm$latent
  
  # Use the same match to assign question text
  mm$text[match(input$mm$manifest, tolower(mm$manifest))] <- input$qt$v1
  
  # Add marketshares to entities
  ents$marketshare <- input$cf$v4[match(input$cf$v2, ents$entity)]
  
  # Combine the results and return them
  input <- list("df" = df, "ents" = ents, "mm" = mm)
  return(input)
  
}

#' Read from Windows/OSX clipboard
#'
#' Thin wrapper for reading from windows/OSX clipboards with the most-used defaults.
#'
#' @param sep The 
#' @param encoding The encoding to use when writing.
#' @author Kristian D. Olsen
#' @note This function only works on Windows or OSX, and the data-size cannot 
#' exceed 128kb in Windows.
#' @export
#' @examples 
#' x <- from_clipboard()

from_clipboard <- function(header = TRUE, sep = "", dec = ".", encoding = "") {
  
  if ((Sys.info()["sysname"] == "Windows")) {
    file <- "clipboard-128"
  } else if (Sys.info()["sysname"] == "Darwin") {
    file <- pipe("pbpaste", "w")
    on.exit(close(file), add = TRUE)
  } else {
    stop("Writing to clipboard is supported only in Windows or OSX")
  }
  
  args <- list(file = file, 
               header = header,
               fileEncoding = encoding, 
               na.strings = cfg$missing_values,
               sep = sep,
               quote = "\"",
               dec = dec,
               comment.char = "",
               fill = TRUE,
               colClasses = "character",
               stringsAsFactors = FALSE)
  
  df <- do.call(utils::read.table, args)
  
  # If there is only one column, return a vector
  if (dim(df)[2] == 1L && !header) {
    df <- unlist(df, use.names = FALSE)
  }
  
  # Lowercase names
  names(df) <- tolower(names(df))
  
  return(df)
  
}

#' Read common data formats
#'
#' A simple wrapper for reading data. Currently supports Rdata, txt,
#' csv, csv2 and xlsx. The function also lowercases all list and column names,
#' and cleans common strings for missing values.
#' 
#'
#' @param file Path to a Rdata, sav (SPSS), txt, csv, csv2 or xlsx file.
#' @param sheet Optional: If you are trying to read a xlsx file, you can also
#' specify which sheets to read.
#' @param encoding The encoding to use for txt and csv-files.
#' @author Kristian D. Olsen
#' @return A data.frame. If more than one sheet is read from a xlsx file 
#' (or you are reading a Rdata file) a list is returned instead.
#' @note When reading csv or txt, all columns will be 'character'. For xlsx files
#' the behaviour is more random.
#' @export
#' @examples 
#' x <- read_data("test.xlsx")

read_data <- function(file, sheet = NULL, sav_mm = FALSE, encoding = "UTF-8") {
  
  file <- validate_path(file)
  
  if (!file.exists(file)) {
    stop("Path does not exist:\n", file, call. = FALSE)
  }
  
  if (!is_supported_ext(file)) {
    stop("Path does not direct to a supported format:\n", file, call. = FALSE)
  }
  
  # Pick input-function based on extension
  switch(tolower(tools::file_ext(file)),
         sav = read_spss(file, sav_mm),
         txt = read_txt(file, encoding),
         csv = read_csv(file, encoding),
         xlsx = read_xlsx(file, sheet),
         rdata = read_rdata(file),
         stop("Unrecognized input format in:\n", file, call. = FALSE))
}


# Input wrappers ---------------------------------------------------------------

read_spss <- function(file, sav_mm = FALSE) {
  
  if (!has_extension(file, "sav")) {
    stop("The specified path does not direct to a 'sav' file:\n", file, call. = FALSE)
  }
  
  df <- haven::read_sav(file)
  
  lst <- lapply(df, function(x) {if (inherits(x, "labelled")) as.character(haven::as_factor(x)) else x })
  lst <- set_missing(lst)
  names(lst) <- tolower(names(lst))
  
  if (isTRUE(sav_mm)) {
    lst <- list("df" = lst, "mm" = add_mm(lst))
    lst$mm$text <- unlist(lapply(df, function(x) { 
      x <- attr(x, "label"); if (is.null(x)) "" else x }))
  }
  
  return(lst)
  
}

read_rdata <- function(file) {
  
  if (!has_extension(file, "rdata")) {
    stop("The specified path does not direct to a 'rdata' file:\n", file, call. = FALSE)
  }
  
  # Create an empty environment to load the rdata
  lst <- new.env(parent = emptyenv())
  load(file, envir = lst)
  
  # Convert the environment to a list
  lst <- as.list(lst)
  
  if (length(lst) == 1L) {
    lst <- lst[[1]]
  }
  
  return(lst)
  
}

read_txt <- function(file, encoding) {
  
  if (!has_extension(file, "txt")) {
    stop("The specified path does not direct to a 'txt' file:\n", file, call. = FALSE)
  }
  
  args <- list(file = file, 
               header = FALSE,
               fileEncoding = encoding, 
               na.strings = cfg$missing_values,
               sep = "\t",
               quote = "\"",
               dec = ".",
               comment.char = "",
               fill = TRUE,
               colClasses = "character",
               stringsAsFactors = FALSE)
  
  df <- do.call(utils::read.table, args)
  
  # If more than one column is returned use header = TRUE
  if (dim(df)[2] > 1L && nrow(df) > 1L) {
    args["header"] <- TRUE
    df <- do.call(utils::read.table, c(args, cfg$input_args$txt))
  }
  
  # Lowercase names
  names(df) <- tolower(names(df))
  
  return(df)
}

read_csv <- function(file, encoding) {
  
  if (!has_extension(file, "csv")) {
    stop("The specified path does not direct to a 'csv' file:\n", file, call. = FALSE)
  }
  
  args <- list(file = file,
               fileEncoding = encoding,
               na.strings = cfg$missing_values,
               header = TRUE, 
               sep = ";", 
               quote = "\"", 
               dec = ",", 
               comment.char = "",
               fill = TRUE, 
               colClasses = "character",
               stringsAsFactors = FALSE)
  
  df <- do.call(utils::read.table, args)
 
  # If only one column is returned - try comma separated
  if (dim(df)[2] == 1L) {
    args[c("sep", "dec")] <- c(",", ".")
    df <- do.call(utils::read.table, args)
  }
  
  # Lowercase names
  names(df) <- tolower(names(df))
  
  return(df)
}

read_xlsx <- function(file, sheet = NULL) {
  
  if(!has_extension(file, "xlsx")) {
    stop("The specified path does not direct to a 'xlsx' file:\n", file, call. = FALSE)
  }
  
  # Get the sheetnames to be read
  wb <- get_sheet_names(file)
  
  if (!is.null(sheet)) {
    sheet <- wb[tolower(wb) %in% tolower(sheet)]
  } else {
    sheet <- wb
  }
  
  # Read data to list and set names
  lst <- lapply(sheet, openxlsx::read.xlsx, xlsxFile = file)
  names(lst) <- sheet
  
  # Set all list entries to be data.frame and/or clean NA
  lst <- lapply(lst, set_missing)
  
  # Lowercase names
  if (inherits(lst, "list")) {
    lst <- lapply(lst, tolower_cols)
  } 
  
  names(lst) <- tolower(names(lst))
  
  # If only one sheet was read, return a data.frame instead
  if (length(lst) == 1L) {
    lst <- lst[[1]]
  }
  
  # Return
  return(lst)
  
}
