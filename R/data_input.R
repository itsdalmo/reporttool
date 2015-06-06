#' Read from Windows/OSX clipboard
#'
#' Thin wrapper for reading from windows/OSX clipboards with the most-used defaults.
#' The function first reads in the lines, checks if the delimiter is present in the lines
#' and then converts it to a data.frame. The parameters are passed to this last step (read.table).
#'
#' @param sep The delimiter for columns.
#' @param header If the data contains headers.
#' @param dec Decimal sign
#' @param encoding The encoding to use when writing.
#' @author Kristian D. Olsen
#' @note This function only works on Windows or OSX, and the data-size cannot 
#' exceed 128kb in Windows.
#' @export
#' @examples 
#' x <- from_clipboard()

from_clipboard <- function(sep = "\t", header = TRUE, dec = ".", encoding = "") {
  
  if ((Sys.info()["sysname"] == "Windows")) {
    file <- "clipboard-128"
  } else if (Sys.info()["sysname"] == "Darwin") {
    file <- pipe("pbpaste", "w")
    on.exit(close(file), add = TRUE)
  } else {
    stop("Writing to clipboard is supported only in Windows or OSX")
  }
  
  # Read lines
  lines <- readLines(file)
  
  # Check if any of the lines contain the sep
  if (any(grepl(paste0("[", sep, "]"), lines))) {
    
    # Open a connection
    con <- textConnection(lines)
    on.exit(close(con), add = TRUE)
    
    # Read as table
    lines <- read.table(con, 
                        header = header,
                        na.strings = cfg$missing_values,
                        sep = sep,
                        dec = dec,
                        fill = TRUE,
                        colClasses = "character",
                        stringsAsFactors = FALSE)
    
    # Lowercase names
    names(lines) <- tolower(names(lines))
  }
  
  return(lines)
  
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
#' @param codebook Optional: When reading sav-files, the function returns a list
#' with the data and the SPSS codebook (formatted like a measurement model).
#' @param encoding The encoding to use for txt and csv-files.
#' @author Kristian D. Olsen
#' @return A data.frame. If more than one sheet is read from a xlsx file 
#' (or you are reading a Rdata file) a list is returned instead.
#' @note When reading csv or txt, all columns will be 'character'. For xlsx files
#' the behaviour is more random.
#' @export
#' @examples 
#' x <- read_data("test.xlsx")

read_data <- function(file, sheet = NULL, codebook = FALSE, encoding = "UTF-8") {
  
  file <- validate_path(file)
  
  if (!file.exists(file)) {
    stop("Path does not exist:\n", file, call. = FALSE)
  }
  
  # Pick input-function based on extension
  switch(tolower(tools::file_ext(file)),
         sav = read_spss(file, codebook),
         txt = read_txt(file, encoding, header = TRUE),
         csv = read_csv(file, encoding),
         xlsx = read_xlsx(file, sheet),
         rdata = read_rdata(file),
         stop("Unrecognized input format in:\n", file, call. = FALSE))
}


# Input wrappers ---------------------------------------------------------------

read_spss <- function(file, codebook) {
  
  if (!has_extension(file, "sav")) {
    stop("The specified path does not direct to a 'sav' file:\n", file, call. = FALSE)
  }
  
  df <- haven::read_sav(file)
  
  # Extract label before converting from labelled
  if (codebook) {
    
    # Create an empty data.frame
    mm <- matrix(rep(NA, ncol(df)*length(cfg$req_structure$mm)), nrow = ncol(df))
    mm <- as.data.frame(mm)
    names(mm) <- cfg$req_structure$mm
    
    # Populate mm
    mm$manifest <- names(df)
    mm$question <- lapply(df, attr, which = "label")
    mm$question <- vapply(mm$question, function(x) ifelse(is.null(x), "", x), character(1))
  }
  
  # Convert labelled to factors
  df <- lapply(df, function(x) { if (inherits(x, "labelled")) haven::as_factor(x) else x })
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  
  # Use factor levels to populate values in mm
  if (codebook) {
    # Insert variable type
    mm$type <- vapply(df, class, character(1))
    
    # Extract factor levels
    factor_vars <- lapply(df, levels)
    scale_vars <- unlist(lapply(factor_vars, function(x) sum(grepl("^[0-9]{1,2}.*", x)) == 10L))
    
    # Clean up the scale variable values (only endpoints)
    factor_vars[scale_vars] <- lapply(factor_vars[scale_vars], function(x) {
      scales <- gsub("^[0-9]{1,2}\\s*=?\\s*([[:alnum:]]*)", "\\1", x)
      scales[scales != ""]
    })
    
    # Set type to scale where true and add all values
    mm$type[scale_vars] <- "scale"
    mm$values <- unlist(lapply(factor_vars, paste, collapse = "\n"))
    
  }
  
  # Convert all columns to character, set missing and lowercase names
  df <- vapply(df, as.character, character(nrow(df)))
  df <- set_missing(df)
  names(df) <- tolower(names(df))
  
  # Return
  if (isTRUE(codebook)) {
    lst <- list("df" = df, "mm" = mm)
    return(lst)
  } else {
    return(df)
  }
  
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

read_txt <- function(file, encoding, header) {
  
  if (!has_extension(file, "txt")) {
    stop("The specified path does not direct to a 'txt' file:\n", file, call. = FALSE)
  }
  
  args <- list(file = file, 
               header = header,
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

read_xlsx <- function(file, sheet) {
  
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
