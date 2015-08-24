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
    file <- pipe("pbpaste", "rb")
    on.exit(close(file), add = TRUE)
  } else {
    stop("Writing to clipboard is supported only in Windows or OSX")
  }
  
  # Read lines
  lines <- suppressWarnings(readLines(file))
  
  # Workaround for OS X
  if (length(lines) != 1L) {
    lines <- stri_c(lines, collapse = "\n")
  }
  
  # Check if any of the lines contain the sep
  if (any(stri_detect(lines, regex = stri_c("[", sep, "]")))) {
    
    # Open a connection
    con <- textConnection(lines)
    on.exit(close(con), add = TRUE)
    
    # Read as table
    lines <- read.table(con, 
                        header = header,
                        na.strings = default$na_strings,
                        sep = sep,
                        dec = dec,
                        fill = TRUE,
                        colClasses = "character",
                        stringsAsFactors = FALSE)
    
    # Lowercase names
    names(lines) <- stri_trans_tolower(names(lines))
  }
  
  return(lines)
  
}

#' Read common data formats
#'
#' A simple wrapper for reading data. Currently supports Rdata, sav, txt,
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
#' @note When reading xlsx, csv or txt, all columns will be 'character'. For 
#' Rdata it is returned as is. SPSS is also not modified, as it contains labelled variables.
#' @export
#' @examples 
#' x <- read_data("test.xlsx")

read_data <- function(file, sheet = NULL, encoding = "UTF-8") {
  
  file <- clean_path(file)
  
  if (!file.exists(file)) {
    stop("Path does not exist:\n", file, call. = FALSE)
  }
  
  # Pick input-function based on extension
  switch(tolower(tools::file_ext(file)),
         sav = read_spss(file),
         txt = read_txt(file, encoding, header = TRUE),
         csv = read_csv(file, encoding),
         xlsx = read_xlsx(file, sheet),
         rdata = read_rdata(file),
         stop("Unrecognized input format in:\n", file, call. = FALSE))
}


# Input wrappers ---------------------------------------------------------------

read_spss <- function(file) {
  
  df <- haven::read_sav(file)
  names(df) <- stri_trans_tolower(names(df))
  df
  
}

read_rdata <- function(file) {
  
  # Create an empty environment to load the rdata
  lst <- new.env(parent = emptyenv())
  load(file, envir = lst)
  
  # Convert the environment to a list and lowercase names
  lst <- as.list(lst)
  lst <- lapply(lst, lowercase_names)
  
  if (length(lst) == 1L) {
    lst <- lst[[1]]
  }
  
  return(lst)
  
}

read_txt <- function(file, encoding, header) {
  
  args <- list(file = file, 
               header = header,
               fileEncoding = encoding, 
               na.strings = default$na_strings,
               sep = "\t",
               quote = "\"",
               dec = ".",
               comment.char = "",
               fill = TRUE,
               colClasses = "character",
               stringsAsFactors = FALSE)
  
  df <- do.call(utils::read.table, args)
  
  # Lowercase names
  names(df) <- stri_trans_tolower(names(df))
  
  return(df)
}

read_csv <- function(file, encoding) {
  
  args <- list(file = file,
               fileEncoding = encoding,
               na.strings = default$na_strings,
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
  names(df) <- stri_trans_tolower(names(df))
  
  return(df)
}

read_xlsx <- function(file, sheet) {
  
  # Get the sheetnames to be read
  wb <- openxlsx::getSheetNames(file)
  
  if (!is.null(sheet)) {
    sheet <- wb[stri_trans_tolower(wb) %in% stri_trans_tolower(sheet)]
  } else {
    sheet <- wb
  }
  
  # Read data to list and set names
  lst <- lapply(sheet, openxlsx::read.xlsx, xlsxFile = file)
  names(lst) <- sheet
  
  # Set all list entries to be data.frame and/or clean NA. All columns to character
  lst <- lapply(lst, function(x) {
    if (length(x)) vapply(x, as.character, character(nrow(x))) })
  lst <- lapply(lst, set_missing)
  
  # Lowercase names
  if (inherits(lst, "list")) {
    lst <- lapply(lst, lowercase_names)
  } 
  
  names(lst) <- stri_trans_tolower(names(lst))
  
  # If only one sheet was read, return a data.frame instead
  if (length(lst) == 1L) {
    lst <- lst[[1]]
  }
  
  # Return
  return(lst)
  
}
