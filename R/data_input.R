#' Read sheets from xlsx-files to a list.
#'
#' This function (a very thin wrapper for openxlsx functions) which reads all
#' sheets from an .xlsx file to a list of data.frames.
#'
#' @param file The path to the .xlsx file.
#' @param sheets Optional: Specify the sheets to be read.
#' @param clean.missing Set to TRUE to clean typical NA-strings from .xlsx file.
#' @author Kristian D. Olsen
#' @return A list containing data.frames matching the sheets in the .xlsx file.
#' If only one sheet is read, the function returns a data.frame instead.
#' @note This function requires openxlsx.
#' @import openxlsx
#' @export
#' @examples 
#' x <- read_sheets("test.xlsx")

read_sheets <- function(file, sheets = NULL, clean.missing = FALSE) {
  
  file <- validate_path(file)
  
  if (!file.exists(file)) {
    stop("Path does not exist:\n", file, call. = FALSE)
  }
  
  if(!has_extension(file, "xlsx")) {
    stop("The specified path does not direct to a 'xlsx' file:\n", file, call. = FALSE)
  }
  
  # Get the sheetnames to be read
  wb <- get_sheet_names(file)
  
  if (!is.null(sheets)) {
    sheets <- wb[tolower(wb) %in% tolower(sheets)]
  } else {
    sheets <- wb
  }
  
  # Read data to list and set names
  lst <- lapply(sheets, openxlsx::read.xlsx, xlsxFile = file)
  names(lst) <- sheets
  
  # Set all list entries to be data.frame and/or clean NA
  if (clean.missing) {
    lst <- lapply(lst, set_missing)
  }
  
  lst <- lapply(lst, as.data.frame, stringsAsFactors = FALSE)
  
  # If only one sheet was read, return a data.frame instead
  if (length(lst) == 1L) {
    lst <- lst[[1]]
  }
  
  # Return
  return(lst)
}

#' Read common data formats
#'
#' A simple wrapper for reading data. Currently supports
#' csv, csv2 and xlsx. The function also lowercases all list and column names,
#' and cleans common strings for missing values.
#'
#' @param file Path to a csv, csv2 or xlsx file.
#' @param encoding The encoding to use for txt and csv-files.
#' @author Kristian D. Olsen
#' @return A data.frame. If more than one sheet is read from a xlsx file a
#' list is returned instead.
#' @note Reading .xlsx requires that the openxlsx package is installed.
#' @import tools
#' @export
#' @examples 
#' x <- read_data("test.xlsx")

read_data <- function(file, encoding = "UTF-8") {
  
  file <- validate_path(file)
  
  if (!file.exists(file)) {
    stop("Path does not exist:\n", file, call. = FALSE)
  }
  
  if (!is_supported_ext(file)) {
    stop("Path does not direct to a supported format:\n", file, call. = FALSE)
  }
  
  # Pick input-function based on extension
  switch(tools::file_ext(file),
         csv = read_csv(file, encoding),
         xlsx = read_xlsx(file),
         read_dir(file, encoding))
}

# Input wrappers ---------------------------------------------------------------

read_csv <- function(file, encoding) {
  
  if (!has_extension(file, "csv")) {
    stop("The specified path does not direct to a 'csv' file:\n", file, call. = FALSE)
  }
  
  args <- list(file = file, 
               fileEncoding = encoding, 
               stringsAsFactors = FALSE,
               na.strings = reporttool$missing_values)
  
  df <- do.call(read.csv2, args)
 
  # If one column is returned, try comma separated
  if (dim(df)[2] == 1L) {
    df <- do.call(read.csv, args)
  }
  
  # Lowercase names
  names(df) <- tolower(names(df))
  
  return(df)
}

read_xlsx <- function(file) {
  
  df <- read_sheets(file, clean.missing = TRUE)
  
  # Lowercase names
  if (inherits(df, "list")) {
    df <- lapply(df, tolower_cols)
  } 
  
  names(df) <- tolower(names(df))
  
  return(df)
}

read_dir <- function(file, encoding) {
  
  if (!has_extension(file, "")) {
    stop("The specified path is not a directory:\n", file, call. = FALSE)
  }

  stop("Directory input is not yet supported", call. = FALSE)
}