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

read_data <- function(file, encoding = "latin1") {
  
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

#' Read sheets from xlsx-files to a list.
#'
#' This function (a very thin wrapper for openxlsx functions) which reads all
#' sheets from an .xlsx file to a list of data.frames.
#'
#' @param file The path to the .xlsx file.
#' @param sheets Optional: Specify the sheets to be read.
#' @param clean.na Set to TRUE to clean typical NA-strings from .xlsx file.
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

  # Load workbook
  wb <- openxlsx::loadWorkbook(file)
  sh <- openxlsx::sheets(wb)
  
  # If sheets are specified, read only these
  if (!is.null(sheets)) {
    sh <- sh[tolower(sh) %in% tolower(sheets)]
  }
  
  # Check if any/specified sheets exist
  if (length(sh) == 0L) {
    stop("The specified sheets were not found in the workbook", call. = FALSE)
  }

  # Read data to list and set names
  lst <- suppressWarnings(lapply(sh, openxlsx::readWorkbook, xlsxFile = wb))
  names(lst) <- sh
  
  # Set all list entries to be data.frame and/or clean NA
  if (isTRUE(clean.missing)) {
    lst <- lapply(lst, clean_missing)
  }
  
  lst <- lapply(lst, as.data.frame, stringsAsFactors = FALSE)

  # If only one sheet was read, return a data.frame instead
  if (length(lst) == 1L) {
    lst <- as.data.frame(lst[[1]])
  }

  # Return
  return(lst)
  
}

#' Collate one or more files to a list (for an EPSI-object).
#'
#' This function takes one or more files (or a directory) and collates them to
#' a list to be converted to a EPSI-object. It is not meant for other
#' uses. The function only looks for prespecified structures.
#' 
#' @param input file to a directory or xlsx-file with the input data
#' @param contrast Optional file to historic data (xlsx or csv format)
#' @param historic Optional file to historic data (xlsx or csv format)
#' @param encoding Encoding for files (if reading directory or csv). Defaults
#' to 'latin1'.
#' @author Kristian D. Olsen
#' @return A list of data.frames from xlsx sheets and/or other files.
#' @seealso \code{\link{read_sheets}} for a function that reads all sheets
#' of a xlsx file to a list.
#' @note This function requires openxlsx. The function attempts to read 
#' CSV-files as semicolon separated first, and then comma.
#' @export
#' @examples 
#' x <- get_input("input.xlsx", "contrast.csv")

get_input <- function(input, contrast = NULL, historic = NULL, encoding = "latin1") {
  
  if (!has_extension(input, "xlsx"))
    stop("Input has to be a xlsx file.", call. = FALSE)
  
  lst <- read_data(input, encoding)
  
  if (!inherits(lst, "list")) {
    lst <- list("input" = lst)
  }
  
  # Add contrast data (and overwrite if necessary)
  if (!is.null(contrast)) {
    if ("contrast data" %in% names(lst))
      message("Overwriting existing contrast data with:\n", contrast)
    
    lst[["contrast data"]] <- read_data(contrast, encoding)
  }
  
  # Add historical data (and overwrite if necessary)
  if (!is.null(historic)) {
    if ("historic data" %in% names(lst))
      message("Overwriting existing historic data with:\n", historic)
    
    lst[["historic data"]] <- read_data(historic, encoding)
  }
  
  return(lst)
  
}

# Input wrappers ---------------------------------------------------------------

read_csv <- function(file, encoding) {
  
  if (!has_extension(file, "csv")) {
    stop("The specified path does not direct to a 'csv' file:\n", file, call. = FALSE)
  }
  
  args <- list(file = file, 
               fileEncoding = encoding, 
               stringsAsFactors = FALSE,
               na.strings = default$missing_values)
  
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