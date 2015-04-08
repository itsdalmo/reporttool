#' Collate one or more files to a list (for an EPSI-object).
#'
#' This function takes one or more files (or a directory) and collates them to
#' a list to be converted to a EPSI-object. It is not meant for other
#' uses. The function only looks for prespecified structures.
#' 
#' @param input Path to a directory or xlsx-file with the input data
#' @param contrast Optional path to historic data (xlsx or csv format)
#' @param historic Optional path to historic data (xlsx or csv format)
#' @param encoding Encoding for files (if reading directory or csv). Defaults
#' to 'latin1'.
#' @author Kristian D. Olsen
#' @return A list of data.frames from sheets and/or files.
#' @seealso \code{\link{read_sheets}} for a function that reads all sheets
#' of a xlsx file to a list.
#' @note This function requires openxlsx. The function attempts to read 
#' CSV-files as semicolon separated first, and then comma.
#' @export
#' @examples #' x <- get_input("input.xlsx", "contrast.csv")

get_input <- function(input, contrast = NULL, historic = NULL, encoding = "latin1") {
  
  lst <- file_input(input, encoding)
  
  # Add contrast data (and overwrite if necessary)
  if (!is.null(contrast)) {
    if ("contrast data" %in% names(lst))
      message("Overwriting existing contrast data with:\n", contrast)
    
    lst[["contrast data"]] <- file_input(contrast, encoding)[[1]]
  }
  
  # Add historical data (and overwrite if necessary)
  if (!is.null(historic)) {
    if ("historic data" %in% names(lst))
      message("Overwriting existing historic data with:\n", historic)
    
    lst[["historic data"]] <- file_input(historic, encoding)[[1]]
  }
  
  return(lst)
  
}

# openxlsx wrapper -------------------------------------------------------------

#' Read sheets from xlsx-files to a list.
#'
#' This function (a very thin wrapper for openxlsx functions) which reads all
#' sheets from an .xlsx file to a list of data.frames.
#'
#' @param path The path to the .xlsx file.
#' @param sheets Optional: Specify the sheets to be read.
#' @param clean.na Set to TRUE to clean typical NA-strings from .xlsx file.
#' @author Kristian D. Olsen
#' @return A list containing data.frames matching the sheets in the .xlsx file.
#' @note This function requires openxlsx.
#' @export
#' @examples #' x <- read_sheets("test.xlsx")

read_sheets <- function(path, sheets = NULL, clean.missing = FALSE) {
  
  # Check path
  if(!all(is_valid_path(path), has_extension(path, "xlsx")))
    stop("The path does not exist or is not a 'xlsx' file", call. = FALSE)
  
  # Load workbook
  wb <- openxlsx::loadWorkbook(path)
  sh <- openxlsx::sheets(wb)
  
  # If sheets are specified, read only these
  if (!is.null(sheets))
    sh <- sh[tolower(sh) %in% tolower(sheets)]
  
  # Check if any/specified sheets exist
  if (length(sh) == 0L)
    stop("The specified sheets were not found in the workbook:\n", 
         paste0("'", sheets, "'", collapse=" "), call. = FALSE)
  
  # Read data to list and set names
  lst <- suppressWarnings(lapply(sh, openxlsx::readWorkbook, xlsxFile = wb))
  names(lst) <- sh
  
  # Set all list entries to be data.frame/clean NA
  if (isTRUE(clean.missing)) {
    lst <- lapply(lst, clean_missing)
    
  } else {
    lst <- lapply(lst, as.data.frame, stringsAsFactor = FALSE)
    
  }

  # Return
  return(lst)
  
}

# Input wrappers ---------------------------------------------------------------

file_input <- function(path, encoding) {
  
  # Check input
  if(!is_valid_ext(path))
    stop(paste("The path", path, "does not lead to a valid input format"), call. = FALSE)
  
  if(!is_valid_path(path))
    stop(paste("The path", path, "is not valid"), call. = FALSE)
  
  # Pick input-function based on extension
  switch(tools::file_ext(path),
         csv = csv_input(path, encoding),
         xlsx = xlsx_input(path),
         dir_input(path, encoding))
}

csv_input <- function(path, encoding) {
  
  args <- list(file = path, 
               fileEncoding = encoding, 
               stringsAsFactor = FALSE,
               na.strings = default$missing_values)
  
  if (!has_extension(path, "csv"))
    stop("The specified path does not direct to a 'csv' file\n", call. = FALSE)
  
  df <- do.call(read.csv2, args)
 
  # If one column is returned, try comma separated
  if (dim(df)[2] == 1L)
    df <- do.call(read.csv, args)
  
  # Lowercase names
  names(df) <- tolower(names(df))
  
  return(df)
}

xlsx_input <- function(path) {
  
  if (!has_extension(path, "xlsx"))
    stop("The specified path does not direct to a 'xlsx' file\n", call. = FALSE)
  
  lst <- read_sheets(path, sheets = default$sheet_names$long, clean.missing = TRUE)
  
  # Lowercase names
  lst <- lapply(lst, tolower_cols)
  names(lst) <- tolower(names(lst))
  
  return(lst)
}

dir_input <- function(path, encoding) {
  
  if (!has_extension(path, ""))
    stop("The specified path does not direct to a directory", call. = FALSE)
  
  stop(paste("Directory input is not yet supported\n", path), call. = FALSE)
}