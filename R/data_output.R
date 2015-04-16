#' Write data to sheet (in an openxlsx workbook)
#'
#' This function (a very thin wrapper for openxlsx functions) that writes/appends 
#' a data.frame to the specified sheet in a loaded openxlsx workbook.
#'
#' @param df The data to write.
#' @param wb A loaded openxlsx workbook (use openxlsx::loadWorkbook)
#' @param sheet Name of the sheet to write to, will be created if it does not exist.
#' @param row Optional: Also specify the startingrow for writing data.
#' @param append Whether or not the function should append data or clean the
#' sheet before writing.
#' @author Kristian D. Olsen
#' @return A list containing data.frames matching the sheets in the .xlsx file.
#' If only one sheet is read, the function returns a data.frame instead.
#' @note This function requires openxlsx.
#' @import openxlsx
#' @export
#' @examples 
#' wb <- openxlsx::loadWorkbook("test.xlsx")
#' x %>% to_sheet(wb, sheet = "test", append = FALSE)
#' openxlsx::saveWorkbook(wb, "test.xlsx", overwrite = TRUE)

to_sheet <- function(df, wb, sheet="analysis", row=1L, append=TRUE) {
  
  # Check input
  if (!is.character(sheet) || length(sheet) != 1L) {
    stop("The sheet has to be a string of length 1 (not an index).", call. = FALSE)
  }
  
  # See if sheet exists
  sheet_exists <- sheet %in% openxlsx::sheets(wb)
  
  # Get last row if sheet exists, or create if it does not.
  if (sheet_exists && isTRUE(append)) {
    row <- 2L + nrow(openxlsx::read.xlsx(wb, sheet = sheet, colNames = FALSE,
                                         skipEmptyRows = FALSE))
  } else if (sheet_exists) {
    openxlsx::removeWorksheet(wb, sheet)
    openxlsx::addWorksheet(wb, sheetName = sheet)
  } else {
    openxlsx::addWorksheet(wb, sheetName = sheet)
  }
  
  # Add data to the workbook
  if (is.null(names(df)) || identical(names(df), character(0))) {
    warning(sheet, ": No columnames in data. An empty sheet was created", call. = FALSE)
  } else {
    openxlsx::writeData(wb, sheet, df, startRow = row, headerStyle = openxlsx_style)
  }
}

#' Write to Windows clipboard
#'
#' Wrapper for writing to windows clipboard with the most-used defaults for a
#' scandinavian locale.
#'
#' @param df The data to write.
#' @param encoding The encoding to use when writing.
#' @author Kristian D. Olsen
#' @note This function only works on Windows, and the data-size cannot exceeed 128kb.
#' @export
#' @examples 
#' x %>% to_clipboard()

to_clipboard <- function(df, encoding = "latin1") {
  
  if (!identical(Sys.info()["sysname"], "Windows")) {
    stop("Writing to clipboard requires Windows OS")
  }
  
  if (object.size(df) > 120000) {
    stop("The data is too large to write to clipboard", call. = FALSE)
  }
  
  if (!inherits(df, "data.frame")) {
    stop("The data must be a data.frame", call. = FALSE)
  }
  
  write.table(x = df,
              file = "clipboard-128",
              sep = "\t",
              na = "",
              dec = ",",
              row.names = FALSE,
              fileEncoding = encoding)
}

#' Write common file formats
#'
#' A simple wrapper for writing common data formats. The specific format is
#' specified by the extension given in the filename. If no filename is given,
#' data will be written to output.xlsx in the current working directory.
#'
#' @param x The data to be written. Can be a data.frame or a list (of df's).
#' @param file Path and extension for saving the data.
#' @param encoding The encoding to use for txt and csv-files.
#' @author Kristian D. Olsen
#' @note Writing .xlsx requires that the openxlsx package is installed.
#' @import tools
#' @export
#' @examples 
#' write_data(x, file = "test.xlsx")

write_data <- function(x, file = NULL, encoding = "latin1") {
  
  # Provide default if file is not specified
  if (is.null(file)) {
    name <- "output"
    ext <- "xlsx"
    file <- file.path(getwd(), paste(name, ext, sep = "."))
    warning("No file specified, writing to: ", file, call. = FALSE)
    
  } else {
    file <- validate_path(file)
    ext <- tools::file_ext(file)
    name <- sub(paste0(".*/(.*).", ext), "\\1", file)
  }
  
  # Check object class
  if (inherits(x, "data.frame")) {
    x <- setNames(list(x), name)
  } else if (!inherits(x, "list")) {
    stop("This function expects a data.frame or list", call. = FALSE)
  }
  
  # Handle NULL in list names
  if (is.null(names(x))) {
    names(x) <- paste("Data", 1:length(x))
  }
  
  # Warn if list can be written to a single file, but no valid filename is given.
  if (name == "" && length(x) == 1) {
    stop("Please provide a valid filename.")
  }
  
  # Use extension to write correct format
  switch(ext,
         xlsx = write_xlsx(x, file),
         csv = write_csv(x, dirname(file), encoding),
         txt = write_txt(x, dirname(file), encoding),
         stop("Unrecognized output format: ", ext))
  
  invisible()
}

# Output wrappers --------------------------------------------------------------

write_xlsx <- function(lst, file) {
  
  if (!inherits(lst, "list")) {
    stop("The data must be of class 'list'", call. = FALSE)
  }
  
  # If the file exists, load and write to it
  if (file.exists(file)) { 
    wb <- openxlsx::loadWorkbook(file)
  } else { 
    wb <- openxlsx::createWorkbook()
  }
  
  
  lapply(names(lst), function(nm, lst, wb) {
    to_sheet(lst[[nm]], wb = wb, sheet = nm, append = FALSE)}, 
    lst, wb)
  
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  
}

write_csv <- function(lst, file, encoding) {
  
  if (!inherits(lst, "list")) {
    stop("The data must be of class 'list'", call. = FALSE)
  }

  lapply(names(lst), function(nm, lst, file, encoding) {
    write.table(x = lst[[nm]], 
                file = paste0(file.path(file, nm), ".csv"), 
                sep = ";", 
                na = "",
                dec = ",",
                row.names = FALSE,
                fileEncoding = encoding)}, 
    lst, file, encoding)

}

write_txt <- function(lst, file, encoding) {
  
  if (!inherits(lst, "list")) {
    stop("The data must be of class 'list'", call. = FALSE)
  }
  
  lapply(names(lst), function(nm, lst, file, encoding) {
    write.table(x = lst[[nm]],
                file = paste0(file.path(file, nm), ".txt"),
                sep = ",",
                na = "",
                dec = ".",
                row.names = FALSE,
                fileEncoding = encoding,
                quote = FALSE)}, 
    lst, file, encoding)
  
}

write_dir <- function(lst, file, encoding) {
  stop("Directory output is not yet supported", call. = FALSE)
}