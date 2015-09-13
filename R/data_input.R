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

from_clipboard <- function(sep = "\t", header = TRUE) {
  
  if ((Sys.info()["sysname"] == "Windows")) {
    file <- "clipboard-128"
  } else if (Sys.info()["sysname"] == "Darwin") {
    file <- pipe("pbpaste", "rb")
    on.exit(close(file), add = TRUE)
  } else {
    stop("Writing to clipboard is supported only in Windows or OSX")
  }
  
  # Read lines
  lines <- suppressWarnings(readr::read_lines(file))
  
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
    lines <- read.table(con, header = header, sep = sep, fill = TRUE, stringsAsFactors = FALSE)
    class(lines) <- c("tbl_df", "tbl", "data.frame")
    
    #lines <- readr::read_delim(con, delim = sep, col_names = header) # Not working?
    
  }
  
  return(lines)
  
}

#' Read common data formats
#'
#' A simple wrapper for reading data which currently supports Rdata, sav, txt,
#' csv, csv2 and xlsx. Under the hood, it uses \code{readxl}, \code{readr} and 
#' \code{haven}.
#'
#' @param file Path to a Rdata, sav (SPSS), txt, csv, csv2 or xlsx file.
#' @param ... Additional arguments passed to \code{readxl} and \code{readr}. For
#' instance you can use \code{sheet} to specify a xlsx sheet when reading.
#' @param encoding The encoding to use for txt and csv-files.
#' @author Kristian D. Olsen
#' @return A data.frame. If more than one sheet is read from a xlsx file 
#' (or you are reading a Rdata file) a list is returned instead.
#' @export
#' @examples 
#' x <- read_data("test.xlsx")

read_data <- function(file, ..., encoding = "UTF-8") {
  
  file <- clean_path(file)
  
  if (!file.exists(file)) {
    stop("Path does not exist:\n", file, call. = FALSE)
  }
  
  # Locale and dots
  loc <- readr::locale(encoding = encoding)
  dots <- list(...)
  
  # Pick input-function based on extension
  switch(tolower(tools::file_ext(file)),
         sav = read_spss(file),
         txt = read_flat(file, sep = "\t", loc, dots),
         tsv = read_flat(file, sep = "\t", loc, dots),
         csv = read_flat(file, sep = ",", loc, dots),
         xlsx = read_xlsx(file, dots),
         rdata = read_rdata(file),
         stop("Unrecognized input format in:\n", file, call. = FALSE))
}


# Input wrappers ---------------------------------------------------------------

read_spss <- function(file) {
  
  haven::read_sav(file)
  
}

read_rdata <- function(file) {
  
  # Create an empty environment to load the rdata
  data <- new.env(parent = emptyenv())
  load(file, envir = data)
  
  # Convert the environment to a list and lowercase names
  data <- as.list(data)
  
  # Return first element if only one exists
  if (length(data) == 1L) data <- as_data_frame(data[[1]])
  
  data
  
}

read_flat <- function(file, sep, loc, dots) {
  
  if (sep == ";") loc$decimal_mark <- ","
  
  # Update standard args
  args <- list(file = file, delim = sep, locale = loc)
  args <- append(dots, args[!names(args) %in% names(dots)])
  
  # Read the data
  do.call(readr::read_delim, args)
  
}

read_xlsx <- function(file, dots) {
  
  # Get the sheetnames to be read
  wb <- readxl::excel_sheets(file)
  
  if (!is.null(dots) && "sheet" %in% names(dots)) {
    sheet <- dots$sheet
    if (is.character(sheet)) {
      sheet <- wb[stri_trans_tolower(wb) %in% stri_trans_tolower(sheet)]
    }
  } else {
    sheet <- wb
  }
  
  # Read data to list
  data <- lapply(sheet, function(x) {
    x <- try(readxl::read_excel(file, x), silent = TRUE) 
    if (inherits(x, "try-error")) data_frame() else x })
  
  # Set names
  names(data) <- sheet
  
  # If only one sheet was read, return a data.frame instead
  if (length(data) == 1L) data <- data[[1]]
  
  # Return
  data
  
}
