# Default values ---------------------------------------------------------------
default <- list(
  
  "supported_input" = c("xlsx", ""),
  
  "latent_names" = c("image", "expect", "prodq", 
                     "servq", "value", "epsi", "loyal"),
  
  "missing_values" = c("NA", " ", "", "#DIV/0!", "#NULL!", "#NAVN?", "#NAME?"),
  
  "sheet_names" = list(
                 "long" = c("data", "raw data", "entities", 
                            "measurement model", "config"),
                 "short" = c("df", "cd", "hd", "rd", "ents", "mm", "cf")),
  
  "beamer_tmp" = list(
                 "dir" = "rmd/beamer",
                 "files" = c("beamer_preamble.tex", 
                             "beamer_template.tex")),
  
  "beamer_thm" = list(
                 "dir" = "rmd/beamer",
                 "files" = c("beamercolorthememetropolis.sty", 
                             "beamerfontthememetropolis.sty", 
                             "beamerthemem.sty", "logo.eps"))
)

# Check input path -------------------------------------------------------------
check_input_path <- function(path) {
  
  if (!all(is.character(path), length(path) == 1L))
    stop(paste("Path should be a string of length", 1L), call. = FALSE)
  
  # Expand path and check if file exists
  path <- normalizePath(path, "/", mustWork = FALSE)
  
  if (!file.exists(path))
    stop(paste0("The file or path does not exist:\n", path), call. = FALSE)
  
  # Check if the extension is supported
  ext <- tolower(tools::file_ext(path))
  
  if (!ext %in% default$supported_input)
    stop(paste0(ext, " is not a supported extension for input files."), call. = FALSE)
  
  return(path)
}


# Read all/specified sheets in a .xlsx file to a list --------------------------
#' @export
read_sheets <- function(path, sheets=NULL) {
  
  # Load workbook
  wb <- openxlsx::loadWorkbook(path)
  sh <- openxlsx::sheets(wb)
  
  # If sheets are specified, read only these
  if (!is.null(sheets))
    sh <- sh[tolower(sh) %in% tolower(sheets)]

  # Check if any sheets exist
  if (length(sh) == 0L)
    stop("The specified sheets were not found in the workbook:\n", 
         paste0("'", sheets, "'", collapse=" "), call. = FALSE)
  
  # Read data to list and set names
  lst <- suppressWarnings(lapply(sh, openxlsx::readWorkbook, xlsxFile = wb))
  lst <- setNames(lst, sh)
  
  # Set all list entries to be data.frames
  lst <- lapply(lst, as.data.frame, stringsAsFactor = FALSE)
  
  # Return
  return(lst)
}
