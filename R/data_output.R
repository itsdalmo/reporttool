# openxlsx wrapper -------------------------------------------------------------

#' @export
add_table <- function(df, wb, sheet="analysis", row=1L) {
  
  # Check input
  if (!is.character(sheet) || length(sheet) != 1L)
    stop("The sheet has to be a string of length 1 (not an index).", call. = FALSE)
  
  # Get last row if sheet exists, or create if it does not.
  if (sheet %in% openxlsx::sheets(wb)) {
    row <- 2L + nrow(openxlsx::readWorkbook(wb, sheet = sheet, 
                                            colNames=FALSE, 
                                            skipEmptyRows = FALSE))
  } else {
    openxlsx::addWorksheet(wb, sheetName = sheet)
  }
  
  # Add data to the workbook
  openxlsx::writeData(wb, sheet, df, startRow = row, headerStyle = openxlsx_style)
}