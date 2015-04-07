# Get input from .xlsx with sheets ---------------------------------------------

#' Read sheets from filePath.xlsx to a list.
#'
#' This function (a very thin wrapper for openxlsx functions), takes a 
#' specially formatted .xlsx file and converts it to a list. 
#'
#' @param filePath The path to the .xlsx file.
#' @author Kristian D. Olsen
#' @return A list containing data.frames matching the sheets in the .xlsx file.
#' @note This function only reads the prespecified sheets, if not all are
#' present - it reads the ones it can find.
#' @export
#' @examples #' x <- input_xlsx("input.xlsx")

input_xlsx <- function(path) {
  
  path <- check_input_path(path)
  
  # Read prespecified sheets
  lst <- read_sheets(path, sheets = default$sheet_names$long)
  
  # Return
  return(lst)
  
}

# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------