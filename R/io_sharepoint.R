#' Extract from a study directory on sharepoint
#'
#' This function extracts whatever data is available in our standard file 
#' structure and tries to structure it like a input.xlsx file.
#' 
#' @param file Path to a study directory on the intranet.
#' @author Kristian D. Olsen
#' @return A list containing the EM-data, entities (observations, marketshare),
#' and a measurement model (latent association, question text, values etc.)
#' @export
#' @examples 
#' x <- read_em("https://the.intranet.se/EPSI/example")

read_sharepoint <- function(file) {
  
  if (!has_extension(file, "")) {
    stop("The specified path is not a directory:\n", file, call. = FALSE)
  }
  
  # Make https links compatible with windows file explorer
  file <- intranet_link(file)
  
  # Check if the specified directory contains the expected folders
  exp_folders <- c("data", "input", "output")
  dir_content <- list.files(file)
  
  # Create paths for each of the expected folders
  file_dirs <- file.path(file, dir_content[tolower(dir_content) %in% exp_folders])
  
  if (length(file_dirs) == length(exp_folders)) {
    file_dirs <- setNames(file_dirs, exp_folders)
  } else {
    stop("The required (model related) folders were not found in the directory:\n", file, call. = FALSE)
  }
  
  # Read in the dataset if it has been converted to .xlsx
  data_dir <- list.files(file_dirs["data"])
  data_files <- data_dir[grepl(".*em\\.sav$", tolower(data_dir))]
  
  if (length(data_files) == 1L) {
    lst <- read_data(file.path(file_dirs["data"], data_files), codebook = TRUE)
  } else {
    stop("There is more than one .sav file ending with \"EM\"\n", call. = FALSE)
  }
  
  # Add measurement model and entities skeleton based on the data
  lst[["ents"]] <- add_entities(lst$df$q1)
  
  input_exp <- c("config.txt", "measurement model.txt")
  input_dir <- list.files(file_dirs["input"])
  input_files <- file.path(file_dirs["input"], input_dir[tolower(input_dir) %in% input_exp])
  
  if (length(input_files) == length(input_exp)) {
    input_files <- setNames(input_files, c("cf", "mm"))
  } else {
    stop("The required files were not found in the input directory:\n", input_exp, call. = FALSE)
  }
  
  input <- list("cf" = read_txt(input_files["cf"], encoding = "latin1", header = FALSE),
                "mm" = read_txt(input_files["mm"], encoding = "latin1", header = TRUE))
  
  # Convert model to the appropriate format
  input$mm <- unlist(lapply(input$mm[-1], function(x, manifest) {manifest[x == -1, 1]}, input$mm[1]))
  input$mm <- data.frame("latent" = names(input$mm), "manifest" = input$mm, stringsAsFactors = FALSE, row.names = NULL)
  
  input$mm$latent <- gsub("([a-z]+)[0-9]+", "\\1", input$mm$latent)
  
  # Assign latent association to the measurement model (use match in case order differs)
  lst$mm$latent[match(input$mm$manifest, tolower(lst$mm$manifest))] <- input$mm$latent
  
  # Add marketshares to entities
  lst$ents$marketshare <- gsub(",", "\\.", input$cf[[4]][match(lst$ents$entity, input$cf[[2]])])
  
  # Combine the results and return them
  return(lst)
  
}

#' Write input files
#'
#' This function creates the necessary input files for model estimation, in the
#' specified directory.
#' 
#' @param lst A survey_data object.
#' @param dir The directory on sharepoint where you would like to write the input
#' files.
#' @author Kristian D. Olsen
#' @return A list containing the EM-data, entities (observations, marketshare),
#' and a measurement model (latent association, question text, values etc.)
#' @export
#' @examples 
#' x <- read_em("https://the.intranet.se/EPSI/example")

write_sharepoint <- function(lst, dir) {
  
  stop("This is not supported yet.", call. = FALSE)
  
}
