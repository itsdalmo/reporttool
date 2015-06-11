#' Write questionnaire
#'
#' This function writes a questionnaire as extracted by get_questionnaire to
#' a specified xlsx-file (with styling). Only writes the latent association,
#' manifest name, possible values and the question text itself. 
#'
#' @param quest A questionnaire as returned by get_questionnaire.
#' @param file The name of the xlsx file you would like to write to.
#' @param study Name of the study. This is used as the sheet-name when writing
#' the questionnaire.
#' @author Kristian D. Olsen
#' @export
#' @examples 
#' studies <- c("Mobile", "Broadband", "DTV")
#' all_questionnaires <- lapply(studies, get_questionnaire, file = masterquest.xlsx, industry = "Telecom")
#' lapply(all_questionnaires, write_questionnaire, file = "ICT Questionnaires.xlsx", study = studies)

write_questionnaire <- function(quest, file, study = "District heating") {
  
  # Start with an empty index
  quest$index <- NA
  
  # Loop through the rows and update the index
  for (i in 1:nrow(quest)) {
    
    # Get the next index value
    index_value <- if (all(is.na(quest$index))) 1 else max(quest$index, na.rm = TRUE) + 1 
    
    # If index and primer are NA - use the rownumber as index
    if (is.na(quest$primer[i]) && is.na(quest$index[i])) {
      quest$index[i] <- index_value
      
      # If only the index is NA - give the same index to all identical primers
    } else if (is.na(quest$index[i])) {
      quest$index[quest$primer %in% quest$primer[i]] <- index_value
    }
  }
  
  # Split the questionnaire by index
  split_quest <- split(quest, quest$index)
  
  # Use the split list to write to a xlsx_file
  wb <- openxlsx::createWorkbook()
  
  lapply(split_quest, function(x) {
    
    df <- x[, c("latent", "manifest", "values", "question")]
    title <- if (nrow(x) == 1L) x$question else paste0(unique(x$primer), collapse = "\n")
    
    to_sheet(df, wb, title = title, sheet = study)
    
  })
  
  # Widen the column containing the question text, and save the workbook
  openxlsx::setColWidths(wb, sheet = study, cols = 4, widths = 140)
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  
  # Make sure nothing is printed
  invisible()
  
}

#' Get a questionnaire with exchanged placeholders
#'
#' A function for internal use. Reads the master questionnaire and exchanges
#' placeholders in the text with their respective dictionary entry. 
#'
#' @param file Path to the master questionnaire
#' @param industry Specify which industry the study you would like to extract
#' belongs to
#' @param study The study to extract and replace values for.
#' @param entity Optional: Replace {XX} in the questionnaire with whatever you
#' specify.
#' @author Kristian D. Olsen
#' @export
#' @examples 
#' x <- get_questionnaire("masterquest.xlsx", "Energy", "Electricity", entity = "[S2]")

get_questionnaire <- function(file, industry = "Energy", study = "District Heating", entity = NULL) {
  
  # Read in the master questionnaire
  mq <- read_data(file)
  
  # Lowercase names for easier referencing
  mq <- lapply(mq, lowercase_names)
  names(mq) <- tolower(names(mq))
  
  # Lowercase the arguments
  industry <- tolower(industry)
  study <- tolower(study)
  
  # Match name of study column in the dictionary
  study_col <- gsub(" ", ".", study)
  
  # Subset the master questionnaire and dictionary
  dict <- mq$dictionary[, c("year", "placeholder", study_col)]
  quest <- mq[[industry]][tolower(mq[[industry]]$study) == study,]
  
  # Replace the scale placeholder with the actual scale
  for (i in 1:nrow(quest)) {
    if (quest$type[i] == "scale") {
      
      scale <- unlist(strsplit(quest$values[i], "\n"))
      
      if (length(scale) >= 2) {
        scale <- paste0("(1 = ", scale[1], ", ", "10 = ", scale[2], ")")
      } else {
        stop("scale variables require at least two 'values' (endpoints)\n", df$values[i], call. = FALSE)
      }
      
      quest[i, ] <- gsub("\\{scale\\}", paste("{scale}", scale), quest[i, ])
      
    }
  }
  
  # Sequence along the dictionary and replace values
  for (i in 1:nrow(dict)) {
    quest[] <- lapply(quest, function(x) gsub(paste0("\\{", dict$placeholder[i], "\\}"), dict[i, study_col], x))
  }
  
  # Replace {XX} with whatever value is specified
  if (!is.null(entity)) {
    quest[] <- lapply(quest, function(x) gsub("\\{XX\\}", entity, x))
  }

  # Return
  quest
  
}
