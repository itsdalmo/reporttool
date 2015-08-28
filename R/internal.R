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
  
  # Write the data and retain rows that have been written to
  merge_rows <- lapply(split_quest, function(x) {
    
    # Get columns
    df <- x[, c("latent", "manifest", "question", "values")]
    title <- if (nrow(x) == 1L) x$question else paste0(unique(x$primer), collapse = "\n")
    
    # Write the question
    to_sheet(df, wb, title = title, sheet = study)
    
  })
  
  lapply(merge_rows, function(x) {
    
    rows <- x["first"]:x["last"]
    
    # Merge values for question matrix
    if (length(rows) > 2L) {
      openxlsx::mergeCells(wb, sheet = study, cols = 4, rows = rows[-1])
    }
    
    # Set values to wrap text
    openxlsx::addStyle(wb, sheet = study, style = openxlsx::createStyle(wrapText = TRUE), 
                       rows = rows[-1], cols = 4, gridExpand = TRUE, stack = TRUE)
    
  })
  
  # Widen the columns containing the question text and values
  openxlsx::setColWidths(wb, sheet = study, cols = 3, widths = 100)
  openxlsx::setColWidths(wb, sheet = study, cols = 4, widths = 50)
  
  # Save and make sure nothing is printed
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  invisible()
  
}

#' Get a questionnaire with exchanged placeholders
#'
#' A function for internal use. Reads the master questionnaire and exchanges
#' placeholders in the text with their respective dictionary entry. 
#'
#' @param file Path to the master questionnaire
#' @param study The study to extract and replace values for.
#' @param segment Business to consumer/business.
#' @param entity Optional: Replace {XX} in the questionnaire with whatever you
#' specify.
#' @author Kristian D. Olsen
#' @export
#' @examples 
#' x <- get_questionnaire("masterquest.xlsx", "Energy", "Electricity", entity = "[S2]")

get_questionnaire <- function(file, study = "Banking", segment = "B2C", entity = NULL) {
  
  # Read in the master questionnaire
  mq <- read_data(file)
  
  # Lowercase names for easier referencing
  mq <- lapply(mq, lowercase_names)
  names(mq) <- tolower(names(mq))
  
  # Lowercase the arguments
  study <- tolower(study)
  segment <- tolower(segment)
  
  # Match name of study column in the dictionary
  study_col <- gsub(" ", ".", study)
  
  # Subset the master questionnaire and dictionary
  dict <- mq$dictionary[, c("year", "placeholder", study_col)]
  quest <- mq[["questionnaires"]][tolower(mq[["questionnaires"]]$study) == study &
                                  tolower(mq[["questionnaires"]]$segment) == segment,]
  
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

#' Create a topline
#'
#' Simple function for \code{survey} objects, that procudes a count and mean per 
#' entity in an ongoing study, as well as open answers for respondents who have 
#' answered 'other'. It requires that both the measurement model and entities are
#' specified, and that the config and translations have been set.
#' 
#' @param survey A survey.
#' @param entity_other The text column which contains the open response for 'other' in 
#' mainentity. Defaults to for instance \code{Q1a}, if mainentity is \code{Q1}.
#' @param sample The name of the column containing sample information.
#' @author Kristian D. Olsen
#' @return A list.
#' @export
#' @examples 
#' x <- topline(df)

topline <- function(survey, entity_other = NULL, sample = NULL) {
  
  # Check the input
  if (!inherits(survey, "survey")) {
    stop("Argument 'survey' is not an object with the class 'survey'. See help(survey).", call. = FALSE)
  }
  
  # Measurement model must be added first
  if (!inherits(survey$mm, "survey_mm") || !nrow(survey$mm)) {
    stop("The measurement model must be added first. See help(add_mm).", call. = FALSE)
  }
  
  # Entities must be added first
  if (!inherits(survey$ents, "survey_ents") || !nrow(survey$ents)) {
    stop("Entities must be added first. See help(add_entities).", call. = FALSE)
  }
  
  # Config and translations must be set beforehand.
  if (!inherits(survey$tr, "survey_tr") || !nrow(survey$tr)) {
    stop("Translations must be set first. See help(set_translation).", call. = FALSE)
  }
  
  if (!inherits(survey$cfg, "survey_cfg") || !nrow(survey$cfg)) {
    stop("Config must be set first. See help(set_config).", call. = FALSE)
  }
  
  # Find mainentity, entity other and scores from measurement model if they are NULL
  if (is.null(mainentity)) {
    mainentity <- survey$mm$manifest[stri_trans_tolower(survey$mm$latent) == "mainentity"]
    mainentity <- mainentity[!is.na(mainentity)]
  }
  
  if (is.null(entity_other)) {
    entity_other <- stri_trans_tolower(stri_c(mainentity, "a"))
    entity_other <- survey$mm$manifest[stri_trans_tolower(survey$mm$manifest) == entity_other]
    entity_other <- entity_other[!is.na(entity_other)]
  }
  
  if (is.null(scores)) {
    scores <- survey$mm$manifest[stri_trans_tolower(survey$mm$latent) == "epsi"]
    scores <- scores[!is.na(scores)]
  }
  
  # Set names for data (merges)
  names(survey$df)[names(survey$df) %in% mainentity] <- "entity"
  names(survey$df)[names(survey$df) %in% entity_other] <- "entity_other"
  
  # Clean, rescale and convert to numeric
  n <- nrow(survey$df)
  survey$df[scores] <- vapply(survey$df[scores], clean_score, numeric(n))
  survey$df[scores] <- vapply(survey$df[scores], rescale_score, numeric(n))
  
  # Get the entities summary
  ents <- survey$ents[c("entity", "n")]
  ents <- merge(ents, aggregate(survey$df[scores], survey$df["entity"], FUN = mean, na.rm = TRUE), by = "entity")
  
  total <- data.frame("entity" = "Total", "n" = length(na.omit(survey$df$entity)))
  total[scores] <- apply(survey$df[scores], 2, mean, na.rm = TRUE)
  ents <- rbind(ents, total)
  
  # Clean NaN's
  ents[scores] <- apply(ents[scores], 2, function(x) ifelse(is.nan(x), NA, x))
  
  # Add row means (KTI)
  satisfaction <- survey$tr$replacement[survey$tr$original %in% "epsi"]
  ents[satisfaction] <- rowMeans(ents[scores], na.rm = TRUE)
  
  # Add sample counts
  if (!is.null(sample)) {
    samp_total <- do.call(cbind, as.list(tapply(survey$df$entity, survey$df[sample], FUN = length)))
    samp_total <- as.data.frame(samp_total, stringsAsFactors = FALSE)
    samp_total$entity <- "Total"
    
    samp <- tapply(survey$df$entity, survey$df[c("entity", sample)], FUN = length)
    samp <- cbind(data.frame("entity" = dimnames(samp)$entity, as.data.frame(samp, stringsAsFactors = FALSE)))
    rownames(samp) <- NULL
    
    ents <- merge(rbind(samp, samp_total), ents, by = "entity")
    names(ents) <- tolower(names(ents))
  }
  
  # Make a table for 'other'
  other <- survey$df$entity_other
  other <- table(other[!is.na(other) & other != ""])
  other <- as.data.frame(other, stringsAsFactors = FALSE)
  names(other) <- c("name", "n")
  
  # Order on decreasing count and alphabetical names
  other <- other[order(-xtfrm(other$n), other$name), ]
  
  # Create a list and return results
  lst <- list("entities" = ents, "other" = other)
  
  return(lst)
  
}

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
#' x <- read_sharepoint("https://the.intranet.se/EPSI/example")

read_sharepoint <- function(file) {
  
  if (!tools::file_ext(file) == "") {
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
  lst$mm$latent[match(tolower(input$mm$manifest), tolower(lst$mm$manifest))] <- input$mm$latent
  
  # Add marketshares to entities
  lst$ents$marketshare <- gsub(",", "\\.", input$cf[[4]][match(lst$ents$entity, input$cf[[2]])])
  
  # Combine the results and return them (set classes as well)
  class(lst$ents) <- append("survey_ents", class(lst$ents))
  class(lst$mm) <- append("survey_mm", class(lst$ents))
  class(lst) <- append("survey", class(lst))
  
  lst
  
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
#' x <- write_sharepoint("https://the.intranet.se/EPSI/example")

write_sharepoint <- function(lst, dir) {
  
  stop("This is not supported yet.", call. = FALSE)
  
}
