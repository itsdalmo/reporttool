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
#' q <- read_data("master questionnaire.xlsx", sheet = "questionnaires")
#' write_questionnaire(q, "Banking B2C.xlsx", study = "Banking", segment = "B2C")

write_questionnaire <- function(quest, file, study = "Banking", segment = "B2C", entity = NULL) {
  
  if (!is.data.frame(quest)) {
    stop("Questionnaire is not a data.frame.", call. = FALSE)
  }
  
  # Lowercase for referencing
  study <- stri_trans_tolower(study)
  segment <- stri_trans_tolower(segment)
  names(quest) <- stri_trans_tolower(names(quest))
  
  # Subset and create index
  quest <- quest[stri_trans_tolower(quest$study) == study & stri_trans_tolower(quest$segment) == segment, ]
  quest$index <- NA
  
  # Expand scale variable levels
  is_scale <- stri_trans_tolower(quest$type) == "scale"
  quest$values[is_scale] <-  vapply(quest$values[is_scale], function(x) {
    x <- split_scale(x); stri_c(x[1], "...", x[10], if (length(x) > 10) x[11] else "", sep = "\n")
    }, character(1))
  
  # Replace {XX} with whatever value is specified
  if (!is.null(entity)) {
    quest[] <- lapply(quest, stri_replace_all, replacement = entity, regex = "\\{XX\\}")
  }
  
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
  
  # Find mainentity variable and scores
  mainentity <- survey$mm$manifest[stri_trans_tolower(survey$mm$latent) == "mainentity"]
  mainentity <- mainentity[!is.na(mainentity)]
  
  scores <- survey$mm$manifest[stri_trans_tolower(survey$mm$latent) == "epsi"]
  scores <- scores[!is.na(scores)]
  
  # Check mainentity + 'a' if entity_other is null
  if (is.null(entity_other)) {
    entity_other <- stri_trans_tolower(stri_c(mainentity, "a"))
    entity_other <- survey$mm$manifest[stri_trans_tolower(survey$mm$manifest) == entity_other]
    entity_other <- entity_other[!is.na(entity_other)]
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
#' structure and converts it to a survey.
#' 
#' @param file Path to a study directory on the intranet.
#' @param mainentity The mainentity variable. Defaults to \code{"q1"}.
#' @author Kristian D. Olsen
#' @return A survey object with measurement model and entities specified.
#' @export
#' @examples 
#' x <- read_sharepoint("https://the.intranet.se/EPSI/example")

read_sharepoint <- function(file, mainentity = "q1") {
  
  if (!tools::file_ext(file) == "") {
    stop("The specified path is not a directory:\n", file, call. = FALSE)
  }
  
  # Make https links compatible with windows file explorer
  file <- intranet_link(file)
  
  # Check if the specified directory contains the expected folders
  req_folders <- c("data", "input", "output")
  dir_folders <- list.files(file)
  
  # Create paths for each of the expected folders
  file_dirs <- file.path(file, dir_folders[stri_trans_tolower(dir_folders) %in% req_folders])
  
  if (length(file_dirs) == length(req_folders)) {
    file_dirs <- setNames(file_dirs, req_folders)
  } else {
    mis_dirs <- setdiff(req_folders, stri_trans_tolower(dir_folders))
    stop("The required (model related) folders were not found in the directory:\n", stri_c(mis_dirs, collapse = ", "), call. = FALSE)
  }
  
  # Read in the dataset if it has been converted to .xlsx
  data_dir <- list.files(file_dirs["data"])
  data_files <- data_dir[stri_detect(stri_trans_tolower(data_dir), regex = ".*em\\.sav$")]
  
  if (length(data_files) == 1L) {
    srv <- read_data(file.path(file_dirs["data"], data_files))
    srv <- survey(srv)
  } else {
    stop("There is more than one .sav file ending with \"EM\"\n", call. = FALSE)
  }
  
  # Find and read in the input files
  input_dir <- list.files(file_dirs["input"])
  input_files <- input_dir[stri_detect(stri_trans_tolower(input_dir), regex = ".*(config|measurement model).*\\.txt$")]
  
  if (length(input_files) == 2L) {
    input_files <- setNames(file.path(file_dirs["input"], input_files), c("cf", "mm"))
  } else {
    stop("The required files were not found in the input directory:\n Measurement model.txt and config.txt.", call. = FALSE)
  }
  
  input <- list("cf" = read_txt(input_files["cf"], encoding = "latin1", header = FALSE),
                "mm" = read_txt(input_files["mm"], encoding = "latin1", header = TRUE))
  
  # Convert to a supported format and extract latent association
  input$mm <- unlist(lapply(input$mm[-1], function(x, manifest) {manifest[x == -1, 1]}, input$mm[1]))
  input$mm <- data.frame("latent" = names(input$mm), "manifest" = input$mm, stringsAsFactors = FALSE, row.names = NULL)
  input$mm$latent <- stri_replace_all(input$mm$latent, "$1", regex = "([a-z]+)[0-9]+")
  
  # Assign latent association to the measurement model (use match in case order differs)
  srv$mm$latent[match(stri_trans_tolower(input$mm$manifest), stri_trans_tolower(srv$mm$manifest))] <- input$mm$latent
  srv <- set_association(srv, mainentity = mainentity)
  
  # Add entities based on the data and update with marketshares
  srv <- add_entities(srv)
  srv$ents$marketshare <- stri_replace(input$cf[[4]][match(srv$ents$entity, input$cf[[2]])], ".", regex = ",")

  # Return
  srv
  
}

#' Write input files
#'
#' This function creates the necessary input files for model estimation, in the
#' specified directory.
#' 
#' @param lst A survey object.
#' @param dir The directory on sharepoint where you would like to write the input
#' files.
#' @author Kristian D. Olsen
#' @export
#' @examples 
#' write_sharepoint("https://the.intranet.se/EPSI/example")

write_sharepoint <- function(survey, file) {
  
  # Check the input
  if (!inherits(survey, "survey")) {
    stop("Argument 'survey' is not an object with the class 'survey'. See help(survey).", call. = FALSE)
  }
  
  if (!inherits(survey$df, "survey_df") || !nrow(survey$df)) {
    stop("Data must be prepared first. See help(prepare_data).", call. = FALSE)
  }
  
  if (!tools::file_ext(file) == "") {
    stop("The specified path is not a directory:\n", file, call. = FALSE)
  } else {
    file <- clean_path(file)
    if (!file.exists(file)) {
      stop("The specified directory does not exist:\n", file, call. = FALSE)
    }
  }
  
  # Get the measurement model
  model <- survey$mm[stri_trans_tolower(survey$mm$latent) %in% default$latents, ]
  model$latent <- factor(stri_trans_tolower(model$latent), levels = default$latents, ordered = TRUE)
  model$EM <- stri_c(model$manifest, "em")
  
  # Order the model and get the cutoff
  model <- model[order(model$latent), ]
  cutoff <- as.numeric(survey$cfg$value[survey$cfg$config %in% "cutoff"])
  
  # Locate or create required directories
  req_folders <- c("Data", "Input")
  dir_folders <- list.files(file)
  
  if (length(dir_folders)) {
    folders_exist <- stri_trans_tolower(req_folders) %in% stri_trans_tolower(dir_folders)
  } else {
    folders_exist <- FALSE
  }
  
  if (!all(folders_exist)) {
    missing <- file.path(file, req_folders[!folders_exist])
    lapply(missing, dir.create, showWarnings = FALSE)
    warning("Created the following (required) folders:\n", stri_c(req_folders[!folders_exist], collapse = ", "), call. = FALSE)
    dir_folders <- list.files(file)
  }
  
  # Update and create file directories
  is_required <- stri_trans_tolower(dir_folders) %in% stri_trans_tolower(req_folders)
  file_dirs <- setNames(file.path(file, dir_folders[is_required]), stri_trans_tolower(req_folders))

  # Write data
  data_file <- survey$cfg$value[survey$cfg$config %in% c("study", "segment", "year")]
  data_file <- stri_c(stri_trans_totitle(data_file[1]), stri_trans_toupper(data_file[2]), data_file[3], sep = " ")
  data_file <- file.path(file_dirs["data"], stri_c(data_file, ".sav"))
  
  write_data(survey, file = data_file)
  
  # Input files
  args <- list(sep = "\t", na = "", dec = ",", fileEncoding = "latin1", 
               row.names = FALSE, col.names = TRUE, quote = FALSE)
  
  # Write EM data
  do.call(write.table, args = append(list(
    x = survey$df[survey$df$percent_missing <= cutoff, c("coderesp", model$EM)],
    file = file.path(file_dirs["input"], "modeldata.txt")),
    args))
  
  # Write config
  args$col.names <- FALSE; args$row.names <- TRUE
  do.call(write.table, args = append(list(
    x = survey$ents[, c("entity", "valid", "marketshare")],
    file = file.path(file_dirs["input"], "config.txt")),
    args))
  
  # Q1 names
  args$row.names <- FALSE
  do.call(write.table, args = append(list(
    x = survey$ents$entity,
    file = file.path(file_dirs["input"], "q1names.txt")),
    args))
  
  # Write question text
  do.call(write.table, args = append(list(
    x = stri_replace(model$question, replacement = "", regex = "^[ -]*"),
    file = file.path(file_dirs["input"], "qtext.txt")),
  args))

  # Create measurement model
  mm <- new_scaffold(c("Manifest", default$latents), size = nrow(model))
  mm$Manifest <- model$manifest
  mm[, 2:ncol(mm)] <- 0
  
  model$latent <- as.character(model$latent)
  for (i in default$latents) {
    mm[mm$Manifest %in% model$manifest[model$latent %in% i], i] <- -1
  }
  
  # Write model
  args$col.names <- TRUE
  do.call(write.table, args = append(list(
    x = mm,
    file = file.path(file_dirs["input"], "measurement model.txt")),
    args))

  # Make sure nothing is printed
  invisible()
  
}
