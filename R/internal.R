#' Write questionnaire
#'
#' This function writes a questionnaire as extracted by get_questionnaire to
#' a specified xlsx-file (with styling). Only writes the latent association,
#' manifest name, possible values and the question text itself. 
#'
#' @param quest The questionnaire (r object).
#' @param file The name of the xlsx file you would like to write to.
#' @param study Name of the study to write.
#' @param segment The segment being studied. 
#' @param entity Optional: Replace {XX} with something else.
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
  quest[] <- lapply(quest, stri_replace_all, replacement = "", regex = "\\r")
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
#' @param srv A survey.
#' @param other The text column which contains the open response for 'other' in 
#' mainentity. Defaults to for instance \code{Q1a}, if mainentity is \code{Q1}.
#' @author Kristian D. Olsen
#' @return A list.
#' @export
#' @examples 
#' x <- topline(df)

topline <- function(srv, other = NULL) {
  
  # Check the input
  if (!is.survey(srv)) {
    stop("Argument 'srv' is not an object with the class 'survey'. See help(survey).", call. = FALSE)
  } else {
    mainentity <- get_association(srv, "mainentity")
  }
  
  # Data must be prepared first
  latents <- filter(srv$mm, stri_trans_tolower(manifest) %in% default$latents)[["manifest"]]
  if (!length(latents)) {
    stop("Latents were not found in the data. See help(prepare_data).", call. = FALSE)
  } 
  
  # Get total n in addition to valid observations from survey_table
  tots <- suppressWarnings(survey_table_(srv, dots = mainentity, contrast = FALSE, weight = FALSE, filter_missing = FALSE))
  ents <- suppressWarnings(survey_table_(srv, dots = latents, contrast = FALSE, weight = FALSE))
  ents <- rename_(ents, .dots = c("valid" = "n"))
  ents <- left_join(tots, ents, by = setNames(mainentity, mainentity))
  
  # Check mainentity + 'a' if 'other' is null
  if (is.null(other)) {
    other <- stri_c(mainentity, "a")
    other <- filter(srv$mm, stri_detect(manifest, regex = stri_c("^", other), case_insensitive = TRUE))[["manifest"]]
  }
  
  lst <- list("entities" = ents)
  
  # Make a table for 'other' if the variable is found
  if (length(other)) {
    other <- select_(srv$df, .dots = setNames(other, "other"))
    other <- filter(other, !is.na(other), !other %in% c("", " "))
    other <- mutate(other, other = stri_trans_tolower(other))
    other <- summarise(group_by(other, other), n = n())
    other <- arrange(other, desc(n))
    lst <- append(lst, list("other" = other))
  } else {
    warning("A column corresponding to 'other' was not found in the data.", call. = FALSE)
  }
  
  lst
  
}

#' Extract from a study directory on sharepoint
#'
#' This function extracts whatever data is available in our standard file 
#' structure and converts it to a survey.
#' 
#' @param file Path to a study directory on the intranet.
#' @param mainentity The mainentity variable. Defaults to \code{"q1"}.
#' @param encoding The encoding used for input-files. Usually "latin1", but
#' R produced files use "UTF-8". 
#' @author Kristian D. Olsen
#' @return A survey object with measurement model and entities specified.
#' @export
#' @examples 
#' x <- read_sharepoint("https://the.intranet.se/EPSI/example")

read_sharepoint <- function(file, mainentity = "q1", encoding = "latin1") {
  
  # Check path
  if (!tools::file_ext(file) == "") {
    stop("The specified path is not a directory:\n", file, call. = FALSE)
  } else {
    file <- clean_path(file)
    if (!file.exists(file)) {
      stop("The specified directory does not exist:\n", file, call. = FALSE)
    }
  }
  
  # Check if the specified directory contains the expected folders
  req_folders <- c("data", "input")
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
    srv <- if(is.spss(srv)) survey(srv) else add_mm(survey(srv))
  } else {
    stop("There is more than one .sav file ending with \"EM\"\n", call. = FALSE)
  }
  
  # Rename andel_missing and set to numeric
  miss <- names(srv$df)[stri_trans_tolower(names(srv$df)) %in% "andel_missing"]
  if (length(miss) && !"percent_missing" %in% names(srv$df)) {
    srv <- rename_(srv, .dots = setNames(miss[1], "percent_missing"))
  }
  srv <- mutate(srv, percent_missing = as.numeric(percent_missing))
  
  # Set config (w/percent missing)
  miss <- max(srv$df$percent_missing, na.rm = TRUE)
  miss <- ceiling(miss*10)/10 # Nearest 10%
  warning("Setting cutoff to ", miss*100, "%.", call. = FALSE)
  
  srv <- set_config(srv)
  srv$cfg$value[srv$cfg$config %in% "cutoff"] <- miss
  
  # Assume that marketshares have been set.
  srv$cfg$value[srv$cfg$config %in% "marketshares"] <- "yes"
  
  # Identify mainentity in manifest
  mainentity <- filter(srv$mm, stri_trans_tolower(manifest) == mainentity)[["manifest"]]
  if (!length(mainentity)) {
    stop("Could not find mainentity.", call. = FALSE)
  } 
  
  # Find and read in the input files
  input_dir <- list.files(file_dirs["input"])
  input_files <- input_dir[stri_detect(stri_trans_tolower(input_dir), regex = ".*(config|measurement model).*\\.txt$")]
  
  if (length(input_files) == 2L) {
    input_files <- setNames(file.path(file_dirs["input"], input_files), c("cf", "mm"))
  } else {
    stop("The required files were not found in the input directory:\n Measurement model.txt and config.txt.", call. = FALSE)
  }
  
  cols_mm <- list("Manifest" = readr::col_character())
  mm <- read_data(input_files["mm"], encoding = encoding, decimal = ",", col_names = TRUE, col_types = cols_mm)
  cf <- read_data(input_files["cf"], encoding = encoding, decimal = ",", col_names = FALSE)
  
  # Lowercase for referencing
  names(mm) <- stri_trans_tolower(names(mm))
  names(cf) <- stri_trans_tolower(names(cf))
  
  # Convert to a supported format and extract latent association
  mm <- unlist(lapply(mm[-1], function(x, manifest) {manifest[x == -1, 1]}, mm[1]))
  mm <- data_frame("latent" = names(mm), "manifest" = mm)
  mm$latent <- stri_replace_all(mm$latent, "$1", regex = "([a-z]+).manifest.*")
  
  # Assign latent association to the measurement model (use match in case order differs)
  srv$mm$latent[match(stri_trans_tolower(mm$manifest), stri_trans_tolower(srv$mm$manifest))] <- mm$latent
  
  # Add entities based on the data and update with marketshares
  srv <- set_association(srv, mainentity = mainentity)
  srv <- add_entities(srv)
  srv$ents$marketshare <- stri_replace(cf[[4]][match(srv$ents$entity, cf[[2]])], ".", regex = ",")
  
  # Return
  srv
  
}

#' Write input files
#'
#' This function creates the necessary input files for model estimation, in the
#' specified directory.
#' 
#' @param srv A survey object.
#' @param file The directory on sharepoint where you would like to write the input
#' files.
#' @author Kristian D. Olsen
#' @export
#' @examples 
#' write_sharepoint("https://the.intranet.se/EPSI/example")

write_sharepoint <- function(srv, file) {
  
  # Check the input
  if (!is.survey(srv)) {
    stop("Argument 'sv' is not an object with the class 'survey'. See help(survey).", call. = FALSE)
  } else if (!is.survey_ents(srv$ents)) {
    stop("Entities must be added first. See help(add_entities).", call. = FALSE)
  }
  
  # Data must be prepared first
  if (!"coderesp" %in% names(srv$df)) {
    stop("Column 'coderesp' was not found in the data. See help(prepare_data).", call. = FALSE)
  } else if (!"percent_missing" %in% names(srv$df)) {
    stop("Column 'percent_missing' was not found in the data. See help(prepare_data).", call. = FALSE)
  }
  
  if (!tools::file_ext(file) == "") {
    stop("The specified path is not a directory:\n", file, call. = FALSE)
  } else {
    file <- clean_path(file)
    if (!file.exists(file)) {
      stop("The specified directory does not exist:\n", file, call. = FALSE)
    }
  }
  
  # Get mainentity
  mainentity <- filter(srv$mm, stri_trans_tolower(latent) == "mainentity")[["manifest"]]
  
  # Get the measurement model and the cutoff
  model <- filter(srv$mm, stri_trans_tolower(latent) %in% default$latents)
  cutoff <- as.numeric(filter(srv$cfg, config == "cutoff")[["value"]])
  
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
  data_file <- filter(srv$cfg, config %in% c("study", "segment", "year"))[["value"]]
  data_file <- stri_c(stri_trans_totitle(data_file[1]), stri_trans_toupper(data_file[2]), data_file[3], sep = " ")
  data_file <- file.path(file_dirs["data"], stri_c(data_file, "EM", ".sav"))
  data_file <- stri_replace_all(data_file, " ", regex = "\\s\\s")
  
  write_data(srv, file = data_file)
  
  # Input files
  args <- list(sep = "\t", na = "", dec = ",", fileEncoding = "latin1", 
               row.names = FALSE, col.names = TRUE, quote = FALSE)
  
  # Factor the data and convert mainentity to numeric
  srv <- factor_data(srv, vars = mainentity)
  me_levels <- levels(srv$df[[mainentity]])
  srv$df[mainentity] <- as.numeric(srv$df[[mainentity]])
  
  if (is.null(me_levels)) {
    stop("The mainentity column must be a factor variable.", call. = FALSE)
  }
  
  # Properly order entities
  srv$ents <- mutate(srv$ents, entity = factor(entity, levels = me_levels, ordered = TRUE))
  srv$ents <- arrange(srv$ents, entity)
  row.names(srv$ents) <- 1:nrow(srv$ents)
  
  # Write EM data
  em_data <- filter(srv$df, percent_missing <= cutoff)
  em_data <- select(em_data, one_of(c(model$manifest, mainentity, "coderesp")))
  em_data <- arrange_(em_data, eval(mainentity))
  em_data <- mutate_each(em_data, funs(clean_score(.)), one_of(model$manifest))
  em_data <- mutate_each(em_data, funs(ifelse(is.na(.), 98, .)), one_of(model$manifest))
  
  do.call(write.table, args = append(list(
    x = em_data,
    file = file.path(file_dirs["input"], "em_data.txt")),
    args))
  
  # Write config
  args$col.names <- FALSE; args$row.names <- TRUE
  do.call(write.table, args = append(list(
    x = srv$ents[, c("entity", "valid", "marketshare")],
    file = file.path(file_dirs["input"], "config.txt")),
    args))
  
  # Q1 names
  args$row.names <- FALSE
  do.call(write.table, args = append(list(
    x = srv$ents$entity,
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
