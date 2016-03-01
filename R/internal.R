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
  
  # Arrange the questions by "order"
  quest <- quest[order(quest$order), ]
  
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
    
    # Set either the primer or question as title
    if (nrow(x) == 1L && is.na(x$primer)) {
      title <- x$question
    } else {
      title <- stri_c(unique(x$primer), collapse = "\n")
    }

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
  tots <- suppressWarnings(survey_table_(srv, dots = mainentity, contrast = FALSE, weight = FALSE, filter_missing = FALSE, filter_response = FALSE))
  ents <- suppressWarnings(survey_table_(srv, dots = latents, contrast = FALSE, weight = FALSE, filter_response = FALSE))
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

#' @export
read_sharepoint <- function(file, mainentity = "q1", weights = FALSE) {
  
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
  dirs <- list.files(file)
  miss <- setdiff(c("data", "input"), stri_trans_tolower(dirs))
  if (length(miss)) {
    stop("The required (model related) folders were not found in the directory:\n",
         conjunct_string(miss), call. = FALSE)
  }
  
  # Create paths for each of the expected folders
  dir <- file.path(file, dirs[stri_trans_tolower(dirs) %in% c("data", "input")])
  dir <- setNames(dir, c("data", "input"))
  
  # Read in the dataset
  srv <- read_em_data(dir["data"])
  
  # Identify mainentity in manifest
  mainentity <- filter(srv$mm, stri_trans_tolower(manifest) == mainentity)[["manifest"]]
  if (!length(mainentity)) {
    stop("Could not find mainentity.", call. = FALSE)
  } 
  
  # Find and read in the input files
  mm <- read_em_model(dir["input"])
  cf <- read_em_config(dir["input"])
  
  # Assign latent association to the measurement model (use match in case order differs)
  srv$mm$latent[match(stri_trans_tolower(mm$manifest), stri_trans_tolower(srv$mm$manifest))] <- mm$latent
  
  # Add entities based on the data and update with marketshares
  srv <- set_association(srv, mainentity = mainentity)
  srv <- add_entities(srv)
  srv$ents$marketshare <- stri_replace(cf[[4]][match(srv$ents$entity, cf[[2]])], ".", regex = ",")
  
  # Optionally - also add inner/outer weights
  if (weights && "output" %in% stri_trans_tolower(dirs)) {
    dir <- dirs[stri_trans_tolower(dirs) %in% "output"]
    srv$inner_weights <- read_inner_weights(file.path(file, dir), srv$ents$entity)
    srv$outer_weights <- read_outer_weights(file.path(file, dir), srv$ents$entity)
  }
  
  # Return
  srv
  
}

read_em_data <- function(dir) {
  if (!is.string(dir)) stop("dir must be a string.")
  file <- list.files(dir)
  file <- file[stri_detect(stri_trans_tolower(file), regex = ".*em\\.sav$")]
  if (length(file) != 1L) {
    msg <- if (length(file) > 1L) "There is more than one" else "Found no"
    stop(stri_c(msg, " .sav file ending with \"EM\"."), call. = FALSE)
  }
  
  # Read in the data
  srv <- read_data(file.path(dir, file))
  srv <- if(is.spss(srv)) survey(srv) else add_mm(survey(srv))
  
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
  
  srv
  
}

read_em_config <- function(dir) {
  if (!is.string(dir)) stop("dir must be a string.")
  file <- list.files(dir)
  file <- file[stri_detect(stri_trans_tolower(file), regex = "config.*\\.txt$")]
  if (!length(file)) stop("'config.txt' was not found.")
  
  cf <- read_data(file.path(dir, file), encoding = "latin1", decimal = ",", col_names = FALSE)
  names(cf) <- stri_trans_tolower(names(cf))
  
  cf
  
}

read_em_model <- function(dir) {
  if (!is.string(dir)) stop("dir must be a string.")
  file <- list.files(dir)
  file <- file[stri_detect(stri_trans_tolower(file), regex = "measurement model.*\\.txt$")]
  if (!length(file)) stop("'measurement model.txt' was not found.")
  
  cols_mm <- list("Manifest" = readr::col_character())
  mm <- read_data(file.path(dir, file), encoding = "latin1", decimal = ",", col_names = TRUE, col_types = cols_mm)
  names(mm) <- stri_trans_tolower(names(mm))
  
  # Convert to correct format and extract latent association
  mm <- unlist(lapply(mm[-1], function(x, manifest) {manifest[x == -1, 1]}, mm[1]))
  mm <- data_frame("latent" = names(mm), "manifest" = mm)
  mm$latent <- stri_replace_all(mm$latent, "$1", regex = "([a-z]+).manifest.*")
  
  mm
  
}

read_inner_weights <- function(dir, entities) {
  if (!is.string(dir)) stop("dir must be a string.")
  file <- list.files(dir)
  file <- file[stri_detect(stri_trans_tolower(file), regex = "main results.*\\.xlsx$")]
  if (!length(file)) warning("'main results.xlsx' was not found.")
  
  inner_weights <- lapply(entities, function(x) {
    iw <- read_data(file.path(dir, file), sheet = x, skip = 5)
    if (is.null(iw) || length(iw) == 0L) stop("Could not find sheet '", x, "' in 'main results.xlsx'.")
    if (nrow(iw) == 0L) stop("Problem reading data from 'main results.xlsx'. This is often solved by opening the file for editing, selecting each sheet in turn, and saving again without further changes.", call. = FALSE)
    iw <- mutate(iw, mainentity = x)
    names(iw) <- c("origin", default$latents, "mainentity")
    iw <- mutate_each(slice(iw, 1:7), funs(as.numeric(.)), image:loyal)
    select(iw, mainentity, everything())
  })
  
  bind_rows(inner_weights)
  
}

read_outer_weights <- function(dir, entities) {
  if (!is.string(dir)) stop("dir must be a string.")
  file <- list.files(dir)
  file <- file[stri_detect(stri_trans_tolower(file), regex = "score weights out.*\\.xlsx$")]
  if (!length(file)) warning("'score weights out.xlsx' was not found.")
  
  outer_weights <- lapply(entities, function(x) {
    ow <- read_data(file.path(dir, file), sheet = x, skip = 3)
    if (is.null(ow) || length(ow) == 0L) stop("Could not find sheet '", x, "' in 'score weights out.xlsx'.")
    if (nrow(ow) == 0L) stop("Problem reading data from 'score weights out.xlsx'. This is often solved by opening the file for editing, selecting each sheet in turn, and saving again without further changes.", call. = FALSE)
    ow <- mutate(ow[, c(2:7, 9)], mainentity = x)
    names(ow) <- c("latent", "manifest", "question", "score", "weight", "std", "epsi_effect", "mainentity")
    ow <- mutate_each(select(ow, mainentity, everything()), funs(as.numeric(.)), score:epsi_effect)
    filter(ow, !is.na(epsi_effect), epsi_effect > 0L)
  })
  
  bind_rows(outer_weights)

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
    warning("Created the following (required) folders:\n", 
            conjunct_string(req_folders[!folders_exist]), call. = FALSE)
    dir_folders <- list.files(file)
  }
  
  # Update and create file directories
  is_required <- stri_trans_tolower(dir_folders) %in% stri_trans_tolower(req_folders)
  
  # Get output directories
  output_dir <- file.path(file, dir_folders[is_required])
  output_dir <- setNames(output_dir, stri_trans_tolower(req_folders))
  
  # Write data
  write_em_data(srv, output_dir["data"])
  write_em_input(srv, output_dir["input"])
  
  # Make sure nothing is printed
  invisible()
  
}

write_em_data <- function(srv, path) {
  
  file_name <- get_config(srv, c("study", "segment", "year"))
  file_name <- stri_c(stri_trans_totitle(file_name[1]), stri_trans_toupper(file_name[2]), file_name[3], sep = " ")
  file_name <- file.path(path, stri_c(file_name, "EM", ".sav"))
  file_name <- stri_replace_all(file_name, " ", regex = "\\s\\s")
  
  write_data(srv, file = file_name)
  
}

write_em_input <- function(srv, dir) {
  
  # Entities must be added
  if (!is.survey_ents(srv$ents)) {
    stop("Entities must be added first. See help(add_entities).", call. = FALSE)
  }
  
  # Get mainentity and cutoff
  mainentity <- get_association(srv, "mainentity")
  cutoff <- get_config(srv, "cutoff")
  
  # Get the measurement model
  model <- filter(srv$mm, stri_trans_tolower(latent) %in% default$latents)
  
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
    file = file.path(dir, "em_data.txt")),
    args))
  
  # Write config
  args$col.names <- FALSE; args$row.names <- TRUE
  do.call(write.table, args = append(list(
    x = srv$ents[, c("entity", "valid", "marketshare")],
    file = file.path(dir, "config.txt")),
    args))
  
  # Q1 names
  args$row.names <- FALSE
  do.call(write.table, args = append(list(
    x = srv$ents$entity,
    file = file.path(dir, "q1names.txt")),
    args))
  
  # Write question text
  do.call(write.table, args = append(list(
    x = stri_replace(model$question, replacement = "", regex = "^[ -]*"),
    file = file.path(dir, "qtext.txt")),
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
    file = file.path(dir, "measurement model.txt")),
    args))
  
  # Make sure nothing is printed
  invisible()
  
}

