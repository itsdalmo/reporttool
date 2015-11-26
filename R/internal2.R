#' @export
read_sharepoint2 <- function(file, mainentity = "q1", weights = TRUE) {
  
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
    ow <- mutate(ow[, c(2:7, 9)], mainentity = x)
    names(ow) <- c("latent", "manifest", "question", "score", "weight", "std", "epsi_effect", "mainentity")
    ow <- mutate_each(select(ow, mainentity, everything()), funs(as.numeric(.)), score:epsi_effect)
    filter(ow, !is.na(epsi_effect), epsi_effect > 0L)
  })
  
  bind_rows(outer_weights)

}
