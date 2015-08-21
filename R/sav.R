from_sav <- function(df, mm = TRUE) {
  
  # Create an empty measurement model
  mm <- new_scaffold(default$structure$mm, ncol(df))
  
  # Populate mm
  mm$manifest <- stri_replace_all(names(df), ".", regex = "[#$]")
  mm$manifest <- stri_trans_tolower(names(df))
  
  # Get question text from labels
  mm$question <- lapply(df, attr, which = "label")
  mm$question <- vapply(mm$question, function(x) ifelse(is.null(x), "", as.character(x)), character(1))
  
  # Convert 'labelled' to factors
  df <- lapply(df, function(x) { if (inherits(x, "labelled")) haven::as_factor(x, drop_na = FALSE) else x })
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  
  # Convert labelled to factors
  df <- lapply(df, function(x) { if (inherits(x, "labelled")) haven::as_factor(x, drop_na = FALSE) else x })
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  
  # Insert variable type
  mm$type <- vapply(df, class, character(1))
  
  # Extract factor levels
  vars <- lapply(df, levels)
  is_scale <- unlist(lapply(vars, function(x) {
    sum(stri_detect(x, regex = default$pattern$detect_scale)) == 10L }))
  
  # Clean up the scale variable values (only endpoints)
  vars[is_scale] <- lapply(vars[is_scale], function(x) {
    scales <- stri_replace(x, "$1", regex = default$pattern$extract_scale)
    scales <- scales[scales != ""]
  })
  
  # Add values and set corresponding type
  is_null <- vapply(vars, is.null, logical(1))
  mm$values[!is_null] <- vapply(vars[!is_null], stri_c, collapse = "\n", character(1))
  mm$type[is_scale] <- "scale"
  
  # Return
  list("df" = df, "mm" = mm)
  
}

to_sav <- function(survey) {
  
}



factor_data <- function(survey, vars = NULL) {
  
  # Check the input
  if (!inherits(survey, "survey")) {
    stop("Argument 'survey' is not an object with the class 'survey'. See help(survey).", call. = FALSE)
  }
  
  # Measurement model must be added first
  if (!inherits(survey$mm, "survey_mm") || !nrow(survey$mm)) {
    stop("The measurement model must be added first. See help(add_mm).", call. = FALSE)
  }
  
  # Only convert factors if not otherwise specified
  if (is.null(vars)) {
    vars <- survey$mm$manifest[survey$mm$type == "factor"]
  }
  
  # Check existence of specified vars
  missing_mm <- setdiff(vars, survey$mm$manifest)
  missing_df <- setdiff(vars, names(survey$df))
  
  if (length(missing_mm) || length(missing_df)) {
    stop(stri_c("Columns not found in model and/or data. Check lower and uppercase.\n", 
                "mm: ", stri_c(missing_mm, sep = " "), "\n", 
                "df: ", stri_c(missing_df, sep = " ")), call. = FALSE)
  }
  
  # Complete the scales
  is_scale <- srv$mm$type == "scale" & survey$mm$manifest %in% vars
  if (any(is_scale)) {
    scale <- lapply(srv$mm$values[is_scale], split_scale)
    s_var <- survey$mm$manifest[is_scale]
    survey$df[s_var] <- Map(haven::as_factor, survey$df[s_var], scale, drop_na = FALSE)
  }
  
  # Split factor variables
  is_factor <- srv$mm$type == "factor" & survey$mm$manifest %in% vars
  if (any(is_factor)) {
    scale <- lapply(srv$mm$values[is_factor], split_fctr)
    f_var <- survey$mm$manifest[is_factor]
    survey$df[f_var] <- Map(haven::as_factor, survey$df[f_var], scale, drop_na = FALSE)
  }
  
  # Return
  survey
  
}

split_scale <- function(x) {
  
  v <- stri_split(x, regex = "\n")
  v <- lapply(ends, function(x) { n <- 1:length(x); stri_c(c(1, 10, 98)[n], x, sep = " ") })
  v <- lapply(v, function(x) { if (length(x) == 2L) c(x[1], 2:9, x[2]) else c(x[1], 2:9, x[2:3])})
  
  # Retrun
  unlist(v)
  
}

split_fctr <- function(x) {
  
  unlist(stri_split(x, regex = "\n"))
  
}