sav_as_list <- function(df) {
  
  if (!inherits(df, "sav")) {
    stop("df is not of type 'sav'.", call. = FALSE)
  }
  
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
    sum(stri_detect(x, regex = default$patterns$detect_scale)) == 10L }))
  
  # Clean up the scale variable values (only endpoints)
  vars[is_scale] <- lapply(vars[is_scale], function(x) {
    scales <- stri_replace(x, "$1", regex = default$patterns$extract_scale)
    scales <- scales[scales != ""]
    if (is.null(scales)) "" else scales
  })
  
  # Set type to scale and add values
  mm$type[is_scale] <- "scale"
  mm$values <- unlist(lapply(vars, stringr::str_c, collapse = "\n"))
  
  
  # Return
  list("df" = df, "mm" = mm)
  
}