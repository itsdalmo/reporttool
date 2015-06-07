#' @method print survey
#' @export
print.survey <- function(lst, width = getOption("width")) {
  
  cat("Object:", quote(lst), "\n")
  
  # Class and dimensions of the objects
  info <- lapply(lst, function(x) {
    classes <- paste("(", class(x)[1], ")", sep = "")
    dimensions <- paste("[", paste(dim(x), collapse = "x"), "]", sep = "")
    paste(classes, dimensions, sep = "")
  })
  
  cat(paste(paste("$", names(lst), sep = ""), info, sep = "\t", collapse = "\n"))
  
}

#' @method print mm
#' @export
print.mm <- function(mm, width = getOption("width")) {
  
  cat("Measurement model:\n")
  
  # Lowercase for easier referencing
  names(mm) <- tolower(names(mm))
  
  # Clean manifest to fit viewport
  mm$manifest <- vapply(mm$manifest, function(x) {
    x <- ifelse(nchar(x) > 10, paste0(substr(x, 1, 8), ".."), x)
    x <- paste0(x, paste0(rep(" ", 10-nchar(x)), collapse = "")) },
    character(1))
  
  # Type
  mm$type <- vapply(mm$type, function(x) {
    switch(x, character = " (char)", 
           factor = " (fctr)", 
           numeric = " (nmbr)", 
           scale = "(scale)",
           integer = "  (int)") }, character(1))
  
  mm$type <- ifelse(!is.na(mm$latent), paste0(mm$type, "*"), paste0(mm$type, " "))
  
  # Get max string length for questions
  max_length <- width - 8 - 11 - 5 #$, manifest, \t, (type), latent
  
  # Collate
  info <- paste(paste0("$", mm$manifest), mm$type, sep = " ")
  info <- paste(info, substr(mm$question, 1, max_length), collapse = "\n")
  
  cat(info)
  cat("\nNote: Latents are marked with *\n")
  
}

#' @method print ents
#' @export
print.ents <- function(ents, width = getOption("width")) {
  
  # Lowercase for easier referencing
  names(ents) <- tolower(names(ents))
  
  # Extract only entity, observations and marketshare
  info <- ents[c("entity", "n", "marketshare")]
  substr(names(info), 1, 1) <- toupper(substr(names(info), 1, 1))
  
  # Add the total and format marketshare as a percentage
  info[nrow(info)+1,] <- c("Total", sum(info$N), sum(as.numeric(info$Marketshare)))
  info$Marketshare <- paste(as.numeric(info$Marketshare)*100, "%")
  
  print.data.frame(info, right = TRUE, row.names = FALSE)
  
}