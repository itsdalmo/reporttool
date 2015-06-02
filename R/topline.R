#' Create a topline
#'
#' Simple function to get a count and mean per entity in an ongoing study, as well
#' as open answers for respondents who have answered 'other'.
#' 
#' @param df The data.frame containing the study data
#' @param mainentity Name of the mainentity (q1) column in the data
#' @param me_open The text column which contains the open response for 'other' in 
#' mainentity.
#' @param scores A vector with columnnames for variables you would like a mean from.
#' @author Kristian D. Olsen
#' @return A list
#' @export
#' @examples 
#' x <- topline(df)

topline <- function(df, mainentity = "q1", entity_other = "q1_open", scores = c("q3", "q6", "q16")) {
  
  n <- nrow(df)
  names(df)[names(df) %in% mainentity] <- "entity"
  
  # Clean, rescale and convert to numeric
  df[scores] <- vapply(df[scores], clean_score, character(n))
  df[scores] <- vapply(df[scores], rescale_score, numeric(n))
  
  # Create a table for the entities
  ents <- add_entities(df$entity)[c("entity", "n")]
  ents <- merge(ents, aggregate(df[scores], df["entity"], FUN = mean, na.rm = TRUE), by = "entity")
  
  # Append the average
  ents_total <- data.frame("entity" = "Total", "n" = nrow(df[!is.na(df$entity)]))
  ents_total[scores] <- apply(df[scores], 2, mean, na.rm = TRUE)
  
  ents <- rbind(ents, ents_total)
  
  # Clean NaN's
  ents[scores] <- apply(ents[scores], 2, function(x) ifelse(is.nan(x), NA, x))

  # Make a table for 'other' and sort it
  other <- as.data.frame(table(na.omit(df[entity_other])), stringsAsFactors = FALSE)
  names(other) <- c("name", "n")
  
  # Order on decreasing count and alphabetical names
  other <- other[order(-xtfrm(other$n), other$name), ]
  
  # Create a list and return results
  lst <- list("entities" = ents, "other" = other)
  
  return(lst)
  
}