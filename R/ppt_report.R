# Mid-work-backup

# rmd_to_script <- function(rmd, encoding = "UTF-8") {
#   
#   rmd <- readLines(rmd, encoding = encoding)
#   pat_begin <- reporttool$rmd_patterns$chunk_begin
#   pat_end <- reporttool$rmd_patterns$chunk_end
#   
#   # Indicate where chunks begin and end
#   chunk_start <- which(grepl(pat_begin, rmd))
#   chunk_end <- which(grepl(pat_end, rmd))
#   
#   if (length(chunk_start) != length(chunk_end)) {
#     stop("The .Rmd file contains unused chunk patterns", call. = FALSE)
#   }
#   
#   chunk_idx <- lapply(seq_along(chunk_start), function(i, cs, ce) {
#     cs[i]:ce[i]
#   }, chunk_start, chunk_end)
#   
#   # Figure out where slides begin and end (# and ## outside chunks)
#   section_begin <- which(grepl("^#\\s|\"#\\s", rmd))
#   section_begin <- slide_begin[which(!slide_begin %in% unlist(chunk_idx))]
#   rmd[section_begin]
#   
#   
#   # Create a list of chunks contents
#   chunks <- lapply(seq_along(chunk_start), function(i, cs, ce, rmd) {
#     rmd[(chunk_start[i]+1):(chunk_end[i]-1)]
#   }, chunk_start, chunk_end, rmd)
#   
#   # Create a list of content outside of chunks (text etc)
#   texts <- lapply(seq_along(chunk_end[1:(length(chunk_end)-1)]), function(i, cs, ce, rmd) {
#     rmd[(chunk_end[i]+1):(chunk_start[i+1]-1)]
#   }, chunk_start, chunk_end, rmd)
#   
#   # Identify
#   texts <- lapply(texts, function(x) {x <- x[x != ""]; x})
#   
#   # Evaluate inline code and replace with results
#   
#   
#   inline <- grepl("`r", texts)
#   
#   
#   
# }


#' @import stringr
#' @export 
eval_inline <- function(lines, pattern = "`r[ [:alnum:][:punct:]][^`]+`") {
  
  inline <- unlist(stringr::str_extract_all(lines, pattern))
  expr <- stringr::str_trim(stringr::str_replace_all(inline, "`r|`", ""))
  
  for (i in seq_along(expr)) {
    res <- as.character(eval(parse(text = expr[i])))
    
    if (identical(res, character(0))) {
      res <- ""
    }
    
    lines <- sub(inline[i], res, lines, fixed = TRUE)
  }
  
  return(lines)
}
