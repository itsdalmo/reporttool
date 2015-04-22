# Mid-work-backup

# rmd_to_script <- function(rmd, encoding = "UTF-8") {
#   
#   rmd <- readLines(rmd, encoding = encoding)
#   pat_begin <- reporttool$rmd_pat$chunk_begin
#   pat_end <- reporttool$rmd_pat$chunk_end
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


# Use unlist(lapply(...)) in the function?
#' @import stringr
#' @export 
eval_inline <- function(line, pattern = reporttool$rmd_pat$inline) {
  
  inline <- unlist(stringr::str_extract_all(line, pattern))
  expr <- stringr::str_replace_all(inline, "`r\\s?|\\s?`", "")
  
  for (i in seq_along(expr)) {
    res <- as.character(eval(parse(text = expr[i])))
    line <- sub(inline[i], paste(res, collapse = " "), line, fixed = TRUE)
  }
  
  return(line)
  
}

#' @import utils
#' @export
eval_chunk <- function(lines) {
  
  
  
  print_idx <- which(grepl("cat\\(|print\\(", lines))
  
  # Separate functions that print results
  if (length(print_idx) > 0) {
    print_funs <- parse(text = lines[print_idx])
    print_funs <- utils::capture.output(eval(print_funs))
    
    # Remove from lines and eval
    lines <- parse(text = lines[-print_idx])
    
    # Eval the rest and bind results
    if (length(lines) > 0) {
      lines <- eval(lines)
      lines <- list(print_funs, lines)
    } else {
      lines <- list(print_funs)
    }
    
  } else if (length(lines) > 0) {
    lines <- list(eval(parse(text = lines)))
  } else {
    lines <- NULL
  }
  
  return(lines)
  
}

replace_chunk_eval <- function(chunk, pattern = reporttool$rmd_pat$chunk_eval) {
  
  chunk_begin <- reporttool$rmd_pat$chunk_begin
  chunk_end <- reporttool$rmd_pat$chunk_end
  
  c_start <- which(grepl(chunk_begin, chunk))
  c_end <- which(grepl(chunk_end, chunk))
  
  opts_eval <- gsub(pattern, "\\1", chunk[c_start])
  
  if (length(opts_eval) == 1L) {
    chunk[c_start] <- paste0("if (", sub(pattern, "\\1", opts_eval), ") {")
    chunk[c_end] <- "}"
  } else if (length(opts_eval) == 0L) {
    chunk[c(c_start, c_end)] <- NULL
  } else {
    stop("The chunk had more than one eval statement")
  }
  
  return(chunk)

}
