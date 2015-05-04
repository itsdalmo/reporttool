rmd_to_slides <- function(rmd, envir = parent.frame(), encoding = "UTF-8") {
  
  path <- validate_path(rmd)
  
  if (!has_extension(path, "rmd")) {
    stop("This function only accepts a .Rmd file", call. = FALSE)
  } else {
    rmd <- readLines(path, encoding = encoding)
  }
  
  # Identify chunks that are evaluated before the first slides
  slide_idx <- c(grep("^##[^#]", rmd), grep("(cat|print)\\s*\\(\\s*\"##[^#]", rmd))
  
  chunk_start <- grep(reporttool$rmd_pat$chunk_start, rmd)
  chunk_end <- grep(reporttool$rmd_pat$chunk_end, rmd)
  all_chunks <- unlist(Map(':', chunk_start, chunk_end))
  
  first_slide <- min(setdiff(slide_idx, all_chunks))
  
  # Split out and evalute the first chunks (preamble)
  preamble_start <- chunk_start[chunk_start < first_slide]
  preamble_end <- vapply(preamble_start, function(x) chunk_end[chunk_end > x][1], numeric(1))
  
  preamble_chunks <- lapply(seq_along(preamble_start), function(i, start, end, rmd) {
    replace_chunk_delim(rmd[start[i]:end[i]])
  }, preamble_start, preamble_end, rmd)

  # Evaluate the preamble in the designated environment
  if (length(preamble_chunks > 1)) {
   test <-  lapply(preamble_chunks, eval_chunk, envir = envir)
  }
  
  return(test)
}



# Evaluate rmarkdown code  -----------------------------------------------------

#' @import stringr
#' @export 
eval_inline <- function(line, envir = parent.frame()) {
  
  pattern <- reporttool$rmd_pat$inline
  inline <- unlist(stringr::str_extract_all(line, pattern))
  expr <- stringr::str_replace_all(inline, "`r\\s?|\\s?`", "")
  
  for (i in seq_along(expr)) {
    res <- as.character(eval(parse(text = expr[i]), envir))
    line <- sub(inline[i], paste(res, collapse = " "), line, fixed = TRUE)
  }
  
  return(line)
  
}

#' @import utils
#' @export
eval_chunk <- function(lines, envir = parent.frame()) {
  
  print_idx <- grep("cat\\(|print\\(", lines)
  
  # Separate functions that print results
  if (length(print_idx) > 0) {
    print_funs <- parse(text = lines[print_idx])
    print_funs <- utils::capture.output(eval(print_funs, envir))
    
    # Remove from lines and eval
    lines <- parse(text = lines[-print_idx])
    
    # Eval the rest and bind results
    if (length(lines) > 0) {
      lines <- eval(lines, envir)
      lines <- list(print_funs, lines)
    } else {
      lines <- list(print_funs)
    }
    
  } else if (length(lines) > 0) {
    lines <- list(eval(parse(text = lines), envir))
  } else {
    lines <- NULL
  }
  
  return(lines)
  
}
