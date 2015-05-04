rmd_to_ppt <- function(rmd, envir = parent.frame()) {
  
  # Load and clean the .Rmd file
  rmd <- rmd_to_code(rmd, write = FALSE)
  
  # Get an index of all chunks
  start_idx <- grep(reporttool$code_pat$chunk_start, rmd)
  end_idx <- grep(reporttool$code_pat$chunk_end, rmd)
  chunk_indicies <- Map(':', start_idx, end_idx)
  
  # Extract the .Rmd for the indicies and evaluate/store return values  
  all_chunks <- lapply(chunk_indicies, function(i, rmd, envir) {
    list("start" = min(i), "end" = max(i), "return" = eval_chunk(rmd[i], envir))
  }, rmd, envir)
  
  
  
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
