#' @import tools
#' @export 
rmd_to_script <- function(rmd, encoding = "UTF-8") {
  
  path <- validate_path(rmd)
  
  if (!has_extension(path, "rmd")) {
    stop("This function only accepts a .Rmd file", call. = FALSE)
  } else {
    rmd <- readLines(path, encoding = encoding)
  }
  
  # Identify chunks
  start_idx <- grep(reporttool$rmd_pat$chunk_start, rmd)
  end_idx <- grep(reporttool$rmd_pat$chunk_end, rmd)
  
  if (length(start_idx) != length(end_idx)) {
    stop("The .Rmd file contains unused chunk start/end indicators", call. = FALSE)
  }
  
  # Get a list of all chunk indices
  chunk_idx <- Map(':', start_idx, end_idx)
  
  # Get and comment out all lines with content that is not in a chunk
  not_chunk_idx <- setdiff(1:length(rmd), unlist(chunk_idx))
  not_chunk_idx <- setdiff(not_chunk_idx, which(rmd == ""))
  
  rmd[not_chunk_idx] <- paste("##+", rmd[not_chunk_idx])
  
  # Comment out text, replace chunk delims and indicate chunknumber
  n <- length(rmd)
  
  for (i in seq_along(chunk_idx)) {
    
    # Update indices if the document has been extended
    idx <- chunk_idx[[i]]+(length(rmd)-n)
    
    ch <- replace_chunk_delim(rmd[idx])
    ch <- c(paste(paste("# CHUNK", i), paste(rep("-", 79-nchar(chnm)), collapse = "")), ch)
    
    rmd <- c(rmd[1:(min(idx)-1)], ch, rmd[(max(idx)+1):length(rmd)])
    
  }
  
  # Save the converted rmd
  path <- file(paste0(tools::file_path_sans_ext(path), ".R"), encoding = encoding)
  on.exit(close(path), add = TRUE)
  
  writeLines(rmd, path)
  
}


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
    lines <- list(eval(parse(text = lines)), envir)
  } else {
    lines <- NULL
  }
  
  return(lines)
  
}


replace_chunk_delim <- function(lines) {
  
  chunk_start <- reporttool$rmd_pat$chunk_start
  chunk_end <- reporttool$rmd_pat$chunk_end
  chunk_eval <- reporttool$rmd_pat$chunk_eval
    
  # Identify which (if any) chunks contain eval options
  eval_idx <- grep(chunk_eval, lines)
  end_idx <- grep(chunk_end, lines)
  
  if (length(eval_idx) > 0) {
    # Find the correct chunk end for each chunk eval
    end_idx <- vapply(eval_idx, function(x) end_idx[end_idx > x][1], numeric(1))
    
    # Replace chunk-delimiters with if-functions
    lines[eval_idx] <- paste0("if (", gsub(chunk_eval, "\\1", lines[eval_idx]), ") {")
    lines[end_idx] <- "}"
    
    # Double indendt the content in if-functions
    content <- setdiff(unlist(Map(':', eval_idx, end_idx)), c(eval_idx, end_idx))
    lines[content] <- paste0("  ", lines[content])
    
  }
  
  # Remove chunk start/endings without eval options
  start_idx <- grep(chunk_start, lines)
  end_idx <- grep(chunk_end, lines)
  
  lines[start_idx] <- ""
  lines[end_idx] <- ""

  return(lines)
}

