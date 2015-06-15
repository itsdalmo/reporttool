# rmd_to_ppt <- function(rmd, envir = parent.frame()) {
#   
#   # Load and clean the .Rmd file
#   rmd <- rmd_to_r(rmd, write = FALSE)
#   
#   # Get an index of all chunks
#   start_idx <- grep(cfg$code_pat$chunk_start, rmd)
#   end_idx <- grep(cfg$code_pat$chunk_end, rmd)
#   chunk_indicies <- Map(':', start_idx, end_idx)
#   
#   # Extract the .Rmd for the indicies and evaluate/store return values  
#   all_chunks <- lapply(chunk_indicies, function(i, rmd, envir) {
#     list("start" = min(i), "end" = max(i), "objects" = eval_chunk(rmd[i], envir))
#   }, rmd, envir)
#   
#   # Get section and slide title for each chunk that returns an object
#   for (i in seq_along(all_chunks)) {
#     
#     chr <- vapply(all_chunks[[i]], function(x) class(x) == "character", logical(1))
#     
#     if (any(chr) == TRUE) {
#       contains_title <- any(grepl("^##[^#].*", all_chunks[[i]][chr]))
#       contains_section <- any(grepl("^#[^#].*", all_chunks[[i]][chr]))
#     } 
#     
#     if (contains_title) {
#       
#     } else {
#       all_chunks[[i]] <- NULL
#     }
#     
#   }
#   
# }

#' Convert .Rmd to .R
#'
#' This function converts a \code{.Rmd} to a \code{.R} file (similar to \code{knitr::purl}) 
#' by replacing chunk delimiters and putting \code{eval} chunk options inside \code{if}
#' statements. Also comments out (\code{##+}) content that is not inside chunks, 
#' and inserts a chunk number and line to distinguish chunks. 
#'
#' @param rmd Path to a rmarkdown file.
#' @param encoding The encoding of both the input .Rmd file and the output.
#' @author Kristian D. Olsen
#' @return A .R file in the same directory and same name as the input .Rmd file.
#' @note \code{UTF-8} is the recommended encoding for scripts and .Rmd files.
#' @export
#' @examples 
#' rmd_to_r("Example report.Rmd", encoding = "latin1")

rmd_to_r <- function(rmd, encoding = "UTF-8", write = TRUE) {
  
  path <- validate_path(rmd)
  
  if (!has_extension(path, "rmd")) {
    stop("This function only accepts a .Rmd file", call. = FALSE)
  } else {
    rmd <- readLines(path, encoding = encoding)
  }
  
  # Identify chunks
  start_idx <- grep(cfg$rmd_pat$chunk_start, rmd)
  end_idx <- grep(cfg$rmd_pat$chunk_end, rmd)
  
  if (length(start_idx) == length(end_idx)) {
    chunk_idx <- Map(':', start_idx, end_idx)   
  } else {
    stop("The .Rmd file contains unused chunk start/end indicators", call. = FALSE)
  }
  
  # Get and comment out all lines with content that is not in a chunk
  not_chunk_idx <- setdiff(1:length(rmd), unlist(chunk_idx))
  not_chunk_idx <- setdiff(not_chunk_idx, which(rmd == ""))
  
  rmd[not_chunk_idx] <- paste("##+", rmd[not_chunk_idx])
  
  # Comment out text, replace chunk delims and indicate chunknumber
  n <- length(rmd)
  
  for (i in seq_along(chunk_idx)) {
    
    # Update indices if the document has been extended
    idx <- chunk_idx[[i]]+(length(rmd)-n)
    
    # Piece together the chunk (w/delimiters) and the rest of the document
    rmd <- c(rmd[1:(min(idx)-1)], 
             paste("# CHUNK", i, paste(rep("-", 72-nchar(i)), collapse = "")),
             replace_chunk_delim(rmd[idx]),
             paste("# CHUNK END", paste(rep("-", 68), collapse = "")),
             if (max(idx) < length(rmd)) rmd[(max(idx)+1):length(rmd)] else "")
    
  }
  
  # Save or return the cleaned Rmd file
  if (isTRUE(write)) {
    path <- file(paste0(tools::file_path_sans_ext(path), ".R"), encoding = "UTF-8")
    on.exit(close(path), add = TRUE)
    writeLines(rmd, path)
  } else {
    return(rmd)
  }
  
  
}

# Evaluate rmarkdown code  -----------------------------------------------------

eval_inline <- function(line, envir = parent.frame()) {
  
  pattern <- cfg$rmd_pat$inline
  inline <- unlist(stringr::str_extract_all(line, pattern))
  expr <- stringr::str_replace_all(inline, "`r\\s?|\\s?`", "")
  
  for (i in seq_along(expr)) {
    res <- as.character(eval(parse(text = expr[i]), envir))
    line <- sub(inline[i], paste(res, collapse = " "), line, fixed = TRUE)
  }
  
  return(line)
  
}

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

# Replace chunk delims  --------------------------------------------------------
replace_chunk_delim <- function(lines) {
  
  chunk_start <- cfg$rmd_pat$chunk_start
  chunk_end <- cfg$rmd_pat$chunk_end
  chunk_eval <- cfg$rmd_pat$chunk_eval
  
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
