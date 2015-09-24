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
  
  # Assume strings are a path
  if (is.string(rmd)) {
    path <- clean_path(rmd)
    
    if (!stri_trans_tolower(tools::file_ext(rmd)) == "rmd") {
      stop("This function only accepts a .Rmd file", call. = FALSE)
    } else {
      rmd <- readLines(path, encoding = encoding)
    }
  } else {
    write <- FALSE
  }
  
  # Get default patterns
  pattern <- default$pattern$rmd
  
  # Identify chunks
  chunk_start <- which(stri_detect(rmd, regex = pattern$chunk_start))
  chunk_end <- which(stri_detect(rmd, regex = pattern$chunk_end))

  if (length(chunk_start) == length(chunk_end)) {
    chunk_index <- Map(':', chunk_start, chunk_end)   
  } else {
    stop("The .Rmd file contains unused chunk start/end indicators", call. = FALSE)
  }
  
  # Get and comment out all lines with content that is not in a chunk
  not_chunk <- setdiff(1:length(rmd), unlist(chunk_index))
  not_chunk <- setdiff(not_chunk, which(rmd == ""))

  rmd[not_chunk] <- stri_c("##+ ", rmd[not_chunk])
  
  # Comment out text, replace chunk delims and indicate chunknumber
  n <- length(rmd)
  
  for (i in seq_along(chunk_index)) {
    
    # Update indices if the document has been extended
    idx <- chunk_index[[i]]+(length(rmd)-n)
    
    # Piece together the chunk (w/delimiters) and the rest of the document
    rmd <- c(rmd[1:(min(idx)-1)], replace_chunk_delim(rmd[idx]),
             if (max(idx) < length(rmd)) rmd[(max(idx)+1):length(rmd)] else "")
  }
  
  # Save or return the cleaned Rmd file
  if (write) {
    path <- file(stri_c(tools::file_path_sans_ext(path), ".R"), encoding = "UTF-8")
    on.exit(close(path), add = TRUE)
    writeLines(rmd, path)
  } else {
    return(rmd)
  }
  
  
}

#' @export
evaluate_rmd <- function(rmd, envir = parent.frame()) {
  
  # Convert the rmd to "r" - easier to identify chunks/evaluate code
  rmd <- rmd_to_r(rmd, write = FALSE)
  
  # Remove yaml
  yaml <- which(stri_detect(rmd, regex = stri_c("^##\\+ ---")))
  rmd <- rmd[-c(yaml[1]:yaml[2])]
  rmd <- rmd[rmd != ""]
  
  # Get indexes
  inlines <- which(stri_detect(rmd, regex = "^##+.*"))
  chunks <- which(!stri_detect(rmd, regex = "^##+.*"))
  
  results <- list()
  n <- length(rmd)
  index <- 0
  
  for (i in seq_along(rmd)) {
    if (i > index) {
      
      is_inline <- stri_detect(rmd[i], regex = "^##+")
      if (is_inline) {
        index <- min(chunks[chunks > i], na.rm = TRUE)
        if (is.infinite(index)) index <- n + 1
        
        # Eval inline code and append to results
        res <- eval_lines(rmd[i:(index-1)], envir = envir)
        results <- c(results, res)
        
      } else {
        index <- min(inlines[inlines > i], na.rm = TRUE)
        if (is.infinite(index)) index <- n + 1
        
        # Eval chunks and append to results
        res <- evaluate::evaluate(rmd[i:(index-1)], envir = envir)
        results <- c(results, res)
      }
      
    }
    
  }
  
  # Return
  results
  
}

# Evaluate rmarkdown code  -----------------------------------------------------
eval_lines <- function(lines, envir) {
  
  lines <- unlist(lapply(lines, eval_inline, envir = envir))
  
  # Remove ##+
  lines <- stri_replace(lines, "", regex = "^##\\+ ")
  titles <- which(stri_detect(lines, regex = "^# |^## "))
  
  lines
  
}

eval_inline <- function(line, envir = parent.frame()) {
  
  pattern <- default$pattern$rmd
  is_inline <- stri_detect(line, regex = pattern$inline)
  
  if (!is.na(is_inline) && isTRUE(is_inline)) {
    inline <- unlist(stri_extract_all(line, regex = pattern$inline))
    expr <- stri_replace_all(inline, "", regex = "`r\\s?|\\s?`")
    for (i in seq_along(expr)) {
      res <- as.character(eval(parse(text = expr[i]), envir))
      if (length(res) > 0L && !is.na(res)) {
        line <- sub(inline[i], stri_c(res, collapse = " "), line, fixed = TRUE)
      } else {
        line <- NULL
      }
    }
    
  }
#   
#   inline <- unlist(stringr::str_extract_all(line, pattern))
#   expr <- stringr::str_replace_all(inline, "`r\\s?|\\s?`", "")
#   
#   for (i in seq_along(expr)) {
#     res <- as.character(eval(parse(text = expr[i]), envir))
#     line <- sub(inline[i], paste(res, collapse = " "), line, fixed = TRUE)
#   }
#   
#   
#   pattern <- default$pattern$rmd
#   is_inline <- stri_detect(lines, regex = pattern$inline)
# 
#   if (any(is_inline, na.rm = TRUE)) {
#     
#     inline <- stringi::stri_extract_all(lines[is_inline], regex = pattern$inline)
#     expres <- stringi::stri_replace_all(inline, "", regex = "`r\\s?|\\s?`")
#     
#     for (i in seq_along(expres)) {
#       result <- as.character(eval(parse(text = expres[i]), envir))
#       lines[is_inline][i] <- stri_replace_all(lines[is_inline][i], stri_c(result, collapse = " "), fixed = inline[i])
#     }
#     
  # }

  # Return
  line
  
}

# Replace chunk delims  --------------------------------------------------------
replace_chunk_delim <- function(lines) {
  
  # Get default pattern
  pattern <- default$pattern$rmd
  
  # Identify which (if any) chunks contain eval options
  chunk_eval <- which(stri_detect(lines, regex = pattern$chunk_eval)) 
  chunk_end <- which(stri_detect(lines, regex = pattern$chunk_end))
  
  if (length(chunk_eval) > 0L) {
    
    # Find the correct chunk end for each chunk eval
    chunk_end <- vapply(chunk_eval, function(x) chunk_end[chunk_end > x][1], numeric(1))

    # Replace chunk-delimiters with if-functions
    lines[chunk_eval] <- stri_c("if (", stri_replace(lines[chunk_eval], "$1", regex = pattern$chunk_eval), ") {")
    lines[chunk_end] <- "}"
    
    # Double indendt the content in if-functions
    chunk_index <- unlist(Map(':', chunk_eval, chunk_end))
    
    content <- setdiff(chunk_index, c(chunk_eval, chunk_end))
    lines[content] <- stri_pad_left(lines[content], width = 2)
    
  }
  
  # Remove chunk start/endings without eval options
  lines <- stri_replace_all(lines, "", regex = stri_c(pattern$chunk_start, ".*"))
  lines <- stri_replace_all(lines, "", regex = stri_c(pattern$chunk_end, ".*"))
  
  # Return
  lines
  
}
