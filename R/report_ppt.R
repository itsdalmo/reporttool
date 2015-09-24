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

#' Convert .Rmd to .R
#'
#' This function converts a \code{.Rmd} to a \code{.R} file (similar to \code{knitr::purl}) 
#' by replacing chunk delimiters and putting \code{eval} chunk options inside \code{if}
#' statements. Also comments out (\code{##+}) content that is not inside chunks.
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
    chunk <- replace_chunk_delim(rmd[idx])
    
    # Piece together the chunk (w/delimiters) and the rest of the document
    if (max(idx) < length(rmd)) {
      last_line <- rmd[(max(idx)+1):length(rmd)]
    } else if (chunk[length(chunk)] == "") {
      last_line <- NULL
    } else {
      last_line <- ""
    }
    
    rmd <- c(if (min(idx) > 1) { rmd[1:(min(idx)-1)] }, chunk, if (!is.null(last_line)) { last_line })
    
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

# Evaluate rmarkdown code  -----------------------------------------------------
eval_lines <- function(lines, envir) {
  
  
  lines <- eval_inline(lines, envir = envir)
  
  # Remove ##+
  lines <- stri_replace(lines, "", regex = "^##\\+ ")
  titles <- which(stri_detect(lines, regex = "^# |^## "))
  
  lines
  
}


eval_inline <- function(lines, envir = parent.frame()) {
  
  pattern <- default$pattern$rmd
  is_inline <- stri_detect(lines, regex = pattern$inline)
  
  if (!any(is_inline, na.rm = TRUE)) return(lines)
  
  for (i in seq_along(lines)) {
    
    inline <- unlist(stri_extract_all(lines[i], regex = pattern$inline))
    expres <- stri_replace_all(inline, "", regex = "`r\\s?|\\s?`")
    results <- lapply(expres, function(x) as.character(eval(parse(text = x), envir)))
    
    lines[i] <- stri_replace_all_fixed(lines[i], 
                                       pattern = inline, 
                                       replacement = unlist(results), 
                                       vectorize_all = FALSE)
    
  }
  
  lines
  
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
    lines[content] <- stri_c("  ", lines[content])
    
  }
  
  # Remove chunk start/endings without eval options
  lines <- stri_replace_all(lines, "", regex = stri_c(pattern$chunk_start, ".*"))
  lines <- stri_replace_all(lines, "", regex = stri_c(pattern$chunk_end, ".*"))
  
  # Return
  lines
  
}
