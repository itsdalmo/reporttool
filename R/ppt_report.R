# Mid-work-backup

rmd_to_script <- function(rmd, encoding = UTF-8) {
  
  rmd <- path <- validate_path(rmd)
  
  if (!has_extension(rmd, "rmd")) {
    stop("This function only accepts a .Rmd file", call. = FALSE)
  } else {
    rmd <- readLines(rmd, encoding = encoding)
  }
  
  # Identify yaml
  idx_yaml <- grep("^---", rmd)
  idx_yaml <- idx_yaml[1]:idx_yaml[2]
  
  # Identify chunks
  start_idx <- grep(reporttool$rmd_pat$chunk_start, rmd)
  end_idx <- grep(reporttool$rmd_pat$chunk_end, rmd)
  
  if (length(start_idx) != length(end_idx)) {
    stop("The .Rmd file contains unused chunk start/end indicators", call. = FALSE)
  }
  
  # Get a list of all chunk indices
  chunk_idx <- Map(':', start_idx, end_idx)
  
  # Get slide and section indices found inside chunks
  chunk_slide <- reporttool$rmd_pat$chunk_slide
  
  slide_idx <- intersect(grep(paste0(chunk_slide, "##[^#]"), rmd), unlist(chunk_idx))
  section_idx <- intersect(grep(paste0(chunk_slide, "#[^#]"), rmd), unlist(chunk_idx))
  
  # Add slide and section indeces outside chunks
  slide_idx <- append(setdiff(grep("(^##|`r.*##)[^#].*", rmd), unlist(chunk_idx)), slide_idx)
  section_idx <- append(setdiff(grep("(^#|`r.*#)[^#].*", rmd), unlist(chunk_idx)), section_idx)
  
  # Replace chunk-evals with if statements and replace chunk identifiers
  for (i in seq_along(chunk_start_idx)) {
    rmd <- c(rmd[1:(chunk_start_idx[i]-1)], 
            paste("# Chunk", i), 
            replace_chunk_eval(rmd[chunk_start_idx[i]:chunk_end_idx[i]]),
            rmd[(chunk_end_idx[i]+1):length(rmd)])
  }
  
  # Write the cleaned file
  path <- tools::file_path_sans_ext(path)
  script <- file(paste0(path, "-script.R"), encoding = encoding)
  on.exit(close(script), add = TRUE)
  writeLines(rmd, script)
}

#' @import tools
#' @export 
rmd_to_script <- function(rmd, encoding = "UTF-8") {
  
  path <- validate_path(rmd)
  
  if (!has_extension(path, "rmd")) {
    stop("This function only accepts a .Rmd file", call. = FALSE)
  } else {
    rmd <- readLines(path, encoding = encoding)
  }
  
  # Identify yaml
  idx_yaml <- grep("^---", rmd)
  idx_yaml <- idx_yaml[1]:idx_yaml[2]
  
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
  
  rmd[not_chunk_idx] <- paste("## -", rmd[not_chunk_idx])
  
  # Iterate through the chunks and clean the document
  for (i in seq_along(chunk_idx)) {
    
    idx <- chunk_idx[[i]]
    chnm <- paste("# CHUNK", i)
    
    ch <- rmd[idx]
    ch <- c(paste(chnm, paste(rep("-", 79-nchar(chnm)), collapse = "")), paste("# ",ch[1]), ch)
    
    ch <- replace_chunk_eval(ch)
    ch[grep("^```", ch)] <- ""
    
    rmd <- c(rmd[1:(min(idx)-1)], ch, rmd[(max(idx)+1):length(rmd)])
    
  }
  
  # Save the converted rmd
  path <- file(paste0(tools::file_path_sans_ext(path), ".R"), encoding = encoding)
  on.exit(close(rmd), add = TRUE)
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


replace_chunk_eval <- function(lines) {
  
  chunk_end <- reporttool$rmd_pat$chunk_end
  chunk_eval <- reporttool$rmd_pat$chunk_eval
  
  # Identify which (if any) chunks contain eval options
  eval_idx <- grep(chunk_eval, lines)
  
  if (length(eval_idx) > 0) {
    end_idx <- grep(chunk_end, lines)
    
    # Find the correct chunk end for each chunk eval
    end_idx <- vapply(eval_idx, function(x) end_idx[end_idx > x][1], numeric(1))
    
    # Replace chunk-delimiters and indent (double space) if statements
    lines[eval_idx] <- paste0("if (", gsub(chunk_eval, "\\1", lines[eval_idx]), ") {")
    lines[end_idx] <- "}"
    
  }

  return(lines)
}

