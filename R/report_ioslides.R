#' ioslides template
#'
#' Template for generating ioslides reports.
#'
#' @param dev Defaults to svg for higher quality graphics. Standard in ioslides
#' is png.
#' @param smaller Use smaller text for the slides by default.
#' @author Kristian D. Olsen
#' @note Simple wrapper. Custom CSS is on the way.
#' @export
#' @examples 
#' render("index.Rmd", "reporttool::io_template")

io_template <- function(dev = "svg", smaller = TRUE) {
  
  # Edit beamer_presentation
  format <- rmarkdown::beamer_presentation(dev = dev, smaller = smaller)
  
  # Change the default chunk-options
  format$knitr$opts_chunk$echo <- FALSE
  format$knitr$opts_chunk$message <- FALSE
  format$knitr$opts_chunk$warning <- FALSE
  format$knitr$opts_chunk$dev <- dev
  format$knitr$opts_chunk$dev.args <- list(bg = "transparent")
  format$knitr$opts_chunk$results <- "asis"
  
  format
  
}

#' @rdname beamer_template

generate_io <- function(entity, dir, envir) {
  
  # Render the document
  rmarkdown::render(file.path(dir, "Markdown", paste0(entity, ".Rmd")),
                    output_format = "all",
                    intermediates_dir = file.path(dir, "Markdown"),
                    output_dir = file.path(dir, "Reports"),
                    quiet = TRUE,
                    envir = envir, 
                    encoding = "UTF-8")
}