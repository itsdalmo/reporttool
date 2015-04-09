prepare_data <- function(lst, latents = NULL) {
  
  # --- Check minimum requirements for the function
  if (!is.list(lst))
    stop("The function requires a list as input", call. = FALSE)

  if (!all(any_in(c("data", "raw data"), lst), any_in("measurement model", lst)))
    stop("Required data are not present, or do not have correct names", call. = FALSE)
  
  if (!valid_format("measurement model", lst))
    stop("Measurement model does not contain the prespecified columns", call = FALSE)
  
  
  
  # --- Check if data should be cleaned
  if (!any_in("data", lst) || nrow(lst[["data"]]) == 0L)
    lst[["data"]] <- clean_data(lst)
  
  # --- Add latents if desired
  if (!is.null(latents) && latents == "mean")
    print("test")
    
}


valid_format <- function(content, lst) {
  
  is_valid <- TRUE  
  cols <- default$required_cols[[content]]
  
  if (!is.null(cols))
    is_valid <- all(cols %in% names(lst[[content]]))
  
  all(in_list, is_valid)
}

any_in <- function(content, lst) {
  any(content %in% names(lst))
}
