# #' Summaries
# #'
# #' A few wrappers to make a common tasks less verbose.
# #' 
# #' @section List of summaries:
# #' 
# #' \describe{
# #' 
# #'    \item{\code{recode}}{Recode variables.} 
# #'
# #' }
# #' 
# #' @name Summaries
# #' @author Kristian D. Olsen
# #' @rdname utilities
# #' @export
# #' @examples 
# #' get_default("palette")
# 
# info_list <- function(srv, entity) {
#   
#   # Check the input
#   if (!is.survey(srv)) {
#     stop("Argument 'survey' is not an object with the class 'survey'. See help(survey).", call. = FALSE)
#   } else {
#     srv <- prepare_report(srv)
#   }
#   
#   # Measurement model must be added first
#   if (!is.survey_mm(srv$mm) || !nrow(srv$mm)) {
#     stop("The measurement model must be added first. See help(add_mm).", call. = FALSE)
#   }
#   
#   # Entities must be added first
#   if (!is.survey_ents(srv$ents) || !nrow(srv$ents)) {
#     stop("The entities must be added first. See help(add_entities).", call. = FALSE)
#   }
#   
#   # Add dates if they exist
#   if (any(srv$mm$type == "Date", na.rm = TRUE)) {
#     
#     # If more than one datevariables - use the first one
#     var <- filter(mm, type == "Date")[["manifest"]][1]
#     var <- filter(srv$df, mainentity == entity)[[var]]
#     
#     if (!inherits(var, "Date")) stop("Date variable is not actually of type Date.", call. = FALSE)
#     
#     dates <- data_frame(start = min(var, na.rm = TRUE), end = max(var, na.rm = TRUE))
#     dates <- mutate(info$dates, month = format(start, "%m"),
#                                     year = format(start, "%Y"),
#                                     start = format(start, "%e. %b. %Y"),
#                                     end = format(end, "%e. %b. %Y"))
#   } else {
#     dates <- NULL
#   }
#   
#   # Add subentities if specified
#   if ("subentity" %in% names(srv$df)) {
#     cutoff <- as.numeric(filter(srv$cfg, config == "cutoff")[[value]])
#     sub <- filter(srv$df, mainentity == entity, percent_missing <= cutoff)
#     sub <- group_by(sub, subentity)
#     sub <- summarise(sub, valid = n())
#     sub <- mutate(valid = stri_c(subentity, valid, sep = " "))
#   } else {
#     sub <- NULL
#   }
#   
#   # Response information
#   resp <- filter(srv$ents, entity == entity)
#   resp <- select(resp, n, valid)
#   resp <- mutate(resp, valid_percent = valid/n)
#   
#   # Model questions
#   questions <- nrow(filter(mm, stri_trans_tolower(latent) %in% default$latents))
#   
#   # Return
#   list(respondents = resp, questions = questions, dates = dates, subentities = sub)
#   
# }
# 
# #' @export
# create_table <- function(srv, ..., entities = NULL, long_format = FALSE, questions = TRUE) {
#   
#   dots <- lazyeval::lazy_dots(...)
#   
#   # Check the input
#   if (!is.survey(srv)) {
#     stop("Argument 'survey' is not an object with the class 'survey'. See help(survey).", call. = FALSE)
#   } else {
#     srv <- prepare_report(srv)
#   }
#   
#   # Measurement model must be added first
#   if (!is.survey_mm(srv$mm) || !nrow(srv$mm)) {
#     stop("The measurement model must be added first. See help(add_mm).", call. = FALSE)
#   }
#   
#   # 2x length dataset to produce average as well
#   if (is.data.frame(srv$cd) && nrow(srv$cd)) {
#     tr <- filter(srv$tr, original == "contrast_average")[["replacement"]]
#     df <- bind_rows(srv$df, mutate(srv$cd, mainentity = tr))
#   } else {
#     tr <- filter(srv$tr, original == "study_average")[["replacement"]]
#     df <- bind_rows(srv$df, mutate(srv$df, mainentity = tr))
#   }
#   
#   # Subset data if entities are specified
#   if (is.null(entities)) {
#     if (is.factor(df$mainentity)) {
#       entities <- levels(df$mainentity)
#     } else {
#       entities <- unique(as.character(df$mainentity))
#     }
#   } else {
#     df <- filter(df, mainentity %in% c(entities, tr))
#   }
#   
#   # Mainentity should be a factor variable
#   df <- mutate(df, mainentity = factor(mainentity, levels = c(entities, tr)))
#   
#   # Set w to 1 for all rows but the average
#   df <- mutate(df, w = ifelse(mainentity == tr, w, 1))
#   df <- mutate(df, w = as.numeric(w))
# 
#   # Select relevant columns and figure out their types
#   df <- select_(df, .dots = c(dots, "mainentity", "w"))
# 
#   is_character <- names(df)[vapply(df, is.character, logical(1))]
#   if (length(is_character)) {
#     warning("The following columns are character vectors and will not be included:\n",
#             stri_c(is_character, collapse = ", "), call. = FALSE)
#     df <- select(df, -one_of(is_character))
#   }
#   
#   # Create the tables
#   tables <- list()
#   
#   # Factors
#   is_factor <- names(df)[vapply(df, is.factor, logical(1))]
#   is_factor <- setdiff(is_factor, "mainentity")
#   
#   if (length(is_factor)) {
#     dff <- select(df, mainentity, w, one_of(is_factor))
#     
#     equal_levels <- lapply(dff[, is_factor], levels)
#     equal_levels <- vapply(equal_levels, identical, y = equal_levels[[1]], logical(1))
#     
#     if (all(equal_levels)) {
#       tables$factors <- prop_table(dff, srv$mm, long = long_format, questions = questions)
#     } else {
#       tables$factors <- lapply(is_factor, function(nm, df, mm) {
#         prop_table(select(df, one_of("mainentity", "w", nm)), srv$mm, long = long_format, questions = questions)
#       }, dff, srv$mm)
#     }
#     
#   }
#   
#   # Numerics
#   is_numeric <- names(df)[vapply(df, is.numeric, logical(1))]
#   is_numeric <- setdiff(is_numeric, "w")
#   
#   if (length(is_numeric)) {
#     dfn <- select(df, mainentity, w, one_of(is_numeric))
#     tables <- list(tables, numerics = score_table(dfn, srv$mm, long = long_format, questions = questions))
#   }
# 
#   # Return
#   tables
#   
# }
# 
# prop_table <- function(df, mm, dots, long, questions) {
#   
#   if (ncol(df) > 3L) {
#     df <- tidyr::gather(df, manifest, answer, -mainentity, -w)
#   } else {
#     df$manifest <- names(df)[3]
#     df$answer <- df[[3]]
#     df <- select(df, mainentity, w, manifest, answer)
#   }
# 
#   df <- filter(df, !is.na(answer))
#   df <- count(df, mainentity, manifest, answer)
#   df <- mutate(df, prop = prop.table(n))
#   
#   if (questions) {
#     df <- left_join(df, mm, by = c("manifest" = "manifest"))
#     df <- select(df, mainentity, manifest, question, answer, n, prop)
#   }
#   
#   if (!long) {
#     df <- mutate(df, n = sum(n))
#     df <- tidyr::spread(df, answer, prop, fill = 0)
#     df <- arrange(df, manifest, mainentity)
#   } else {
#     df <- arrange(df, manifest, mainentity)
#   }
#   
#   # Return
#   df
#   
# }
# 
# score_table <- function(df, mm, long, questions) {
#   
#   # Subset and summarise
#   df <- mutate_each(df, funs(as.numeric(.)), -mainentity)
#   df <- group_by(df, mainentity)
#   df <- summarise_each(df, funs(weighted.mean(., w = w, na.rm = TRUE)), -w)
#   
#   # If long format
#   if (long) {
#     df <- tidyr::gather(df, manifest, score, -mainentity)
#     df <- mutate(df, manifest = as.character(manifest))
#     df <- left_join(df, mm, by = c("manifest" = "manifest"))
#     df <- mutate(df, question = factor(question, levels = unique(question), ordered = TRUE))
#     df <- select(df, mainentity, manifest, question, score)
#     df <- arrange(df, question, mainentity)
#   } else if (questions) {
#     mm <- filter(mm, manifest %in% names(df))
#     df <- arrange(df, mainentity)
#     names(df) <- ordered_replace(names(df), setNames(mm$manifest, mm$question))
#   }
# 
#   # Return
#   df
#   
# }