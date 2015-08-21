context("Survey")

lst <- read_data("test_input.xlsx")
raw <- read_data("test_raw.xlsx")


test_that("Creating a survey from raw" , {
  
  x <- survey(raw)
  x <- add_mm(x)
  x <- set_association(x, common = TRUE)
  x <- add_entities(x)
  
  expect_true(all(c("mainentity", default$latents) %in% x$mm$latent[1:8]))
  expect_identical(x$ents$entity, "Example")
  expect_identical(x$ents$n, 19L)
  
})

test_that("Creating a survey with input" , {

  x <- survey(raw)
  x <- add_mm(x, lst[["measurement model"]])
  x <- set_association(x, common = TRUE)
  
  # Lowercase fix
  x$mm$manifest <- stringi::stri_trans_tolower(x$mm$manifest)
  x <- add_entities(x)
  
  expect_true(all(c("mainentity", default$latents) %in% tolower(x$mm$latent)))
  expect_true(all(is.na(x$mm$type)))
  expect_identical(x$ents$entity, "Example")
  expect_identical(x$ents$n, 19L)
  
})

test_that("Creating new scaffolds", {
  
  nm <- default$structure$mm
  df <- new_scaffold(nm)
  
  expect_true(is.data.frame(df))
  expect_identical(names(df), nm)
  expect_identical(names(df), stringi::stri_trans_tolower(names(df)))
  expect_true(nrow(df) == 0L)
  
})
