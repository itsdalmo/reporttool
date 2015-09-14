context("Survey")

lst <- read_data("test_input.xlsx")
raw <- read_data("test_raw.xlsx")
srv <- survey(raw)

test_that("Creating new scaffolds", {
  
  nm <- default$structure$mm
  df <- new_scaffold(nm)
  
  expect_true(is.data.frame(df))
  expect_identical(names(df), nm)
  expect_identical(names(df), stringi::stri_trans_tolower(names(df)))
  expect_true(nrow(df) == 0L)
  
})

test_that("Creating a survey from raw" , {
  
  x <- add_mm(srv)
  x <- set_association(x, common = TRUE)
  x <- add_entities(x)
  
  expect_true(all(c("mainentity", default$latents) %in% x$mm$latent[1:8]))
  expect_identical(x$ents$entity, "Example")
  expect_identical(x$ents$n, 19L)
  
})

test_that("Creating a survey with input" , {
  
  x <- add_mm(srv, lst[["measurement model"]])
  x <- set_association(x, common = TRUE)
  
  # Lowercase fix
  x$mm$manifest <- stringi::stri_trans_tolower(x$mm$manifest)
  x <- add_entities(x)
  
  expect_true(all(c("mainentity", default$latents) %in% tolower(x$mm$latent)))
  expect_true(all(is.na(x$mm$type)))
  expect_identical(x$ents$entity, "Example")
  expect_identical(x$ents$n, 19L)
  
})

test_that("Changing columnames for surveys", {
  
  x <- add_mm(srv)
  x <- set_association(x, common = TRUE)
  x <- rename(x, mainentity = q1)
  
  expect_identical(names(x$df)[1], "mainentity")
  expect_identical(names(x$df), x$mm$manifest)
  
})

test_that("Changing marketshares for entities", {
  
  x <- add_mm(srv)
  x <- set_association(x, common = TRUE)
  x <- add_entities(x)
  x <- set_marketshares(x, Example = .3)
  
  expect_identical(x$ents$marketshare, .3)
  
})

test_that("Setting config and translations for survey", {
  
  x <- set_config(srv)
  x <- set_translation(x)
  
  expect_identical(default$config$value, x$cfg$value)
  expect_identical(default$translation$norwegian, x$tr$replacement)
  
  x <- set_config(srv, reporttool = "test")
  x <- set_translation(x, image = "test")
  
  expect_identical(x$cfg$value[1], "test")
  expect_identical(x$tr$replacement[1], "test")
  
})

test_that("Preparing data works for survey", {
  
  x <- add_mm(srv)
  x <- set_association(x, common = TRUE)
  x <- add_entities(x)
  x <- set_config(x)
  x <- set_translation(x)
  x <- prepare_data(x, "mean")
  
  expect_true(all(default$latents %in% names(x$df)))
  expect_true(all(default$latents %in% x$mm$manifest))
  expect_identical(names(x$df)[1], "coderesp")
  expect_true("percent_missing" %in% names(x$df))
  expect_identical(x$df$percent_missing[6], 0)
  expect_more_than(x$df$percent_missing[7], 0.7142)
  
})