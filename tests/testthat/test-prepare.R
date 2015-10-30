context("Prepare data")

raw <- read_data("test_raw.xlsx")

test_that("Prepare data", {
  
  x <- survey(raw)
  x <- add_mm(x)
  x <- set_association(x, common = TRUE)
  x <- add_entities(x)
  x <- set_config(x)
  x <- set_translation(x)
  
  # Prepare data
  y <- prepare_data(x, type = "mean")
  z <- prepare_data(x, type = "pls")
  
  # Check results
  expect_identical(names(y$df)[1], "coderesp")
  expect_true(all(c(default$latents, "percent_missing", "q5aem") %in% names(y$df)))
  expect_identical(names(y$df), y$mm$manifest)
  
  # PLS
  expect_identical(names(z$df)[1], "coderesp")
  expect_identical(names(z$df)[ncol(z$df)], "percent_missing")
  expect_true(!any(default$latents %in% names(z$df)))
  
})