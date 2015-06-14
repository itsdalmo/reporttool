context("Data preparation")

lst <- read_data("test_input.xlsx")
raw <- read_data("test_raw.xlsx")


test_that("Prepare data (mean) from raw works" , {
  
  x <- prepare_data(raw, latents = "mean", impute = FALSE)
  
  expect_true(all(c("mainentity", cfg$latent_names) %in% x$mm$latent[1:8]))
  expect_true(all(c("percent_missing", "w", cfg$latent_names) %in% names(x$df)))
  expect_identical(x$ents$entity, "Example")
  expect_identical(x$ents$n, 19L)
  
})

test_that("Prepare data (mean) from raw works" , {
  
  lst[["data"]] <- raw
  x <- prepare_data(lst, latents = "mean", impute = FALSE)
  
  expect_true(all(c("mainentity", cfg$latent_names) %in% tolower(x$mm$latent)))
  expect_true(all(c("percent_missing", "w", cfg$latent_names) %in% names(x$df)))
  expect_identical(x$ents$entity, "Example")
  expect_identical(x$ents$n, 19L)
  
})
