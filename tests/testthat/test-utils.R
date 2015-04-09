# Test utilities ---------------------------------------------------------------
path <- system.file("tests/testthat/test.xlsx", package="reporttool")

context("Utils - paths and extensions")
test_that("Path validation is working", {
  
  expect_false(grepl("/$", validate_path(paste0(path, "/"))))
  
  expect_error(validate_path(rep(path, 2)))
  expect_error(validate_path(numeric(1)))
  expect_error(validate_path("invalid path"))
  
})

test_that("Extensions checking and validation works", {
  
  expect_true(has_extension(path, "xlsx"))
  expect_true(is_supported_ext(path))
  expect_true(is_supported_ext(dirname(path), path))
  
})