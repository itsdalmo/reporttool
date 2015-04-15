# Test utilities ---------------------------------------------------------------
path <- system.file("tests/testthat/xlsx.xlsx", package="reporttool")

context("Utility functions work as expected")
test_that("Path validation is working", {
  
  expect_false(grepl("/$", validate_path(paste0(path, "/"))))
  
  expect_error(validate_path(rep(path, 2)))
  expect_error(validate_path(numeric(1)))
  
})

test_that("Extensions checking and validation works", {
  
  expect_true(has_extension(path, "xlsx"))
  expect_true(is_supported_ext(path))
  expect_true(is_supported_ext(dirname(path), path))
  
})