# Test utilities ---------------------------------------------------------------
path <- system.file("tests/testthat/xlsx.xlsx", package="reporttool")

context("Utility functions work as expected")
test_that("path validation is working", {
  
  expect_false(grepl("/$", validate_path(paste0(path, "/"))))
  
  expect_error(validate_path(rep(path, 2)))
  expect_error(validate_path(numeric(1)))
  
})

test_that("extensions checking and validation works", {
  
  expect_true(has_extension(path, "xlsx"))
  expect_true(is_supported_ext(path))
  expect_true(is_supported_ext(basename(path), path))
  
})

test_that("clean scores works", {
  
  expect_true(clean_score("1 aa") == "1")
  expect_identical(clean_score(c("1 aa", "bb 1", "10 cc")), c("1", "bb 1", "10"))
  
})

test_that("cleaning and rescaling scores works", {
  
  expect_true(clean_score("1 aa") == "1")
  expect_identical(clean_score(c("1 aa", "bb 1", "10 cc")), c("1", "bb 1", "10"))
  expect_identical(rescale_score(c("1", 10)), c(0, 100))
  
})

test_that("extracting sheetnames from xlsx works", {
  
  expect_identical(get_sheet_names("xlsx.xlsx"), "xlsx")
  
  
})