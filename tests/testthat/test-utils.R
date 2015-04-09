context("Utils - paths and extensions")
fdir <- system.file("tests/testthat/test.xlsx", package="reporttool")

test_that("Validation of paths and extensions works", {
  
  # File path
  expect_true(has_extension(fdir, "xlsx"))
  expect_true(is_supported_ext(fdir))
  expect_true(is_supported_ext(dirname(fdir), fdir))
  
  # Check error handeling
  expect_error(validate_path(rep(path, 2)))
  expect_error(validate_path(numeric(1)))
  expect_error(validate_path("exists_not"))
  
  # Path validation
  vp <- validate_path(fdir)
  expect_true(is_supported_ext(paste0(vp, "/")))
  
})