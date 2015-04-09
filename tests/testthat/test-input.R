# # Data types
# sdir <- system.file("tests/testthat", package="reporttool")
# xlsx <- file.path(dir, "test.xlsx")
# csv2 <- file.path(dir, "semicolon.csv")
# csv <- file.path(dir, "colon.csv")
# 


context("Funs  - read_sheets")
test_that("Handles input and returns expected format", {
  
  fp <- system.file("tests/testthat/test.xlsx", package="reporttool")
  sh <- c("raw Data", "measurement model")
  
  # Check format
  df <- read_sheets(fp)
  
  expect_true(is.list(df))
  expect_true(all(vapply(df, is.data.frame, logical(1))))
  
  # Check result
  expect_equal(sh, names(df))
  expect_equal(read_sheets(fp, sh), df)
  expect_true(is.na(df[[2]][1,3]))
  
  # Clean missing
  df <- read_sheets(fp, clean.missing = TRUE)
  expect_true(all(is.na(df[[1]][1:9,2:4])))
  
  # Error handeling
  expect_error(read_sheets(dirname(fp)))
  expect_error(read_sheets(fp, sheets = "exists_not"))
  
})

context("Input - basic wrappers")
test_that("read_xlsx handles input and returns expected format", {
  
  fp <- system.file("tests/testthat/test.xlsx", package="reporttool")
  sh <- c("raw Data", "measurement model")
  
  # Check format
  df <- read_xlsx(fp)
  
  expect_true(is.list(df))
  expect_true(all(vapply(df, is.data.frame, logical(1))))
  expect_true(all(is.na(df[[1]][1:9,2:4])))
  expect_true(all(tolower(sh) %in% names(df)))
  
})

test_that("read_csv handles input and returns expected format", {
  
  # Files
  semi <- system.file("tests/testthat/semicolon.csv", package="reporttool")
  colon <- system.file("tests/testthat/colon.csv", package="reporttool")
  
  enc <- "latin1"
  
  # Check that semi/colon yield same result
  expect_equal(read_csv(semi, enc), read_csv(colon, enc))
  
  # Check results
  df <- read_csv(semi, enc)
  
  expect_true(is.data.frame(df))
  expect_true(all(is.na(df[1:9,2:4])))
  
  # Error handeling
  expect_error(read_sheets(dirname(semi)))
})

test_that("read_dir handles input and returns expected format", {
  
  # Files
  fp <- system.file("tests/testthat/test.xlsx", package="reporttool")
  dp <- dirname(fp)
  
  # Error handeling
  expect_error(read_dir(fp))
  expect_error(read_dir(dp))
})

context("Input - switch function")
test_that("read_input handles input and returns expected format", {
  
  fp <- system.file("tests/testthat/test.xlsx", package="reporttool")
  
  df <- read_input(fp)
  expect_true(is.data.frame(df[[1]]))
  
  # Check csv format
  semi <- system.file("tests/testthat/semicolon.csv", package="reporttool")
  expect_equal(df[[1]], read_input(semi))

  # Check errorhandeling
  fp <- system.file("test/testthat/invalid.docx", package="reporttool")
  expect_error(read_input(fp))
  
})

context("Funs  - get_input")
test_that("Handles input and returns expected format", {
  
  fp <- system.file("tests/testthat/test.xlsx", package="reporttool")
  semi <- system.file("tests/testthat/semicolon.csv", package="reporttool")
  sh <- c("raw Data", "measurement model")
  
  # Check format
  df <- read_sheets(fp)
  
  expect_true(is.list(df))
  expect_true(all(vapply(df, is.data.frame, logical(1))))
  
  # Check result
  expect_equal(sh, names(df))
  expect_equal(read_sheets(fp, sh), df)
  expect_true(is.na(df[[2]][1,3]))
  
  # Clean missing
  df <- read_sheets(fp, clean.missing = TRUE)
  expect_true(all(is.na(df[[1]][1:9,2:4])))
  
  # Error handeling
  expect_error(read_sheets(dirname(fp)))
  expect_error(read_sheets(fp, sheets = "exists_not"))
  
})
