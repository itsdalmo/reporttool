# Test input  ------------------------------------------------------------------
path <- system.file("tests/testthat", package = "reporttool")

xlsx <- file.path(path, "test.xlsx")
csv2 <- file.path(path, "semicolon.csv")
csv <- file.path(path, "colon.csv") 
docx <- file.path(path, "invalid.docx")

context("Funs  - read_sheets")
test_that("Handles input and returns expected format", {
  
  # Input
  df <- read_sheets(xlsx)
  sh <- c("raw Data", "measurement model")
  
  expect_true(is.list(df))
  expect_true(all(vapply(df, is.data.frame, logical(1))))
  
  # Check result
  expect_identical(sh, names(df))
  expect_identical(read_sheets(xlsx, sh), df)
  expect_true(is.na(df[[2]][1,3]))
  
  # Clean missing
  df <- read_sheets(xlsx, clean.missing = TRUE)
  expect_true(all(is.na(df[[1]][1:9,2:4])))
  
  # Error handeling
  expect_error(read_sheets(path))
  expect_error(read_sheets(xlsx, sheets = "invalid sheet"))
  
})

context("Input - basic wrappers")
test_that("read_xlsx handles input and returns expected format", {
  
  # Check format
  df <- read_xlsx(xlsx)
  sh <- c("raw Data", "measurement model")
  
  expect_true(all(is.na(df[[1]][1:9,2:4])))
  expect_identical(tolower(sh), names(df))
  
})

test_that("read_csv handles input and returns expected format", {
  
  enc <- "latin1"
  
  # Check that semi/colon yield same result
  expect_equal(read_csv(csv2, enc), read_csv(csv, enc))
  
  # Check results
  df <- read_csv(csv2, enc)
  
  expect_true(is.data.frame(df))
  expect_true(all(is.na(df[1:9,2:4])))
  
  # Error handeling
  expect_error(read_sheets(path))
})

test_that("read_dir handles input and returns expected format", {
  
  expect_error(read_dir(path))
  
})

context("Input - switch function")
test_that("read_data handles input and returns expected format", {
  
  df <- read_data(xlsx)[[1]]
  
  expect_equal(read_data(csv2), df)
  expect_equal(read_data(csv), df)
  
  expect_error(read_data(docx))
})

context("Funs  - list_input")
test_that("Handles input and returns expected format", {
  
  df <- list_input(xlsx, xlsx, xlsx)
  sh <- c("raw data", "measurement model", "contrast data", "historic data")
  
  expect_true(is.list(df))
  expect_true(all(sh %in% names(df)))
  expect_true(all(vapply(df, is.data.frame, logical(1))))
  
  expect_equal(df[[1]], df[[3]])
  expect_equal(df[[1]], df[[4]])
  
  expect_error(list_input(csv))
  
})
