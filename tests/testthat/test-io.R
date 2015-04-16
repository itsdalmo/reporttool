context("Get input")
test_that("Read/write .xlsx works and returns expected result", {
  lst <- get_input("xlsx.xlsx", "csv.csv", "csv2.csv")
  
  expect_true(identical(names(lst), c("input", "contrast data", "historic data")))
  expect_identical(lst[[1]], lst[[2]])
  expect_true(inherits(lst, "list"))
})


context("Reading and writing data")
test_that("Read/write .xlsx works and returns expected result", {
  
  # Read
  xlsx <- read_data("xlsx.xlsx")
  
  expect_true(inherits(xlsx, "data.frame"))
  expect_true(identical(tolower(names(xlsx)), names(xlsx)))
  expect_true(identical(xlsx$mainentity, paste("Test", 1:3)))
  expect_true(identical(xlsx$missing, rep(NA, 3)))
  
  # Write
  fileName <- file.path(tempdir(), "xlsx.xlsx")
  write_data(xlsx, fileName)
  
  # Read data again and convert missing to numeric (openxlsx prob.)
  w_xlsx <- read_data(fileName)
  w_xlsx$missing <- as.logical(w_xlsx$missing)
  
  expect_identical(w_xlsx, xlsx)

  unlink(fileName, recursive = TRUE, force = TRUE)
})

test_that("Read/write works with lists of data", {
  
  # Read example data
  sheet1 <- read_data("xlsx.xlsx")
  sheet2 <- read_data("csv2.csv")
  
  lst <- list("csv" = sheet1, "csv2" = sheet2)
  
  # Write csv
  dirName <- file.path(tempdir())
  
  write_data(lst, file.path(dirName, "test.csv"))
  
  expect_false(file.exists(file.path(dirName, "test.csv")))
  expect_true(file.exists(file.path(dirName, "csv.csv")))
  expect_true(file.exists(file.path(dirName, "csv2.csv")))

  expect_identical(read_data(file.path(dirName, "csv.csv")),
                   read_data(file.path(dirName, "csv2.csv")))
  
  unlink(dirName, recursive = TRUE, force = TRUE)
  
  # Write xlsx
  fileName <- file.path(tempdir(), "xlsx.xlsx")
  write_data(lst, fileName)
  
  expect_true(file.exists(fileName))
  
  # Read data again and convert missing to numeric (openxlsx prob.)
  w_xlsx <- read_data(fileName)
  w_xlsx$csv$missing <- as.logical(w_xlsx$csv$missing)
  w_xlsx$csv2$missing <- as.logical(w_xlsx$csv2$missing)
  
  expect_identical(lst, w_xlsx)

  unlink(fileName, recursive = TRUE, force = TRUE)
})

test_that("Read/write .csv works and returns expected result", {
  
  # Read
  xlsx <- read_data("xlsx.xlsx")
  csv <- read_data("csv.csv")
  csv2 <- read_data("csv2.csv")
  
  expect_identical(csv, csv2)
  expect_identical(csv2, xlsx)
  expect_identical(names(csv2), tolower(names(csv2)))
  
  # Write
  fileName <- file.path(tempdir(), "csv2.csv")
  write_data(csv2, fileName)
  
  w_csv2 <- read_data(fileName)
  expect_identical(w_csv2, csv2)
  
  unlink(fileName, recursive = TRUE, force = TRUE)
})

test_that("Write .txt works and returns expected result", {
  
  # Write
  fileName <- file.path(tempdir(), "txt.txt")
  
  csv2 <- read_data("csv2.csv")
  write_data(csv2, fileName)
  
  txt <- read.table(fileName, sep=",", header=TRUE, fileEncoding="latin1",
                    stringsAsFactors = FALSE)
  
  expect_identical(txt, csv2)
  expect_identical(names(txt), tolower(names(txt)))
  
  unlink(fileName, recursive = TRUE, force = TRUE)
})

test_that("Read/write directory works and returns expected result", {
  
  fileName <- file.path(tempdir())
  csv2 <- read_data("csv2.csv")
  
  expect_error(write_data(csv2, fileName))
  expect_error(read_data("./"))
  
  unlink(fileName, recursive = TRUE, force = TRUE)
})

test_that("Read/write error handeling", {
  
  expect_error(read_data("invalid.xlsx"))
  
})