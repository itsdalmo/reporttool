context("i/o")
test_that("Read/write single-sheet-xlsx works (clean missing and lowercase names)" , {
  
  # Read
  xlsx <- read_data("xlsx.xlsx")
  
  expect_is(xlsx, "data.frame")
  expect_identical(tolower(names(xlsx)), names(xlsx))
  expect_identical(xlsx$mainentity, paste("Test", 1:3))
  expect_identical(xlsx$missing, rep(NA_character_, 3))
  
  # Write
  fileName <- file.path(tempdir(), "xlsx.xlsx")
  write_data(xlsx, fileName)
  
  # Read data again
  w_xlsx <- read_data(fileName)
  
  expect_identical(w_xlsx, xlsx)
  
  unlink(fileName, recursive = TRUE, force = TRUE)
  
})

test_that("Writing tables to xlsx with to_sheet works:" , {
  
  # Read
  xlsx <- read_data("xlsx.xlsx")
  
  # Create workbook
  fileName <- file.path(tempdir(), "xlsx.xlsx")
  wb <- openxlsx::createWorkbook()
  
  # Add table and save
  to_sheet(xlsx, wb, title = "Table", sheet = "analysis")
  openxlsx::saveWorkbook(wb, fileName)
  
  # Read data again
  w_xlsx <- read_data(fileName)
  
  # Mutate to match
  names(w_xlsx) <- tolower(w_xlsx[1, ])
  w_xlsx <- w_xlsx[2:4, ]
  row.names(w_xlsx) <- 1:3
  
  expect_equal(w_xlsx, xlsx)
  
  unlink(fileName, recursive = TRUE, force = TRUE)
  
})

test_that("Read/write works with lists of data", {
  
  # Read example data
  sheet1 <- read_data("xlsx.xlsx")
  sheet2 <- read_data("csv2.csv", encoding = "latin1")
  
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
  expect_identical(lst, w_xlsx)

  unlink(fileName, recursive = TRUE, force = TRUE)
  
})

test_that("Read/write .csv works and returns expected result", {
  
  # Read
  xlsx <- read_data("xlsx.xlsx")
  csv <- read_data("csv.csv", encoding = "latin1")
  csv2 <- read_data("csv2.csv", encoding = "latin1")
  
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
  
  csv2 <- read_data("csv2.csv", encoding = "latin1")
  write_data(csv2, fileName)
  
  txt <- read.table(fileName, sep="\t", header=TRUE, fileEncoding="UTF-8",
                    colClasses = "character", stringsAsFactors = FALSE)
  txt <- read_data(fileName)
  txt <- set_missing(txt)
  
  expect_identical(txt, csv2)
  expect_identical(names(txt), tolower(names(txt)))
  
  unlink(fileName, recursive = TRUE, force = TRUE)
  
})

test_that("Read/write .Rdata works and returns expected result", {
  
  # Write
  fileName <- file.path(tempdir(), "rdata.Rdata")
  
  csv2 <- read_data("csv2.csv", encoding = "latin1")
  write_data(csv2, fileName)
  
  rdata <- read_data("rdata.Rdata")
  w_rdata <- read_data(fileName)
  
  # TODO: String encoding problems
  if ((Sys.info()["sysname"] == "Windows")) {
    Encoding(rdata$score) <- "UTF-8"
    Encoding(rdata$string) <- "UTF-8"
  }

  expect_identical(w_rdata, csv2)
  expect_identical(w_rdata, rdata)
  expect_identical(names(w_rdata), tolower(names(w_rdata)))
  
  unlink(fileName, recursive = TRUE, force = TRUE)
  
})

test_that("Read .sav works and returns expected result", {
  
  sav <- read_data("sav.sav")
  xlsx <- read_data("xlsx.xlsx")
  
  expect_identical(sav, xlsx)
  
  sav_cb <- read_data("sav.sav", codebook = TRUE)
  
  # Note that, mm will list type as numeric because conversion
  # to character is done after.
  expect_identical(sav_cb$mm$manifest, names(sav_cb$df))
  
})

test_that("Read/write error handeling", {
  
  expect_error(read_data("invalid.xlsx"))
  expect_error(read_data("invalid.inv"))
  
})
