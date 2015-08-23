# Test utilities ---------------------------------------------------------------
path <- system.file("tests/testthat/xlsx.xlsx", package="reporttool")

context("Utility functions")
test_that("path cleaning", {
  
  expect_false(stri_detect(clean_path(paste0(path, "/")), regex = "/$"))
  
  expect_error(validate_path(rep(path, 2)))
  expect_error(validate_path(numeric(1)))
  
})

test_that("clean scores", {
  
  expect_true(clean_score("1 aa") == "1")
  expect_identical(clean_score(c("1 aa", "bb 1", "10 cc")), c(1, NA, 10))
  
})

test_that("cleaning and rescaling scores", {
  
  expect_true(clean_score("1 aa") == "1")
  expect_identical(clean_score(c("1 aa", "bb 1", "10 cc")), c(1, NA, 10))
  expect_identical(rescale_score(c("1", 10)), c(0, 100))
  
})

test_that("extracting sheetnames from xlsx", {
  
  expect_identical(openxlsx::getSheetNames("xlsx.xlsx"), "xlsx")
  
})

test_that("ordered replace", {

  x <- c("a", "b", "c", "d")
  y <- c("data", "measurement model", "entities")
  
  expect_identical(ordered_replace(x, setNames(c("c", "a"), c("bar", "foo"))),
                   c("foo", "b", "bar", "d"))
  expect_identical(ordered_replace(y, with(default$structure, setNames(sheet, survey))),
                   c("df", "mm", "ents"))
  
})

test_that("intranet links", {
  
  x <- "https://test.se/Sharepoint/Folder"
  x_w <- "\\\\test.se@SSL/DavWWWRoot/Sharepoint/Folder"
  
  if (Sys.info()["sysname"] == "Windows") {
    expect_identical(intranet_link(x), x_w)
  } else {
    expect_identical(x, x)
  }
  
})

test_that("rt_defaults return expected results", {
  
  expect_identical(get_default("pal"), default$palette)
  expect_identical(get_default("laten"), default$latents)
  expect_identical(get_default("latent"), default$latents)
  
})