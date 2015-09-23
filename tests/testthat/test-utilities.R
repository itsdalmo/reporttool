# Test utilities ---------------------------------------------------------------
path <- system.file("tests/testthat/xlsx.xlsx", package="reporttool")

context("Utility functions")

test_that("recode works with 'by'", {
  
  x <- c("a", "b", "c")
  y <- c("d", "e", "f")
  
  z <- recode(x, "a" = "f", by = y)
  expect_identical(z, c("a", "b", "a"))
  
})

test_that("recode works for numeric", {
  
  x <- c(1, 2, 3)
  y <- recode(x, "test" = 1:3)
  expect_identical(y, rep("test", 3))
  
})

test_that("recode works for character", {
  
  x <- c("a", "b", "c")
  y <- recode(x, "a" = "c")
  expect_identical(y, c("a", "b", "a"))
  expect_identical(z, c("a", "b", "a"))
  
})

test_that("recode works for factors", {
  
  x <- factor(c("a", "b", "c"))
  y <- recode(x, "a" = "c", drop = FALSE)
  expect_identical(as.character(y), c("a", "b", "a"))
  expect_identical(levels(y), c("a", "b", "c"))
  
  # Drop works
  y <- recode(x, "a" = "c", drop = TRUE)
  expect_identical(as.character(y), c("a", "b", "a"))
  expect_identical(levels(y), c("a", "b"))
  
  # But only if it is explicitly recoded
  levels(x) <- c(levels(x), "d")
  y <- recode(x, "a" = "c", drop = TRUE)
  expect_identical(as.character(y), c("a", "b", "a"))
  expect_identical(levels(y), c("a", "b", "d"))
  
})

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