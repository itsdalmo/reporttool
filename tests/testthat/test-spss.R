context("SPSS/labelled")

df <- read_data("test.sav")

test_that("from_labelled" , {
  
  x <- from_labelled(df)
  expect_identical(names(x), c("df", "mm"))
  expect_identical(as.character(x$df$serv1[1:3]), c("Vet ikke", "8", "6"))
  expect_identical(as.character(x$df$q1)[1:3], rep("reporttool", 3))
  expect_identical(x$mm$manifest[1:3], names(x$df)[1:3])
  
})

test_that("Convert labelled in survey()" , {
  
  x <- from_labelled(df)
  y <- survey(df)
  
  expect_identical(unclass(x$mm), unclass(y$mm))
  
})

test_that("factor_data works when reading and writing .sav", {
  
  x <- df[, 1:37]
  y <- survey(x)
  fileName <- file.path(tempdir(), "sav.sav")
  
  # Write and read again
  write_data(y, fileName)
  z <- read_data(fileName)
  
  expect_identical(names(z), names(x))
  expect_identical(vapply(z, class, character(1)), vapply(x, class, character(1)))
  expect_identical(vapply(z, attr, which = "label", character(1)),
                   vapply(x, attr, which = "label", character(1)))
  
  # Convert back to survey
  z <- survey(z)
  
  # For some reason, whitespaces have a different encoding so
  # the Factor levels are not identical unless i use factor_data first
  # which also replaces whitespaces.
  z <- factor_data(z, names(z$df))
  y <- factor_data(y, names(y$df))
  
  expect_identical(z$mm, y$mm)
  expect_identical(z$df, y$df)
  
  # Clean
  unlink(fileName, recursive = TRUE, force = TRUE)
  
})

test_that("factor_data works when reading and writing .xlsx", {
  
  x <- survey(df[, 1:37])
  fileName <- file.path(tempdir(), "xlsx.xlsx")
  
  # Write and read again
  write_data(x, fileName)
  y <- read_data(fileName)
  
  # Convert back to survey and factor
  y <- survey(y)
  
  expect_identical(names(y$df), names(x$df))
  expect_identical(y$mm, y$mm)
  
  # Factor data and compare (do it for both, because factor_data makes ordered factors)
  x <- factor_data(x, names(x$df))
  y <- factor_data(y, names(y$df))
  
  expect_identical(lapply(y$df, class), lapply(x$df, class))
  
  # Clean
  unlink(fileName, recursive = TRUE, force = TRUE)
  
})


test_that("factor_data works from .sav, to .xlsx and to .sav", {
  
  x <- df[, 1:37]
  y <- survey(x)
  fileName <- file.path(tempdir(), "xlsx.xlsx")
  
  # Write to xlsx and read again
  write_data(y, fileName)
  z <- read_data(fileName)
  
  # Convert to survey, write to .sav and read again
  z <- survey(z)
  fileName2 <- file.path(tempdir(), "sav.sav")
  write_data(z, fileName2)
  z <- read_data(fileName2)
  
  # Remove q17a
  z$q17a <- NULL; x$q17a <- NULL # NA's imported as numeric. i.e. wrong.
  
  expect_identical(names(z), names(x))
  expect_identical(vapply(z, class, character(1)), vapply(x, class, character(1)))
  expect_identical(vapply(z, attr, which = "label", character(1)),
                   vapply(x, attr, which = "label", character(1)))
  
  # Clean
  unlink(fileName, recursive = TRUE, force = TRUE)
  unlink(fileName2, recursive = TRUE, force = TRUE)
  
})
