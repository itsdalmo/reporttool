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

