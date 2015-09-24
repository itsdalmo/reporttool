context("rmd")

test_that("rmd to r works", {
  
  expect_error(x <- rmd_to_r("test"))
  
  x <- rmd_to_r(c("test", "test2"))
  expect_identical(x, c("##+ test", "##+ test2"))
  
  x <- c("```{r eval = TRUE}", "1+1", "```")
  y <- rmd_to_r(x)
  z <- rmd_to_r(c(x, ""))
  
  expect_identical(y, c("if (TRUE) {", "  1+1", "}", ""))
  expect_identical(y, z)
  
  x <- c("```{r}", "test", "```")
  y <- rmd_to_r(x)
  
})

test_that("eval inline works", {
  
  
  x <- "some text with r code: "
  y <- stri_c(x, "`r 1+1`")
  z <- eval_inline(y)
  
  expect_identical(z, stri_c(x, "2"))
  
  # More than one expression
  xx <- ". Also some more: "
  yy <- stri_c(y, xx, "`r 2+2`.")
  zz <- eval_inline(yy)
  
  expect_identical(zz, stri_c(x, "2", xx, "4."))
  
  # Multiple lines
  mm <- eval_inline(rep(y, 2))
  expect_identical(mm, rep(z, 2))
  
  # Multiple lines and expressions
  mm <- eval_inline(rep(yy, 2))
  expect_identical(mm, rep(zz, 2))
  
})