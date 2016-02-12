context("Recode")
path <- system.file("tests/testthat/xlsx.xlsx", package="reporttool")

test_that("recode works with dplyr/magrittr pipe", {
  
  x <- read_data(path)
  y <- x %>% 
       mutate(score = rescale_score(clean_score(score)), score2 = score) %>%
       mutate_each(funs(recode(., "Great" = . <= 100, 
                                  "Average" = . <= 75, 
                                  "Bad" = . < 60, as_factor = TRUE)), score, score2)
  
  expect_identical(y$score, y$score2)
  expect_identical(y$score, factor(c("Bad", "Great", NA), levels = c("Great", "Average", "Bad")))
  
})

test_that("recode works successively", {
  
  x <- 1:3
  y <- recode(x, "<3.5" = . < 3.5, "<2.5" = . < 2.5, "<1.5" = . < 1.5, as_factor = FALSE)
  
  expect_identical(y, c("<1.5", "<2.5", "<3.5"))
  
})

test_that("recode works with 'by'", {
  
  x <- c("a", "b", "c")
  y <- c("d", "e", "f")
  
  z <- recode(x, "a" = . %in% "f", by = y, as_factor = FALSE)
  expect_identical(z, c("a", "b", "a"))
    
})

test_that("recode works with 'by' when converting to factor", {
  
  x <- c("a", "b", "c")
  y <- c("d", "e", "f")
  
  # Intended functionality. Make x a factor before recoding to keep all levels.
  z <- recode(x, "a" = . %in% "f", by = y, as_factor = TRUE)
  expect_identical(z, factor(c("a", NA, "a"), levels = "a"))
  
    
})

test_that("recode works for integer", {
  
  x <- c(1L, 2L, 3L)
  y <- recode(x, "test" = . %in% 1:2, as_factor = FALSE)
  expect_identical(y, c("test", "test", "3"))
  
})

test_that("recode works for numerics", {
  
  x <- c(0, 50, 100)
  x <- x + .5
  y <- recode(x, "test" = . <= 50.5, as_factor = FALSE)
  expect_identical(y, c("test", "test", "100.5"))
  
})

test_that("recode works for character", {
  
  x <- c("a", "b", "c")
  y <- recode(x, "a" = . %in% "c", as_factor = FALSE)
  expect_identical(y, c("a", "b", "a"))
  
})

test_that("recode with as_factor works for integer/numeric/character", {
  
  # Integer
  x <- c(1L, 2L, 3L)
  y <- recode(x, "test" = 1:3, as_factor = TRUE)
  expect_identical(y, factor(rep("test", 3), levels = "test"))

  # Numeric 
  x <- c(0, 50, 100)
  x <- x + .5
  y <- recode(x, "test" = . <= 50.5, as_factor = TRUE)
  expect_identical(y, factor(c("test", "test", NA), levels = "test"))
 
  # Character
  x <- c("a", "b", "c")
  y <- recode(x, "a" = . %in% "c", as_factor = TRUE)
  expect_identical(y, factor(c("a", NA, "a"), levels = "a"))
   
})


test_that("recode works for factors", {
  
  x <- factor(c("a", "b", "c"))
  y <- recode(x, "a" = . %in% "c", drop = FALSE)
  expect_identical(as.character(y), c("a", "b", "a"))
  expect_identical(levels(y), c("a", "b", "c"))

  # Add works
  y <- recode(x, "d" = . %in% "a", drop = TRUE, add = TRUE)
  expect_identical(y, factor(c("d", "b", "c"), levels = c("b", "c", "d"))) 
  y <- recode(x, "d" = . %in% "a", drop = FALSE, add = TRUE)
  expect_identical(y, factor(c("d", "b", "c"), levels = c("a", "b", "c", "d"))) 
  
  # Drop works
  y <- recode(x, "a" = . %in% "c", drop = TRUE)
  expect_identical(as.character(y), c("a", "b", "a"))
  expect_identical(levels(y), c("a", "b"))
  
  # But only if it is explicitly recoded
  levels(x) <- c(levels(x), "d")
  y <- recode(x, "a" = . %in% "c", drop = TRUE)
  expect_identical(as.character(y), c("a", "b", "a"))
  expect_identical(levels(y), c("a", "b", "d"))
  
})