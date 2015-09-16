context("dplyr")

raw <- read_data("test_raw.xlsx")
srv <- survey(raw)
srv <- add_mm(srv)

test_that("selecting variables", {
  
  # Select with specified name
  x <- select(srv, q1)
  
  expect_true(ncol(x$df) == 1)
  expect_true(names(x$df) == "q1")
  expect_identical(names(x$df), x$mm$manifest)
  
  # Remove specific columns
  x <- select(srv, -q1)
  
  expect_true(!"q1" %in% names(x$df))
  expect_identical(names(x$df), x$mm$manifest)
  
  # Selecting with verb
  x <- select(srv, matches("q[456]"))
  
  expect_identical(names(x$df), c("q4a", "q5a"))
  expect_identical(names(x$df), x$mm$manifest)
  
  # Renaming when selecting
  x <- select(srv, mainentity = q1, one_of("q4a", "q5a"))
  
  expect_identical(names(x$df), c("mainentity", "q4a", "q5a"))
  expect_identical(names(x$df), x$mm$manifest)
  
  # When changing order
  x <- select(srv, q4a, mainentity = q1)
  
  expect_identical(names(x$df), c("q4a", "mainentity"))
  expect_identical(names(x$df), x$mm$manifest)
  
})

test_that("mutating variables", {
  
  # Mutate single variables
  x <- mutate(srv, mainentity = q1)
  
  expect_identical(x$df$q1, x$df$mainentity)
  expect_identical(names(x$df), x$mm$manifest)
  expect_identical(x$mm$type[nrow(x$mm)], class(x$df$mainentity))
  
  # Mutate several variables
  x <- mutate_each(srv, funs(clean_score(.)), one_of("q4a", "q5a"))
  
  expect_identical(names(x$df), x$mm$manifest)
  expect_identical(unname(vapply(x$df[c("q4a", "q5a")], class, character(1))), c("numeric", "numeric"))
  expect_identical(filter(x$mm, manifest %in% c("q4a", "q5a"))[["type"]], c("numeric", "numeric"))
  
})

test_that("renaming variables", {
  
  # Rename
  l <- length(names(srv$df))
  x <- rename(srv, mainentity = q1)
  
  expect_true(length(names(x$df)) == l)
  expect_identical(names(x$df), x$mm$manifest)
  expect_true(!"q1" %in% names(x$df))
  expect_true("mainentity" %in% names(x$df))

})

test_that("filtering survey data", {
  
  # Filter
  x <- filter(srv, q1 == "Example")
  expect_identical(x, srv)
  
  x <- mutate(srv, q4a = clean_score(q4a))
  x <- filter(x, q4a >= 9)
  
  expect_true(all(x$df$q4a >= 9))
  expect_identical(x$df$q4a, c(rep(9, 5), 10, 10))
  
  
})

test_that("summarising survey data", {
  
  x <- set_association(srv, common = TRUE)
  x <- add_entities(x)
  x <- set_config(x)
  x <- prepare_data(x)
  x <- select(x, q1, one_of(default$latents))
  x <- group_by(x, q1)
  x <- summarise_each(x, funs(mean(., na.rm = TRUE)), -q1)
  
  expect_identical(names(x$df), c("q1", default$latents))
  expect_identical(nrow(x$df), 1L)
  expect_identical(names(x$df), x$mm$manifest)
  
})