context("survey_table")

srv <- survey(read_data("test_raw.xlsx"))
srv <- set_association(add_mm(srv), common = TRUE)
srv <- set_translation(set_config(srv), study_average = "avg")
srv <- prepare_data(srv, "mean")

# Add a factor variable
fct <- factor(c(rep("a", 4), rep("b", 15)), levels = c("a", "b", "c"))
srv <- mutate(srv, test_factor = fct)


test_that("Single group and single numeric", {
  
  x <- group_by(srv, test_factor)
  
  # Wide
  y <- survey_table(x, epsi, wide = TRUE)
  
  expect_identical(as.character(y$test_factor), c("a", "b", "c", "avg"))
  expect_equal(y$Kundetilfredshet[3], NA_real_)
  expect_equal(y$n[3], 0)
  expect_equal(sum(y$n[1:3]), y$n[4])
  
  # Long
  z <- survey_table(x, epsi, wide = FALSE)
  
  expect_identical(as.character(z$question)[1], names(y)[3])
  expect_identical(z$n, y$n)
  expect_identical(z$answer, y$Kundetilfredshet)

})

test_that("Multiple groups and single numeric", {
  
  x <- group_by(srv, q1, test_factor)
  
  # Wide
  y <- survey_table(x, epsi, wide = TRUE)
  
  expect_true(all(c("a", "b", "c") %in% names(y)))
  expect_identical(as.character(y$q1), c("Example", "avg"))
  
  # Long
  z <- survey_table(x, epsi, wide = FALSE)
  
  expect_equal(sum(z$n[1:3]), y$n[1])
  expect_equal(z$answer[1:3], c(y$a[1], y$b[1], y$c[1]))
  
})

test_that("single group and multiple numeric (unequal count)", {
  
  x <- group_by(srv, q1)
  
  # Wide
  y <- survey_table(x, epsi, q7paem, wide = TRUE)
  
  expect_true("Kundetilfredshet" %in% names(y))
  expect_true(all(c("Example", "avg") %in% y$q1))
  
  # Long
  z <- survey_table(x, epsi, q7paem, wide = FALSE)
  
  expect_true(all(unique(y$n) %in% unique(z$n)))
  
})

test_that("single group and multiple numeric (equal count)", {
  
  x <- group_by(srv, q1)
  
  # Wide
  y <- survey_table(x, epsi, image, wide = TRUE)
  
  expect_true("Kundetilfredshet" %in% names(y))
  expect_true(all(c("Example", "avg") %in% y$q1))
  
  # Long
  z <- survey_table(x, epsi, image, wide = FALSE)
  
  expect_true(all(unique(z$n) %in% unique(z$n)))
  
})

test_that("single group and single factor", {
  
  x <- group_by(srv, q1)
  
  # Wide
  y <- survey_table(x, test_factor, wide = TRUE)
  
  expect_equal(levels(fct), names(y)[-c(1:4)])
  expect_true(all(c("Example", "avg") %in% y$q1))
  
  # Long
  z <- survey_table(x, test_factor, wide = FALSE)
  
  expect_equal(y$a[1], 4/18)
  expect_equal(z$proportion[1], 4/18)
  expect_equal(z$proportion[3], 0)
  
})

test_that("multiple groups and single factor", {
  
  x <- mutate(srv, q3 = as.factor(q3))
  x <- group_by(x, q1, q3)
  
  # Wide
  y <- survey_table(x, test_factor, wide = TRUE)
  
  expect_equal(levels(fct), names(y)[-c(1:5)])
  expect_true(all(c("Example", "avg") %in% y$q1))
  
  # Long
  z <- survey_table(x, test_factor, wide = FALSE)
  
  expect_equal(y$a[5], 1/2)
  
})