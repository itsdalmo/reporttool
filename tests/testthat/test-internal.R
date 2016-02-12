context("internal")
raw <- read_data("test_raw.xlsx")
srv <- survey(raw)

test_that("Creating a topline", {
  
  x <- add_mm(srv)
  x <- set_config(x, study = "example", year = "2015")
  x <- set_translation(x)
  x <- set_association(x, common = TRUE)
  x <- prepare_data(x)
  x <- add_entities(x)
  y <- topline(x)
  
  expect_identical(names(y$entities)[4:10], x$tr$replacement[1:7])
  
})

test_that("Reading/writing sharepoint structure", {
  
  x <- add_mm(srv)
  x <- set_config(x, study = "example", year = "2015")
  x <- set_translation(x)
  x <- set_association(x, common = TRUE)
  x <- prepare_data(x)
  x <- add_entities(x)
  
  # Write
  fileName <- tempdir()
  expect_error(write_sharepoint(x, fileName))
  x <- mutate(x, q1 = as.factor(q1))
  write_sharepoint(x, fileName)
  
  # Read data again and compare
  w_x <- read_sharepoint(fileName)
  
  # TODO: Compare
  unlink(fileName, recursive = TRUE, force = TRUE)
  
})

test_that("Writing questionnaires", {
  
  q <- data_frame("year" = 2015, 
                  "segment" = "B2C", 
                  "study" = "Test", 
                  "latent" = NA, 
                  "manifest" = "Q1", 
                  "primer" = "Test primer",
                  "question" = "Test question",
                  "type" = "scale",
                  "values" = "Lowest\r\nHighest",
                  "conditional_on" = NA)
  
  # Write
  fileName <- file.path(tempdir(), "quest.xlsx")
  write_questionnaire(q, fileName, study = "Test", "B2C")
  
  # Read data again and compare
  w_q <- read_data(fileName)
  expect_identical(names(w_q)[1], "Test primer")
  
  # Remove title row and compare again
  names(w_q) <- w_q[1, ]; w_q <- w_q[2,]
  
  expect_identical(w_q$Manifest, "Q1")
  expect_identical(w_q$Question, "Test question")
  
  if (Sys.info()["sysname"] %in% "Windows") {
    expect_identical(w_q$Values, "1 Lowest\r\n...\r\n10 Highest\r\n")
  } else {
    expect_identical(w_q$Values, "1 Lowest\n...\n10 Highest\n")
  }
  
  unlink(fileName, recursive = TRUE, force = TRUE)
  
  
})