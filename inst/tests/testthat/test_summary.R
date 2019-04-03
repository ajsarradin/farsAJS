context("test_summary")

test_that("test_summary", {
  setwd(system.file("extdata", package = "farsAJS"))
  testthat::expect_that(fars_summarize_years(2013:2015)$MONTH[1], equals(1))
})



