context("Test the fars package using testthat")

library(dplyr)
library(maps)

test_that("fars_summarize_years() works correctly for a year range", {
  expect_is(fars_summarize_years(2013:2015), "tbl_df")
})
