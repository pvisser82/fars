context("Test the basic functionality of the package")

library(dplyr)
library(maps)

setwd(system.file("./data", package = "fars"))

test_that("fars_summarize_years() works correctly", {
  expect_is(fars_summarize_years(2013:2015), "tbl_df")
  expect_is(fars_summarize_years(list(2013, 2014)), "tbl_df")
  expect_equal(names(fars_summarize_years(2013:2015)), c("MONTH", 2013:2015))
  expect_error(fars_summarize_years(2016))
})
