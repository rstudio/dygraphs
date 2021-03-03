
context("dyRangeSelector")

test_that("range selector creation", {
  d <- dygraph(nhtemp, main = "New Haven Temperatures") %>%
    dyRangeSelector()
  expect_identical(d$x$attrs$showRangeSelector, TRUE)
})
