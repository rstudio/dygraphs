
context("dyRoller")

test_that("roller creation", {
  d <- dygraph(discoveries, main = "Important Discoveries") %>%
    dyRoller(rollPeriod = 5)
  expect_identical(d$x$attrs$showRoller, TRUE)
  expect_identical(d$x$attrs$rollPeriod, 5)
})
