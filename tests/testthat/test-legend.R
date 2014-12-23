
context("dyLegend")

test_that("legend creation", {
  d <- dygraph(ldeaths) %>% dyLegend(show = "always", hideOnMouseOut = TRUE)
  expect_identical(d$x$attrs$legend, "always")
  expect_identical(d$x$attrs$hideOverlayOnMouseOut, TRUE)
})

