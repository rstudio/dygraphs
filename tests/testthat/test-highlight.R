
context("dyHighlight")

test_that("highlight creation", {
  lungDeaths <- cbind(ldeaths, mdeaths, fdeaths)
  d <- dygraph(lungDeaths, main = "Deaths from Lung Disease (UK)") %>%
    dyHighlight(
      highlightCircleSize = 5,
      highlightSeriesBackgroundAlpha = 0.2,
      hideOnMouseOut = FALSE
    )
  expect_identical(d$x$attrs$highlightCircleSize, 5)
  expect_identical(d$x$attrs$highlightSeriesBackgroundAlpha, 0.2)
})
