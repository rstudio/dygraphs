context("dygraph")

test_that("dygraph creation", {
  expect_is(dygraph(ldeaths), class = "dygraphs")
  expect_identical(dygraph(ldeaths, xlab = "x")$x$attrs$xlabel, "x")
  expect_identical(dygraph(ldeaths, ylab = "y")$x$attrs$ylabel, "y")
})
