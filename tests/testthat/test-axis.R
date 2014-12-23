
context("dyAxis")

test_that("axis creation", {
  d <- dygraph(ldeaths) %>% dyAxis("x", label = "x-axis", drawGrid = FALSE)
  expect_identical(d$x$attrs$xlabel, "x-axis")
  expect_identical(d$x$attrs$axes$x$drawGrid, FALSE)
})
