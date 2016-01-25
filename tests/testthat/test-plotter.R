context("dyOptions")

test_that("custom plotter", {
  d <- dygraph(ldeaths) %>% dyOptions(plotter = "function(){}")
  expect_false(is.null(d$x$attrs$plotter))
})