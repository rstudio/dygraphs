
context("dyCallbacks")

test_that("callback creation", {
  d <- dygraph(ldeaths) %>% dyCallbacks(clickCallback = "function(e, x, points) {}")
  expect_is(d$x$attrs$clickCallback, "JS_EVAL")
})
