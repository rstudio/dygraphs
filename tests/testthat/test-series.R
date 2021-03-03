
context("dySeries")

test_that("series creation", {
  d <- dygraph(ldeaths) %>% dySeries(label = "series", fillGraph = FALSE)
  expect_identical(d$x$attrs$labels[[2]], "series")
  expect_identical(d$x$attrs$series[[1]]$fillGraph, FALSE)
})
