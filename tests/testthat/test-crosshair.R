
context("dyCrosshair")

test_that("crosshair creation", {
  d <- dygraph(nhtemp, main = "New Haven Temperatures") %>% 
    dyCrosshair()
  expect_identical(d$dependencies[[1]]$name, 'Dygraph.Plugins.Crosshair')
})

