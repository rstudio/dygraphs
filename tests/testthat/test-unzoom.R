
context("dyUnzoom")

test_that("unzoom input creation", {
  d <- dygraph(nhtemp, main = "New Haven Temperatures") %>% 
    dyUnzoom()
  expect_identical(d$dependencies[[1]]$name, 'Dygraph.Plugins.Unzoom')
})

