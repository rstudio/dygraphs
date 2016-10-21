
context("dySliderInput")

test_that("slider input creation", {
  d <- dygraph(nhtemp, main = "New Haven Temperatures") %>% 
         dySliderInput()
  expect_identical(d$dependencies[[1]]$name, 'Dygraph.Plugins.SliderInput')
})

