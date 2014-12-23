
context("dyShading")

test_that("shading creation", {
  d <- dygraph(nhtemp, main = "New Haven Temperatures") %>% 
         dyShading(from = "1920-1-1", to = "1930-1-1") %>%
         dyShading(from = "1940-1-1", to = "1950-1-1")   
  expect_equal(length(d$x$shadings), 2)
  expect_equal(length(d$x$shadings[[1]]), 3)
  expect_equal(length(d$x$shadings[[2]]), 3)
})
