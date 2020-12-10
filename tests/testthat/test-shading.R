
context("dyShading")

d <- dygraph(nhtemp, main = "New Haven Temperatures")

test_that("shading creation", {
  d_shade <- d %>% 
    dyShading(from = "1920-1-1", to = "1930-1-1") %>%
    dyShading(from = "1940-1-1", to = "1950-1-1")   
  expect_equal(length(d_shade$x$shadings), 2)
  expect_equal(length(d_shade$x$shadings[[1]]), 4)
  expect_equal(length(d_shade$x$shadings[[2]]), 4)
})

test_that("vectorized shadings works", {
  d_shade_vec <- d %>% 
    dyShading(from = c("1920-1-1", "1940-1-1"), to = c("1930-1-1", "1950-1-1"))
  d_shade <- d %>% 
    dyShading(from = "1920-1-1", to = "1930-1-1") %>%
    dyShading(from = "1940-1-1", to = "1950-1-1")  
  expect_identical(d_shade_vec, d_shade)
  d_shade_color <- d %>% 
    dyShading(from = c("1920-1-1", "1940-1-1"), 
              to = c("1930-1-1", "1950-1-1"), 
              color = c("#8F2F57", "##2F8F67"))
  expect_equal(length(d_shade_color$x$shadings), 2)
  expect_equal(length(d_shade_color$x$shadings[[1]]), 4)
  expect_equal(length(d_shade_color$x$shadings[[2]]), 4)
  
})

test_that("different length of parameters gives a warning", {
  expect_warning(d %>% 
                   dyShading(from = c("1920-1-1", "1940-1-1"), to = "1930-1-1"))
  expect_warning(d %>% 
                   dyShading(from = "1920-1-1", to = c("1930-1-1", "1950-1-1")))
})
