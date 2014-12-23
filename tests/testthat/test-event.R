
context("dyEvent")

test_that("event creation", {
  d <- dygraph(presidents, main = "Presidential Approval") %>%
         dyEvent(date = "1950-6-30", "Korea", labelLoc = "bottom") %>%
         dyEvent(date = "1965-2-09", "Vietnam", labelLoc = "bottom")   
  expect_equal(length(d$x$events), 2)
  expect_identical(d$x$events[[1]]$label, "Korea")
  expect_identical(d$x$events[[2]]$label, "Vietnam")
})
