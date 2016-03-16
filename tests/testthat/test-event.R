
context("dyEvent")

test_that("event creation", {
  d <- dygraph(presidents, main = "Presidential Approval") %>%
         dyEvent(x = "1950-6-30", "Korea", labelLoc = "bottom") %>%
         dyEvent(x = "1965-2-09", "Vietnam", labelLoc = "bottom")
  dd <- dygraph(presidents, main = "Presidential Approval") %>%
         dyEvent(x = c("1950-6-30", "1965-2-09"), c("Korea", "Vietnam"), labelLoc = "bottom")
  expect_equal(length(d$x$events), 2)
  expect_identical(d, dd)
  expect_identical(d$x$events[[1]]$label, "Korea")
  expect_identical(d$x$events[[2]]$label, "Vietnam")
})
