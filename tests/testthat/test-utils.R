context("utils")

test_that("ISO8601 time formatting pads the year to four digits", {
  # Regression test: the year used to be formatted with the "%04Y" conversion,
  # which is a glibc extension. C libraries that follow POSIX (e.g. OpenBSD)
  # render it literally, producing dates like "04Y-01-01T00:00:00.000Z".
  expect_identical(
    asISO8601Time(as.POSIXct("1912-01-01", tz = "UTC")),
    "1912-01-01T00:00:00.000Z"
  )

  # Years before 1000 must be zero-padded as well. The value is built from a
  # POSIXlt to avoid relying on the platform date parser for short years.
  lt <- as.POSIXlt("2000-01-01", tz = "UTC")
  lt$year <- 5L - 1900L
  expect_identical(
    asISO8601Time(as.POSIXct(lt)),
    "0005-01-01T00:00:00.000Z"
  )
})
