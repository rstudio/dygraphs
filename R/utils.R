mergeLists <- function(base_list, overlay_list, recursive = TRUE) {
  if (length(base_list) == 0) {
    overlay_list
  } else if (length(overlay_list) == 0) {
    base_list
  } else {
    merged_list <- base_list
    for (name in names(overlay_list)) {
      base <- base_list[[name]]
      overlay <- overlay_list[[name]]
      if (is.list(base) && is.list(overlay) && recursive) {
        merged_list[[name]] <- mergeLists(base, overlay)
      } else {
        merged_list[[name]] <- NULL
        merged_list <- append(
          merged_list,
          overlay_list[which(names(overlay_list) %in% name)]
        )
      }
    }
    merged_list
  }
}

asISO8601Time <- function(x) {
  if (!inherits(x, "POSIXt")) {
    x <- as.POSIXct(x, tz = "UTC")
  }
  # Every component comes from a single POSIXlt, so no component depends on the
  # C library. The year is padded with sprintf() instead of the %04Y
  # conversion: %04Y is a glibc extension, and C libraries that follow POSIX
  # strictly (e.g. OpenBSD) emit it literally, which produced dates like
  # "04Y-01-01T00:00:00.000Z".
  lt <- as.POSIXlt(x, tz = "UTC")
  res <- sprintf(
    "%04d-%02d-%02dT%02d:%02d:%06.3fZ",
    lt$year + 1900L, lt$mon + 1L, lt$mday, lt$hour, lt$min, lt$sec
  )
  # sprintf() renders missing components as "NA"; keep them missing.
  res[is.na(x)] <- NA_character_
  res
}

resolveStrokePattern <- function(strokePattern) {
  if (is.character(strokePattern)) {
    if (strokePattern == "dotted") {
      strokePattern <- c(2, 2)
    } else if (strokePattern == "dashed") {
      strokePattern <- c(7, 3)
    } else if (strokePattern == "dotdash") {
      strokePattern <- c(7, 2, 2, 2)
    } else if (strokePattern == "solid") {
      strokePattern <- c(1, 0)
    } else {
      stop(
        "Invalid stroke pattern: valid values are dotted, ",
        "dashed, and dotdash"
      )
    }
  }
  strokePattern
}

#' @importFrom stats end start
defaultPeriodicity <- function(data) {
  periodicity <- structure(
    list(
      difftime = structure(0, units = "secs", class = "difftime"),
      frequency = 0,
      start = start(data),
      end = end(data),
      units = "secs",
      scale = "seconds",
      label = "second"
    ),
    class = "periodicity"
  )
}
