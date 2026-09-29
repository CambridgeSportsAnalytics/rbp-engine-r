test_that("math_lib_version reads the record embedded in a library file", {
  blob <- tempfile(fileext = ".dylib")
  on.exit(unlink(blob), add = TRUE)
  writeBin(
    c(
      as.raw(c(0L, 1L)),
      charToRaw("RBP-ENGINE-VERSION=4.5.6"),
      as.raw(0L),
      charToRaw("tail")
    ),
    blob
  )

  out <- capture.output(ver <- math_lib_version(blob))
  expect_identical(ver, "4.5.6")
  expect_match(out, "^rbp-math-lib 4\\.5\\.6 \\(", perl = TRUE)
  expect_match(out, "\\.dylib\\)$")
})

test_that("a file without the version record is an error", {
  blob <- tempfile(fileext = ".so")
  on.exit(unlink(blob), add = TRUE)
  writeBin(charToRaw("not a versioned engine"), blob)
  expect_error(math_lib_version(blob), regexp = "RBP-ENGINE-VERSION")
})

test_that("RBP_ENGINE_LIB is the only file inspected when set", {
  missing <- tempfile(fileext = ".dll")
  old <- Sys.getenv("RBP_ENGINE_LIB", unset = NA_character_)
  on.exit(
    {
      if (is.na(old)) {
        Sys.unsetenv("RBP_ENGINE_LIB")
      } else {
        Sys.setenv(RBP_ENGINE_LIB = old)
      }
    },
    add = TRUE
  )
  Sys.setenv(RBP_ENGINE_LIB = missing)
  expect_error(math_lib_version(), regexp = "RBP_ENGINE_LIB")
})

test_that("newline terminates the embedded version", {
  blob <- tempfile(fileext = ".so")
  on.exit(unlink(blob), add = TRUE)
  writeBin(charToRaw("RBP-ENGINE-VERSION=9.8.7\nrest"), blob)
  out <- capture.output(ver <- math_lib_version(blob))
  expect_identical(ver, "9.8.7")
  expect_match(out, "rbp-math-lib 9\\.8\\.7")
})

test_that("an installed library embeds a semver", {
  found <- tryCatch(
    {
      printed <- capture.output(ver <- math_lib_version())
      list(ver = ver, printed = printed)
    },
    error = function(e) NULL
  )
  if (is.null(found)) {
    skip("no rbp-math-lib shared library on this machine")
  }
  expect_match(found$ver, "^[0-9]+\\.[0-9]+\\.[0-9]+")
  expect_match(found$printed, "rbp-math-lib")
})
