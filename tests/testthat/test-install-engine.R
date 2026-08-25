test_that("runtime id and default home are well-formed", {
  id <- engine_runtime_id()
  expect_type(id, "character")
  expect_true(id %in% c(
    "macos-arm64", "macos-x86_64",
    "linux-x64", "linux-arm64",
    "windows-x64", "windows-arm64"
  ))
  home <- default_engine_home()
  expect_match(home, "rbp-engine")
  expect_equal(engine_releases_url(), "https://docs.csanalytics.io/releases")
})

test_that("install_engine unpacks a local tarball and uninstalls", {
  dest <- tempfile("rbp-engine-home-")
  staged <- tempfile("rbp-staged-")
  tar <- tempfile(fileext = ".tar.gz")
  on.exit(
    {
      unlink(dest, recursive = TRUE, force = TRUE)
      unlink(staged, recursive = TRUE, force = TRUE)
      unlink(tar)
    },
    add = TRUE
  )

  libname <- if (identical(Sys.info()[["sysname"]], "Darwin")) {
    "librbp_math_lib.dylib"
  } else if (identical(Sys.info()[["sysname"]], "Windows")) {
    "rbp_math_lib.dll"
  } else {
    "librbp_math_lib.so"
  }
  dir.create(file.path(staged, "lib"), recursive = TRUE)
  dir.create(file.path(staged, "bin"), recursive = TRUE)
  dir.create(file.path(staged, "include"), recursive = TRUE)
  writeBin(raw(8), file.path(staged, "lib", libname))
  writeLines("#!/bin/sh\necho stub\n", file.path(staged, "bin", "rbp-license-info"))
  writeLines("header", file.path(staged, "include", "rbp_math.h"))

  old <- setwd(staged)
  on.exit(setwd(old), add = TRUE)
  utils::tar(
    tarfile = tar,
    files = c("lib", "bin", "include"),
    compression = "gzip",
    tar = "internal"
  )
  setwd(old)

  install_engine(path = tar, dest = dest, load = FALSE)
  expect_true(file.exists(file.path(dest, "lib", libname)))
  expect_true(file.exists(file.path(dest, "bin", "rbp-license-info")))
  expect_true(file.exists(file.path(dest, "include", "rbp_math.h")))
  expect_equal(Sys.getenv("RBP_ENGINE_HOME"), dest)

  expect_true(uninstall_engine(dest))
  expect_false(dir.exists(dest))
})

test_that("uninstall_engine refuses a directory that is not a runtime", {
  dest <- tempfile("not-runtime-")
  dir.create(dest)
  on.exit(unlink(dest, recursive = TRUE, force = TRUE), add = TRUE)
  expect_error(uninstall_engine(dest), "Refusing")
})

test_that("engine_candidate_paths includes the default install prefix", {
  expected <- normalizePath(
    file.path(default_engine_home(), "lib"),
    winslash = "/",
    mustWork = FALSE
  )
  hits <- engine_candidate_paths()
  expect_true(any(startsWith(hits, expected)))
})
