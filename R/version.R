#' Package / ABI version and path diagnostics.
#'
#' @return [package_version_rbpengine()] returns the installed package version
#'   as a string. [abi_version()] returns the loaded engine C ABI version as an
#'   integer (errors if the engine is not loaded). [engine_path_loaded()]
#'   returns the resolved engine library path, or `NA_character_` when none is
#'   loaded. [library_path()] returns a named list of engine/license diagnostics
#'   including candidate search paths.
#'
#' @seealso [engine_available()], [ensure_engine()], [license_source()],
#'   [math_lib_version()]
#' @name version-helpers
#'
#' @examples
#' package_version_rbpengine()
#' engine_path_loaded()
NULL

#' @rdname version-helpers
#' @export
package_version_rbpengine <- function() {
  as.character(utils::packageVersion("rbpengine"))
}

#' @rdname version-helpers
#' @export
abi_version <- function() {
  ensure_engine()
  as.integer(.Call(C_rbpengine_abi_version))
}

#' @rdname version-helpers
#' @export
engine_path_loaded <- function() {
  p <- .Call(C_rbpengine_engine_path)
  if (length(p) == 1L && is.na(p)) {
    return(NA_character_)
  }
  as.character(p)
}

#' @rdname version-helpers
#' @export
library_path <- function() {
  list(
    engine_loaded = engine_available(),
    engine_path = engine_path_loaded(),
    RBP_ENGINE_LIB = Sys.getenv("RBP_ENGINE_LIB", unset = NA_character_),
    RBP_ENGINE_HOME = Sys.getenv("RBP_ENGINE_HOME", unset = NA_character_),
    RBP_LICENSE = if (nzchar(Sys.getenv("RBP_LICENSE"))) "(set)" else NA_character_,
    RBP_LICENSE_FILE = Sys.getenv("RBP_LICENSE_FILE", unset = NA_character_),
    license_source = license_source(),
    candidates = engine_candidate_paths()
  )
}

#' rbp-math-lib semver embedded in the shared library on this machine.
#'
#' Reads the `RBP-ENGINE-VERSION=` record compiled into the engine dll, dylib,
#' or so. This does not read package, config, or version files, and it does not
#' load the library, so a missing BLAS companion or a wrong-architecture binary
#' can still report the version baked into that file.
#'
#' When `path` is omitted, `RBP_ENGINE_LIB` is the only file inspected if it is
#' set. Otherwise each existing file from [engine_candidate_paths()] is read
#' until one contains the record. A binary without that record is skipped, so
#' an older install does not hide a newer build. The filename depends on the
#' OS: `librbp_math_lib.dylib` (macOS), `librbp_math_lib.so` (Linux), or
#' `rbp_math_lib.dll` (Windows).
#'
#' Prints one line to the terminal, `rbp-math-lib <version> (<path>)`.
#'
#' @param path Optional full path to a shared library. When `NULL`, the library
#'   on this system is located as described above.
#' @return The semver string (for example `"1.3.5"`), invisibly.
#' @seealso [engine_candidate_paths()], [abi_version()], [package_version_rbpengine()]
#' @export
math_lib_version <- function(path = NULL) {
  ver <- NULL
  if (is.null(path)) {
    located <- .resolve_math_lib_path()
    path <- as.character(located)
    ver <- attr(located, "version", exact = TRUE)
  } else {
    path <- path.expand(as.character(path)[[1L]])
    if (!file.exists(path) || dir.exists(path)) {
      stop("Shared library not found: ", path, call. = FALSE)
    }
  }
  if (is.null(ver)) {
    ver <- .engine_version_from_file(path)
  }
  if (is.na(ver)) {
    stop(
      "No RBP-ENGINE-VERSION record in ", path, ". ",
      "The file is not an rbp-math-lib build that embeds its crate semver.",
      call. = FALSE
    )
  }
  shown <- tryCatch(
    normalizePath(path, winslash = "/", mustWork = TRUE),
    error = function(e) path
  )
  cat("rbp-math-lib ", ver, " (", shown, ")\n", sep = "")
  invisible(ver)
}

.resolve_math_lib_path <- function() {
  env <- Sys.getenv("RBP_ENGINE_LIB", unset = "")
  if (nzchar(env)) {
    path <- path.expand(env)
    if (!file.exists(path) || dir.exists(path)) {
      stop("RBP_ENGINE_LIB=", path, " is not a file", call. = FALSE)
    }
    return(path)
  }

  tried <- character()
  unmarked <- character()
  for (p in engine_candidate_paths()) {
    tried <- c(tried, p)
    if (!file.exists(p) || dir.exists(p)) {
      next
    }
    ver <- .engine_version_from_file(p)
    if (!is.na(ver)) {
      return(structure(p, version = ver))
    }
    unmarked <- c(unmarked, p)
  }
  if (length(unmarked) > 0L) {
    stop(
      "Found ", .engine_lib_filename(), " but no RBP-ENGINE-VERSION record in:\n  - ",
      paste(unmarked, collapse = "\n  - "),
      call. = FALSE
    )
  }
  stop(
    "Could not find the RBP Math Library (", .engine_lib_filename(), ") on this system.\n",
    "Install it with rbpengine::install_engine(), set RBP_ENGINE_LIB, ",
    "or build rbp-math-lib (`cargo build --release`).\n",
    "Tried:\n  - ",
    paste(tried, collapse = "\n  - "),
    call. = FALSE
  )
}

.engine_version_from_file <- function(path) {
  info <- file.info(path, extra_cols = FALSE)
  sz <- info$size[[1L]]
  if (is.na(sz)) {
    stop("Could not determine size of ", path, call. = FALSE)
  }
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  raw <- if (sz == 0) raw() else readBin(con, what = "raw", n = sz)
  .engine_version_from_raw(raw)
}

.engine_version_from_raw <- function(raw) {
  marker <- charToRaw("RBP-ENGINE-VERSION=")
  pos <- grepRaw(marker, raw, fixed = TRUE, all = FALSE)
  if (length(pos) != 1L) {
    return(NA_character_)
  }
  start <- pos + length(marker)
  if (start > length(raw)) {
    return(NA_character_)
  }
  rest <- raw[start:length(raw)]
  stop_at <- which(rest == as.raw(0L) | rest == charToRaw("\n"))
  if (length(stop_at) == 0L || stop_at[[1L]] < 2L) {
    return(NA_character_)
  }
  rawToChar(rest[seq_len(stop_at[[1L]] - 1L)])
}
