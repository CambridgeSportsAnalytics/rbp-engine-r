#' Locate and load the private RBP Math Engine runtime.
#'
#' The CRAN/open **rbpengine** package is a thin client. Predictions require the
#' private **RBP Math Engine** shared library (`librbp_math_lib.dylib` /
#' `.so` / `rbp_math_lib.dll`) plus platform companions (OpenBLAS/LAPACK where
#' needed) and a valid license.
#'
#' Discovery order (first path that loads and exposes the expected ABI symbols):
#' 1. `RBP_ENGINE_LIB` (full path to the shared library)
#' 2. `RBP_ENGINE_HOME` / `lib/` (+ OS filename)
#' 3. [install_engine()] prefix ([default_engine_home()])
#' 4. Well-known install prefixes (`/Library/rbp-engine/lib`,
#'    `$HOME/.local/share/rbp-engine/lib`, ...)
#' 5. Developer cargo tree next to this package (`../target/release` or
#'    `../target/debug`)
#'
#' @return [engine_available()] returns a logical. [engine_candidate_paths()]
#'   returns character paths that may be probed. [find_engine_path()] and
#'   [engine_load()] return the loaded path (invisibly). [ensure_engine()]
#'   returns `TRUE` invisibly when the engine is available.
#'
#' @seealso [library_path()], [abi_version()]
#' @name engine
NULL

.engine_lib_filename <- function() {
  sysname <- Sys.info()[["sysname"]]
  if (identical(sysname, "Darwin")) {
    "librbp_math_lib.dylib"
  } else if (identical(sysname, "Windows")) {
    "rbp_math_lib.dll"
  } else {
    "librbp_math_lib.so"
  }
}

#' @rdname engine
#' @export
engine_candidate_paths <- function() {
  name <- .engine_lib_filename()
  paths <- character()

  env_lib <- Sys.getenv("RBP_ENGINE_LIB", unset = "")
  if (nzchar(env_lib)) {
    paths <- c(paths, path.expand(env_lib))
  }

  env_home <- Sys.getenv("RBP_ENGINE_HOME", unset = "")
  if (nzchar(env_home)) {
    paths <- c(paths, file.path(path.expand(env_home), "lib", name))
    paths <- c(paths, file.path(path.expand(env_home), name))
  }

  home <- path.expand("~")
  opt_home <- getOption("rbpengine.engine_home", default = "")
  if (is.character(opt_home) && nzchar(opt_home)) {
    paths <- c(
      paths,
      file.path(opt_home, "lib", name),
      file.path(opt_home, name)
    )
  }
  paths <- c(
    paths,
    file.path(default_engine_home(), "lib", name),
    file.path("/Library/rbp-engine/lib", name),
    file.path("/Library/rbp-engine/lib/python/rbp_engine/lib", name),
    file.path(home, ".local/share/rbp-engine/lib", name),
    file.path(home, ".rbp-engine/lib", name),
    file.path("/usr/local/lib/rbp-engine", name),
    file.path("/opt/rbp-engine/lib", name)
  )
  if (identical(Sys.info()[["sysname"]], "Windows")) {
    pf <- Sys.getenv("ProgramFiles", unset = "")
    if (nzchar(pf)) {
      paths <- c(paths, file.path(pf, "rbp-engine", "lib", name))
    }
  }

  # Monorepo develop: r/ is next to target/
  pkg_dir <- tryCatch(system.file(package = "rbpengine"), error = function(e) "")
  # When installed from source in-repo, library path differs; also probe relative to cwd
  roots <- unique(c(
    file.path(".."),
    file.path("../.."),
    if (nzchar(pkg_dir)) file.path(pkg_dir, "..", "..") else character()
  ))
  for (root in roots) {
    paths <- c(
      paths,
      file.path(root, "target", "release", name),
      file.path(root, "target", "debug", name)
    )
  }

  cargo_target <- Sys.getenv("CARGO_TARGET_DIR", unset = "")
  if (nzchar(cargo_target)) {
    paths <- c(
      paths,
      file.path(cargo_target, "release", name),
      file.path(cargo_target, "debug", name)
    )
  }

  unique(normalizePath(paths, winslash = "/", mustWork = FALSE))
}

#' @rdname engine
#' @export
engine_available <- function() {
  isTRUE(.Call(C_rbpengine_engine_loaded))
}

#' Load the engine shared library at an explicit path.
#'
#' @param path Full path to `librbp_math_lib.*` (or set `RBP_ENGINE_LIB` and call
#'   [engine_load()] with no args after relying on [find_engine_path()]).
#' @return Invisibly, the path loaded.
#' @export
engine_load <- function(path = NULL) {
  if (is.null(path) || !nzchar(path)) {
    path <- find_engine_path()
  }
  path <- path.expand(path)
  if (!file.exists(path)) {
    stop("Engine library not found: ", path, call. = FALSE)
  }
  loaded <- .Call(C_rbpengine_engine_load, path)
  invisible(loaded)
}

#' @rdname engine
#' @export
find_engine_path <- function() {
  tried <- character()
  for (p in engine_candidate_paths()) {
    tried <- c(tried, p)
    if (!file.exists(p)) {
      next
    }
    # Probe with native loader (symbols + ABI)
    ok <- tryCatch(
      {
        .Call(C_rbpengine_engine_load, p)
        TRUE
      },
      error = function(e) FALSE
    )
    if (ok) {
      return(invisible(p))
    }
  }
  stop(
    "Could not find a usable RBP Math Engine runtime (",
    .engine_lib_filename(),
    ").\n",
    "Install it with:\n",
    "  rbpengine::install_engine()\n",
    "or set RBP_ENGINE_LIB to the shared library.\n",
    "Tried:\n  - ",
    paste(tried, collapse = "\n  - "),
    call. = FALSE
  )
}

#' Ensure the engine is loaded; stop with install instructions if not.
#'
#' Called by prediction helpers. Safe to call repeatedly.
#'
#' @return Invisibly `TRUE` when available.
#' @export
ensure_engine <- function() {
  if (engine_available()) {
    return(invisible(TRUE))
  }
  engine_load()
  invisible(TRUE)
}

#' Attempt to load the engine without erroring (used from `.onLoad`).
#' @keywords internal
#' @noRd
.try_load_engine_quiet <- function() {
  suppressWarnings(tryCatch(
    {
      engine_load()
      TRUE
    },
    error = function(e) FALSE
  ))
}
