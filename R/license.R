#' Adopt a default on-disk license when the process environment has none.
#'
#' Matches the Python `rbp_engine._license` resolution order:
#' 1. `RBP_LICENSE`
#' 2. `RBP_LICENSE_FILE`
#' 3. `~/.rbp-engine/license.txt`
#' 4. `/Library/rbp-engine/license.txt` (macOS system install)
#'
#' Never errors. Called automatically from `.onLoad`.
#'
#' @return The path adopted (invisibly), or `NULL` when the environment already
#'   carries a license or no default file exists.
#' @export
ensure_license_env <- function() {
  if (nzchar(Sys.getenv("RBP_LICENSE", unset = ""))) {
    return(invisible(NULL))
  }
  if (nzchar(Sys.getenv("RBP_LICENSE_FILE", unset = ""))) {
    return(invisible(NULL))
  }
  paths <- c(
    path.expand("~/.rbp-engine/license.txt"),
    "/Library/rbp-engine/license.txt"
  )
  for (p in paths) {
    if (file.exists(p)) {
      txt <- tryCatch(readLines(p, warn = FALSE), error = function(e) character())
      if (any(nzchar(trimws(paste(txt, collapse = "\n"))))) {
        Sys.setenv(RBP_LICENSE_FILE = p)
        return(invisible(p))
      }
    }
  }
  invisible(NULL)
}

#' Human-readable description of where the license is coming from.
#'
#' @return A short character string such as `"RBP_LICENSE (environment)"`,
#'   `"RBP_LICENSE_FILE=..."`, or a message indicating none is configured.
#'
#' @examples
#' license_source()
#'
#' @export
license_source <- function() {
  if (nzchar(Sys.getenv("RBP_LICENSE", unset = ""))) {
    return("RBP_LICENSE (environment)")
  }
  path <- Sys.getenv("RBP_LICENSE_FILE", unset = "")
  if (nzchar(path)) {
    return(paste0("RBP_LICENSE_FILE=", path))
  }
  "none (debug builds run unlicensed; release builds will fail)"
}
