#' Run the `rbp-license-info` CLI shipped with the engine runtime.
#'
#' Looks in the installed runtime `bin/` (see [install_engine()]), well-known
#' prefixes, cargo `target/`, then `PATH`.
#'
#' @param ... Arguments passed to the binary (character; e.g. `"--viewlicense"`).
#' @return Exit status (invisibly).
#' @export
license_info <- function(...) {
  bin <- .find_license_info()
  if (!nzchar(bin)) {
    stop(
      "Could not find rbp-license-info.\n",
      "Install the engine with rbpengine::install_engine() ",
      "(it ships in the runtime bin/ directory), or put rbp-license-info on PATH.",
      call. = FALSE
    )
  }
  args <- c(...)
  status <- system2(bin, args = args)
  invisible(status)
}

#' @keywords internal
#' @noRd
run_license_info <- function(...) {
  license_info(...)
}

.find_license_info <- function() {
  sys <- Sys.info()[["sysname"]]
  exe <- if (identical(sys, "Windows")) "rbp-license-info.exe" else "rbp-license-info"
  homes <- unique(c(
    Sys.getenv("RBP_ENGINE_HOME", unset = ""),
    getOption("rbpengine.engine_home", default = ""),
    default_engine_home(),
    "/Library/rbp-engine",
    file.path(path.expand("~"), ".local/share/rbp-engine"),
    file.path(path.expand("~"), ".rbp-engine")
  ))
  homes <- homes[nzchar(homes)]
  candidates <- c(
    file.path(homes, "bin", exe),
    file.path("..", "target", "release", "rbp-license-info"),
    file.path("..", "target", "debug", "rbp-license-info"),
    Sys.which(c("rbp-license-info", exe))
  )
  for (p in candidates) {
    if (nzchar(p) && file.exists(p)) {
      return(p)
    }
  }
  ""
}
