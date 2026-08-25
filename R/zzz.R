# Package load hooks for rbpengine.

.onLoad <- function(libname, pkgname) {
  ensure_license_env()
  # Optional: do not fail attach when the private engine runtime is absent
  # (CRAN checks, thin client install). Predictions call ensure_engine().
  .try_load_engine_quiet()
}

.onAttach <- function(libname, pkgname) {
  if (engine_available()) {
    abi <- tryCatch(as.character(abi_version()), error = function(e) "unknown")
    packageStartupMessage(
      "rbpengine ", utils::packageVersion("rbpengine"),
      " + engine ABI ", abi,
      " (", engine_path_loaded(), ")"
    )
  } else {
    packageStartupMessage(
      "rbpengine ", utils::packageVersion("rbpengine"),
      " (engine runtime not loaded — run rbpengine::install_engine(); ",
      "see ?install_engine)"
    )
  }
}
