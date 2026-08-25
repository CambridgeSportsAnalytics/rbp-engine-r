#' Package / ABI version and path diagnostics.
#'
#' @return [package_version_rbpengine()] returns the installed package version
#'   as a string. [abi_version()] returns the loaded engine C ABI version as an
#'   integer (errors if the engine is not loaded). [engine_path_loaded()]
#'   returns the resolved engine library path, or `NA_character_` when none is
#'   loaded. [library_path()] returns a named list of engine/license diagnostics
#'   including candidate search paths.
#'
#' @seealso [engine_available()], [ensure_engine()], [license_source()]
#' @name version-helpers
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
