#' Install the private RBP Math Engine runtime.
#'
#' The CRAN/open **rbpengine** package is a thin client. Predictions need the
#' private engine (shared library + companions + `rbp-license-info`). This
#' helper fetches the platform tarball from
#' `https://docs.csanalytics.io/releases/` (or unpacks a local archive) into a
#' well-known prefix that [engine_candidate_paths()] already searches.
#'
#' This is a **user-called** step, never run at package install time (CRAN
#' policy). Analogous to `pip install rbp-engine` for the engine files only.
#'
#' @param version Release to fetch. `"latest"` (default) or a semver such as
#'   `"1.3.0"`. Ignored when `path` or `url` is set.
#' @param dest Install prefix. Default is [default_engine_home()]
#'   (`~/.local/share/rbp-engine` on Unix, `%LOCALAPPDATA%/rbp-engine` on
#'   Windows).
#' @param url Full URL of a `.tar.gz` (skips the manifest). `sha256` is not
#'   verified unless `manifest_url` is also reachable via `version`.
#' @param path Local `.tar.gz` or an already-staged runtime directory (`lib/`,
#'   `bin/`).
#' @param load If `TRUE` (default), [engine_load()] the newly installed library.
#' @param quiet Suppress download progress.
#'
#' @return The install prefix, invisibly.
#'
#' @examples
#' \dontrun{
#' install_engine()                 # latest for this machine
#' install_engine(version = "1.3.0")
#' install_engine(path = "macos-arm64.tar.gz")
#' }
#'
#' @seealso [ensure_engine()], [engine_available()], [uninstall_engine()]
#' @export
install_engine <- function(
    version = "latest",
    dest = NULL,
    url = NULL,
    path = NULL,
    load = TRUE,
    quiet = FALSE
) {
  dest <- if (is.null(dest) || !nzchar(dest)) default_engine_home() else path.expand(dest)

  if (!is.null(path) && nzchar(path)) {
    path <- path.expand(path)
    if (!file.exists(path)) {
      stop("path does not exist: ", path, call. = FALSE)
    }
    .install_from_local(path, dest, quiet = quiet)
  } else if (!is.null(url) && nzchar(url)) {
    tar <- .download_to_temp(url, quiet = quiet)
    on.exit(unlink(tar), add = TRUE)
    .unpack_runtime_tar(tar, dest)
  } else {
    .install_from_releases(version, dest, quiet = quiet)
  }

  Sys.setenv(RBP_ENGINE_HOME = dest)
  options(rbpengine.engine_home = dest)

  if (isTRUE(load)) {
    lib <- file.path(dest, "lib", .engine_lib_filename())
    engine_load(lib)
  }
  invisible(dest)
}

#' Default prefix used by [install_engine()].
#'
#' @return A directory path.
#'
#' @examples
#' default_engine_home()
#'
#' @export
default_engine_home <- function() {
  sysname <- Sys.info()[["sysname"]]
  if (identical(sysname, "Windows")) {
    local <- Sys.getenv("LOCALAPPDATA", unset = "")
    if (nzchar(local)) {
      return(file.path(local, "rbp-engine"))
    }
    return(file.path(path.expand("~"), "AppData", "Local", "rbp-engine"))
  }
  file.path(path.expand("~"), ".local", "share", "rbp-engine")
}

#' Remove a runtime tree installed by [install_engine()].
#'
#' Refuses to delete a directory that does not look like an RBP runtime
#' (`lib/librbp_math_lib.*` or `lib/rbp_math_lib.dll` present).
#'
#' @param dest Prefix to remove. Default [default_engine_home()].
#' @return `TRUE` invisibly if removed.
#' @export
uninstall_engine <- function(dest = NULL) {
  dest <- if (is.null(dest) || !nzchar(dest)) default_engine_home() else path.expand(dest)
  lib <- file.path(dest, "lib", .engine_lib_filename())
  if (!file.exists(lib)) {
    stop(
      "Refusing to delete ", dest,
      ": no engine library at ", lib,
      call. = FALSE
    )
  }
  unlink(dest, recursive = TRUE, force = TRUE)
  invisible(TRUE)
}

#' Public HTTPS prefix for runtime tarballs.
#'
#' Override with env `RBP_ENGINE_RELEASES_URL` (no trailing slash).
#'
#' @return Character scalar.
#'
#' @examples
#' engine_releases_url()
#'
#' @export
engine_releases_url <- function() {
  env <- Sys.getenv("RBP_ENGINE_RELEASES_URL", unset = "")
  if (nzchar(env)) {
    return(sub("/+$", "", env))
  }
  "https://docs.csanalytics.io/releases"
}

#' Platform id matching `manifest.json` artifacts (`macos-arm64`, …).
#'
#' @return Character scalar.
#'
#' @examples
#' engine_runtime_id()
#'
#' @export
engine_runtime_id <- function() {
  sys <- Sys.info()[["sysname"]]
  mach <- tolower(Sys.info()[["machine"]])
  if (identical(sys, "Darwin")) {
    if (mach %in% c("arm64", "aarch64")) {
      return("macos-arm64")
    }
    return("macos-x86_64")
  }
  if (identical(sys, "Windows")) {
    if (mach %in% c("arm64", "aarch64")) {
      return("windows-arm64")
    }
    return("windows-x64")
  }
  if (mach %in% c("arm64", "aarch64")) {
    return("linux-arm64")
  }
  "linux-x64"
}

.install_from_releases <- function(version, dest, quiet = FALSE) {
  base <- engine_releases_url()
  version <- sub("^/+|/+$", "", version)
  manifest_url <- paste0(base, "/", version, "/manifest.json")
  raw <- .download_to_temp(manifest_url, quiet = quiet)
  on.exit(unlink(raw), add = TRUE)
  manifest <- .read_manifest(raw)
  rid <- engine_runtime_id()
  art <- NULL
  for (a in manifest$artifacts) {
    if (identical(a$id, rid)) {
      art <- a
      break
    }
  }
  if (is.null(art)) {
    ids <- vapply(manifest$artifacts, function(a) a$id, character(1))
    stop(
      "No runtime tarball for this platform (", rid, ").\n",
      "Available: ", paste(ids, collapse = ", "), "\n",
      "See ", manifest_url,
      call. = FALSE
    )
  }
  tar_url <- art$url
  if (is.null(tar_url) || !nzchar(tar_url)) {
    tar_url <- paste0(base, "/", version, "/", art$filename)
  }
  tar <- .download_to_temp(tar_url, quiet = quiet)
  on.exit(unlink(tar), add = TRUE)
  expected <- art$sha256
  if (!is.null(expected) && nzchar(expected)) {
    got <- .file_sha256(tar)
    if (!identical(tolower(got), tolower(expected))) {
      stop(
        "Checksum mismatch for ", art$filename, ".\n",
        "  expected: ", expected, "\n",
        "  got:      ", got,
        call. = FALSE
      )
    }
  }
  .unpack_runtime_tar(tar, dest)
}

.install_from_local <- function(path, dest, quiet = FALSE) {
  if (dir.exists(path)) {
    lib <- file.path(path, "lib", .engine_lib_filename())
    if (!file.exists(lib) && file.exists(file.path(path, .engine_lib_filename()))) {
      # Bare native dir (engine sitting at prefix root).
      dir.create(file.path(dest, "lib"), recursive = TRUE, showWarnings = FALSE)
      file.copy(file.path(path, .engine_lib_filename()), file.path(dest, "lib"), overwrite = TRUE)
      extras <- list.files(path, pattern = "\\.(so|dll|dylib)(\\.[0-9]+)*$", full.names = TRUE)
      extras <- extras[!grepl(paste0(.engine_lib_filename(), "$"), basename(extras))]
      if (length(extras)) {
        file.copy(extras, file.path(dest, "lib"), overwrite = TRUE)
      }
      cli <- file.path(path, "rbp-license-info")
      if (!file.exists(cli)) {
        cli <- file.path(path, "rbp-license-info.exe")
      }
      if (file.exists(cli)) {
        dir.create(file.path(dest, "bin"), recursive = TRUE, showWarnings = FALSE)
        file.copy(cli, file.path(dest, "bin"), overwrite = TRUE)
      }
      return(invisible(dest))
    }
    if (!file.exists(lib)) {
      stop("Not a staged runtime (missing ", lib, ")", call. = FALSE)
    }
    .copy_tree(path, dest)
    return(invisible(dest))
  }
  .unpack_runtime_tar(path, dest)
}

.unpack_runtime_tar <- function(tar, dest) {
  tmp <- tempfile("rbp-runtime-")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)
  utils::untar(tar, exdir = tmp)
  # Tolerate a single wrapping directory.
  root <- tmp
  kids <- list.files(tmp, full.names = TRUE)
  if (length(kids) == 1L && dir.exists(kids[[1]]) && !dir.exists(file.path(tmp, "lib"))) {
    root <- kids[[1]]
  }
  lib <- file.path(root, "lib", .engine_lib_filename())
  if (!file.exists(lib)) {
    stop(
      "Archive did not contain lib/", .engine_lib_filename(),
      call. = FALSE
    )
  }
  .copy_tree(root, dest)
  cli <- file.path(dest, "bin", "rbp-license-info")
  if (!file.exists(cli)) {
    cli <- file.path(dest, "bin", "rbp-license-info.exe")
  }
  if (file.exists(cli) && !identical(Sys.info()[["sysname"]], "Windows")) {
    Sys.chmod(cli, mode = "0755")
  }
  invisible(dest)
}

.copy_tree <- function(from, to) {
  dir.create(to, recursive = TRUE, showWarnings = FALSE)
  kids <- list.files(from, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  for (k in kids) {
    dest_k <- file.path(to, basename(k))
    if (dir.exists(k)) {
      unlink(dest_k, recursive = TRUE, force = TRUE)
      dir.create(dest_k, recursive = TRUE, showWarnings = FALSE)
      .copy_tree(k, dest_k)
    } else {
      file.copy(k, dest_k, overwrite = TRUE)
    }
  }
  invisible(to)
}

.download_to_temp <- function(url, quiet = FALSE) {
  dest <- tempfile("rbp-dl-")
  # Keep a suffix so untar/json sniffers can guess; URLs often end in .json / .tar.gz.
  ext <- sub(".*\\.", ".", sub("[?#].*$", "", url))
  if (nchar(ext) > 1L && nchar(ext) < 10L) {
    dest <- paste0(dest, ext)
  }
  ok <- tryCatch(
    {
      utils::download.file(url, destfile = dest, mode = "wb", quiet = quiet)
      TRUE
    },
    error = function(e) e
  )
  if (!isTRUE(ok)) {
    stop("Failed to download ", url, "\n", conditionMessage(ok), call. = FALSE)
  }
  dest
}

.read_manifest <- function(path) {
  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
  parsed <- .parse_json(txt)
  arts <- parsed$artifacts
  if (is.null(arts) || !length(arts)) {
    stop("manifest.json has no artifacts", call. = FALSE)
  }
  # jsonlite may return a data.frame; normalize to list of lists.
  if (is.data.frame(arts)) {
    parsed$artifacts <- lapply(seq_len(nrow(arts)), function(i) {
      as.list(arts[i, , drop = TRUE])
    })
  }
  parsed
}

.parse_json <- function(txt) {
  jsonlite::fromJSON(txt, simplifyVector = FALSE)
}

.file_sha256 <- function(path) {
  if (requireNamespace("digest", quietly = TRUE)) {
    return(digest::digest(path, algo = "sha256", file = TRUE))
  }
  if (nzchar(Sys.which("shasum"))) {
    out <- system2("shasum", c("-a", "256", path), stdout = TRUE, stderr = FALSE)
    return(.first_hex(out))
  }
  if (nzchar(Sys.which("sha256sum"))) {
    out <- system2("sha256sum", path, stdout = TRUE, stderr = FALSE)
    return(.first_hex(out))
  }
  if (identical(Sys.info()[["sysname"]], "Windows") && nzchar(Sys.which("certutil"))) {
    out <- system2("certutil", c("-hashfile", path, "SHA256"), stdout = TRUE, stderr = FALSE)
    hex <- grep("^[0-9a-fA-F ]+$", out, value = TRUE)
    hex <- hex[nzchar(trimws(hex))]
    if (length(hex)) {
      return(tolower(gsub("\\s+", "", hex[[1]])))
    }
  }
  stop(
    "Need sha256 to verify the download (install the digest package, or shasum/sha256sum).",
    call. = FALSE
  )
}

.first_hex <- function(out) {
  line <- out[[1]]
  sub("\\s.*$", "", line)
}
