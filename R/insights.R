#' Standalone insight scores (no full predict required).
#'
#' These call the loaded RBP Math Engine directly. Use [ensure_engine()] /
#' [engine_available()] if you need to check the runtime first.
#'
#' @param X Numeric matrix, N rows by K columns.
#' @param theta Circumstances vector, length K (not used by [info_x()]).
#' @return For [relevance()] and [similarity()], a numeric vector of length N.
#'   [info_x()] returns a numeric vector of length K. [info_theta()] returns a
#'   single numeric. [relevance_metrics()] returns a named list of metric
#'   vectors/scalars from the engine.
#'
#' @examples
#' \dontrun{
#' X <- matrix(rnorm(20 * 3), 20, 3)
#' theta <- colMeans(X)
#' relevance(X, theta)
#' similarity(X, theta)
#' info_x(X)
#' info_theta(X, theta)
#' relevance_metrics(X, theta)
#' }
#'
#' @export
relevance <- function(X, theta) {
  ensure_engine()
  X <- as_matrix_x(X)
  theta <- as_numeric_vec(theta, "theta")
  if (length(theta) != ncol(X)) {
    stop("theta length must equal X cols", call. = FALSE)
  }
  .Call(C_rbpengine_insight, "relevance", X, theta)
}

#' @rdname relevance
#' @export
similarity <- function(X, theta) {
  ensure_engine()
  X <- as_matrix_x(X)
  theta <- as_numeric_vec(theta, "theta")
  if (length(theta) != ncol(X)) {
    stop("theta length must equal X cols", call. = FALSE)
  }
  .Call(C_rbpengine_insight, "similarity", X, theta)
}

#' @rdname relevance
#' @export
info_x <- function(X) {
  ensure_engine()
  X <- as_matrix_x(X)
  .Call(C_rbpengine_insight, "info_x", X, NULL)
}

#' @rdname relevance
#' @export
info_theta <- function(X, theta) {
  ensure_engine()
  X <- as_matrix_x(X)
  theta <- as_numeric_vec(theta, "theta")
  if (length(theta) != ncol(X)) {
    stop("theta length must equal X cols", call. = FALSE)
  }
  as.numeric(.Call(C_rbpengine_insight, "info_theta", X, theta))
}

#' @rdname relevance
#' @export
relevance_metrics <- function(X, theta) {
  ensure_engine()
  X <- as_matrix_x(X)
  theta <- as_numeric_vec(theta, "theta")
  if (length(theta) != ncol(X)) {
    stop("theta length must equal X cols", call. = FALSE)
  }
  .Call(C_rbpengine_insight, "metrics", X, theta)
}
