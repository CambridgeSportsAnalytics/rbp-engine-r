#' Fit-weighted composite across attribute combinations.
#'
#' Evaluates combinations of predictive attributes across relevance and
#' similarity thresholds, then forms a fit-weighted composite prediction for
#' circumstances `theta`. Combinations that fit this task more reliably pull
#' harder on the forecast. The result also includes variable-level Impact on
#' Fit and Impact on Prediction.
#'
#' Recommended default path for analysis.
#'
#' @param y Numeric outcomes, length N.
#' @param X Numeric matrix, N rows by K columns (R column-major is native).
#' @param theta Circumstances vector, length K.
#' @param options Optional [PredictOptions], [MaxFitOptions], or [GridOptions].
#' @return A nested list of class `PredictionResults` (same field layout as
#'   Python `rbp_engine.PredictionResults`). Access fields with `$`, e.g.
#'   `result$yhat`, `result$insights$relevance`.
#'
#' @examples
#' PredictOptions(threshold = 0.5)
#' if (engine_available()) {
#'   set.seed(1)
#'   N <- 40; K <- 3
#'   X <- matrix(rnorm(N * K), N, K)
#'   y <- rnorm(N)
#'   theta <- colMeans(X)
#'   out <- predict_grid(y, X, theta, GridOptions(k = 1L, max_iter = 50L))
#'   as.numeric(out$yhat[1])
#'   best <- predict_maxfit(y, X, theta)
#'   res <- predict_psr(y, X, theta, PredictOptions(threshold = 0.5))
#'   res$yhat
#' }
#'
#' @export
predict_grid <- function(y, X, theta, options = NULL) {
  ensure_engine()
  X <- as_matrix_x(X)
  y <- as_numeric_vec(y, "y")
  theta <- as_numeric_vec(theta, "theta")
  check_dims(y, X, theta)
  if (is.null(options)) {
    options <- GridOptions()
  }
  .Call(C_rbpengine_run, "grid", y, X, theta, as_options_list(options))
}

#' Strongest prediction on a fixed set of attributes.
#'
#' Searches candidate thresholds and returns the single winner on your
#' objective: fit, adjusted fit, or k-fit (the default).
#'
#' Useful when you want that maximum on one attribute set, without a full
#' combination grid. For primary analysis, prefer [predict_grid()], which
#' diversifies across attribute combinations and thresholds, then blends by fit.
#'
#' @inheritParams predict_grid
#' @return A nested list of class `PredictionResults`. `yhat`, `fit`, and
#'   `thresholds` have length 1 (the winner).
#'
#' @examples
#' if (engine_available()) {
#'   set.seed(1)
#'   N <- 40; K <- 3
#'   X <- matrix(rnorm(N * K), N, K)
#'   y <- rnorm(N)
#'   theta <- colMeans(X)
#'   best <- predict_maxfit(y, X, theta)
#'   best$yhat
#'   best <- predict_maxfit(y, X, theta, MaxFitOptions(objective = "fit"))
#' }
#'
#' @export
predict_maxfit <- function(y, X, theta, options = NULL) {
  ensure_engine()
  X <- as_matrix_x(X)
  y <- as_numeric_vec(y, "y")
  theta <- as_numeric_vec(theta, "theta")
  check_dims(y, X, theta)
  if (is.null(options)) {
    options <- MaxFitOptions()
  }
  .Call(C_rbpengine_run, "maxfit", y, X, theta, as_options_list(options))
}

#' Partial-sample regression at thresholds you choose.
#'
#' Forms a relevance-weighted forecast at the censoring thresholds you choose.
#' The prediction uses the informative, relevant subset of the sample, rather
#' than treating every past observation equally.
#'
#' This is the evaluation inside each Grid cell and inside MaxFit. Use
#' `predict_psr()` when the attribute set and thresholds are already fixed.
#' For primary analysis, prefer [predict_grid()].
#'
#' Named `predict_psr` (not `predict`) so base R's [stats::predict] is not masked.
#'
#' @inheritParams predict_grid
#' @return A nested list of class `PredictionResults`. `yhat` has one prediction
#'   per threshold.
#'
#' @examples
#' if (engine_available()) {
#'   set.seed(1)
#'   N <- 40; K <- 3
#'   X <- matrix(rnorm(N * K), N, K)
#'   y <- rnorm(N)
#'   theta <- colMeans(X)
#'   res <- predict_psr(y, X, theta, PredictOptions(threshold = 0.5))
#'   res$yhat
#' }
#'
#' @export
predict_psr <- function(y, X, theta, options = NULL) {
  ensure_engine()
  X <- as_matrix_x(X)
  y <- as_numeric_vec(y, "y")
  theta <- as_numeric_vec(theta, "theta")
  check_dims(y, X, theta)
  .Call(C_rbpengine_run, "predict", y, X, theta, as_options_list(options))
}

#' Print a compact summary of prediction results.
#'
#' @param x A `PredictionResults` object from [predict_grid], [predict_maxfit],
#'   or [predict_psr].
#' @param ... Unused.
#' @return `x`, invisibly.
#' @method print PredictionResults
#' @export
print.PredictionResults <- function(x, ...) {
  cat(
    "PredictionResults(n=", x$n_observations,
    ", k=", x$n_variables,
    ", T=", x$n_thresholds,
    ", yhat=", paste(sprintf("%.4g", x$yhat), collapse = ", "),
    sep = ""
  )
  if (!is.null(x$grid_insights)) {
    cat(", grid_insights")
  }
  if (!is.null(x$grid_cells)) {
    cat(", grid_cells")
  }
  cat(")\n")
  invisible(x)
}

# ---- internals ----

as_matrix_x <- function(X) {
  if (is.data.frame(X)) {
    X <- data.matrix(X)
  }
  if (!is.matrix(X)) {
    stop("X must be a numeric matrix (N x K)", call. = FALSE)
  }
  storage.mode(X) <- "double"
  X
}

as_numeric_vec <- function(x, name) {
  x <- as.numeric(x)
  if (anyNA(x)) {
    stop(name, " must be finite numeric (no NA)", call. = FALSE)
  }
  x
}

check_dims <- function(y, X, theta) {
  if (length(y) != nrow(X)) {
    stop("y length (", length(y), ") must equal X rows (", nrow(X), ")", call. = FALSE)
  }
  if (length(theta) != ncol(X)) {
    stop(
      "theta (circumstances) length (", length(theta),
      ") must equal X cols (", ncol(X), ")",
      call. = FALSE
    )
  }
}
