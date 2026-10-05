#' Configuration objects (named lists) for predict / maxfit / grid.
#'
#' Mirrors Python `PredictOptions`, `MaxFitOptions`, and `GridOptions`.
#' Pass the list to [predict_psr], [predict_maxfit], or [predict_grid].
#' Fields set to `NULL` are omitted so the engine applies its defaults.
#'
#' @param threshold Numeric vector of thresholds, or `NULL` for engine default.
#' @param censor_type Character: `"relevance"`, `"similarity"`, or `"both"`
#'   (`"both"` only for maxfit/grid).
#' @param censor_unit `"percent"` or `"score"`.
#' @param censor_operator Comparison vs threshold. Default `"greater_than_or_equal_to"` (`">="`).
#' @param prediction_scale `"response"` or `"logistic"`.
#' @param adj_fit_multiplier `"k"`, `"identity"`, or `"log"`.
#' @param inv_method `"gaussian"`, `"cholesky"`, or `"pseudoinverse"`.
#' @param verify_missing_data Logical. Predict / MaxFit only (not [GridOptions]).
#' @param include_linear_regression Logical; fills `yhat_linear` when available.
#' @param verbose Logical; when `TRUE`, the engine may print library status
#'   diagnostics (default `FALSE`).
#' @return A named list with class `PredictOptions` or `MaxFitOptions`.
#' @seealso [GridOptions], [predict_psr], [predict_maxfit]
#'
#' @examples
#' PredictOptions(threshold = 0.5)
#' MaxFitOptions(objective = "kfit")
#'
#' @export
PredictOptions <- function(
    threshold = NULL,
    censor_type = "relevance",
    censor_unit = "percent",
    censor_operator = "greater_than_or_equal_to",
    prediction_scale = "response",
    adj_fit_multiplier = "k",
    inv_method = "gaussian",
    verify_missing_data = FALSE,
    include_linear_regression = FALSE,
    verbose = FALSE
) {
  structure(
    list(
      threshold = threshold,
      censor_type = censor_type,
      censor_unit = censor_unit,
      censor_operator = censor_operator,
      prediction_scale = prediction_scale,
      adj_fit_multiplier = adj_fit_multiplier,
      inv_method = inv_method,
      verify_missing_data = isTRUE(verify_missing_data),
      include_linear_regression = isTRUE(include_linear_regression),
      verbose = isTRUE(verbose)
    ),
    class = c("PredictOptions", "list")
  )
}

#' @rdname PredictOptions
#' @param objective MaxFit objective: `"kfit"`, `"fit"`, or `"adjusted_fit"`.
#' @param inner_parallel `"auto"` or `"off"` (within-call parallelism).
#' @export
MaxFitOptions <- function(
    threshold = c(0.0, 0.2, 0.5, 0.8),
    censor_type = "both",
    censor_unit = "percent",
    censor_operator = "greater_than_or_equal_to",
    prediction_scale = "response",
    adj_fit_multiplier = "k",
    inv_method = "gaussian",
    verify_missing_data = FALSE,
    include_linear_regression = FALSE,
    verbose = FALSE,
    objective = "kfit",
    inner_parallel = "auto"
) {
  base <- PredictOptions(
    threshold = threshold,
    censor_type = censor_type,
    censor_unit = censor_unit,
    censor_operator = censor_operator,
    prediction_scale = prediction_scale,
    adj_fit_multiplier = adj_fit_multiplier,
    inv_method = inv_method,
    verify_missing_data = verify_missing_data,
    include_linear_regression = include_linear_regression,
    verbose = verbose
  )
  base$objective <- objective
  base$`_inner_parallel` <- inner_parallel
  class(base) <- c("MaxFitOptions", "PredictOptions", "list")
  base
}

#' Grid search options for [predict_grid].
#'
#' Same censor / scale / inversion knobs as [PredictOptions], plus combination
#' search settings. Does **not** take `verify_missing_data` / `missing_moments`:
#' Grid chooses complete vs pairwise moments per combination.
#'
#' @param threshold Numeric vector of thresholds. Default `c(0, 0.2, 0.5, 0.8)`.
#' @param censor_type `"relevance"`, `"similarity"`, or `"both"` (default).
#' @param censor_unit `"percent"` or `"score"`.
#' @param censor_operator Comparison vs threshold. Default `"greater_than_or_equal_to"`.
#' @param prediction_scale `"response"` or `"logistic"`.
#' @param adj_fit_multiplier `"k"`, `"identity"`, or `"log"`.
#' @param inv_method `"gaussian"`, `"cholesky"`, or `"pseudoinverse"`.
#' @param include_linear_regression Logical; fills `yhat_linear` when available.
#' @param verbose Logical; engine diagnostics (default `FALSE`).
#' @param max_iter Combination search budget.
#' @param min_k Minimum number of attributes turned on in a combination.
#'   Combinations may be larger, up to `max_k` or all attributes, until
#'   `max_iter` is filled. Default `1`.
#' @param max_k Maximum number of attributes turned on in a combination.
#'   Omit it to allow every attribute. Set it equal to `min_k` for that
#'   exact size.
#' @param allowed_k Integer vector of exact combination sizes, such as
#'   `c(1L, 3L, 7L, 10L)`. Sizes that are not listed are not legal. Replaces
#'   `min_k` and `max_k`. A contiguous vector matches that band.
#' @param k Alias of `min_k`. Still accepted. If both are set to different
#'   values, this is an error unless `k` was left at its default.
#' @param seed RNG seed.
#' @param retain_all Retain all grid cell objects.
#' @param retain_grid_objects Character policy string (overrides `retain_all`).
#' @param attribute_combi Optional matrix of attribute combinations. Cannot
#'   be combined with `required_attributes` or `attribute_groups`.
#' @param required_attributes Length-K 0/1 mask of attributes that are on in
#'   every generated combination. Column order matches `X`.
#' @param attribute_groups G by K 0/1 mask. Each row is a group of attributes
#'   selected together. Rows must be disjoint. A row with a single 1 is
#'   ignored. A required attribute inside a group forces the whole group.
#' @param adjust_impact_for_missing Logical; incomplete-column IOF/IOP vs a
#'   μ+σZ include-k null (default `TRUE`). `FALSE` is the pre-adjustment /
#'   PSR-parity baseline.
#' @param inner_parallel `"auto"` or `"off"` (within-call parallelism).
#' @return A named list with class `GridOptions`.
#' @seealso [PredictOptions], [predict_grid]
#'
#' @examples
#' GridOptions(min_k = 2L, max_iter = 100L)
#'
#' @export
GridOptions <- function(
    threshold = c(0.0, 0.2, 0.5, 0.8),
    censor_type = "both",
    censor_unit = "percent",
    censor_operator = "greater_than_or_equal_to",
    prediction_scale = "response",
    adj_fit_multiplier = "k",
    inv_method = "gaussian",
    include_linear_regression = FALSE,
    verbose = FALSE,
    max_iter = 1000L,
    min_k = NULL,
    max_k = NULL,
    allowed_k = NULL,
    k = 1L,
    seed = 42L,
    retain_all = FALSE,
    retain_grid_objects = NULL,
    attribute_combi = NULL,
    required_attributes = NULL,
    attribute_groups = NULL,
    adjust_impact_for_missing = TRUE,
    inner_parallel = "auto"
) {
  base <- PredictOptions(
    threshold = threshold,
    censor_type = censor_type,
    censor_unit = censor_unit,
    censor_operator = censor_operator,
    prediction_scale = prediction_scale,
    adj_fit_multiplier = adj_fit_multiplier,
    inv_method = inv_method,
    include_linear_regression = include_linear_regression,
    verbose = verbose
  )
  if (!is.null(allowed_k)) {
    if (!is.null(min_k) || !missing(k) || !is.null(max_k)) {
      stop(
        "allowed_k replaces min_k and max_k. Pass allowed_k, or pass min_k and max_k, not both.",
        call. = FALSE
      )
    }
  }
  if (!is.null(min_k)) {
    if (!missing(k) && as.integer(k) != as.integer(min_k)) {
      stop("pass min_k or k, not both with different values", call. = FALSE)
    }
    k <- min_k
  }
  base$verify_missing_data <- NULL
  base$max_iter <- as.integer(max_iter)
  base$k <- as.integer(k)
  base$max_k <- if (is.null(max_k)) NULL else as.integer(max_k)
  base$allowed_k <- if (is.null(allowed_k)) NULL else as.integer(allowed_k)
  base$seed <- as.integer(seed)
  base$retain_all <- isTRUE(retain_all)
  base$retain_grid_objects <- retain_grid_objects
  base$attribute_combi <- attribute_combi
  base$required_attributes <- required_attributes
  base$attribute_groups <- attribute_groups
  base$adjust_impact_for_missing <- isTRUE(adjust_impact_for_missing)
  base$`_inner_parallel` <- inner_parallel
  class(base) <- c("GridOptions", "list")
  base
}

#' Drop NULL fields so C applies engine defaults for those keys.
#' @noRd
as_options_list <- function(options) {
  if (is.null(options)) {
    return(NULL)
  }
  if (!is.list(options)) {
    stop("options must be a list (PredictOptions / MaxFitOptions / GridOptions)", call. = FALSE)
  }
  keep <- !vapply(options, is.null, logical(1))
  options[keep]
}
