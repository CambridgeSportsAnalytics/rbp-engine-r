#' Process-wide percentile cut algorithm used when `censor_unit` is percent.
#'
#' @param algorithm Integer code or string naming the algorithm (for example
#'   `"full_sort"`, `"order_statistics"`). Exact accepted values are defined by
#'   the loaded engine ABI.
#' @return [set_percentile_value_algorithm()] returns `NULL` invisibly.
#'   [get_percentile_value_algorithm()] returns the current algorithm code as an
#'   integer.
#' @export
set_percentile_value_algorithm <- function(algorithm) {
  ensure_engine()
  .Call(C_rbpengine_set_percentile, algorithm)
  invisible(NULL)
}

#' @rdname set_percentile_value_algorithm
#' @export
get_percentile_value_algorithm <- function() {
  ensure_engine()
  as.integer(.Call(C_rbpengine_get_percentile))
}
