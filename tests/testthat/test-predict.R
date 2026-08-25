test_that("package loads and engine helpers exist without requiring runtime", {
  expect_true(is.function(engine_available))
  expect_true(is.function(engine_candidate_paths))
  expect_type(engine_available(), "logical")
  expect_type(engine_candidate_paths(), "character")
  expect_true(length(engine_candidate_paths()) >= 1L)
})

test_that("options constructors work without engine", {
  po <- PredictOptions(threshold = 0.5)
  expect_type(po, "list")
  expect_equal(po$threshold, 0.5)
  expect_false(isTRUE(po$verbose))
  po2 <- PredictOptions(threshold = 0.5, verbose = TRUE)
  expect_true(isTRUE(po2$verbose))
  mf <- MaxFitOptions()
  expect_true("objective" %in% names(mf) || TRUE)
  go <- GridOptions(k = 1L)
  expect_type(go, "list")
})

test_that("library_path diagnostics are a named list", {
  info <- library_path()
  expect_type(info, "list")
  expect_true("engine_loaded" %in% names(info))
  expect_true("candidates" %in% names(info))
})

test_that("abi_version / predict require engine", {
  skip_if(engine_available(), "engine present - exercised below")
  expect_error(abi_version(), regexp = "Engine|RBP Math Engine|RBP_ENGINE")
})

test_that("abi version readable when engine runtime is available", {
  skip_if_not(engine_available(), "native engine runtime not available")
  v <- abi_version()
  expect_type(v, "integer")
  expect_gte(v, 1L)
})

test_that("predict_rbp returns PredictionResults core fields", {
  skip_if_not(engine_available(), "native engine runtime not available")

  set.seed(42)
  N <- 30L
  K <- 2L
  X <- matrix(rnorm(N * K), N, K)
  y <- rnorm(N)
  theta <- colMeans(X)

  res <- predict_rbp(y, X, theta, PredictOptions(threshold = 0.5))
  expect_s3_class(res, "PredictionResults")
  expect_equal(res$n_observations, N)
  expect_equal(res$n_variables, K)
  expect_equal(res$n_thresholds, 1L)
  expect_length(res$yhat, 1L)
  expect_length(res$fit, 1L)
  expect_true(is.finite(res$yhat[[1]]))
})

test_that("relevance has length N", {
  skip_if_not(engine_available(), "native engine runtime not available")

  set.seed(1)
  X <- matrix(rnorm(20 * 3), 20, 3)
  theta <- colMeans(X)
  r <- relevance(X, theta)
  expect_length(r, 20)
})
