/**
 * R .Call bridge to the rbp-math-lib C ABI.
 *
 * Model path matches Python: run engine → harvest nested snapshot → free handles.
 * Matrices use RBP_LAYOUT_COL_MAJOR (R's native storage).
 */

#include <R.h>
#include <Rinternals.h>
#include <stdint.h>
#include <string.h>

#include "rbp_math.h"
#include "engine_api.h"

/** Fail .Call entry points when the private engine runtime is not loaded. */
static void require_engine(void) {
    if (!rbp_engine_is_loaded()) {
        error(
            "RBP Math Engine runtime is not loaded.\n"
            "Install the private RBP Math Engine bundle (see package README),\n"
            "or set RBP_ENGINE_LIB to librbp_math_lib.{dylib,so,dll},\n"
            "then call engine_load() or re-attach the package.");
    }
}

#define RBP_OK_ 0
#define RBP_ERR_LICENSE_ 4
#define LAYOUT_COL  ((int32_t)RBP_LAYOUT_COL_MAJOR)
#define CENSOR_REL  RBP_GRID_CENSOR_RELEVANCE
#define CENSOR_SIM  RBP_GRID_CENSOR_SIMILARITY

/* ------------------------------------------------------------------------ */
/* Status / errors                                                          */
/* ------------------------------------------------------------------------ */

static void raise_status(RbpStatus st) {
    const char *msg = rbp_last_error();
    if (!msg || !msg[0]) {
        msg = "rbp-math-lib error";
    }
    if ((int)st == RBP_ERR_LICENSE_) {
        error("RbpLicenseError (%d): %s\n"
              "Install or view a license with rbp-license-info "
              "(see package README / product docs).",
              (int)st, msg);
    }
    error("RbpError (%d): %s", (int)st, msg);
}

static void check_st(RbpStatus st) {
    if (st != RBP_OK_) {
        raise_status(st);
    }
}

/* ------------------------------------------------------------------------ */
/* SEXP helpers                                                             */
/* ------------------------------------------------------------------------ */

static SEXP mk_named_list(int n, const char **names) {
    SEXP out = PROTECT(allocVector(VECSXP, n));
    SEXP nm = PROTECT(allocVector(STRSXP, n));
    for (int i = 0; i < n; i++) {
        SET_STRING_ELT(nm, i, mkChar(names[i]));
    }
    setAttrib(out, R_NamesSymbol, nm);
    UNPROTECT(2);
    return out;
}

static void set_list_el(SEXP list, int i, SEXP val) {
    SET_VECTOR_ELT(list, i, val);
}

/**
 * Protect stack helper for harvest_results: leave all PROTECT until the end.
 * Pass &nprot; every KEEP increments; one UNPROTECT(nprot) at exit.
 */
static SEXP keep(SEXP x, int *nprot) {
    PROTECT(x);
    (*nprot)++;
    return x;
}

static SEXP scalar_int(int v) {
    return ScalarInteger(v);
}

/**
 * Copy 2-D flat buffer (layout already COL_MAJOR for R) into matrix nrow x ncol.
 */
static SEXP matrix_from_colmajor(const double *src, size_t nrow, size_t ncol) {
    SEXP m = allocMatrix(REALSXP, (int)nrow, (int)ncol);
    size_t n = nrow * ncol;
    if (n > 0 && src) {
        memcpy(REAL(m), src, n * sizeof(double));
    }
    return m;
}

/**
 * 3-D array dims d0,d1,d2 from C-order flat buffer into R column-major array.
 */
static SEXP array3_from_c_order(const double *src, size_t d0, size_t d1, size_t d2) {
    SEXP a = allocVector(REALSXP, (R_xlen_t)(d0 * d1 * d2));
    SEXP dims = allocVector(INTSXP, 3);
    INTEGER(dims)[0] = (int)d0;
    INTEGER(dims)[1] = (int)d1;
    INTEGER(dims)[2] = (int)d2;
    setAttrib(a, R_DimSymbol, dims);
    double *out = REAL(a);
    for (size_t i0 = 0; i0 < d0; i0++) {
        for (size_t i1 = 0; i1 < d1; i1++) {
            for (size_t i2 = 0; i2 < d2; i2++) {
                size_t c_idx = i0 * (d1 * d2) + i1 * d2 + i2;
                size_t r_idx = i0 + i1 * d0 + i2 * d0 * d1;
                out[r_idx] = src[c_idx];
            }
        }
    }
    return a;
}

static int is_null_or_missing(SEXP x) {
    return x == R_NilValue || (TYPEOF(x) == LGLSXP && LENGTH(x) == 1 && LOGICAL(x)[0] == NA_LOGICAL);
}

static const char *list_get_str(SEXP list, const char *name, const char *default_val) {
    if (list == R_NilValue) {
        return default_val;
    }
    SEXP names = getAttrib(list, R_NamesSymbol);
    if (names == R_NilValue) {
        return default_val;
    }
    int n = length(list);
    for (int i = 0; i < n; i++) {
        if (strcmp(CHAR(STRING_ELT(names, i)), name) == 0) {
            SEXP el = VECTOR_ELT(list, i);
            if (is_null_or_missing(el)) {
                return default_val;
            }
            if (TYPEOF(el) == STRSXP && LENGTH(el) >= 1) {
                return CHAR(STRING_ELT(el, 0));
            }
            if (TYPEOF(el) == LGLSXP && LENGTH(el) >= 1) {
                return LOGICAL(el)[0] ? "true" : "false";
            }
        }
    }
    return default_val;
}

static int list_has(SEXP list, const char *name) {
    if (list == R_NilValue) {
        return 0;
    }
    SEXP names = getAttrib(list, R_NamesSymbol);
    if (names == R_NilValue) {
        return 0;
    }
    int n = length(list);
    for (int i = 0; i < n; i++) {
        if (strcmp(CHAR(STRING_ELT(names, i)), name) == 0) {
            SEXP el = VECTOR_ELT(list, i);
            return !is_null_or_missing(el);
        }
    }
    return 0;
}

static SEXP list_get(SEXP list, const char *name) {
    if (list == R_NilValue) {
        return R_NilValue;
    }
    SEXP names = getAttrib(list, R_NamesSymbol);
    if (names == R_NilValue) {
        return R_NilValue;
    }
    int n = length(list);
    for (int i = 0; i < n; i++) {
        if (strcmp(CHAR(STRING_ELT(names, i)), name) == 0) {
            return VECTOR_ELT(list, i);
        }
    }
    return R_NilValue;
}

static int list_get_bool(SEXP list, const char *name, int default_val) {
    SEXP el = list_get(list, name);
    if (is_null_or_missing(el)) {
        return default_val;
    }
    if (TYPEOF(el) == LGLSXP && LENGTH(el) >= 1) {
        return LOGICAL(el)[0] ? 1 : 0;
    }
    if (TYPEOF(el) == INTSXP && LENGTH(el) >= 1) {
        return INTEGER(el)[0] != 0;
    }
    if (TYPEOF(el) == REALSXP && LENGTH(el) >= 1) {
        return REAL(el)[0] != 0.0;
    }
    return default_val;
}

static size_t list_get_size(SEXP list, const char *name, size_t default_val) {
    SEXP el = list_get(list, name);
    if (is_null_or_missing(el)) {
        return default_val;
    }
    if (TYPEOF(el) == INTSXP && LENGTH(el) >= 1) {
        return (size_t)INTEGER(el)[0];
    }
    if (TYPEOF(el) == REALSXP && LENGTH(el) >= 1) {
        return (size_t)REAL(el)[0];
    }
    return default_val;
}

static uint32_t list_get_u32(SEXP list, const char *name, uint32_t default_val) {
    return (uint32_t)list_get_size(list, name, (size_t)default_val);
}

/* ------------------------------------------------------------------------ */
/* Enum codes (match Python / header)                                      */
/* ------------------------------------------------------------------------ */

static int code_censor_type(const char *s, int allow_both) {
    if (!s) {
        return 0;
    }
    if (strcmp(s, "relevance") == 0) {
        return 0;
    }
    if (strcmp(s, "similarity") == 0) {
        return 1;
    }
    if (strcmp(s, "both") == 0) {
        if (!allow_both) {
            error("censor_type='both' is only valid for predict_maxfit / predict_grid");
        }
        return 2;
    }
    /* numeric string? */
    if (s[0] >= '0' && s[0] <= '2' && s[1] == '\0') {
        int v = s[0] - '0';
        if (v == 2 && !allow_both) {
            error("censor_type='both' is only valid for predict_maxfit / predict_grid");
        }
        return v;
    }
    error("invalid censor_type '%s'", s);
    return 0;
}

static int code_censor_unit(const char *s) {
    if (!s || strcmp(s, "percent") == 0) {
        return 1;
    }
    if (strcmp(s, "score") == 0) {
        return 0;
    }
    error("invalid censor_unit '%s'", s);
    return 1;
}

static int code_censor_op(const char *s) {
    if (!s) {
        return 0;
    }
    if (strcmp(s, "greater_than") == 0 || strcmp(s, "gt") == 0 || strcmp(s, ">") == 0) {
        return 0;
    }
    if (strcmp(s, "less_than") == 0 || strcmp(s, "lt") == 0 || strcmp(s, "<") == 0) {
        return 1;
    }
    if (strcmp(s, "greater_than_or_equal_to") == 0 || strcmp(s, "gte") == 0 || strcmp(s, ">=") == 0) {
        return 2;
    }
    if (strcmp(s, "less_than_or_equal_to") == 0 || strcmp(s, "lte") == 0 || strcmp(s, "<=") == 0) {
        return 3;
    }
    error("invalid censor_operator '%s'", s);
    return 0;
}

static int code_pred_scale(const char *s) {
    if (!s || strcmp(s, "response") == 0 || strcmp(s, "default") == 0) {
        return 0;
    }
    if (strcmp(s, "logistic") == 0) {
        return 1;
    }
    error("invalid prediction_scale '%s'", s);
    return 0;
}

static int code_adj_fit(const char *s) {
    if (!s) {
        return 1;
    }
    if (strcmp(s, "identity") == 0 || strcmp(s, "1") == 0 || strcmp(s, "none") == 0) {
        return 0;
    }
    if (strcmp(s, "k") == 0 || strcmp(s, "k_multiplier") == 0) {
        return 1;
    }
    if (strcmp(s, "log") == 0 || strcmp(s, "logarithmic") == 0) {
        return 2;
    }
    error("invalid adj_fit_multiplier '%s'", s);
    return 1;
}

static int code_inv(const char *s) {
    if (!s || strcmp(s, "gaussian") == 0 || strcmp(s, "lu") == 0) {
        return 0;
    }
    if (strcmp(s, "cholesky") == 0 || strcmp(s, "chol") == 0) {
        return 1;
    }
    if (strcmp(s, "pseudoinverse") == 0 || strcmp(s, "pinv") == 0) {
        return 2;
    }
    error("invalid inv_method '%s'", s);
    return 0;
}

static int code_inner_parallel(const char *s) {
    if (!s || strcmp(s, "auto") == 0) {
        return 0;
    }
    if (strcmp(s, "off") == 0 || strcmp(s, "false") == 0 || strcmp(s, "0") == 0 ||
        strcmp(s, "sequential") == 0 || strcmp(s, "seq") == 0) {
        return 1;
    }
    error("invalid _inner_parallel / inner_parallel '%s'", s);
    return 0;
}

/* ------------------------------------------------------------------------ */
/* Options from R list                                                      */
/* ------------------------------------------------------------------------ */

#define APPLY_COMMON(set_prefix, opts, list, allow_both)                       \
    do {                                                                       \
        if (list_has((list), "threshold")) {                                   \
            SEXP thr = list_get((list), "threshold");                          \
            thr = PROTECT(coerceVector(thr, REALSXP));                         \
            check_st(set_prefix##_set_threshold(                               \
                (opts), REAL(thr), (size_t)LENGTH(thr)));                      \
            UNPROTECT(1);                                                      \
        }                                                                      \
        check_st(set_prefix##_set_censor_type(                                 \
            (opts),                                                            \
            code_censor_type(list_get_str((list), "censor_type", "relevance"), \
                             (allow_both))));                                  \
        check_st(set_prefix##_set_censor_unit(                                 \
            (opts),                                                            \
            code_censor_unit(list_get_str((list), "censor_unit", "percent"))));\
        check_st(set_prefix##_set_censor_operator(                             \
            (opts),                                                            \
            code_censor_op(                                                    \
                list_get_str((list), "censor_operator", "greater_than"))));    \
        check_st(set_prefix##_set_prediction_scale(                            \
            (opts),                                                            \
            code_pred_scale(                                                   \
                list_get_str((list), "prediction_scale", "response"))));       \
        check_st(set_prefix##_set_adj_fit_multiplier(                          \
            (opts),                                                            \
            code_adj_fit(list_get_str((list), "adj_fit_multiplier", "k"))));   \
        check_st(set_prefix##_set_inv_method(                                  \
            (opts), code_inv(list_get_str((list), "inv_method", "gaussian"))));\
        check_st(set_prefix##_set_include_linear_regression(                   \
            (opts), list_get_bool((list), "include_linear_regression", 0)));   \
        check_st(set_prefix##_set_verbose(                                     \
            (opts), list_get_bool((list), "verbose", 0)));                     \
    } while (0)

#define APPLY_MISSING_MOMENTS(set_prefix, opts, list)                          \
    do {                                                                       \
        check_st(set_prefix##_set_verify_missing_data(                         \
            (opts), list_get_bool((list), "verify_missing_data", 0)));         \
    } while (0)

static RbpPredictOptions *make_predict_options(SEXP list) {
    if (list == R_NilValue) {
        return NULL;
    }
    RbpPredictOptions *opts = rbp_predict_options_create();
    if (!opts) {
        error("rbp_predict_options_create returned null");
    }
    APPLY_COMMON(rbp_predict_options, opts, list, 0);
    APPLY_MISSING_MOMENTS(rbp_predict_options, opts, list);
    return opts;
}

static RbpMaxFitOptions *make_maxfit_options(SEXP list) {
    /* MaxFit always needs a handle (defaults). */
    RbpMaxFitOptions *opts = rbp_maxfit_options_create();
    if (!opts) {
        error("rbp_maxfit_options_create returned null");
    }
    if (list == R_NilValue) {
        return opts;
    }
    const char *obj = list_get_str(list, "objective", "kfit");
    check_st(rbp_maxfit_options_set_objective(opts, obj));
    APPLY_COMMON(rbp_maxfit_options, opts, list, 1);
    APPLY_MISSING_MOMENTS(rbp_maxfit_options, opts, list);
    const char *ip = list_get_str(list, "_inner_parallel", NULL);
    if (!ip) {
        ip = list_get_str(list, "inner_parallel", "auto");
    }
    check_st(rbp_maxfit_options_set_inner_parallel(opts, code_inner_parallel(ip)));
    return opts;
}

static RbpGridOptions *make_grid_options(SEXP list) {
    RbpGridOptions *opts = rbp_grid_options_create();
    if (!opts) {
        error("rbp_grid_options_create returned null");
    }
    if (list == R_NilValue) {
        return opts;
    }
    check_st(rbp_grid_options_set_max_iter(opts, list_get_size(list, "max_iter", 1000)));
    check_st(rbp_grid_options_set_k(opts, list_get_size(list, "k", 1)));
    check_st(rbp_grid_options_set_seed(opts, list_get_u32(list, "seed", 42)));

    if (list_has(list, "retain_grid_objects")) {
        check_st(rbp_grid_options_set_retain_grid_objects_str(
            opts, list_get_str(list, "retain_grid_objects", "none")));
    } else {
        check_st(rbp_grid_options_set_retain_all(
            opts, list_get_bool(list, "retain_all", 0)));
    }

    if (list_has(list, "attribute_combi")) {
        SEXP combi = list_get(list, "attribute_combi");
        if (!isMatrix(combi)) {
            error("attribute_combi must be a matrix");
        }
        combi = PROTECT(coerceVector(combi, REALSXP));
        int nr = nrows(combi);
        int nc = ncols(combi);
        check_st(rbp_grid_options_set_attribute_combi(
            opts, REAL(combi), (size_t)nr, (size_t)nc, LAYOUT_COL));
        UNPROTECT(1);
    }

    check_st(rbp_grid_options_set_adjust_impact_for_missing(
        opts, list_get_bool(list, "adjust_impact_for_missing", 1)));

    if (list_has(list, "verify_missing_data") || list_has(list, "missing_moments")) {
        error("verify_missing_data / missing_moments are not Grid options");
    }

    APPLY_COMMON(rbp_grid_options, opts, list, 1);
    const char *ip = list_get_str(list, "_inner_parallel", NULL);
    if (!ip) {
        ip = list_get_str(list, "inner_parallel", "auto");
    }
    check_st(rbp_grid_options_set_inner_parallel(opts, code_inner_parallel(ip)));
    return opts;
}

/* ------------------------------------------------------------------------ */
/* Results harvest (mirrors python/rbp_engine/results.py)                     */
/* ------------------------------------------------------------------------ */

/** Copy helpers that KEEP-protect onto *nprot. */

static SEXP try_copy1(const RbpPredictionResults *h,
                      RbpStatus (*copy)(const RbpPredictionResults *, double *, size_t),
                      size_t len,
                      int *nprot) {
    SEXP v = keep(allocVector(REALSXP, (R_xlen_t)len), nprot);
    if (copy(h, REAL(v), len) != RBP_OK_) {
        return R_NilValue; /* orphaned protect ok until end of harvest */
    }
    return v;
}

static SEXP require_copy1(const RbpPredictionResults *h,
                          RbpStatus (*copy)(const RbpPredictionResults *, double *, size_t),
                          size_t len,
                          int *nprot) {
    SEXP v = keep(allocVector(REALSXP, (R_xlen_t)len), nprot);
    check_st(copy(h, REAL(v), len));
    return v;
}

static SEXP try_copy2(const RbpPredictionResults *h,
                      int32_t (*has)(const RbpPredictionResults *),
                      RbpStatus (*dims)(const RbpPredictionResults *, size_t *, size_t *),
                      RbpStatus (*copy)(const RbpPredictionResults *, double *, size_t, int32_t),
                      int *nprot) {
    if (!has(h)) {
        return R_NilValue;
    }
    size_t nr = 0, nc = 0;
    check_st(dims(h, &nr, &nc));
    size_t ntot = nr * nc;
    double *tmp = (double *)R_alloc(ntot == 0 ? 1 : ntot, sizeof(double));
    check_st(copy(h, tmp, ntot, LAYOUT_COL));
    return keep(matrix_from_colmajor(tmp, nr, nc), nprot);
}

static SEXP try_copy2_censor(
    const RbpPredictionResults *h,
    int32_t censor,
    int32_t (*has)(const RbpPredictionResults *, int32_t),
    RbpStatus (*dims)(const RbpPredictionResults *, int32_t, size_t *, size_t *),
    RbpStatus (*copy)(const RbpPredictionResults *, int32_t, double *, size_t, int32_t),
    int *nprot) {
    if (!has(h, censor)) {
        return R_NilValue;
    }
    size_t nr = 0, nc = 0;
    check_st(dims(h, censor, &nr, &nc));
    size_t n = nr * nc;
    double *tmp = (double *)R_alloc(n == 0 ? 1 : n, sizeof(double));
    check_st(copy(h, censor, tmp, n, LAYOUT_COL));
    return keep(matrix_from_colmajor(tmp, nr, nc), nprot);
}

static SEXP try_copy3_censor(
    const RbpPredictionResults *h,
    int32_t censor,
    int32_t (*has)(const RbpPredictionResults *, int32_t),
    RbpStatus (*dims)(const RbpPredictionResults *, int32_t, size_t *, size_t *, size_t *),
    RbpStatus (*copy)(const RbpPredictionResults *, int32_t, double *, size_t),
    int *nprot) {
    if (!has(h, censor)) {
        return R_NilValue;
    }
    size_t d0 = 0, d1 = 0, d2 = 0;
    check_st(dims(h, censor, &d0, &d1, &d2));
    size_t n = d0 * d1 * d2;
    double *tmp = (double *)R_alloc(n == 0 ? 1 : n, sizeof(double));
    check_st(copy(h, censor, tmp, n));
    return keep(array3_from_c_order(tmp, d0, d1, d2), nprot);
}

static SEXP censor_cells(const RbpPredictionResults *h, int32_t censor, int *nprot) {
    SEXP yhat = try_copy2_censor(h, censor, rbp_results_has_yhat_cells,
                                 rbp_results_yhat_cells_dims, rbp_results_copy_yhat_cells, nprot);
    SEXP adj = try_copy2_censor(h, censor, rbp_results_has_adjusted_fit_cells,
                                rbp_results_adjusted_fit_cells_dims,
                                rbp_results_copy_adjusted_fit_cells, nprot);
    SEXP n_cells = try_copy2_censor(h, censor, rbp_results_has_n_cells,
                                    rbp_results_n_cells_dims, rbp_results_copy_n_cells, nprot);
    SEXP weights = try_copy3_censor(h, censor, rbp_results_has_weights_cells,
                                    rbp_results_weights_cells_dims, rbp_results_copy_weights_cells,
                                    nprot);
    SEXP xi = try_copy3_censor(h, censor, rbp_results_has_xi_solo_cells,
                               rbp_results_xi_solo_cells_dims, rbp_results_copy_xi_solo_cells, nprot);
    if (yhat == R_NilValue && adj == R_NilValue && n_cells == R_NilValue &&
        weights == R_NilValue && xi == R_NilValue) {
        return R_NilValue;
    }
    const char *names[] = {
        "yhat_cells", "adjusted_fit_cells", "n_cells", "weights_cells", "xi_solo_cells"
    };
    SEXP out = keep(mk_named_list(5, names), nprot);
    set_list_el(out, 0, yhat);
    set_list_el(out, 1, adj);
    set_list_el(out, 2, n_cells);
    set_list_el(out, 3, weights);
    set_list_el(out, 4, xi);
    return out;
}

/**
 * Harvest + free results handle. Nested list matches Python PredictionResults.
 * All allocations stay PROTECTed via keep() until UNPROTECT at return.
 */
static SEXP harvest_results(RbpPredictionResults *h) {
    if (!h) {
        error("null prediction results handle");
    }

    int nprot = 0;
    size_t n = rbp_results_num_observations(h);
    size_t k = rbp_results_num_variables(h);
    size_t t = rbp_results_num_thresholds(h);

    const char *rootn[] = {
        "n_observations", "n_variables", "n_thresholds", "thresholds",
        "yhat", "fit", "adjusted_fit", "agreement", "asymmetry", "k_fit",
        "outlier_influence", "yhat_linear", "auxiliary", "insights",
        "prediction_weights", "solo_distribution", "grid_insights", "grid_cells",
        "maxfit_index"
    };
    SEXP out = keep(mk_named_list(19, rootn), &nprot);
    set_list_el(out, 0, keep(scalar_int((int)n), &nprot));
    set_list_el(out, 1, keep(scalar_int((int)k), &nprot));
    set_list_el(out, 2, keep(scalar_int((int)t), &nprot));
    set_list_el(out, 3, require_copy1(h, rbp_results_copy_thresholds, t, &nprot));
    set_list_el(out, 4, require_copy1(h, rbp_results_copy_yhat, t, &nprot));
    set_list_el(out, 5, require_copy1(h, rbp_results_copy_fit, t, &nprot));
    set_list_el(out, 6, require_copy1(h, rbp_results_copy_adjusted_fit, t, &nprot));
    set_list_el(out, 7, require_copy1(h, rbp_results_copy_agreement, t, &nprot));
    set_list_el(out, 8, require_copy1(h, rbp_results_copy_asymmetry, t, &nprot));
    set_list_el(out, 9, require_copy1(h, rbp_results_copy_k_fit, t, &nprot));
    set_list_el(out, 10, require_copy1(h, rbp_results_copy_outlier_influence, t, &nprot));

    if (rbp_results_has_yhat_linear(h)) {
        set_list_el(out, 11, require_copy1(h, rbp_results_copy_yhat_linear, t, &nprot));
    }

    {
        SEXP relevance = try_copy1(h, rbp_results_copy_relevance, n, &nprot);
        SEXP similarity = try_copy1(h, rbp_results_copy_similarity, n, &nprot);
        SEXP info_x = try_copy1(h, rbp_results_copy_info_x, n, &nprot);
        SEXP info_theta = try_copy1(h, rbp_results_copy_info_theta, 1, &nprot);
        SEXP wconc = try_copy1(h, rbp_results_copy_weights_concentration, t, &nprot);
        if (relevance != R_NilValue || similarity != R_NilValue || info_x != R_NilValue ||
            info_theta != R_NilValue || wconc != R_NilValue) {
            const char *inn[] = {
                "relevance", "similarity", "info_x", "info_theta", "weights_concentration"
            };
            SEXP insights = keep(mk_named_list(5, inn), &nprot);
            set_list_el(insights, 0, relevance);
            set_list_el(insights, 1, similarity);
            set_list_el(insights, 2, info_x);
            set_list_el(insights, 3, info_theta);
            set_list_el(insights, 4, wconc);
            set_list_el(out, 13, insights);
        }
    }

    {
        SEXP weights =
            try_copy2(h, rbp_results_has_weights, rbp_results_weights_dims,
                      rbp_results_copy_weights, &nprot);
        SEXP weights_excluded =
            try_copy2(h, rbp_results_has_weights_excluded, rbp_results_weights_excluded_dims,
                      rbp_results_copy_weights_excluded, &nprot);
        SEXP include = R_NilValue;
        if (rbp_results_has_include(h)) {
            size_t nr = 0, nc = 0;
            check_st(rbp_results_include_dims(h, &nr, &nc));
            size_t len = nr * nc;
            uint8_t *tmp = (uint8_t *)R_alloc(len == 0 ? 1 : len, sizeof(uint8_t));
            check_st(rbp_results_copy_include(h, tmp, len, LAYOUT_COL));
            SEXP m = keep(allocMatrix(LGLSXP, (int)nr, (int)nc), &nprot);
            for (size_t i = 0; i < len; i++) {
                LOGICAL(m)[i] = tmp[i] ? 1 : 0;
            }
            include = m;
        }
        if (include != R_NilValue || weights != R_NilValue || weights_excluded != R_NilValue) {
            const char *wn[] = {"include", "weights", "weights_excluded"};
            SEXP prediction_weights = keep(mk_named_list(3, wn), &nprot);
            set_list_el(prediction_weights, 0, include);
            set_list_el(prediction_weights, 1, weights);
            set_list_el(prediction_weights, 2, weights_excluded);
            set_list_el(out, 14, prediction_weights);
        }
    }

    if (rbp_results_has_auxiliary(h)) {
        const char *an[] = {
            "phi", "lambda_sq", "full_var", "part_var", "r_star", "r_star_percent",
            "n", "k", "rho"
        };
        SEXP auxiliary = keep(mk_named_list(9, an), &nprot);
        set_list_el(auxiliary, 0, require_copy1(h, rbp_results_copy_aux_phi, t, &nprot));
        set_list_el(auxiliary, 1, require_copy1(h, rbp_results_copy_aux_lambda_sq, t, &nprot));
        set_list_el(auxiliary, 2, require_copy1(h, rbp_results_copy_aux_full_var, t, &nprot));
        set_list_el(auxiliary, 3, require_copy1(h, rbp_results_copy_aux_part_var, t, &nprot));
        set_list_el(auxiliary, 4, require_copy1(h, rbp_results_copy_aux_r_star, t, &nprot));
        set_list_el(auxiliary, 5, require_copy1(h, rbp_results_copy_aux_r_star_percent, t, &nprot));
        set_list_el(auxiliary, 6, require_copy1(h, rbp_results_copy_aux_n, t, &nprot));
        set_list_el(auxiliary, 7, require_copy1(h, rbp_results_copy_aux_k, t, &nprot));
        set_list_el(auxiliary, 8, require_copy1(h, rbp_results_copy_aux_rho, t, &nprot));
        set_list_el(out, 12, auxiliary);
    }

    if (rbp_results_has_maxfit_index(h)) {
        size_t mlen = rbp_results_maxfit_index_len(h);
        if (mlen > 0) {
            set_list_el(out, 18, require_copy1(h, rbp_results_copy_maxfit_index, mlen, &nprot));
        }
    }

    {
        SEXP y_solo = rbp_results_has_y_solo(h)
                          ? try_copy1(h, rbp_results_copy_y_solo, n, &nprot)
                          : R_NilValue;
        SEXP xi_solo =
            try_copy2(h, rbp_results_has_xi_solo, rbp_results_xi_solo_dims,
                      rbp_results_copy_xi_solo, &nprot);
        SEXP sigma = try_copy1(h, rbp_results_copy_ysolo_sigma, t, &nprot);
        SEXP skew = try_copy1(h, rbp_results_copy_ysolo_skewness, t, &nprot);
        SEXP kurt = try_copy1(h, rbp_results_copy_ysolo_kurtosis, t, &nprot);
        SEXP pmi = try_copy1(h, rbp_results_copy_ysolo_pearson_modality_index, t, &nprot);
        SEXP bimod = try_copy1(h, rbp_results_copy_ysolo_bimodal_index, t, &nprot);

        SEXP statistics = R_NilValue;
        if (sigma != R_NilValue || skew != R_NilValue || kurt != R_NilValue ||
            pmi != R_NilValue || bimod != R_NilValue) {
            const char *sn[] = {
                "sigma", "skewness", "kurtosis", "pearson_modality_index", "bimodal_index"
            };
            statistics = keep(mk_named_list(5, sn), &nprot);
            set_list_el(statistics, 0, sigma);
            set_list_el(statistics, 1, skew);
            set_list_el(statistics, 2, kurt);
            set_list_el(statistics, 3, pmi);
            set_list_el(statistics, 4, bimod);
        }

        SEXP distribution = R_NilValue;
        if (rbp_results_has_ysolo_distribution(h)) {
            size_t nb = 0, nc = 0;
            check_st(rbp_results_ysolo_distribution_dims(h, &nb, &nc));
            SEXP edges = try_copy1(h, rbp_results_copy_ysolo_bin_edges, nb + 1, &nprot);
            SEXP centers = try_copy1(h, rbp_results_copy_ysolo_bin_centers, nb, &nprot);
            SEXP widths = try_copy1(h, rbp_results_copy_ysolo_bin_widths, nb, &nprot);
            SEXP counts =
                try_copy2(h, rbp_results_has_ysolo_distribution,
                          rbp_results_ysolo_distribution_dims, rbp_results_copy_ysolo_bin_counts,
                          &nprot);
            if (edges != R_NilValue && centers != R_NilValue && widths != R_NilValue &&
                counts != R_NilValue) {
                const char *hn[] = {"bin_edges", "bin_centers", "bin_widths", "count"};
                distribution = keep(mk_named_list(4, hn), &nprot);
                set_list_el(distribution, 0, edges);
                set_list_el(distribution, 1, centers);
                set_list_el(distribution, 2, widths);
                set_list_el(distribution, 3, counts);
            }
        }

        if (y_solo != R_NilValue || xi_solo != R_NilValue || statistics != R_NilValue ||
            distribution != R_NilValue) {
            const char *sdn[] = {"y_solo", "xi_solo", "statistics", "distribution"};
            SEXP solo_distribution = keep(mk_named_list(4, sdn), &nprot);
            set_list_el(solo_distribution, 0, y_solo);
            set_list_el(solo_distribution, 1, xi_solo);
            set_list_el(solo_distribution, 2, statistics);
            set_list_el(solo_distribution, 3, distribution);
            set_list_el(out, 15, solo_distribution);
        }
    }

    if (rbp_results_has_grid_insights(h)) {
        SEXP vw = try_copy1(h, rbp_results_copy_variable_weights, k, &nprot);
        SEXP mctc = try_copy1(h, rbp_results_copy_mctc, k, &nprot);
        SEXP mctp = try_copy1(h, rbp_results_copy_mctp, k, &nprot);
        SEXP cctp = try_copy1(h, rbp_results_copy_cctp, k, &nprot);
        SEXP xic = try_copy1(h, rbp_results_copy_xi_solo_composite, n, &nprot);
        if (vw != R_NilValue && mctc != R_NilValue && mctp != R_NilValue &&
            cctp != R_NilValue) {
            const char *gin[] = {
                "variable_weights",
                "marginal_contribution_to_conviction",
                "marginal_contribution_to_prediction",
                "component_contribution_to_prediction",
                "xi_solo_composite",
                "impact_on_fit",
                "impact_on_prediction"
            };
            SEXP grid_insights = keep(mk_named_list(7, gin), &nprot);
            set_list_el(grid_insights, 0, vw);
            set_list_el(grid_insights, 1, mctc);
            set_list_el(grid_insights, 2, mctp);
            set_list_el(grid_insights, 3, cctp);
            set_list_el(grid_insights, 4, xic);
            set_list_el(grid_insights, 5, mctc);
            set_list_el(grid_insights, 6, mctp);
            classgets(grid_insights, keep(mkString("GridInsights"), &nprot));
            set_list_el(out, 16, grid_insights);
        }
    }

    if (rbp_results_has_grid_cells(h)) {
        const char *gcn[] = {
            "k_cells", "combi_cells", "ysolo_cells", "relevance", "similarity"
        };
        SEXP grid_cells = keep(mk_named_list(5, gcn), &nprot);
        set_list_el(grid_cells, 0,
                    try_copy2(h, rbp_results_has_k_cells, rbp_results_k_cells_dims,
                              rbp_results_copy_k_cells, &nprot));
        set_list_el(grid_cells, 1,
                    try_copy2(h, rbp_results_has_combi_cells, rbp_results_combi_cells_dims,
                              rbp_results_copy_combi_cells, &nprot));
        set_list_el(grid_cells, 2,
                    try_copy2(h, rbp_results_has_ysolo_cells, rbp_results_ysolo_cells_dims,
                              rbp_results_copy_ysolo_cells, &nprot));
        set_list_el(grid_cells, 3, censor_cells(h, CENSOR_REL, &nprot));
        set_list_el(grid_cells, 4, censor_cells(h, CENSOR_SIM, &nprot));
        classgets(grid_cells, keep(mkString("GridCells"), &nprot));
        set_list_el(out, 17, grid_cells);
    }

    classgets(out, keep(mkString("PredictionResults"), &nprot));
    rbp_prediction_results_free(h);
    UNPROTECT(nprot);
    return out;
}

/* ------------------------------------------------------------------------ */
/* Run models                                                               */
/* ------------------------------------------------------------------------ */

static void prepare_yx_theta(SEXP y, SEXP X, SEXP theta,
                             double **y_out, double **x_out, double **theta_out,
                             size_t *n_out, size_t *k_out,
                             SEXP *y_prot, SEXP *X_prot, SEXP *theta_prot) {
    *y_prot = PROTECT(coerceVector(y, REALSXP));
    if (!isMatrix(X)) {
        error("X must be a numeric matrix (N x K)");
    }
    *X_prot = PROTECT(coerceVector(X, REALSXP));
    *theta_prot = PROTECT(coerceVector(theta, REALSXP));

    size_t n = (size_t)LENGTH(*y_prot);
    size_t n_rows = (size_t)nrows(*X_prot);
    size_t k = (size_t)ncols(*X_prot);
    size_t tk = (size_t)LENGTH(*theta_prot);

    if (n != n_rows) {
        error("y length (%zu) must equal X rows (%zu)", n, n_rows);
    }
    if (tk != k) {
        error("theta (circumstances) length (%zu) must equal X cols (%zu)", tk, k);
    }

    *y_out = REAL(*y_prot);
    *x_out = REAL(*X_prot);
    *theta_out = REAL(*theta_prot);
    *n_out = n;
    *k_out = k;
}

SEXP rbpengine_run(SEXP kind, SEXP y, SEXP X, SEXP theta, SEXP options) {
    require_engine();
    if (TYPEOF(kind) != STRSXP || LENGTH(kind) < 1) {
        error("kind must be a string: 'predict', 'maxfit', or 'grid'");
    }
    const char *knd = CHAR(STRING_ELT(kind, 0));

    double *yp, *xp, *tp;
    size_t n, k;
    SEXP y_p, X_p, th_p;
    prepare_yx_theta(y, X, theta, &yp, &xp, &tp, &n, &k, &y_p, &X_p, &th_p);

    RbpPredictionResults *results = NULL;
    RbpStatus st = RBP_OK_;

    if (strcmp(knd, "predict") == 0) {
        RbpPredictOptions *opts = make_predict_options(options);
        st = rbp_predict(yp, n, xp, n, k, LAYOUT_COL, tp, k, opts, &results);
        if (opts) {
            rbp_predict_options_free(opts);
        }
    } else if (strcmp(knd, "maxfit") == 0) {
        RbpMaxFitOptions *opts = make_maxfit_options(options);
        st = rbp_maxfit(yp, n, xp, n, k, LAYOUT_COL, tp, k, opts, &results);
        rbp_maxfit_options_free(opts);
    } else if (strcmp(knd, "grid") == 0) {
        RbpGridOptions *opts = make_grid_options(options);
        st = rbp_grid(yp, n, xp, n, k, LAYOUT_COL, tp, k, opts, &results);
        rbp_grid_options_free(opts);
    } else {
        UNPROTECT(3);
        error("unknown kind '%s' (use predict, maxfit, grid)", knd);
    }

    UNPROTECT(3);

    if (st != RBP_OK_) {
        if (results) {
            rbp_prediction_results_free(results);
        }
        raise_status(st);
    }
    if (!results) {
        error("%s succeeded but returned a null results handle", knd);
    }
    return harvest_results(results);
}

/* ------------------------------------------------------------------------ */
/* Standalone insights                                                      */
/* ------------------------------------------------------------------------ */

SEXP rbpengine_insight(SEXP which, SEXP X, SEXP theta) {
    require_engine();
    if (TYPEOF(which) != STRSXP || LENGTH(which) < 1) {
        error("which must be a string");
    }
    const char *w = CHAR(STRING_ELT(which, 0));

    if (!isMatrix(X)) {
        error("X must be a numeric matrix (N x K)");
    }
    SEXP Xp = PROTECT(coerceVector(X, REALSXP));
    size_t n = (size_t)nrows(Xp);
    size_t k = (size_t)ncols(Xp);
    double *x = REAL(Xp);

    if (strcmp(w, "info_x") == 0) {
        SEXP out = PROTECT(allocVector(REALSXP, (R_xlen_t)n));
        check_st(rbp_info_x(x, n, k, LAYOUT_COL, NULL, 0, REAL(out), n));
        UNPROTECT(2);
        return out;
    }

    SEXP thp = PROTECT(coerceVector(theta, REALSXP));
    if ((size_t)LENGTH(thp) != k) {
        error("theta length must equal X cols");
    }
    double *th = REAL(thp);

    if (strcmp(w, "relevance") == 0) {
        SEXP out = PROTECT(allocVector(REALSXP, (R_xlen_t)n));
        check_st(rbp_relevance(x, n, k, LAYOUT_COL, th, k, NULL, 0, REAL(out), n));
        UNPROTECT(3);
        return out;
    }
    if (strcmp(w, "similarity") == 0) {
        SEXP out = PROTECT(allocVector(REALSXP, (R_xlen_t)n));
        check_st(rbp_similarity(x, n, k, LAYOUT_COL, th, k, NULL, 0, REAL(out), n));
        UNPROTECT(3);
        return out;
    }
    if (strcmp(w, "info_theta") == 0) {
        SEXP out = PROTECT(allocVector(REALSXP, 1));
        check_st(rbp_info_theta(x, n, k, LAYOUT_COL, th, k, NULL, 0, REAL(out), 1));
        UNPROTECT(3);
        return out;
    }
    if (strcmp(w, "metrics") == 0) {
        SEXP rel = PROTECT(allocVector(REALSXP, (R_xlen_t)n));
        SEXP sim = PROTECT(allocVector(REALSXP, (R_xlen_t)n));
        SEXP ix = PROTECT(allocVector(REALSXP, (R_xlen_t)n));
        SEXP it = PROTECT(allocVector(REALSXP, 1));
        check_st(rbp_relevance_metrics(
            x, n, k, LAYOUT_COL, th, k, NULL, 0,
            REAL(rel), n, REAL(sim), n, REAL(ix), n, REAL(it), 1));
        const char *names[] = {"relevance", "similarity", "info_x", "info_theta"};
        SEXP out = PROTECT(mk_named_list(4, names));
        set_list_el(out, 0, rel);
        set_list_el(out, 1, sim);
        set_list_el(out, 2, ix);
        set_list_el(out, 3, it);
        classgets(out, mkString("RelevanceMetrics"));
        UNPROTECT(7); /* Xp, thp, rel, sim, ix, it, out */
        return out;
    }

    UNPROTECT(2);
    error("unknown insight '%s'", w);
    return R_NilValue;
}

/* ------------------------------------------------------------------------ */
/* Misc exported .Call                                                      */
/* ------------------------------------------------------------------------ */

SEXP rbpengine_abi_version(void) {
    require_engine();
    return ScalarInteger((int)rbp_abi_version());
}

SEXP rbpengine_last_error(void) {
    if (!rbp_engine_is_loaded()) {
        return mkString("");
    }
    const char *msg = rbp_last_error();
    return mkString(msg ? msg : "");
}

SEXP rbpengine_set_percentile(SEXP algorithm) {
    require_engine();
    if (TYPEOF(algorithm) == STRSXP && LENGTH(algorithm) >= 1) {
        check_st(rbp_set_percentile_value_algorithm_str(CHAR(STRING_ELT(algorithm, 0))));
    } else {
        algorithm = PROTECT(coerceVector(algorithm, INTSXP));
        check_st(rbp_set_percentile_value_algorithm(INTEGER(algorithm)[0]));
        UNPROTECT(1);
    }
    return R_NilValue;
}

SEXP rbpengine_get_percentile(void) {
    require_engine();
    return ScalarInteger((int)rbp_get_percentile_value_algorithm());
}

SEXP rbpengine_library_ok(void) {
    return ScalarLogical(rbp_engine_is_loaded() != 0);
}

SEXP rbpengine_engine_load(SEXP path) {
    if (TYPEOF(path) != STRSXP || LENGTH(path) < 1) {
        error("path must be a character string");
    }
    const char *p = CHAR(STRING_ELT(path, 0));
    char err[1024];
    if (rbp_engine_load(p, err, (int)sizeof(err)) != 0) {
        error("Failed to load RBP Math Engine at '%s': %s", p, err);
    }
    /* ABI probe: macros expand after engine_api.h include. */
    if (!rbp_results_has_weights || !rbp_results_has_grid_cells) {
        rbp_engine_unload();
        error("Loaded library at '%s' is missing required FFI symbols", p);
    }
    uint32_t abi = rbp_abi_version();
    if ((int)abi != (int)RBP_ABI_VERSION) {
        /* Soft warning via R — keep loaded for forward-compatible runs. */
        warning(
            "Engine ABI %u differs from client header RBP_ABI_VERSION=%d; "
            "upgrade the rbpengine package or the engine runtime.",
            (unsigned)abi, (int)RBP_ABI_VERSION);
    }
    return mkString(p);
}

SEXP rbpengine_engine_loaded(void) {
    return ScalarLogical(rbp_engine_is_loaded() != 0);
}

SEXP rbpengine_engine_path(void) {
    const char *p = rbp_engine_loaded_path();
    if (!p) {
        return ScalarString(NA_STRING);
    }
    return mkString(p);
}
