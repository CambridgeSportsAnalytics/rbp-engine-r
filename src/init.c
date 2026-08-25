#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <stdlib.h>

/* Defined in rbpengine.c / engine_load.c */
extern SEXP rbpengine_abi_version(void);
extern SEXP rbpengine_last_error(void);
extern SEXP rbpengine_set_percentile(SEXP algorithm);
extern SEXP rbpengine_get_percentile(void);
extern SEXP rbpengine_run(SEXP kind, SEXP y, SEXP X, SEXP theta, SEXP options);
extern SEXP rbpengine_insight(SEXP which, SEXP X, SEXP theta);
extern SEXP rbpengine_library_ok(void);
extern SEXP rbpengine_engine_load(SEXP path);
extern SEXP rbpengine_engine_loaded(void);
extern SEXP rbpengine_engine_path(void);

static const R_CallMethodDef CallEntries[] = {
    {"rbpengine_abi_version", (DL_FUNC)&rbpengine_abi_version, 0},
    {"rbpengine_last_error", (DL_FUNC)&rbpengine_last_error, 0},
    {"rbpengine_set_percentile", (DL_FUNC)&rbpengine_set_percentile, 1},
    {"rbpengine_get_percentile", (DL_FUNC)&rbpengine_get_percentile, 0},
    {"rbpengine_run", (DL_FUNC)&rbpengine_run, 5},
    {"rbpengine_insight", (DL_FUNC)&rbpengine_insight, 3},
    {"rbpengine_library_ok", (DL_FUNC)&rbpengine_library_ok, 0},
    {"rbpengine_engine_load", (DL_FUNC)&rbpengine_engine_load, 1},
    {"rbpengine_engine_loaded", (DL_FUNC)&rbpengine_engine_loaded, 0},
    {"rbpengine_engine_path", (DL_FUNC)&rbpengine_engine_path, 0},
    {NULL, NULL, 0}
};

void R_init_rbpengine(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
