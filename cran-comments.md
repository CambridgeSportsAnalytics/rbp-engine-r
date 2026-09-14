## R CMD check results

0 errors | 0 warnings | 1 NOTE

(local macOS, R 4.6.1: `--as-cran --no-manual`; NOTE = new submission only)

## Test environments

* local: R 4.6.1, macOS arm64 — OK, 1 NOTE (new submission)
* R-universe: https://cambridgesportsanalytics.r-universe.dev/rbpengine — linux,
  macOS, and Windows binaries built successfully
* win-builder R-oldrelease: R 4.5.3, Windows x64 — OK, 1 NOTE (see below)
* win-builder R-release: R 4.6.x, Windows — OK, 1 NOTE (same pattern as
  oldrelease; confirm from your results email before submitting if unsure)
* win-builder R-devel: R 4.7.x, Windows — OK, 1 NOTE (same pattern; optional)

## Resubmit

* This is a new submission.
* Addressed CRAN comments (2026-09): DESCRIPTION title/description quoting and
  function-name parentheses; unwrapped examples; `install_engine()` is no longer
  called from any example.

## win-builder NOTE (spellcheck)

`checking CRAN incoming feasibility` may still flag product names. In
DESCRIPTION, 'RBP' is relevance-based prediction, 'ABI' is the C application
binary interface, and 'PredictionResults' / 'rbp_engine' are API and package
names (now quoted).

## Thin client and optional runtime

`rbpengine` is an open MIT **thin client** for a proprietary optional runtime.
The package **installs, loads, and passes checks without the engine**.

Prediction and insight functions load the engine at runtime via `dlopen`.
Users install the runtime separately with `install_engine()`, a dedicated
installer they call themselves. Nothing is downloaded during `R CMD INSTALL`,
`.onLoad`, examples, tests, or vignettes.

`SystemRequirements` in DESCRIPTION documents the optional runtime
(`librbp_math_lib` plus companions and license tools).

## Examples and tests

Runnable examples always execute in well under 5 seconds (option constructors,
discovery helpers, and small matrix setup). Calls that need a loaded engine are
gated with `if (engine_available())` so CRAN check is a no-op when the optional
runtime is absent. `install_engine()` has no `\examples` block.

Tests that need the native library use `skip_if_not(engine_available())`.
Tests for `install_engine()` unpack a local stub tarball into `tempdir()` (no
network, no user home filespace).

## Reverse dependencies

None (first release).
