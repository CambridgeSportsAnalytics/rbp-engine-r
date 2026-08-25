# rbpengine (R)

Open R thin client for the [RBP Math Engine](https://www.csanalytics.io) C ABI.
This repository is a **public mirror** of `r/` in the private math library.
It builds and installs **without** the engine. Predictions need the separately
installed runtime and a CSA license.

**Python users:** this is not the Python package. Use
`pip install rbp-engine` ([PyPI](https://pypi.org/project/rbp-engine/)).

The R **package** name is `rbpengine` (CRAN forbids hyphens). This GitHub repo
is `rbp-engine-r` so it sits next to `rbp-engine` / `rbp-math-c-abi` and is not
mistaken for the engine itself.

## Install

Docs: [Install / R](https://docs.csanalytics.io/Install/R)

```r
install.packages(c("jsonlite", "remotes"))
remotes::install_github("CambridgeSportsAnalytics/rbp-engine-r")

library(rbpengine)
install_engine()       # fetches https://docs.csanalytics.io/releases/latest/
engine_available()     # TRUE
```

When the package is on R-universe or CRAN, that first line becomes
`install.packages("rbpengine")`.

The MIT license on this tree covers **this R client only**. The native library
(`librbp_math_lib`), companions, and `rbp-license-info` are **not** in this
repository and remain under CSA’s Library License.

C ABI contract (public header): [rbp-math-c-abi](https://github.com/CambridgeSportsAnalytics/rbp-math-c-abi).
Provenance for this snapshot: [SOURCE.md](SOURCE.md).
