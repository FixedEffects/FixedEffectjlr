
# R interface for Fixed Effect Models

[![Build Status](https://travis-ci.org/eloualiche/FixedEffectjlr.svg?branch=master)](https://travis-ci.org/eloualiche/FixedEffectjlr)
[![codecov](https://codecov.io/gh/eloualiche/FixedEffectjlr/branch/master/graph/badge.svg)](https://codecov.io/gh/eloualiche/FixedEffectjlr)



This package uses the [`FixedEffectModels.jl`](https://github.com/matthieugomez/FixedEffectModels.jl) julia package and the [JuliaCall](https://github.com/Non-Contradiction/JuliaCall) R library to estimate large fixed effects models in R.

It is a substitute to the [`felm`](https://cran.r-project.org/web/packages/lfe/index.html) R package. It is usually faster (see [benchmarks](./vignettes/benchmark.md). I find it also to be more robust to actually converge. I use the [`JuliaCall`](https://github.com/Non-Contradiction/JuliaCall) library to pass datasets into julia and process them using the `FixedEffectModels.jl` and  `InteractiveFixedEffectModels.jl` libraries.
The package is very preliminary, so please use it and let me know if you run into issues!

[Thanks](#thanks) to Matthieu and Changcheng.


## Usage

Install the package directly from github
```r
devtools::install_github("eloualiche/FixedEffectjlr")
```

To actually use the package in `R`, first setup julia to work within R with `JuliaCall` (see more details about setting up on the [package webpage](https://github.com/Non-Contradiction/JuliaCall))
```r
library(FixedEffectjlr)
JULIA_HOME <- "/Applications/Julia-0.6.app/Contents/Resources/julia/bin/"
FixedEffect_setup(JULIA_HOME)
```

To run a regression with fixed effects
```r
df <- Ecdat::Cigar
reg_res <- FixedEffect(df,
  lhs = "sales", rhs = "ndi",
  fe      = "state + year",
  weights = "pop",
  vcov    = "cluster(state)")
  
#                          Fixed Effect Model
# =====================================================================
# Number of obs:               1380   Degrees of freedom:            31
# R2:                         0.804   R2 within:                  0.139
# F-Statistic:              13.3481   p-value:                    0.000
# Iterations:                     6   Converged:                   true
# =====================================================================
#         Estimate  Std.Error  t value Pr(>|t|)   Lower 95%   Upper 95%
# ---------------------------------------------------------------------
# ndi  -0.00526264 0.00144043 -3.65351    0.000 -0.00808837 -0.00243691
# =====================================================================  
```
  
+ For now the functions takes as inputs different strings for independent variable, regressors and fixed effects instead of a formula as in `felm`.
+ `FixedEffectModels.jl` accepts arbitrary number of fixed effects as well as interaction of categorical variables:

```r
fe = "year + firm"
fe = "year:industry"
```

+ Standard errors are either adjusted for heteroscedasticity, `robust` or clustered by categories:
```r
vcov = "robust"
vcov = "cluster(industry + year)"
```

+ It is also possible to add weights:
```r
weights = "pop"
```


## Output

The functions prints out a table that is generated directly from julia. Moreover it collects information about the regressions and output two lists:
  
  * `reg_res$results` should eventually become a regression class like `lm` or `felm` and keeps in memory most of the julia output
  * `reg_res$summary` is most useful to create regression tables and keep regressors, standard errors, etc...
  * `reg_res$summary$coeftest` for a coeftest object, to be used directly in stargazer  
  
  
## Vignettes

  * For standard estimation:    [FixedEffectModels.Rmd](./vignettes/FixedEffectModels.Rmd)
  * For IV and Interactive FE:  [OtherFEModels.Rmd](./vignettes/OtherFEModels.Rmd)
  * A practical example of IFE: [IFE_PCA_ex.Rmd](./vignettes/IFE_PCA_ex.md)

## Prerequisite:

The package requires a working installation of julia (last tested with julia 0.6.4)


## <a name = "thanks">Thanks</a>

Thanks to Changcheng for developing [JuliaCall](https://github.com/Non-Contradiction/JuliaCall).

Thanks to [Matthieu](https://github.com/matthieugomez) for all his packages!

See other packages built on JuliaCall: [`convexjlr`](https://github.com/Non-Contradiction/convexjlr) and [`ipoptjlr`](https://github.com/Non-Contradiction/ipoptjlr).


