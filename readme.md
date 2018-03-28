
# R interface for Fixed Effect Models

This package uses the [`FixedEffectModels`](https://github.com/matthieugomez/FixedEffectModels.jl) julia package to estimate large fixed effects models in R.

It is a substitute for the [`felm`](https://cran.r-project.org/web/packages/lfe/index.html) R package that happens to not converge for simple datasets.
This uses the [`JuliaCall`](https://github.com/Non-Contradiction/JuliaCall) library to pass datasets into julia and process them.

The package is very preliminary!

Thanks to [Matthieu](https://github.com/matthieugomez) for writing `FixedEffectModels`!

## Usage

Install the package directly from github
```r
devtools::install_github("eloualiche/FixedEffectjlr")
```

First setup julia to work within R with `JuliaCall`

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
  
  
## Benchmark

I reproduced Matthieu's benchmark [here](./benchmark/benchmark.md)


## To do

- [ ] failsafe linear reg if no fe specified
- [\] `FixedEffectIV` function
- [ ] class lm output
- [ ] Interactive fixed effect models from [`InteractiveFixedEffectModels.jl`](https://github.com/matthieugomez/InteractiveFixedEffectModels.jl)
- [ ] implement with felm like formula
- [x] standard errors in line with stata
- [x] getfe option


## Prerequisite:

The package requires a working installation of julia (tested with 0.6.1)

