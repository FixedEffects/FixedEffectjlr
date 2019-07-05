First load the package

``` r
library(dplyr);
library(data.table)
library(FixedEffectjlr)
JULIA_HOME <- "/Applications/Julia-1.1.app/Contents/Resources/julia/bin/"
FixedEffect_setup(JULIA_HOME)
```

How to recover principal component analysis from Interacted Fixed Effects
-------------------------------------------------------------------------

We are going to use the standard dataset:

``` r
dt <- Ecdat::Cigar %>% data.table
```

### Interactive fixed effects as PC

IFE is similar to a principal component analysis whenever there is no
right hand side parameter. The standard specification is:

``` r
ife <- 
  FixedEffectInteract(dt, lhs = "sales", rhs = "0", 
                      ife = "state+year", rank=1, 
                      fe = "0")
```

To compare with the principal component procedure, we look at loadings
and factors first and then the residuals.

``` r
ife_loadings <- unique(ife$dt_augment[, .(state, ife_loadings1=loadings1)])
ife_factors  <- unique(ife$dt_augment[, .(year, ife_factors1=factors1)])
```

### Comparing with Principal Component Analysis

Before we can use `prcomp` we need to transform the data from long to
wide:

``` r
dt_wide <- dt[, .(state, year, sales) ] %>%
    dcast(year ~ state, value.var = "sales")
```

Now we are able to directly run the PC analysis. Note that IFE does not
rescale the variable, so we are going to run it with both centering and
rescaling options turned off:

``` r
pca_res <- prcomp(dt_wide[, -"year"], center=FALSE, scale=FALSE)
# keep loadings and factors separately
pca_loadings <- cbind(unique(dt[, .(state)]), 
                      data.table(pca_res$rotation)[, .(pc_loadings1=PC1, pc_loadings2=PC2) ] )
pca_factors  <- cbind(unique(dt[, .(year)]),  
                      data.table(pca_res$x)[, .(pc_factors1 = PC1, pc_factors2 = PC2) ] )
# remerge to estimate residuals
dt_pca <- dt[, .(state, year, sales) ] %>%
  merge(., pca_loadings, by = c("state") , all.x = T) %>%
  merge(., pca_factors, by = c("year"), all.x = T)
```

Finally we compare loadings, factors and *R*<sup>2</sup>:

``` r
# loadings
dt_loadings <- merge(pca_loadings, ife_loadings, by = c("state"))
# are they proportional to each other (difference in scale)
dt_loadings[, .(state, prop_loadings1 = ife_loadings1/pc_loadings1)] %>% as_tibble
```

    ## # A tibble: 46 x 2
    ##    state prop_loadings1
    ##    <int>          <dbl>
    ##  1     1          -4721
    ##  2     3          -4721
    ##  3     4          -4721
    ##  4     5          -4721
    ##  5     7          -4721
    ##  6     8          -4721
    ##  7     9          -4721
    ##  8    10          -4721
    ##  9    11          -4721
    ## 10    13          -4721
    ## # ... with 36 more rows

We do the same thing for factors

``` r
# factors
dt_factors <- merge(pca_factors, ife_factors, by = c("year"))
# are they proportional to each other (difference in scale)
dt_factors[, .(year, prop_factors1 = ife_factors1/pc_factors1)] %>% as_tibble
```

    ## # A tibble: 30 x 2
    ##     year prop_factors1
    ##    <int>         <dbl>
    ##  1    63     -0.000212
    ##  2    64     -0.000212
    ##  3    65     -0.000212
    ##  4    66     -0.000212
    ##  5    67     -0.000212
    ##  6    68     -0.000212
    ##  7    69     -0.000212
    ##  8    70     -0.000212
    ##  9    71     -0.000212
    ## 10    72     -0.000212
    ## # ... with 20 more rows

Last note that the proportionality factors are actually the inverse of
each other:

``` r
dt_loadings[, .(mean(ife_loadings1/pc_loadings1)) ] * 
  dt_factors[, .(mean(ife_factors1/pc_factors1))]
```

    ##    V1
    ## 1:  1

And finally we look to see if residuals are the same:

``` r
# residuals for only one factor
dt_residuals <- 
  merge(ife$dt_augment[, .(state, year, ife_residuals=residuals)],
        dt_pca[, .(state, year, pc_residuals=sales-pc_loadings1*pc_factors1)],
        by = c("year", "state"))
dt_residuals[, .(ife_residuals = sum(ife_residuals^2), 
                 pc_residuals = sum(pc_residuals^2)) ]
```

    ##    ife_residuals pc_residuals
    ## 1:      242566.2     242566.2

How to do we get the Principal Component Analysis when the factors are demeaned
-------------------------------------------------------------------------------

We start with the PC analysis. Except that this time we allow for a
centering:

``` r
pca_res <- prcomp(dt_wide[, -"year"], center=TRUE, scale=FALSE)
# keep loadings and factors separately
pca_loadings <- cbind(unique(dt[, .(state)]), 
                      data.table(pca_res$rotation)[, .(pc_loadings1=PC1, pc_loadings2=PC2) ] )
pca_factors  <- cbind(unique(dt[, .(year)]),  
                      data.table(pca_res$x)[, .(pc_factors1 = PC1, pc_factors2 = PC2) ] )
# remerge to estimate residuals
dt_pca <- dt[, .(state, year, sales) ] %>%
  merge(., pca_loadings, by = c("state") , all.x = T) %>%
  merge(., pca_factors, by = c("year"), all.x = T)
```

This is similar to removing the within group average, which corresponds
to group fixed effects. We obtain the centering in IFE through:

``` r
ife <- 
  FixedEffectInteract(dt, lhs = "sales", rhs = "0", 
                      ife = "state+year", rank=1, 
                      fe = "state")
ife_loadings <- unique(ife$dt_augment[, .(state, ife_loadings1=loadings1)])
ife_factors  <- unique(ife$dt_augment[, .(year, ife_factors1=factors1)])
```

As in the previous example, we merge loadings, factors and compare
*R*<sup>2</sup>:

``` r
# loadings
dt_loadings <- merge(pca_loadings, ife_loadings, by = c("state"))
# are they proportional to each other (difference in scale)
dt_loadings[, .(state, prop_loadings1 = ife_loadings1/pc_loadings1)] %>% as_tibble
```

    ## # A tibble: 46 x 2
    ##    state prop_loadings1
    ##    <int>          <dbl>
    ##  1     1            488
    ##  2     3            488
    ##  3     4            488
    ##  4     5            488
    ##  5     7            488
    ##  6     8            488
    ##  7     9            488
    ##  8    10            488
    ##  9    11            488
    ## 10    13            488
    ## # ... with 36 more rows

``` r
# factors
dt_factors <- merge(pca_factors, ife_factors, by = c("year"))
# are they proportional to each other (difference in scale)
dt_factors[, .(year, prop_factors1 = ife_factors1/pc_factors1)] %>% as_tibble
```

    ## # A tibble: 30 x 2
    ##     year prop_factors1
    ##    <int>         <dbl>
    ##  1    63       0.00205
    ##  2    64       0.00205
    ##  3    65       0.00205
    ##  4    66       0.00205
    ##  5    67       0.00205
    ##  6    68       0.00205
    ##  7    69       0.00205
    ##  8    70       0.00205
    ##  9    71       0.00205
    ## 10    72       0.00205
    ## # ... with 20 more rows

We confirm that the proportionality factors are actually the inverse of
each other:

``` r
dt_loadings[, .(mean(ife_loadings1/pc_loadings1)) ] * 
  dt_factors[, .(mean(ife_factors1/pc_factors1))]
```

    ##          V1
    ## 1: 1.000045

And finally we look to see if residuals are the same:

``` r
# residuals for only one factor
dt_residuals <- 
  merge(ife$dt_augment[, .(state, year, ife_residuals=residuals)],
        dt_pca[, .(year, pc_residuals=sales-mean(sales)-pc_loadings1*pc_factors1),
               by = .(state) ],
        by = c("year", "state"))
dt_residuals[, .(ife_residuals = sum(ife_residuals^2), 
                 pc_residuals = sum(pc_residuals^2)) ]
```

    ##    ife_residuals pc_residuals
    ## 1:      174608.2     174608.2
