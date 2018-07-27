## ---- eval=TRUE, message=F, warnings=F, highlight=TRUE, cache=F, results='hide'----
library(magrittr); library(dplyr);
library(data.table)
library(FixedEffectjlr)
JULIA_HOME <- "/Applications/Julia-0.6.app/Contents/Resources/julia/bin/"
FixedEffect_setup(JULIA_HOME)

## ---- eval=T, message = FALSE, warnings = FALSE, highlight=TRUE----------
dt <- Ecdat::Cigar %>% data.table

## ---- eval=TRUE, message=F, warnings=F, highlight=TRUE-------------------
ife <- 
  FixedEffectInteract(dt, lhs = "sales", rhs = "0", 
                      ife = "state+year", rank=1, 
                      fe = "0")

## ---- eval=TRUE, message=F, warnings=F, highlight=TRUE-------------------
ife_loadings <- unique(ife$dt_augment[, .(state, ife_loadings1=loadings1)])
ife_factors  <- unique(ife$dt_augment[, .(year, ife_factors1=factors1)])

## ---- eval=TRUE, message=F, warnings=F, highlight=TRUE-------------------
dt_wide <- dt[, .(state, year, sales) ] %>%
    dcast(year ~ state, value.var = "sales")

## ---- eval=TRUE, message=F, warnings=F, highlight=TRUE-------------------
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

## ---- eval=TRUE, message=F, warnings=F, highlight=TRUE-------------------
# loadings
dt_loadings <- merge(pca_loadings, ife_loadings, by = c("state"))
# are they proportional to each other (difference in scale)
dt_loadings[, .(state, prop_loadings1 = ife_loadings1/pc_loadings1)] %>% as_tibble

## ---- eval=TRUE, message=F, warnings=F, highlight=TRUE-------------------
# factors
dt_factors <- merge(pca_factors, ife_factors, by = c("year"))
# are they proportional to each other (difference in scale)
dt_factors[, .(year, prop_factors1 = ife_factors1/pc_factors1)] %>% as_tibble

## ---- eval=TRUE, message=F, warnings=F, highlight=TRUE-------------------
dt_loadings[, .(mean(ife_loadings1/pc_loadings1)) ] * 
  dt_factors[, .(mean(ife_factors1/pc_factors1))]

## ---- eval=TRUE, message=F, warnings=F, highlight=TRUE-------------------
# residuals for only one factor
dt_residuals <- 
  merge(ife$dt_augment[, .(state, year, ife_residuals=residuals)],
        dt_pca[, .(state, year, pc_residuals=sales-pc_loadings1*pc_factors1)],
        by = c("year", "state"))
dt_residuals[, .(ife_residuals = sum(ife_residuals^2), 
                 pc_residuals = sum(pc_residuals^2)) ]

## ---- eval=TRUE, message=F, warnings=F, highlight=TRUE-------------------
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

## ---- eval=TRUE, message=F, warnings=F, highlight=TRUE-------------------
ife <- 
  FixedEffectInteract(dt, lhs = "sales", rhs = "0", 
                      ife = "state+year", rank=1, 
                      fe = "state")
ife_loadings <- unique(ife$dt_augment[, .(state, ife_loadings1=loadings1)])
ife_factors  <- unique(ife$dt_augment[, .(year, ife_factors1=factors1)])

## ---- eval=TRUE, message=F, warnings=F, highlight=TRUE-------------------
# loadings
dt_loadings <- merge(pca_loadings, ife_loadings, by = c("state"))
# are they proportional to each other (difference in scale)
dt_loadings[, .(state, prop_loadings1 = ife_loadings1/pc_loadings1)] %>% as_tibble
# factors
dt_factors <- merge(pca_factors, ife_factors, by = c("year"))
# are they proportional to each other (difference in scale)
dt_factors[, .(year, prop_factors1 = ife_factors1/pc_factors1)] %>% as_tibble

## ---- eval=TRUE, message=F, warnings=F, highlight=TRUE-------------------
dt_loadings[, .(mean(ife_loadings1/pc_loadings1)) ] * 
  dt_factors[, .(mean(ife_factors1/pc_factors1))]

## ---- eval=TRUE, message=F, warnings=F, highlight=TRUE-------------------
# residuals for only one factor
dt_residuals <- 
  merge(ife$dt_augment[, .(state, year, ife_residuals=residuals)],
        dt_pca[, .(year, pc_residuals=sales-mean(sales)-pc_loadings1*pc_factors1),
               by = .(state) ],
        by = c("year", "state"))
dt_residuals[, .(ife_residuals = sum(ife_residuals^2), 
                 pc_residuals = sum(pc_residuals^2)) ]

