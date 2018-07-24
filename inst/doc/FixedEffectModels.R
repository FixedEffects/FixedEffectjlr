## ---- results='hide', warning = F, error = F, message = F, eval= F-------
#  library(FixedEffectjlr)
#  JULIA_HOME <- "/Applications/Julia-0.6.app/Contents/Resources/julia/bin/"
#  FixedEffect_setup(JULIA_HOME)

## ---- warning = F, error = F, message = F, eval= F-----------------------
#  df <- Ecdat::Cigar
#  reg_res <- FixedEffect(df,
#    lhs = "sales", rhs = "ndi",
#    fe      = "state + year",
#    weights = "pop",
#    vcov    = "cluster(state)")

## ---- warning = F, error = F, message = F, eval= F-----------------------
#  reg_res <- FixedEffect_nse(df, sales~ndi, state+year, pop, cluster(state))

## ---- warning = F, error = F, message = F, eval= F-----------------------
#  df <- Ecdat::Cigar
#  reg1 <- FixedEffect_nse(df, sales~ndi, state+year, pop, robust)
#  reg2 <- FixedEffect_nse(df, sales~ndi, state+year, pop, cluster(state))
#  reg3 <- FixedEffect_nse(df, sales~ndi, state+year, vcov = robust)
#  reg4 <- FixedEffect_nse(df, sales~ndi, state+year, vcov = cluster(state))
#  stargazer::stargazer(reg1$summary$coeftest, reg2$summary$coeftest,
#                       reg3$summary$coeftest, reg4$summary$coeftest,
#                       type = "text")

## ---- warning = F, error = F, message = T, eval= F-----------------------
#  df <- Ecdat::Cigar
#  reg_res <- FixedEffect_models(
#    df,
#    lhs = "sales", rhs = "ndi",
#    fe      = "state + year",
#    weights = c("", "pop"),
#    vcov    = c("robust", "cluster(state)"))

