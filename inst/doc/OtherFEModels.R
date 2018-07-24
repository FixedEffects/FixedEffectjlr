## ---- results='hide', warning = F, error = F, message = F, eval= F-------
#  library(FixedEffectjlr)
#  JULIA_HOME <- "/Applications/Julia-0.6.app/Contents/Resources/julia/bin/"
#  FixedEffect_setup(JULIA_HOME)

## ---- warning = F, error = F, message = F, eval= F-----------------------
#  df <- Ecdat::Cigar
#  FixedEffectIV_nse(df, sales ~ ndi, price ~ pimin, state+year, weights = NULL, vcov = robust)

## ---- warning = F, error = F, message = F, eval= F-----------------------
#  df <- Ecdat::Cigar
#  FixedEffectInteract(df, "sales", "price", "state+year", 2, "state", vcov = "robust")

## ---- warning = F, error = F, message = F, eval= F-----------------------
#  R> ife <- FixedEffectInteract(df, "sales", "price", "state+year", 2, "state", vcov = "robust")
#  R> str(ife$statistics)
#  R> stargazer::stargazer(ife$summary)
#  R> ife$dt_augment
#        loadings1   factors1 loadings2   factors2   residuals    state
#     1:  123.4569 -0.2474458  17.67403 0.27993746  -5.2228250 136.8897
#     2:  123.4569 -0.2646732  17.67403 0.22784788  -0.1649076 136.8897
#     3:  123.4569 -0.2576672  17.67403 0.21923759   2.2223407 136.8897
#     4:  123.4569 -0.3021446  17.67403 0.07257907   8.9285553 136.8897
#     5:  123.4569 -0.2563533  17.67403 0.01662833   3.4067214 136.8897
#    ---
#  1376:   55.5659  0.1690440 -43.31349 0.16784489  -2.6102131 162.8116
#  1377:   55.5659  0.1700898 -43.31349 0.18365775  -2.4587907 162.8116
#  1378:   55.5659  0.2031630 -43.31349 0.19880231 -13.5040138 162.8116
#  1379:   55.5659  0.2218755 -43.31349 0.21521906  -2.6961542 162.8116
#  1380:   55.5659  0.2861259 -43.31349 0.25092751   8.9333349 162.8116

