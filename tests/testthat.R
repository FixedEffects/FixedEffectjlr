Sys.setenv("R_TESTS" = "")
JuliaCall::julia_setup()

library(testthat)
library(FixedEffectjlr)

test_check("FixedEffectjlr")
