Sys.setenv("R_TESTS" = "")
#JuliaCall::julia_setup()

library(testthat)
library(FixedEffectjlr)

df = fread("./Cigar.csv")

test_check("FixedEffectjlr")
