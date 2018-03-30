Sys.setenv("R_TESTS" = "")
#JuliaCall::julia_setup()

library(testthat)
library(FixedEffectjlr)

library(data.table)
df = fread("./Cigar.csv")

test_check("FixedEffectjlr")
