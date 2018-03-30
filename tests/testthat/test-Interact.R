context("Interactive Fixed Effects")

test_that("run the Interactive Fixed Effect example", {
  skip_on_cran()

  # JULIA_HOME = "/Applications/Julia-0.6.app/Contents/Resources/julia/bin"
  # FixedEffect_setup(JULIA_HOME)  # my computer cannot find julia
  FixedEffect_setup()

  # ################################
  # Test of standard base function
  r <- FixedEffectInteract(df, "sales", "price", "state+year", 2, "state", vcov = "robust")

  expect_is(r, "JuliaObject")

})
