context("Fixed Effect Regression with Instrumental Variable")

test_that("run the IV example", {
  skip_on_cran()

  # JULIA_HOME = "/Applications/Julia-0.6.app/Contents/Resources/julia/bin"
  # FixedEffect_setup(JULIA_HOME)  # my computer cannot find julia
  FixedEffect_setup()

  r <- FixedEffectIV_nse(df, sales ~ ndi, price ~ pimin,
                         state+year, weights = NULL, vcov = robust)
  expect_is(r, "JuliaObject")

})
