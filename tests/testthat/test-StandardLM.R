context("Standard Linear Model")

test_that("run the Linear Model example", {
  skip_on_cran()

  #JULIA_HOME = "/Applications/Julia-0.6.app/Contents/Resources/julia/bin"
  FixedEffect_setup()

  r <- FixedEffect_nse(df, sales~ndi, state+year, pop, cluster(state))

  expect_equal(names(r),
               c("results", "summary"))
  expect_equal(as.vector(r$summary$mat),
               c(-0.005262641, 0.001440434, -3.653511, 0.0002685935, -0.008088374, -0.002436907),
               tolerance = 0.001)

})
