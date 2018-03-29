context("Standard Linear Model")

df = Ecdat::Cigar

test_that("run the Linear Model example", {
  skip_on_cran()

  FixedEffect_setup()

  r <- FixedEffect_nse(df, sales~ndi, state+year, pop, cluster(state))

  expect_named(r, c("results", "summary"), ignore.order = TRUE)
  expect_equal(r$summary$mat,
               c(-0.005262641, 0.001440434, -3.653511, 0.0002685935, -0.008088374, -0.002436907),
               tolerance = 0.001)

  expect_equal(r$x, c(1.000, 4.743, 3.821, 1.379), tolerance = 0.001)

})

