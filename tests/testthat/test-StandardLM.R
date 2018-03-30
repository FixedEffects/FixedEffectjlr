context("Standard Linear Model")

test_that("run the Linear Model example", {
  skip_on_cran()

  # JULIA_HOME = "/Applications/Julia-0.6.app/Contents/Resources/julia/bin"
  # FixedEffect_setup(JULIA_HOME)  # my computer cannot find julia
  FixedEffect_setup()

  # ################################
  # Test of standard base function
  r <- FixedEffect_nse(df, sales~ndi, state+year, pop, cluster(state))

  # 1. Test of structure of result stuff
  expect_equal(names(r),
               c("results", "summary"))
  # 2. Test of summary output
  expect_equal(as.vector(r$summary$mat),
               c(-0.005262641, 0.001440434, -3.653511, 0.0002685935, -0.008088374, -0.002436907),
               tolerance = 0.001)
  # 3. Test of coefficient output
  expect_is(r$summary$coeftest, "coeftest")
  tbl <- stargazer(r$summary$coeftest, type="text")
  expect_output(str(tbl), "chr [1:12]", fixed = TRUE)

  # ############################
  # Test the version without nse
  r <- FixedEffect(df, lhs = "sales", rhs = "ndi", fe = "state + year",
                   weights = "pop", vcov = "cluster(state)")
  expect_equal(as.vector(r$summary$mat),
               c(-0.005262641, 0.001440434, -3.653511, 0.0002685935, -0.008088374, -0.002436907),
               tolerance = 0.001)

  # ################################
  # Test the multiple models version
  r <- FixedEffect_models(df, "sales", "ndi",
                          fe = c("state", "year", "state + year"),
                          vcov = c("robust", "cluster(state)") )

  # 1. Check that this actually runs 6 regressions (3x2)
  expect_equal(length(r$coef), 6)
  # 2. Check that the output is a JuliaObject
  expect_is(r$julia_reg[[1]], "JuliaObject")
  # 3. Check the coefficients
  expect_equal(r$coef %>% purrr::map(.x = ., function(x) x$coefficients) %>% unlist %>% as.vector,
               c(-0.001704679,  0.002657041, -0.006843845, -0.001704679,  0.002657041, -0.006843845),
               tolerance = 0.001)
  # 4. Test other stuff like output in stargazer of coeftest
  expect_is(r$coef[[1]]$coeftest, "coeftest")
  tbl <- r$coef %>% purrr::map(.x = ., function(x) x$coeftest) %>% stargazer(. , type="text")
  expect_output(str(tbl), "chr [1:13]", fixed = TRUE)

})
