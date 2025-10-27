test_that("fit and diagnose run", {
  data(sim_data, package = "causalSenseCheck")
  fit <- csc_fit(sim_data, y = "y", d = "d", x = c("x1","x2"),
                 estimators = c("ols","did"),
                 did = list(id = "id", time = "time"))
  expect_s3_class(fit, "csc_fit")

  diag <- csc_diagnose(fit, placebo = c("random_assign"))
  expect_s3_class(diag, "csc_diag")
})


test_that("csc_fit runs IV branch when AER is available", {
  skip_if_not_installed("AER")
  data(sim_data, package = "causalSenseCheck")
   
  sim_iv <- sim_data 
  sim_iv$z_inst <- sim_iv$d + rnorm(nrow(sim_iv), sd = 0.01)
  
  fit_iv <- csc_fit(
    data = sim_iv,
    y = "y",
    d = "d",
    x = c("x1","x2"),
    estimators = c("iv"),
    iv = list(z = "z_inst")
  )
  
  expect_s3_class(fit_iv, "csc_fit")
  expect_true("iv" %in% names(fit_iv$models))
  expect_true(all(c("term","estimate","conf.low","conf.high") %in% names(fit_iv$models$iv$est)))
})

test_that("csc_fit runs grf branch when grf is available", {
  skip_if_not_installed("grf")
  data(sim_data, package = "causalSenseCheck")
  
  fit_grf <- csc_fit(
    data = sim_data,
    y = "y",
    d = "d",
    x = c("x1","x2"),
    estimators = c("grf"),
    grf = list(num.trees = 50, seed = 1) # drop trees for speed
  )
  
  expect_s3_class(fit_grf, "csc_fit")
  expect_true("grf" %in% names(fit_grf$models))
  grf_est <- fit_grf$models$grf$est
  expect_true(all(c("term","estimate","conf.low","conf.high") %in% names(grf_est)))
})