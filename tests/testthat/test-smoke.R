test_that("fit and diagnose run", {
  data(sim_data, package = "causalSenseCheck")
  fit <- csc_fit(sim_data, y = "y", d = "d", x = c("x1","x2"),
                 estimators = c("ols","did"),
                 did = list(id = "id", time = "time"))
  expect_s3_class(fit, "csc_fit")

  diag <- csc_diagnose(fit, placebo = c("random_assign"))
  expect_s3_class(diag, "csc_diag")
})
