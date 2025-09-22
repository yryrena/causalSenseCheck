test_that("placebo random_assign returns expected columns", {
  data(sim_data, package = "causalSenseCheck")
  res <- csc_placebo(sim_data, y = "y", d = "d",
                     x = c("x1","x2"),
                     type = "random_assign")
  expect_true(all(c("term","estimate","conf.low","conf.high","placebo") %in% names(res)))
})

test_that("fallback sensitivity grid returns tibble-like data", {
  data(sim_data, package = "causalSenseCheck")
  fit <- csc_fit(sim_data, y = "y", d = "d", x = c("x1","x2"), estimators = "ols")
  mod <- fit$models$ols$fit
  grid <- csc_sensitivity_grid(mod, "d", r2yu = c(0, .1), r2du = c(0, .1))
  expect_true(all(c("r2_yu","r2_du","bias","co_adj","z_adj","sig") %in% names(grid)))
  expect_gte(nrow(grid), 4)
})
