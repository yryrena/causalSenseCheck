test_that("autoplot works and returns ggplot", {
  skip_if_not_installed("ggplot2")
  data(sim_data, package = "causalSenseCheck")

  fit <- csc_fit(sim_data, y = "y", d = "d",
                 x = c("x1","x2"),
                 estimators = c("ols", "dr", "did"),
                 did = list(id = "id", time = "time"))

  diag <- csc_diagnose(fit, placebo = c("random_assign", "lead_treatment"))

  p1 <- autoplot(diag, "compare")
  p2 <- autoplot(diag, "placebo")
  expect_true(inherits(p1, "ggplot"))
  expect_true(inherits(p2, "ggplot"))
})

test_that("csc_report creates an html file if rmarkdown is available", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("ggplot2")
  data(sim_data, package = "causalSenseCheck")

  fit <- csc_fit(sim_data, y = "y", d = "d",
                 x = c("x1","x2"),
                 estimators = c("ols"),
                 did = list(id = "id", time = "time"))

  diag <- csc_diagnose(fit, placebo = "random_assign")

  out <- tempfile(fileext = ".html")
  csc_report(diag, out = out)
  expect_true(file.exists(out))
})
