test_that("autoplot works and returns ggplot", {
  skip_if_not_installed("ggplot2")
  data(sim_data, package = "causalSenseCheck")
  
  fit <- csc_fit(
    data = sim_data,
    y = "y", d = "d",
    x = c("x1","x2"),
    estimators = c("ols", "dr", "did"),
    did = list(id = "id", time = "time")
  )
  
  diag <- csc_diagnose(
    fit,
    placebo = c("random_assign", "lead_treatment")
  )
  
  p1 <- autoplot(diag, "compare")
  p2 <- autoplot(diag, "placebo")
  
  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
})

test_that("csc_report creates an html file if rmarkdown is available", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("ggplot2")
  data(sim_data, package = "causalSenseCheck")
  
  fit <- csc_fit(
    data = sim_data,
    y = "y", d = "d",
    x = c("x1","x2"),
    estimators = c("ols"),
    did = list(id = "id", time = "time")
  )
  
  diag <- csc_diagnose(fit, placebo = "random_assign")
  
  out <- tempfile(fileext = ".html")
  csc_report(diag, out = out)
  expect_true(file.exists(out))
  unlink(out)
})

test_that("autoplot.csc_diag('tipping') handles fallback grid", {
  skip_if_not_installed("ggplot2")
   
  sens_grid <- tibble::tibble(
    r2_du = c(0, 0.1, 0.0, 0.1),
    r2_yu = c(0, 0.0, 0.1, 0.1),
    sig   = c(TRUE, FALSE, TRUE, FALSE), 
    bias   = c(0, 0, 0, 0),
    co_adj = c(0, 0, 0, 0),
    z_adj  = c(0, 0, 0, 0)
  )
  
  fake_diag <- list(
    input = list(d = "d"),
    results = list(
      sensitivity = sens_grid
    )
  )
  class(fake_diag) <- "csc_diag"
  
  p_tip <- autoplot(fake_diag, which = "tipping")
  expect_s3_class(p_tip, "ggplot")
})