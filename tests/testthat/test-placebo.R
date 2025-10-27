test_that("csc_placebo lead_treatment works with panel id+time", {
  data(sim_data, package = "causalSenseCheck")
   
  expect_true(all(c("id","time","d","y","x1","x2") %in% names(sim_data)))
  
  res <- csc_placebo(
    df  = sim_data,
    y   = "y",
    d   = "d",
    x   = c("x1","x2"),
    type = "lead_treatment",
    lead_k = 1,
    did = list(id = "id", time = "time")
  )
  
  expect_s3_class(res, "tbl_df")
  expect_true(all(c("term","estimate","conf.low","conf.high","placebo") %in% names(res)))
  expect_true(any(grepl("lead_treatment_", unique(res$placebo))))
})

test_that("csc_placebo lead_treatment works without panel id", {
  data(sim_data, package = "causalSenseCheck")
   
  sim_noid <- sim_data
  sim_noid$id <- NULL
  
  res <- csc_placebo(
    df  = sim_noid,
    y   = "y",
    d   = "d",
    x   = c("x1","x2"),
    type = "lead_treatment",
    lead_k = 2,
    did = list(id = NULL, time = "time")
  )
  
  expect_s3_class(res, "tbl_df")
  expect_true(any(grepl("lead_treatment_", unique(res$placebo))))
})

test_that("csc_placebo event_lead works if fixest is installed", {
  skip_if_not_installed("fixest")
  data(sim_data, package = "causalSenseCheck")
  
  res <- csc_placebo(
    df  = sim_data,
    y   = "y",
    d   = "d",
    x   = c("x1","x2"),
    type = "event_lead",
    lead_k = 1,
    did = list(id = "id", time = "time")
  )
  
  expect_s3_class(res, "tbl_df")
  expect_true("placebo" %in% names(res))
  expect_true(any(grepl("event_lead_", res$placebo)))
})