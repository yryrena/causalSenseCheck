#' Sensitivity analysis: prefer sensemakr if available; fallback to simple grid
#' @keywords internal
#' Sensitivity analysis: prefer sensemakr if available; fallback to simple grid
#' @keywords internal
csc_sensitivity <- function(mod, treat) {
  if (requireNamespace("sensemakr", quietly = TRUE)) {
    ## sensemakr returns robustness values in terms of partial R2 and 'k' multiples
    sm <- sensemakr::sensemakr(mod, treatment = treat, q = 1,
                               ci = 0.95, reduce = TRUE)
    tb <- tibble::tibble(
      estimate = sm$point_estimate,
      se       = sm$se,
      r2ydx    = sm$r2yxj,      ## partial R2 of treatment with outcome
      r2dx     = sm$r2dxj,      ## partial R2 of treatment with controls
      t_crit   = sm$t_critical,
      r2_omitted_tipping = sm$robustness_value,   ## RV at which effect tips
      r2_omitted_q90     = sm$robustness_value_q  ## RV at q=90%
    )
    tb$method <- "sensemakr"
    return(tb)
  } else {
    ## fallback: heatmap grid (approximate)
    grid <- csc_sensitivity_grid(mod, treat)
    grid$method <- "grid"
    return(grid)
  }
}


#' (fallback) Sensitivity grid (tipping map) for unobserved confounding (approx)
#' @keywords internal
csc_sensitivity_grid <- function(mod, treat,
                                 r2yu = seq(0, .3, by=.01),
                                 r2du = seq(0, .3, by=.01)) {
  co <- stats::coef(mod)[treat]
  se <- sqrt(diag(vcov_hc3(mod)))[treat]
  grid <- tidyr::crossing(r2_yu = r2yu, r2_du = r2du)
  grid$bias   <- sign(co) * sqrt(pmax(grid$r2_yu, 0)) * sqrt(pmax(grid$r2_du, 0)) / pmax(1 - grid$r2_du, 1e-6)
  grid$co_adj <- co - grid$bias * se
  grid$z_adj  <- grid$co_adj / se
  grid$sig    <- abs(grid$z_adj) > stats::qnorm(0.975)
  tibble::as_tibble(grid)
}
