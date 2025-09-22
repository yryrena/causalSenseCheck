#' Unified diagnostics: sensitivity + placebo + estimator compare
#' @param fit csc_fit
#' @param placebo character vector among c("random_assign","lead_treatment","event_lead")
#' @param lead_k integer, lead horizon for placebo types using leads
#' @export
csc_diagnose <- function(fit,
                         placebo = c("random_assign"),
                         lead_k = 1) {
  stopifnot(inherits(fit, "csc_fit"))
  y <- fit$input$y; d <- fit$input$d; x <- fit$input$x

  # Sensitivity (sensemakr preferred)
  mod_ols <- try(fit$models$ols$fit, silent = TRUE)
  sens <- if (!inherits(mod_ols, "try-error")) csc_sensitivity(mod_ols, treat = d) else NULL


  # Placebos
  plc <- NULL
  if (length(placebo)) {
    plc <- purrr::map_dfr(placebo, function(tp) {
      csc_placebo(fit$data, y, d, x, type = tp, lead_k = lead_k, did = fit$input$did)
    })
  }

  # Compare (stack all available estimators; filter to treatment term)
  cmp <- purrr::imap_dfr(fit$models, function(m, nm) {
    est <- tibble::as_tibble(m$est)
    if ("term" %in% names(est)) est <- dplyr::filter(est, .data$term == d)
    est$estimator <- nm
    est
  }) |>
    dplyr::select(estimator, tidyselect::any_of(c("term","estimate","std.error","conf.low","conf.high")))

  out <- list(
    input   = fit$input,
    results = list(
      sensitivity = sens,
      placebo     = plc,
      compare     = cmp
    )
  )
  class(out) <- "csc_diag"
  out
}
