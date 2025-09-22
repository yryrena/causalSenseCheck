# R/plot.R

#' Plot diagnostics
#'
#' @param object A \code{csc_diag} object.
#' @param which One of \code{"tipping"}, \code{"placebo"}, \code{"compare"}.
#' @param ...  Additional arguments passed to methods (currently unused).
#' @return A \code{ggplot} object.
#' @method autoplot csc_diag
#' @importFrom ggplot2 autoplot
#' @export
autoplot.csc_diag <- function(object, which = c("tipping","placebo","compare"), ...) {
  which <- match.arg(which)

  # -------------------------
  # TIPPING (Sensitivity Plot)
  # -------------------------
  if (which == "tipping") {
    sens <- object$results$sensitivity
    if (is.null(sens)) {
      rlang::abort("No sensitivity results (try OLS in csc_fit or install 'sensemakr').")
    }

    nm <- names(sens)

    # Case 1: our fallback grid (approximate tipping map)
    # expects columns r2_du, r2_yu, sig
    if (all(c("r2_du", "r2_yu", "sig") %in% nm)) {
      return(
        ggplot2::ggplot(sens, ggplot2::aes(x = r2_du, y = r2_yu, fill = sig)) +
          ggplot2::geom_raster() +
          ggplot2::labs(
            x = expression(R^2[D~"~U"]),
            y = expression(R^2[Y~"~U"]),
            fill = "Significant?",
            title = "Tipping Map (approx)"
          ) +
          ggplot2::theme_minimal()
      )
    }

    # Case 2: sensemakr nested output (tibble with a 'se' data.frame).
    # We read rv_q (tipping RV) and rv_qa (q=90% RV) from sens$se.
    if ("se" %in% nm && is.data.frame(sens$se)) {
      df <- tibble::as_tibble(sens$se)
      if (all(c("rv_q", "rv_qa") %in% names(df))) {
        df2 <- tibble::tibble(
          metric = c("Tipping RV", "RV @ q=90%"),
          value  = c(df$rv_q[1], df$rv_qa[1])
        )
        return(
          ggplot2::ggplot(df2, ggplot2::aes(x = metric, y = value)) +
            ggplot2::geom_col(alpha = 0.85) +
            ggplot2::geom_hline(yintercept = 0, linetype = 3) +
            ggplot2::labs(
              x = NULL,
              y = "Robustness value (partial R^2)",
              title = "Sensitivity (sensemakr)"
            ) +
            ggplot2::theme_minimal()
        )
      }
    }

    # Case 3: older/flattened sensemakr shape (robustness_value, robustness_value_q)
    rv_tip <- dplyr::coalesce(sens$r2_omitted_tipping, sens$robustness_value)
    rv_q90 <- dplyr::coalesce(sens$r2_omitted_q90, sens$robustness_value_q)
    if (!is.null(rv_tip) && !is.null(rv_q90)) {
      df <- tibble::tibble(
        metric = c("Tipping RV", "RV @ q=90%"),
        value  = c(rv_tip[1], rv_q90[1])
      )
      return(
        ggplot2::ggplot(df, ggplot2::aes(x = metric, y = value)) +
          ggplot2::geom_col(alpha = 0.85) +
          ggplot2::geom_hline(yintercept = 0, linetype = 3) +
          ggplot2::labs(
            x = NULL,
            y = "Robustness value (partial R^2)",
            title = "Sensitivity (sensemakr)"
          ) +
          ggplot2::theme_minimal()
      )
    }

    # Fallback: tell the user what we saw to ease debugging
    rlang::abort(paste0(
      "Unsupported structure of sensitivity results for tipping plot. Columns: ",
      paste(nm, collapse = ", ")
    ))
  }

  # -------------------------
  # PLACEBO
  # -------------------------
  if (which == "placebo") {
    plc <- object$results$placebo
    if (is.null(plc)) rlang::abort("No placebo results.")
    d <- object$input$d

    return(
      plc |>
        dplyr::filter(.data$term == d) |>
        ggplot2::ggplot(ggplot2::aes(x = placebo, y = estimate)) +
        ggplot2::geom_point(position = ggplot2::position_jitter(width = .1, height = 0)) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = conf.low, ymax = conf.high), width = .12) +
        ggplot2::geom_hline(yintercept = 0, linetype = 3) +
        ggplot2::labs(x = NULL, y = "Estimate", title = "Placebo Checks") +
        ggplot2::theme_minimal()
    )
  }

  # -------------------------
  # CROSS-ESTIMATOR COMPARISON
  # -------------------------
  if (which == "compare") {
    cmp <- object$results$compare
    return(
      ggplot2::ggplot(cmp, ggplot2::aes(y = estimator, x = estimate)) +
        ggplot2::geom_point() +
        ggplot2::geom_errorbar(
          ggplot2::aes(xmin = conf.low, xmax = conf.high, y = estimator),
          width = .2,
          orientation = "y"
        ) +
        ggplot2::geom_vline(xintercept = 0, linetype = 3) +
        ggplot2::labs(y = NULL, x = "Estimate", title = "Cross-Estimator Comparison") +
        ggplot2::theme_minimal()
    )
  }
}
