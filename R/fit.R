#' Fit core causal model(s): OLS, DR/AIPW, DID (fixest), IV (AER), CATE (grf)
#'
#' @param data data.frame
#' @param y outcome column
#' @param d treatment column (0/1 preferred; absorbing for DID)
#' @param x covariates (character vector)
#' @param estimators character vector among c("ols","dr","did","iv","grf")
#' @param did list with panel keys, e.g. list(id="id", time="time", cluster="id")
#' @param iv list with instrument name, e.g. list(z="zname")
#' @param grf list of controls, e.g. list(num.trees=2000, seed=1)
#' @param interactions logical; include D:X interactions in OLS
#' @return csc_fit object with per-estimator tidied results in $models[[name]]$est
#' @export
csc_fit <- function(data, y, d, x = NULL,
                    estimators = c("ols"),
                    did = list(id = NULL, time = NULL, cluster = NULL),
                    iv  = list(z = NULL),
                    grf = list(num.trees = 2000, seed = 1),
                    interactions = FALSE) {

  df <- tibble::as_tibble(data)
  stopifnot(all(c(y, d, x) %in% names(df)))

  out_models <- list()

  # Utilities ---------------------------------------------------------------
  vc <- function(m) vcov_hc3(m)
  ci95 <- function(est, se) {
    tibble::tibble(conf.low = est - qnorm(0.975)*se,
                   conf.high = est + qnorm(0.975)*se)
  }
  tidy_row <- function(term, estimate, std.error) {
    tibble::tibble(term = term,
                   estimate = estimate,
                   std.error = std.error) |>
      dplyr::bind_cols(ci95(estimate, std.error))
  }

  # OLS ---------------------------------------------------------------------
  if ("ols" %in% estimators) {
    f <- csc_formula(y, d, x, interactions = interactions)
    m <- stats::lm(f, data = df)
    out_models$ols <- list(
      fit  = m,
      vcov = vc(m),
      est  = tidy_ci(m, vcov = vc(m)),
      type = "ols"
    )
  }

  # DR / AIPW ---------------------------------------------------------------
  if ("dr" %in% estimators) {
    stopifnot(length(x) > 0)
    # Propensity
    pf <- stats::as.formula(paste(d, "~", paste(x, collapse = " + ")))
    ps_mod <- stats::glm(pf, data = df, family = stats::binomial())
    p_hat <- as.numeric(stats::predict(ps_mod, type = "response"))

    # Outcome models
    y1f <- stats::as.formula(paste(y, "~", paste(x, collapse = " + ")))
    y0f <- y1f
    m1 <- stats::lm(y1f, data = df[df[[d]] == 1, , drop = FALSE])
    m0 <- stats::lm(y0f, data = df[df[[d]] == 0, , drop = FALSE])

    mu1_hat <- as.numeric(stats::predict(m1, newdata = df))
    mu0_hat <- as.numeric(stats::predict(m0, newdata = df))

    D <- df[[d]]; Y <- df[[y]]
    # AIPW influence function
    phi <- (mu1_hat - mu0_hat) +
      D*(Y - mu1_hat)/p_hat - (1 - D)*(Y - mu0_hat)/(1 - p_hat)
    tau_hat <- mean(phi)
    se_hat <- stats::sd(phi)/sqrt(nrow(df))

    out_models$dr <- list(
      fit  = list(ps = ps_mod, m1 = m1, m0 = m0),
      vcov = NULL,
      est  = tidy_row(d, tau_hat, se_hat),
      type = "dr"
    )
  }

  # DID via fixest (TWFE) ---------------------------------------------------
  if ("did" %in% estimators) {
    if (!requireNamespace("fixest", quietly = TRUE)) {
      warning("fixest not installed; skipping DID.")
    } else {
      id   <- did$id; time <- did$time
      if (is.null(id) || is.null(time))
        rlang::abort("For DID, provide did = list(id='...', time='...').")
      rhs <- c(d, x)
      fml <- stats::as.formula(paste(y, "~", paste(rhs, collapse = " + "),
                                     "|", id, "+", time))
      m   <- fixest::feols(fml, data = df, cluster = did$cluster %||% id)
      est <- broom::tidy(m, conf.int = TRUE)
      out_models$did <- list(fit = m, vcov = NULL, est = est, type = "did")
    }
  }

  # IV via AER --------------------------------------------------------------
  if ("iv" %in% estimators) {
    if (!requireNamespace("AER", quietly = TRUE)) {
      warning("AER not installed; skipping IV.")
    } else {
      z <- iv$z
      if (is.null(z) || !(z %in% names(df)))
        rlang::abort("For IV, provide iv = list(z='instrument').")
      rhs_x <- if (length(x)) paste(x, collapse = " + ") else "1"
      # y ~ d + x | z + x
      f_iv  <- stats::as.formula(paste0(y, " ~ ", d,
                                        if (length(x)) paste0(" + ", rhs_x) else "",
                                        " | ",
                                        z,
                                        if (length(x)) paste0(" + ", rhs_x) else ""))
      m <- AER::ivreg(f_iv, data = df)
      est <- broom::tidy(m, conf.int = TRUE, vcov = vc(m))
      out_models$iv <- list(
        fit  = m,
        vcov = vc(m),
        est  = est,
        type = "iv"
      )
    }
  }

  # GRF CATE + ATE ----------------------------------------------------------
  if ("grf" %in% estimators) {
    if (!requireNamespace("grf", quietly = TRUE)) {
      warning("grf not installed; skipping GRF.")
    } else {
      stopifnot(length(x) > 0)
      set.seed(grf$seed %||% 1)
      X <- as.matrix(df[, x, drop = FALSE])
      W <- as.numeric(df[[d]])
      Y <- as.numeric(df[[y]])
      forest <- grf::causal_forest(X = X, Y = Y, W = W,
                                   num.trees = grf$num.trees %||% 2000)
      ate <- grf::average_treatment_effect(forest)
      est <- tidy_row(d, unname(ate[["estimate"]]),
                      unname(ate[["std.err"]]))
      out_models$grf <- list(
        fit  = forest,
        vcov = NULL,
        est  = est,
        type = "grf"
      )
    }
  }

  out <- list(
    input  = list(y = y, d = d, x = x,
                  estimators = estimators,
                  did = did, iv = iv, grf = grf,
                  interactions = interactions),
    data   = df,
    models = out_models
  )
  class(out) <- "csc_fit"
  out
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
