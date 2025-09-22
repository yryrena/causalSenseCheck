#' @keywords internal
csc_formula <- function(y, d, x = NULL, interactions = FALSE) {
  stopifnot(is.character(y), is.character(d))
  rhs <- c(d, x)
  f <- if (length(rhs)) paste(rhs, collapse = " + ") else "1"
  if (interactions && length(x)) {
    f <- paste0(f, " + ", d, ":(", paste(x, collapse = " + "), ")")
  }
  stats::as.formula(paste(y, "~", f))
}

#' @keywords internal
vcov_hc3 <- function(mod) sandwich::vcovHC(mod, type = "HC3")

#' @keywords internal
tidy_ci <- function(mod, level = 0.95, vcov = NULL) {
  broom::tidy(mod, conf.int = TRUE, conf.level = level, vcov = vcov)
}
#' @importFrom stats qnorm
NULL
