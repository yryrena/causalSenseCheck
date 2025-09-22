# R/zzz.R

utils::globalVariables(c(
  ".", ".data", ":=",
  "r2_du", "r2_yu", "sig", "placebo",
  "estimate", "conf.low", "conf.high", "estimator",
  "value", "metric"
))

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "Loaded causalSenseCheck. See ?csc_fit or vignette('getting-started-causalSenseCheck')."
  )
}
