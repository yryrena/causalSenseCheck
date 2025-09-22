#' Simulated panel data for causalSenseCheck
#'
#' A small simulated panel dataset with outcome, binary treatment,
#' covariates, and unit-time identifiers. Useful for examples, README,
#' vignettes, and tests.
#'
#' @format A data frame/tibble with columns:
#' \describe{
#'   \item{id}{Panel unit id (integer).}
#'   \item{time}{Time period (integer).}
#'   \item{y}{Outcome (numeric).}
#'   \item{d}{Binary treatment indicator (0/1).}
#'   \item{x1}{Covariate 1 (numeric).}
#'   \item{x2}{Covariate 2 (numeric).}
#' }
#'
#' @usage data(sim_data)
#'
#' @examples
#' data(sim_data, package = "causalSenseCheck")
#' head(sim_data)
#'
#' @source Simulated in \code{data-raw/sim_data.R}
#' @keywords datasets
#' @docType data
#' @name sim_data
"sim_data"
