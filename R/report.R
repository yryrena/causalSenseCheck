#' Render a compact robustness report
#' @param diag csc_diag
#' @param out output html path
#' @export
csc_report <- function(diag, out = "csc_report.html") {
  stopifnot(inherits(diag, "csc_diag"))
  if (!requireNamespace("rmarkdown", quietly = TRUE)) rlang::abort("Install 'rmarkdown' to use csc_report().")
  tmp <- tempfile(fileext = ".Rmd")
  writeLines(csc_report_rmd(), con = tmp)
  rmarkdown::render(tmp, output_file = out, params = list(diag = diag), envir = new.env())
}

csc_report_rmd <- function() {
  paste0(c(
    "---",
    'title: "Causal Sense Check Report"',
    "output: html_document",
    "params:",
    "  diag: NULL",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(message = FALSE, warning = FALSE)",
    "diag <- params$diag",
    "library(ggplot2)",
    "```",
    "",
    "## Tipping Map",
    "```{r}",
    "sens <- diag$results$sensitivity",
    "if (is.null(sens)) {",
    "  cat('No sensitivity results available.\\n')",
    "} else {",
    "  print(autoplot(diag, 'tipping'))",
    "}",
    "```",
    "",
    "## Placebo Checks",
    "```{r}",
    "if (!is.null(diag$results$placebo)) print(autoplot(diag, 'placebo'))",
    "```",
    "",
    "## Estimator Summary",
    "```{r}",
    "print(autoplot(diag, 'compare'))",
    "```"
  ), collapse = "\n")
}
