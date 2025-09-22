#' Simple placebo tests
#'
#' - random_assign: permute treatment labels and re-fit OLS
#' - lead_treatment: shift D forward by k in time-sorted data
#' - event_lead: for panel with absorbing D, add k-period *lead* of D in TWFE
#'
#' @keywords internal
csc_placebo <- function(df, y, d, x,
                        type = c("random_assign","lead_treatment","event_lead"),
                        lead_k = 1, seed = 123,
                        did = list(id = NULL, time = NULL)) {
  type <- match.arg(type)

  if (type == "random_assign") {
    set.seed(seed)
    df2 <- df
    df2[[d]] <- sample(df[[d]])
    m <- stats::lm(csc_formula(y, d, x), data = df2)
    res <- tidy_ci(m, vcov = vcov_hc3(m))
    res$placebo <- "random_assign"
    return(tibble::as_tibble(res))
  }

  ## lead_treatment (require both id and time)
  if (type == "lead_treatment") {
    if (!all(c("time") %in% names(df))) rlang::abort("lead_treatment needs a 'time' column.")
    id_var <- did$id %||% NULL
    if (!is.null(id_var)) {
      df2 <- df |>
        dplyr::arrange(.data[[id_var]], .data$time) |>
        dplyr::group_by(.data[[id_var]]) |>
        dplyr::mutate(!!d := dplyr::lag(.data[[d]], lead_k)) |>
        dplyr::ungroup() |>
        tidyr::drop_na(d)
    } else {
      df2 <- df[order(df$time), , drop = FALSE]
      df2[[d]] <- dplyr::lag(df2[[d]], lead_k)
      df2 <- tidyr::drop_na(df2, d)
    }
    m <- stats::lm(csc_formula(y, d, x), data = df2)
    res <- tidy_ci(m, vcov = vcov_hc3(m))
    res$placebo <- paste0("lead_treatment_", lead_k)
    return(tibble::as_tibble(res))
  }

  ## event_lead (panel lead within id)
  if (type == "event_lead") {
    if (!requireNamespace("fixest", quietly = TRUE)) rlang::abort("event_lead requires fixest.")
    id <- did$id; time <- did$time
    if (is.null(id) || is.null(time)) rlang::abort("event_lead needs did = list(id='...', time='...').")
    df2 <- df |>
      dplyr::arrange(.data[[id]], .data[[time]]) |>
      dplyr::group_by(.data[[id]]) |>
      dplyr::mutate(!!paste0(d, "_lead", lead_k) := dplyr::lead(.data[[d]], lead_k)) |>
      dplyr::ungroup()
    d_lead <- paste0(d, "_lead", lead_k)
    fml <- stats::as.formula(paste(y, "~", paste(c(d_lead, x), collapse = " + "),
                                   "|", id, "+", time))
    m <- fixest::feols(fml, data = df2, cluster = id)
    res <- broom::tidy(m, conf.int = TRUE)
    res$placebo <- paste0("event_lead_", lead_k)
    return(tibble::as_tibble(res))
  }
}
