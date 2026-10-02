# Evidence-layer formatting and narrative helpers

#' Summarize how many parameters fall in each evidence tier
#'
#' Tiers (`config/model_parameters.csv$evidence_tier`): A = directly
#' observed data specific to Lynch syndrome surveillance populations; B =
#' contemporary U.S. public cost/reimbursement data (CMS, real CPI
#' anchors); C = general or adjacent-population literature; D =
#' provisional assumption/placeholder with no source yet; `structural` =
#' an analysis convention rather than an evidence claim (e.g.
#' `reference_dollar_year`). A full leave-one-source-out or
#' value-of-information analysis is not implemented -- this is a
#' first-pass count, not a sensitivity result.
#'
#' @param model_parameters Tibble from [load_model_parameters()].
#' @return A tibble with one row per tier: `evidence_tier`, `n_parameters`,
#'   `pct_parameters`.
#' @export
summarize_evidence_tiers <- function(model_parameters) {
  base::message("Summarizing parameter counts by evidence tier.")

  n_total <- base::nrow(model_parameters)

  tier_summary <- model_parameters %>%
    dplyr::count(.data$evidence_tier, name = "n_parameters") %>%
    dplyr::mutate(pct_parameters = 100 * .data$n_parameters / n_total) %>%
    dplyr::arrange(.data$evidence_tier)

  purrr::walk2(
    tier_summary$evidence_tier, tier_summary$n_parameters,
    ~ base::message("  Tier ", .x, ": ", .y, " parameters")
  )

  tier_summary
}

#' Format a number as a whole-dollar amount
#'
#' @param x Numeric vector to format.
#' @return Character vector of dollar-formatted strings (e.g. `"$1,234"`).
#' @export
format_cost <- function(x) {
  scales::dollar(
    x,
    accuracy = 1,
    big.mark = ","
  )
}

#' Format a proportion as a percentage
#'
#' @param x Numeric vector to format, on a 0-1 scale.
#' @return Character vector of percent-formatted strings (e.g. `"12.3%"`).
#' @export
format_percent <- function(x) {
  scales::percent(
    x,
    accuracy = 0.1
  )
}

#' Compose a one-sentence summary of an external cost-validation comparison
#'
#' Describes the direction (higher/lower/the same "in Colorado") and
#' magnitude of `comparison_tbl$absolute_difference[[1]]` over the given
#' year range, with an optional p-value.
#'
#' @param comparison_tbl Tibble with an `absolute_difference` column; only
#'   the first row/element is used.
#' @param start_year First year of the comparison period.
#' @param end_year Last year of the comparison period.
#' @param p_value Numeric p-value to report, or `NA_real_` (default) to
#'   report that no p-value was estimated.
#' @return Character scalar, a complete sentence describing the comparison.
#' @export
format_external_validation_sentence <- function(
    comparison_tbl,
    start_year,
    end_year,
    p_value = NA_real_) {
  difference <- comparison_tbl$absolute_difference[[1]]

  direction <- dplyr::case_when(
    difference > 0 ~ "higher in Colorado",
    difference < 0 ~ "lower in Colorado",
    TRUE ~ "the same"
  )

  p_text <- if (is.na(p_value)) {
    "p-value not estimated"
  } else {
    base::paste0(
      "p = ",
      format.pval(
        p_value,
        digits = 3,
        eps = 0.001
      )
    )
  }

  base::paste0(
    "Across ",
    start_year,
    "-",
    end_year,
    ", the adjusted incremental cost was ",
    direction,
    " by ",
    format_cost(abs(difference)),
    " (",
    p_text,
    ")."
  )
}
