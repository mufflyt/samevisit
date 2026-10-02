# Societal-perspective secondary analysis (patient time/travel opportunity cost)
#
# The base case (`R/strategy_costs.R`) takes a U.S. healthcare-sector
# perspective and explicitly excludes patient time, transportation, and
# lost productivity (see Methods). This file adds a SEPARATE, clearly
# labeled societal-perspective add-on: the expected number of dedicated,
# patient-borne in-person encounters each strategy requires, valued at a
# single per-visit opportunity-cost estimate (`patient_time_opportunity_cost_per_visit`,
# Ray et al. 2015, PMID 26295356). It does NOT change
# `compute_strategy_costs()`'s output and is not part of the probabilistic
# sensitivity analysis -- a deterministic point-estimate secondary
# analysis, the same scope discipline already used for
# `compute_diagnostic_yield()` (see `R/diagnostic_yield.R`'s file-level
# docblock).
#
# **Encounter counting, by strategy** (mirrors the escalation/repeat-attempt
# probabilities already in `R/strategy_costs.R`, deliberately recomputed
# here from the same underlying parameters rather than reusing
# `compute_strategy_costs()`'s output shape -- see
# `tests/testthat/test-societal-costs.R`'s consistency test, the same
# pattern already used in `R/diagnostic_yield.R`):
# - `dnc`: exactly 2 encounters (a preoperative clinic visit plus a
#   separate OR/procedure day) -- fixed, no escalation branch of its own.
# - `office_emb`: 1 initial office visit, plus `repeat_attempt_probability`
#   of one more office visit, plus `escalation_probability` of D&C's own 2
#   encounters (escalating to D&C means undergoing D&C's full pathway, on
#   top of the office visit already taken).
# - `combined_emb`: `combined_requires_preop_office_visit` (a structural
#   scenario toggle, TRUE in the base case) worth of 1 dedicated
#   preoperative office visit, plus `escalation_probability` of D&C's own
#   2 encounters. The colonoscopy day itself is never counted, under the
#   same incremental-cost principle used for dollar costs -- the patient
#   is assumed to be undergoing it regardless of which EMB strategy is
#   chosen.
#
# **Known conservative bias, disclosed rather than corrected for:** every
# encounter is valued at the SAME per-visit opportunity cost, including
# D&C's OR/anesthesia-day encounter, which plausibly costs the patient
# (and any companion/caregiver, not counted at all here) substantially
# more time than a routine office visit -- no differentiated,
# procedure-day-specific patient-time-cost source was identified. This
# likely UNDERSTATES D&C's true relative societal-cost disadvantage. See
# `patient_time_opportunity_cost_per_visit`'s own notes in
# `config/model_parameters.csv` for the full sourcing and scope caveats
# (does not include out-of-pocket travel expense or caregiver time).

#' Compute each strategy's expected number of dedicated patient encounters
#'
#' @param model_parameters Tibble from [load_model_parameters()].
#' @return A tibble with one row per strategy: `strategy`,
#'   `expected_encounters`.
#' @export
compute_strategy_expected_encounters <- function(model_parameters) {
  failure_probability <- get_parameter_value(model_parameters, "emb_failure_lynch")
  repeat_attempt_fraction <- get_parameter_value(
    model_parameters, "office_repeat_attempt_fraction"
  )
  repeat_attempt_success_probability <- get_parameter_value(
    model_parameters, "office_repeat_attempt_success_probability"
  )
  repeat_attempt_probability <- failure_probability * repeat_attempt_fraction
  office_escalation_probability <- failure_probability *
    (1 - repeat_attempt_fraction * repeat_attempt_success_probability)

  combined_escalation_probability <- get_parameter_value(
    model_parameters, "combined_to_dnc_probability"
  )
  requires_preop_visit <- get_parameter_value(
    model_parameters, "combined_requires_preop_office_visit", as_numeric = FALSE
  )

  dnc_encounters <- 2
  office_encounters <- 1 + repeat_attempt_probability * 1 +
    office_escalation_probability * dnc_encounters
  combined_encounters <- base::as.numeric(isTRUE(requires_preop_visit)) * 1 +
    combined_escalation_probability * dnc_encounters

  tibble::tibble(
    strategy = c("office_emb", "combined_emb", "dnc"),
    expected_encounters = c(office_encounters, combined_encounters, dnc_encounters)
  )
}

#' Compute each strategy's societal-perspective total cost
#'
#' Adds a patient time/travel opportunity-cost add-on to the healthcare-
#' sector `expected_total_cost` already computed by
#' [compute_strategy_costs()].
#'
#' @param model_parameters Tibble from [load_model_parameters()].
#' @param strategy_costs Tibble from `compute_strategy_costs()$strategy_costs`.
#' @param all_items_price_index_table Tibble from
#'   [load_price_index_table()], loaded from `data/cpi_all_items.csv` (a
#'   general CPI-U series -- deliberately NOT the medical-care CPI series
#'   used for healthcare-sector costs elsewhere in this model, since a
#'   wage/time-based cost should track general price/wage inflation, not
#'   medical-service-price inflation).
#' @param reference_year Numeric scalar target dollar year.
#' @return A tibble with one row per strategy: `strategy`,
#'   `expected_encounters`, `patient_time_cost_per_encounter`,
#'   `societal_addon`, `healthcare_sector_cost`, `societal_total_cost`.
#' @export
compute_strategy_societal_costs <- function(
  model_parameters,
  strategy_costs,
  all_items_price_index_table,
  reference_year
) {
  base::message("Computing societal-perspective secondary analysis.")

  encounters <- compute_strategy_expected_encounters(model_parameters)

  patient_time_cost_per_encounter <- get_adjusted_cost_parameter(
    model_parameters, "patient_time_opportunity_cost_per_visit",
    all_items_price_index_table, reference_year
  )

  societal_costs <- encounters %>%
    dplyr::mutate(
      patient_time_cost_per_encounter = patient_time_cost_per_encounter,
      societal_addon = .data$expected_encounters * patient_time_cost_per_encounter
    ) %>%
    dplyr::left_join(
      strategy_costs %>%
        dplyr::select("strategy", healthcare_sector_cost = "expected_total_cost"),
      by = "strategy"
    ) %>%
    dplyr::mutate(
      societal_total_cost = .data$healthcare_sector_cost + .data$societal_addon
    ) %>%
    dplyr::arrange(.data$societal_total_cost)

  base::message("Societal-perspective total cost (lowest to highest):")
  purrr::walk2(
    societal_costs$strategy, societal_costs$societal_total_cost,
    ~ base::message("  ", .x, ": $", base::round(.y, 2))
  )

  societal_costs
}
