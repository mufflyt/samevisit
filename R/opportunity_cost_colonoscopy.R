# Opportunity cost of a displaced colonoscopy-suite case: real hospital data
#
# README.md's "Opportunity cost of a displaced endoscopy-suite case"
# section (added 2026-09-12) documented that no generalizable
# opportunity-cost-per-minute figure exists in the general OR-economics
# literature, and flagged this project as the more consequential of the
# two sibling projects to resolve it, since Childers & Maggard-Gibbons's
# own paper says opportunity cost is highest for short, high-throughput
# procedures -- exactly what a colonoscopy suite is. This file answers
# that with real, hospital-specific data rather than a borrowed national
# ratio, mirroring (and in one respect improving on) the method used for
# the sibling `iud_bariatric` project's bariatric-surgery analysis.
#
# Method: for CPT 45378 ("Colonoscopy, flexible; diagnostic") or, at one
# hospital, the more clinically precise HCPCS G0105 ("colorectal cancer
# screening; colonoscopy on individual at high risk"), six real
# hospitals' own CMS-mandated price-transparency files were downloaded
# and parsed directly (Denver Health CO, UCHealth University of
# Colorado Hospital CO, Emory University Hospital GA, University of
# Mississippi Medical Center MS, University of Arkansas for Medical
# Sciences AR, NYU Langone Tisch NY --
# `data/colonoscopy_multi_hospital_rates.csv`, full citation trail in
# `docs/data_sources.md`). For each hospital, that hospital's OWN real
# Medicare rate is used as the cost proxy for that SAME hospital's
# commercial rate -- a real improvement on the sibling project's
# national-vs-local mismatch problem, since both sides of the
# comparison now come from the identical institution. This substitutes
# for a true cost-accounting figure because none exists for outpatient
# procedures: HCUP's own documentation states plainly that "an
# equivalent [cost-to-charge] ratio for outpatient hospital data is
# currently not available" (checked directly, 2026-09-13) -- so, unlike
# the sibling project's Ng et al. 2023 inpatient cost study, no
# outpatient cost study could be found or fabricated here.
#
# The result is reported as a BOUND, not a single blended number,
# because no real gastroenterology-specific payer-mix data exists for
# any of these six hospitals (unlike Denver Health's own
# bariatric-surgery-specific payer mix in the sibling project): the
# floor is $0 by construction (a displaced Medicare-paying colonoscopy
# costs nothing under this method, since Medicare payment IS the cost
# proxy), and the ceiling is the hospital's own real commercial margin
# (Medicare payment vs. commercial payment at the SAME hospital),
# spread across `colonoscopy_typical_duration_minutes` and scaled to
# `combined_emb_added_minutes`. One striking real finding from this
# data: Mississippi's commercial rate is actually BELOW its own
# Medicare rate (77% of it), the opposite direction from the other
# five hospitals (135%-632%) -- real, hospital-specific variance, not
# an error (see `data/colonoscopy_multi_hospital_rates.csv`'s own notes
# for why the excluded, lower-confidence Mississippi commercial rows
# would only have widened this gap further, not closed it). A second
# real finding: the two Colorado academic hospitals (Denver Health and
# UCHealth University of Colorado Hospital) show substantial
# within-state variance (400% vs. 234% commercial-to-Medicare ratio),
# underscoring that even same-state hospitals cannot be assumed
# interchangeable for this kind of estimate.
#
# Like the sibling `opportunity_cost_national.R` and
# `opportunity_cost_sensitivity.R`, nothing in this file is called by
# the base-case cost engine (`compute_combined_emb_strategy_cost()`) or
# either sensitivity module.

#' Load the real, multi-hospital colonoscopy (CPT 45378) rate table
#'
#' @param path Character scalar path to the rate CSV.
#' @return A tibble, one row per hospital.
#' @export
load_colonoscopy_hospital_rates_table <- function(path = "data/colonoscopy_multi_hospital_rates.csv") {
  if (!base::file.exists(path)) {
    base::stop("Colonoscopy hospital rate file not found at: ", path)
  }

  readr::read_csv(
    path,
    col_types = readr::cols(
      state_abbr = readr::col_character(),
      hospital = readr::col_character(),
      medicare_rate = readr::col_double(),
      commercial_mean_rate = readr::col_double(),
      medicare_confidence = readr::col_character(),
      commercial_confidence = readr::col_character(),
      notes = readr::col_character(),
      commercial_to_medicare_ratio = readr::col_double(),
      source = readr::col_character()
    ),
    show_col_types = FALSE
  )
}

#' Compute each real hospital's bounded opportunity-cost-of-displaced-case estimate
#'
#' @param model_parameters Tibble from [load_model_parameters()].
#' @param rates_table Tibble from [load_colonoscopy_hospital_rates_table()].
#' @return A tibble, one row per hospital: `state_abbr`, `hospital`,
#'   `medicare_rate`, `commercial_mean_rate`, `commercial_margin` (that
#'   hospital's own commercial rate minus its own Medicare rate),
#'   `opportunity_cost_ceiling` (the commercial-margin-based bound on
#'   the added minutes' opportunity cost). The floor is always $0 by
#'   construction (see file docstring) and is not a separate column.
#' @export
compute_colonoscopy_opportunity_cost_bound <- function(
  model_parameters,
  rates_table = load_colonoscopy_hospital_rates_table()
) {
  typical_duration <- get_parameter_value(model_parameters, "colonoscopy_typical_duration_minutes")
  added_minutes <- get_parameter_value(model_parameters, "combined_emb_added_minutes")

  rates_table |>
    dplyr::mutate(
      commercial_margin = .data$commercial_mean_rate - .data$medicare_rate,
      opportunity_cost_ceiling = (.data$commercial_margin / typical_duration) * added_minutes
    ) |>
    dplyr::select(
      "state_abbr", "hospital", "medicare_rate", "commercial_mean_rate",
      "commercial_margin", "opportunity_cost_ceiling"
    )
}
