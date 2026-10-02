# Scenario analysis
#
# Distinguishes the base-case analysis (Medicare-anchored CPT allowed
# amounts; as of 2026-08-30 the combined arm's base case includes a
# separate preop office visit, per the model owner's confirmed clinical
# practice -- see `combined_requires_preop_office_visit` in
# `config/model_parameters.csv` and `docs/methods_notes.md`) from named
# scenario analyses that substitute alternative payer assumptions or
# structural choices. Every scenario is expressed as a named list of
# parameter overrides applied via [override_model_parameters()], so
# scenario definitions stay declarative and auditable.
#
# The Medicaid and commercial scenarios scale each reimbursement
# parameter by its own empirical payer-to-Medicare ratio: the median,
# across hospitals, of each hospital's professional-fee rate for that
# payer divided by the same hospital's Medicare rate for the same CPT code,
# measured in national hospital price-transparency data (Trilliant Health
# Hospital MRF Data Directory, 2026-07-21 snapshot, processed with the
# hpt_prices pipeline). The ratios are the `payer_multiplier_*` rows of
# config/model_parameters.csv. They replaced the earlier provisional
# flat multipliers (Medicaid 0.70, commercial 1.75) on 2026-09-13.

#' Parameter names treated as "reimbursement" costs for payer-mix scenarios
#' @export
REIMBURSEMENT_PARAMETER_NAMES <- c(
  "emb_office_professional_cost",
  "emb_pathology_cost",
  "dc_professional_cost",
  "office_visit_em_cost"
)

#' Build a reimbursement-multiplier scenario override list
#'
#' @param model_parameters Tibble from [load_model_parameters()].
#' @param multiplier Numeric scalar applied to every parameter in
#'   `REIMBURSEMENT_PARAMETER_NAMES`.
#' @return A named list suitable for [override_model_parameters()].
#' @export
build_reimbursement_multiplier_overrides <- function(model_parameters, multiplier) {
  overrides <- purrr::map(
    REIMBURSEMENT_PARAMETER_NAMES,
    ~ get_parameter_value(model_parameters, .x) * multiplier
  )
  stats::setNames(overrides, REIMBURSEMENT_PARAMETER_NAMES)
}

#' Parameter holding the payer-to-Medicare ratio for one reimbursement input
#'
#' @param payer "medicaid" or "commercial".
#' @param parameter_name One of `REIMBURSEMENT_PARAMETER_NAMES`.
#' @return Character scalar, the `payer_multiplier_<payer>_<parameter_name>`
#'   row name in `config/model_parameters.csv`.
#' @export
payer_multiplier_parameter <- function(payer, parameter_name) {
  base::paste0("payer_multiplier_", payer, "_", parameter_name)
}

#' Payer-scenario overrides with a separate empirical multiplier per
#' reimbursement parameter
#'
#' @param model_parameters Tibble from [load_model_parameters()].
#' @param payer "medicaid" or "commercial".
#' @return A named list suitable for [override_model_parameters()].
#' @export
build_payer_multiplier_overrides <- function(model_parameters, payer) {
  overrides <- purrr::map(
    REIMBURSEMENT_PARAMETER_NAMES,
    function(parameter_name) {
      get_parameter_value(model_parameters, parameter_name) *
        get_parameter_value(model_parameters, payer_multiplier_parameter(payer, parameter_name))
    }
  )
  stats::setNames(overrides, REIMBURSEMENT_PARAMETER_NAMES)
}

#' Named scenario definitions
#'
#' @param model_parameters Tibble from [load_model_parameters()].
#' @param price_index_table Tibble from [load_price_index_table()], used
#'   to inflation-adjust the historical Ladabaum et al. 2011 office-EMB
#'   cost anchor for the `office_cost_ladabaum_historical` scenario.
#' @param reference_year Numeric scalar target year for that adjustment.
#' @return A named list of override lists, one per scenario.
#' @export
build_scenario_definitions <- function(
  model_parameters,
  price_index_table = load_price_index_table(),
  reference_year = get_parameter_value(model_parameters, "reference_dollar_year")
) {
  ladabaum_row <- model_parameters %>%
    dplyr::filter(.data$parameter == "cost_emb_ladabaum_2010")
  ladabaum_inflated_cost <- adjust_for_inflation(
    cost_value = base::as.numeric(ladabaum_row$base_value[[1]]),
    source_year = ladabaum_row$dollar_year[[1]],
    reference_year = reference_year,
    price_index_table = price_index_table
  )

  list(
    base_case_medicare = list(
      overrides = list(),
      description = "Base case: CPT-based Medicare national allowed amounts, including the combined arm's separate preop office visit (combined_requires_preop_office_visit = TRUE, per the model owner's confirmed clinical practice).",
      provisional = FALSE
    ),
    medicaid = list(
      overrides = build_payer_multiplier_overrides(model_parameters, "medicaid"),
      description = "Medicaid reimbursement: each professional-fee input scaled by its empirical Medicaid-to-Medicare ratio from national hospital price-transparency data (payer_multiplier_medicaid_* parameters).",
      provisional = FALSE
    ),
    commercial = list(
      overrides = build_payer_multiplier_overrides(model_parameters, "commercial"),
      description = "Commercial reimbursement: each professional-fee input scaled by its empirical commercial-to-Medicare ratio from national hospital price-transparency data (payer_multiplier_commercial_* parameters).",
      provisional = FALSE
    ),
    combined_without_preop_visit = list(
      overrides = list(combined_requires_preop_office_visit = "FALSE"),
      description = paste0(
        "Structural scenario (the FORMER base case, superseded 2026-08-30): the combined ",
        "strategy's consent/risk assessment is folded into existing care rather than requiring ",
        "a separate preop office visit. Kept so the base case's sensitivity to this structural ",
        "assumption stays checkable now that the toggle's default is TRUE (see ",
        "docs/methods_notes.md)."
      ),
      provisional = FALSE
    ),
    office_cost_ladabaum_historical = list(
      overrides = list(emb_office_professional_cost = ladabaum_inflated_cost),
      description = base::paste0(
        "Cross-validation scenario: substitutes the office-EMB professional fee with the ",
        "Ladabaum et al. 2011 $224 (2010 dollars) literature anchor, inflated to ",
        reference_year, " dollars via the real BLS CPI-U Medical Care series (~$",
        base::round(ladabaum_inflated_cost, 0), "). Tests whether the base-case conclusion ",
        "is sensitive to using an independent historical cost track instead of the CMS-2026 ",
        "fee-schedule figure."
      ),
      provisional = FALSE
    )
  )
}

#' Run the strategy-cost model under every named scenario
#'
#' @param model_parameters Tibble from [load_model_parameters()].
#' @param price_index_table Tibble from [load_price_index_table()].
#' @return A tibble binding `strategy_costs` for every scenario, with a
#'   leading `scenario` column and the scenario's `provisional` flag.
#' @export
run_scenario_analysis <- function(
  model_parameters,
  price_index_table = load_price_index_table()
) {
  scenario_definitions <- build_scenario_definitions(
    model_parameters, price_index_table
  )

  base::message("Running ", length(scenario_definitions), " scenario(s).")

  scenario_rows <- purrr::imap(scenario_definitions, function(scenario_definition, scenario_name) {
    base::message(
      "  Scenario: ", scenario_name,
      if (isTRUE(scenario_definition$provisional)) " [PROVISIONAL]" else ""
    )

    scenario_parameters <- override_model_parameters(
      model_parameters, scenario_definition$overrides
    )
    strategy_result <- compute_strategy_costs(
      scenario_parameters, price_index_table = price_index_table
    )

    strategy_result$strategy_costs %>%
      dplyr::mutate(
        scenario = scenario_name,
        scenario_description = scenario_definition$description,
        scenario_provisional = scenario_definition$provisional
      ) %>%
      dplyr::select("scenario", dplyr::everything())
  })

  dplyr::bind_rows(scenario_rows)
}
