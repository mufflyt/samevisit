# External validation against published models
#
# A generic harness for checking whether this repository's cost engine
# (`compute_strategy_costs()`) reproduces a published study's result when
# fed that study's own parameters -- real external validation, not
# fabricated parameters chosen to hit a target. See
# `docs/validation_notes.md` for why this repository refuses to
# reverse-engineer unknown internal parameters from a known headline
# output.

#' Compare this model's output to a published target, given the
#' published study's own parameters
#'
#' @param model_parameters Tibble from [load_model_parameters()] or
#'   [override_model_parameters()], already set to the published study's
#'   own input values (not this repository's base case).
#' @param price_index_table Tibble from [load_price_index_table()].
#' @param targets Named list of target values to compare against, using
#'   any subset of `office_emb`, `combined_emb`, `dnc` as names (matching
#'   `strategy_costs$strategy`).
#' @param tolerance_pct Numeric scalar. Percent difference within which a
#'   comparison is flagged `within_tolerance = TRUE`. Default 10.
#' @return A tibble with one row per target: `strategy`, `modeled_cost`,
#'   `target_cost`, `pct_difference`, `within_tolerance`.
#' @export
validate_against_published_model <- function(
  model_parameters,
  price_index_table = load_price_index_table(),
  targets,
  tolerance_pct = 10
) {
  base::message(
    "Validating model output against ", length(targets), " published target(s)."
  )

  strategy_result <- compute_strategy_costs(model_parameters, price_index_table)

  validation_rows <- purrr::imap(targets, function(target_cost, strategy_name) {
    modeled_cost <- strategy_result$strategy_costs$expected_total_cost[
      strategy_result$strategy_costs$strategy == strategy_name
    ]

    if (length(modeled_cost) != 1) {
      base::stop("No modeled cost found for strategy '", strategy_name, "'.")
    }

    pct_difference <- 100 * (modeled_cost - target_cost) / target_cost

    tibble::tibble(
      strategy = strategy_name,
      modeled_cost = modeled_cost,
      target_cost = target_cost,
      pct_difference = pct_difference,
      within_tolerance = base::abs(pct_difference) <= tolerance_pct
    )
  })

  validation_tbl <- dplyr::bind_rows(validation_rows)

  purrr::pwalk(validation_tbl, function(strategy, modeled_cost, target_cost, pct_difference, within_tolerance) {
    base::message(
      "  ", strategy, ": modeled $", base::round(modeled_cost, 2),
      " vs. target $", base::round(target_cost, 2), " (",
      base::round(pct_difference, 1), "% difference, ",
      if (within_tolerance) "within" else "OUTSIDE", " tolerance)"
    )
  })

  validation_tbl
}

#' Status of every planned external-validation target
#'
#' @return A tibble documenting each candidate published model, whether
#'   its internal parameters have been extracted into this repository,
#'   and what is still needed. Deliberately does NOT attempt to reproduce
#'   Yi et al. 2018 or Havrilesky et al. 2009's results. Havrilesky's
#'   internal parameters have not been extracted from the primary source.
#'   Yi et al. 2018's have (2026-08-31), but their decision tree models
#'   diagnostic sensitivity/specificity, disease prevalence, treatment
#'   cost, and life-expectancy effectiveness that this repository's
#'   cost-only compute_strategy_costs() engine does not implement --
#'   fabricating internal parameters, or bolting on ad hoc adjustment
#'   factors, to force a numeric match to either study's known output
#'   would not be a real validation. See docs/validation_notes.md.
#' @export
literature_replication_status <- function() {
  base::message("Building literature-replication status table.")

  tibble::tribble(
    ~study, ~target_description, ~status, ~notes,
    "Ladabaum et al. 2011 (PMC3793257)",
    "Office EMB resource cost anchor: $224 (2010 dollars)",
    "cross_checked",
    "Not a decision-tree output to replicate -- it is a single cost input. This repository cross-checks its own inflation-adjustment machinery by reproducing the ~1.529x multiplier (2010->2026 real BLS CPI-U Medical Care) in the office_cost_ladabaum_historical scenario (R/scenarios.R) and tests/testthat/test-model-identity.R. That is a unit-level cross-check, not an external validation of the three-strategy model.",
    "Yi et al. 2018 (PubMed 29747864)",
    "Pipelle $1,897.80 vs. D&C $2,999.11 (2017 Medicare dollars)",
    "extracted_structurally_incomparable",
    "Full internal parameter set extracted 2026-08-31 from the primary source (Table 1: sampling success probabilities, Pipelle/D&C sensitivity/specificity, EC prevalence, life expectancy by outcome, procedure/treatment costs). NOT numerically reproduced, and this has been confirmed to be a structural mismatch rather than a missing-data problem: Yi et al.'s decision tree models diagnostic sensitivity/specificity, EC prevalence, treatment cost (hysterectomy), and life-expectancy effectiveness with an explicit repeat-Pipelle-then-D&C branch; this repository's compute_strategy_costs() is a cost-only engine with no diagnostic-accuracy or effectiveness machinery. Plugging Yi's cost inputs into this repository's simpler formula would not reproduce their output and would not constitute real validation -- see docs/validation_notes.md for the full comparison and the convergent cost-only cross-check performed instead. Do not attempt to force a numeric match by adding ad hoc adjustment factors.",
    "Havrilesky et al. 2009",
    "Cost-effectiveness of annual endometrial cancer screening strategies",
    "pending_parameter_extraction",
    "Identified as a likely source for EMB/TVUS follow-up and diagnostic-D&C cost parameters (referenced by Ladabaum et al. 2011 for TAH-BSO and cancer-treatment costs), but not yet obtained or extracted. No target output has been recorded."
  )
}
