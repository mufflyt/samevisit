# Budget impact
#
# Scales the per-patient incremental cost from [compare_combined_vs_office()]
# (or any pairwise comparison from `R/comparison.R`) up to a cohort size,
# answering "what would a health system serving N Lynch patients per year
# expect to save (or spend) by adopting the combined strategy?"

#' Estimate cohort-level savings from a per-patient incremental cost
#'
#' @param per_patient_incremental_cost Numeric scalar. Combined-arm cost
#'   minus comparator-arm cost, per patient (negative = combined is
#'   cheaper). Typically
#'   `compare_combined_vs_office(strategy_costs)$incremental_cost_combined_vs_office`.
#' @param cohort_sizes Numeric vector of annual patient volumes to
#'   evaluate. Default `c(10, 25, 50, 100, 1000)`.
#' @param comparator_label Character scalar naming the strategy combined
#'   EMB is being compared against, for the `comparator` column.
#' @return A tibble with one row per cohort size: `cohort_size`,
#'   `comparator`, `per_patient_savings`, `annual_savings`.
#' @export
estimate_budget_impact <- function(
  per_patient_incremental_cost,
  cohort_sizes = c(10, 25, 50, 100, 1000),
  comparator_label = "office_emb"
) {
  base::message(
    "Estimating budget impact vs. ", comparator_label,
    " across cohort sizes: ", base::paste(cohort_sizes, collapse = ", ")
  )

  per_patient_savings <- -per_patient_incremental_cost

  budget_impact_tbl <- tibble::tibble(
    cohort_size = cohort_sizes,
    comparator = comparator_label,
    per_patient_savings = per_patient_savings,
    annual_savings = per_patient_savings * cohort_sizes
  )

  purrr::walk2(
    budget_impact_tbl$cohort_size, budget_impact_tbl$annual_savings,
    ~ base::message(
      "  ", .x, " patients/year: ",
      if (.y >= 0) "$" else "-$",
      scales::comma(base::round(base::abs(.y), 0)),
      if (.y >= 0) " saved annually" else " additional cost annually"
    )
  )

  budget_impact_tbl
}

#' Budget impact of combined EMB vs. both alternatives
#'
#' @param strategy_costs Tibble from
#'   `compute_strategy_costs()$strategy_costs`.
#' @param cohort_sizes Numeric vector of annual patient volumes.
#' @return A tibble binding [estimate_budget_impact()] results for
#'   combined EMB vs. office EMB and vs. D&C.
#' @export
estimate_budget_impact_all_comparators <- function(
  strategy_costs,
  cohort_sizes = c(10, 25, 50, 100, 1000)
) {
  combined_cost <- strategy_costs$expected_total_cost[
    strategy_costs$strategy == "combined_emb"
  ]

  dplyr::bind_rows(
    estimate_budget_impact(
      compare_combined_vs_office(strategy_costs)$incremental_cost_combined_vs_office,
      cohort_sizes, "office_emb"
    ),
    estimate_budget_impact(
      combined_cost - strategy_costs$expected_total_cost[
        strategy_costs$strategy == "dnc"
      ],
      cohort_sizes, "dnc"
    )
  )
}
