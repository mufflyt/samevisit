# Threshold analysis
#
# Answers "how far can parameter X move before the conclusion flips?"
# using root-finding (`stats::uniroot()`) on the full strategy-cost model
# rather than a closed-form formula, so every threshold automatically
# accounts for second-order effects -- e.g. raising the D&C cost also
# raises the office and combined arms' expected cost through their
# escalation-to-D&C branches (see `R/strategy_costs.R`).

#' Generic threshold finder
#'
#' @param model_parameters Tibble from [load_model_parameters()].
#' @param parameter_name Character scalar parameter to vary.
#' @param target_metric_fn Function of `strategy_costs` returning a
#'   numeric scalar whose root (crossing `target_value`) is of interest.
#' @param price_index_table Tibble from [load_price_index_table()].
#' @param search_lower,search_upper Numeric scalars bounding the search
#'   interval for `parameter_name`.
#' @param target_value Numeric scalar the metric should cross. Default 0.
#' @param strategy_cost_fn Passed to [evaluate_metric_at()]. Defaults to
#'   [compute_strategy_costs()]; pass a different function to find a
#'   threshold over a different set of strategies.
#' @return A tibble with `parameter`, `threshold_value`, `converged`
#'   (logical), and `search_lower`/`search_upper` for provenance. If no
#'   sign change is found in the search interval, `threshold_value` is
#'   `NA` and `converged` is `FALSE` (with a message explaining why,
#'   rather than a silent failure).
#' @export
find_parameter_threshold <- function(
  model_parameters,
  parameter_name,
  target_metric_fn,
  price_index_table = load_price_index_table(),
  search_lower,
  search_upper,
  target_value = 0,
  strategy_cost_fn = compute_strategy_costs
) {
  base::message(
    "Threshold search on '", parameter_name, "' over [",
    search_lower, ", ", search_upper, "]."
  )

  objective_fn <- function(candidate_value) {
    evaluate_metric_at(
      model_parameters, parameter_name, candidate_value,
      price_index_table, target_metric_fn, strategy_cost_fn
    ) - target_value
  }

  value_at_lower <- objective_fn(search_lower)
  value_at_upper <- objective_fn(search_upper)

  if (base::sign(value_at_lower) == base::sign(value_at_upper)) {
    base::message(
      "  No sign change across the search interval -- the metric does not ",
      "cross the target value for any '", parameter_name, "' in [",
      search_lower, ", ", search_upper, "]. Widen the interval if a ",
      "threshold is expected to exist."
    )
    return(tibble::tibble(
      parameter = parameter_name,
      threshold_value = NA_real_,
      converged = FALSE,
      search_lower = search_lower,
      search_upper = search_upper
    ))
  }

  root_result <- stats::uniroot(objective_fn, lower = search_lower, upper = search_upper)

  base::message("  Threshold found at ", parameter_name, " = ", base::round(root_result$root, 4))

  tibble::tibble(
    parameter = parameter_name,
    threshold_value = root_result$root,
    converged = TRUE,
    search_lower = search_lower,
    search_upper = search_upper
  )
}

#' Metric: combined_emb cost minus the cheaper of the two alternatives
#'
#' Used to ask "how far can a parameter move before combined_emb stops
#' being the least expensive strategy?"
#'
#' @param strategy_costs Tibble from
#'   `compute_strategy_costs()$strategy_costs`.
#' @return Numeric scalar.
#' @export
metric_combined_vs_min_other <- function(strategy_costs) {
  combined_cost <- strategy_costs$expected_total_cost[
    strategy_costs$strategy == "combined_emb"
  ]
  other_costs <- strategy_costs$expected_total_cost[
    strategy_costs$strategy != "combined_emb"
  ]
  combined_cost - base::min(other_costs)
}

#' Metric: D&C cost minus the more expensive of the two alternatives
#'
#' Crossing zero means D&C is dominated (strictly more expensive than
#' both office_emb and combined_emb).
#'
#' @inheritParams metric_combined_vs_min_other
#' @return Numeric scalar.
#' @export
metric_dnc_dominated <- function(strategy_costs) {
  dnc_cost <- strategy_costs$expected_total_cost[strategy_costs$strategy == "dnc"]
  other_costs <- strategy_costs$expected_total_cost[
    strategy_costs$strategy != "dnc"
  ]
  dnc_cost - base::max(other_costs)
}

#' Threshold: maximum incremental colonoscopy-suite minutes
#'
#' How much additional colonoscopy-suite time can EMB require before the
#' combined strategy stops being less expensive than the cheaper
#' alternative?
#'
#' @inheritParams find_parameter_threshold
#' @param search_upper Numeric scalar, the upper bound of the search
#'   interval (the lower bound is fixed at 0).
#' @return A one-row tibble, see [find_parameter_threshold()].
#' @export
threshold_combined_added_minutes <- function(
  model_parameters,
  price_index_table = load_price_index_table(),
  search_upper = 200
) {
  find_parameter_threshold(
    model_parameters, "combined_emb_added_minutes",
    metric_combined_vs_min_other, price_index_table,
    search_lower = 0, search_upper = search_upper
  )
}

#' Threshold: office EMB failure probability
#'
#' At what probability of failed office EMB does coordinated colonoscopy
#' sampling become cost-saving relative to standalone office EMB?
#'
#' @inheritParams find_parameter_threshold
#' @return A one-row tibble, see [find_parameter_threshold()].
#' @export
threshold_office_failure_probability <- function(
  model_parameters,
  price_index_table = load_price_index_table()
) {
  find_parameter_threshold(
    model_parameters, "emb_failure_lynch",
    metric_combined_vs_office_incremental, price_index_table,
    search_lower = 1e-4, search_upper = 1 - 1e-4
  )
}

#' Threshold: D&C cost at which D&C becomes dominated
#'
#' How expensive does operative D&C need to become (via its facility fee)
#' before it is dominated by both other strategies?
#'
#' @inheritParams find_parameter_threshold
#' @param search_upper Numeric scalar, the upper bound of the search
#'   interval (the lower bound is fixed at 0).
#' @return A one-row tibble, see [find_parameter_threshold()].
#' @export
threshold_dnc_dominated_facility_fee <- function(
  model_parameters,
  price_index_table = load_price_index_table(),
  search_upper = 20000
) {
  find_parameter_threshold(
    model_parameters, "dnc_facility_or_asc_fee",
    metric_dnc_dominated, price_index_table,
    search_lower = 0, search_upper = search_upper
  )
}

#' Threshold: maximum coordination cost
#'
#' How much coordination/staffing cost can be added to the combined
#' strategy while it retains a cost advantage over standalone office EMB?
#'
#' @inheritParams find_parameter_threshold
#' @param search_upper Numeric scalar, the upper bound of the search
#'   interval (the lower bound is fixed at 0).
#' @return A one-row tibble, see [find_parameter_threshold()].
#' @export
threshold_coordination_cost_ceiling <- function(
  model_parameters,
  price_index_table = load_price_index_table(),
  search_upper = 5000
) {
  find_parameter_threshold(
    model_parameters, "coordination_cost",
    metric_combined_vs_office_incremental, price_index_table,
    search_lower = 0, search_upper = search_upper
  )
}

#' Run every named threshold analysis and bind the results
#'
#' @param model_parameters Tibble from [load_model_parameters()].
#' @param price_index_table Tibble from [load_price_index_table()].
#' @return A tibble binding the results of every `threshold_*()`
#'   convenience function, with a `question` column describing each row
#'   in plain language.
#' @export
run_threshold_analyses <- function(
  model_parameters,
  price_index_table = load_price_index_table()
) {
  base::message("Running the full set of threshold analyses.")

  minutes_threshold <- threshold_combined_added_minutes(
    model_parameters, price_index_table
  )
  failure_threshold <- threshold_office_failure_probability(
    model_parameters, price_index_table
  )
  dnc_threshold <- threshold_dnc_dominated_facility_fee(
    model_parameters, price_index_table
  )
  coordination_threshold <- threshold_coordination_cost_ceiling(
    model_parameters, price_index_table
  )

  threshold_estimates <- dplyr::bind_rows(
    minutes_threshold %>%
      dplyr::mutate(
        question = "Max incremental colonoscopy-suite minutes before combined stops being cheapest"
      ),
    failure_threshold %>%
      dplyr::mutate(
        question = "Office EMB failure probability at which combined becomes cost-saving vs. office"
      ),
    dnc_threshold %>%
      dplyr::mutate(
        question = "D&C facility fee at which D&C becomes dominated by both alternatives"
      ),
    coordination_threshold %>%
      dplyr::mutate(
        question = "Max coordination cost before combined loses its advantage vs. office"
      )
  ) %>%
    dplyr::select("question", dplyr::everything())

  base::message("Threshold analyses complete.")

  threshold_estimates
}
