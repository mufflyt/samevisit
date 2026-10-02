# Deterministic (one-way) sensitivity analysis
#
# Re-runs the full strategy-cost model with one parameter perturbed to
# its `low_value` and `high_value`, holding everything else at base
# case, and reports the effect on a chosen scalar metric. Adapted from
# the tornado-diagram pattern in `colpocleisis_costeff/generate_figures.R`
# (which looped a `get_preferred_nmb()` helper over a small hard-coded
# parameter table); here the mechanism is generalized to work over any
# row of `config/model_parameters.csv` and any target metric function.

#' Default target metric: incremental cost of combined_emb vs. office_emb
#'
#' @param strategy_costs Tibble from
#'   `compute_strategy_costs()$strategy_costs`.
#' @return Numeric scalar.
#' @export
metric_combined_vs_office_incremental <- function(strategy_costs) {
  compare_combined_vs_office(strategy_costs)$incremental_cost_combined_vs_office
}

#' Target metric factory: expected total cost of a named strategy
#'
#' @param strategy_name One of `"office_emb"`, `"dnc"`, `"combined_emb"`.
#' @return A function of `strategy_costs` returning that strategy's
#'   `expected_total_cost`.
#' @export
metric_expected_total_cost <- function(strategy_name) {
  function(strategy_costs) {
    strategy_costs$expected_total_cost[strategy_costs$strategy == strategy_name]
  }
}

#' Run the strategy-cost model with one parameter overridden
#'
#' @param model_parameters Tibble from [load_model_parameters()].
#' @param parameter_name Character scalar.
#' @param parameter_value Numeric scalar override value.
#' @param price_index_table Tibble from [load_price_index_table()].
#' @param target_metric_fn Function of `strategy_costs` returning a
#'   numeric scalar.
#' @param strategy_cost_fn Function of `(model_parameters,
#'   price_index_table = ...)` returning a list with a `strategy_costs`
#'   element, matching [compute_strategy_costs()]'s signature/return
#'   shape. Defaults to [compute_strategy_costs()]; pass a different
#'   function (e.g. one built on [compute_multi_strategy_costs()]) to run
#'   this same perturb-and-reread mechanism over a different set of
#'   strategies.
#' @return Numeric scalar: the target metric at `parameter_value`.
#' @export
evaluate_metric_at <- function(
  model_parameters,
  parameter_name,
  parameter_value,
  price_index_table,
  target_metric_fn,
  strategy_cost_fn = compute_strategy_costs
) {
  perturbed_parameters <- override_model_parameters(
    model_parameters,
    stats::setNames(list(parameter_value), parameter_name)
  )

  strategy_result <- strategy_cost_fn(
    perturbed_parameters,
    price_index_table = price_index_table
  )

  target_metric_fn(strategy_result$strategy_costs)
}

#' One-way (deterministic) sensitivity analysis across a set of parameters
#'
#' @param model_parameters Tibble from [load_model_parameters()].
#' @param parameter_names Character vector of parameter names to vary.
#'   Each must have non-missing `low_value`/`high_value` in
#'   `model_parameters`. Defaults to every non-fixed, non-provisional-only
#'   numeric parameter with bounds defined.
#' @param price_index_table Tibble from [load_price_index_table()].
#' @param target_metric_fn Function of `strategy_costs` returning a
#'   numeric scalar. Defaults to
#'   [metric_combined_vs_office_incremental()].
#' @param strategy_cost_fn Passed to [evaluate_metric_at()]. Defaults to
#'   [compute_strategy_costs()].
#' @return A tibble with one row per parameter: `parameter`,
#'   `base_value`, `low_value`, `high_value`, `metric_at_base`,
#'   `metric_at_low`, `metric_at_high`, and `spread` (the absolute range
#'   of the metric across low/high, used to rank a tornado plot).
#' @export
run_one_way_sensitivity <- function(
  model_parameters,
  parameter_names = model_parameters$parameter[
    !base::is.na(model_parameters$low_value) &
      !base::is.na(model_parameters$high_value)
  ],
  price_index_table = load_price_index_table(),
  target_metric_fn = metric_combined_vs_office_incremental,
  strategy_cost_fn = compute_strategy_costs
) {
  base::message(
    "Running one-way sensitivity analysis on ", length(parameter_names),
    " parameter(s)."
  )

  base_metric <- evaluate_metric_at(
    model_parameters,
    parameter_names[[1]],
    get_parameter_value(model_parameters, parameter_names[[1]]),
    price_index_table,
    target_metric_fn,
    strategy_cost_fn
  )

  sensitivity_rows <- purrr::map(parameter_names, function(parameter_name) {
    base::message("  Varying: ", parameter_name)

    parameter_row <- model_parameters %>%
      dplyr::filter(.data$parameter == parameter_name)

    metric_at_low <- evaluate_metric_at(
      model_parameters, parameter_name, parameter_row$low_value[[1]],
      price_index_table, target_metric_fn, strategy_cost_fn
    )
    metric_at_high <- evaluate_metric_at(
      model_parameters, parameter_name, parameter_row$high_value[[1]],
      price_index_table, target_metric_fn, strategy_cost_fn
    )

    tibble::tibble(
      parameter = parameter_name,
      base_value = base::as.numeric(parameter_row$base_value[[1]]),
      low_value = parameter_row$low_value[[1]],
      high_value = parameter_row$high_value[[1]],
      metric_at_base = base_metric,
      metric_at_low = metric_at_low,
      metric_at_high = metric_at_high,
      spread = base::abs(metric_at_high - metric_at_low)
    )
  })

  sensitivity_estimates <- dplyr::bind_rows(sensitivity_rows) %>%
    dplyr::arrange(dplyr::desc(.data$spread))

  base::message("One-way sensitivity analysis complete.")

  sensitivity_estimates
}
