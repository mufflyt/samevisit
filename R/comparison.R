# Strategy comparison
#
# Builds the primary-analysis comparison table: incremental cost versus
# the least expensive strategy, absolute and percentage differences
# between every pair of strategies, and the specific incremental cost of
# adding EMB to an already-planned colonoscopy compared with performing
# EMB as a separate office procedure -- the central economic quantity
# this repository exists to estimate (see docs/methods_notes.md).

#' Compare strategies against the least expensive option
#'
#' @param strategy_costs Tibble from
#'   `compute_strategy_costs()$strategy_costs`.
#' @return `strategy_costs` with added columns `incremental_cost_vs_cheapest`,
#'   `pct_difference_vs_cheapest`, and `is_cheapest`.
#' @export
compare_strategies_to_cheapest <- function(strategy_costs) {
  base::message("Comparing strategies to the least expensive alternative.")

  cheapest_cost <- base::min(strategy_costs$expected_total_cost)
  cheapest_strategy <- strategy_costs$strategy[
    strategy_costs$expected_total_cost == cheapest_cost
  ][[1]]

  strategy_comparison <- strategy_costs %>%
    dplyr::mutate(
      incremental_cost_vs_cheapest = .data$expected_total_cost - cheapest_cost,
      pct_difference_vs_cheapest = if (cheapest_cost == 0) {
        NA_real_
      } else {
        100 * .data$incremental_cost_vs_cheapest / cheapest_cost
      },
      is_cheapest = .data$strategy == cheapest_strategy
    ) %>%
    dplyr::arrange(.data$expected_total_cost)

  base::message("  Cheapest strategy: ", cheapest_strategy)

  strategy_comparison
}

#' Incremental cost of adding EMB to colonoscopy vs. performing EMB separately
#'
#' Reports `expected_total_cost[combined_emb] - expected_total_cost[office_emb]`
#' -- the direct answer to "how much does coordinating biopsy with
#' colonoscopy save (or cost) relative to arranging it separately?"
#'
#' @param strategy_costs Tibble from
#'   `compute_strategy_costs()$strategy_costs`.
#' @return A one-row tibble with the incremental cost and percent
#'   difference of `combined_emb` relative to `office_emb`.
#' @export
compare_combined_vs_office <- function(strategy_costs) {
  combined_cost <- strategy_costs$expected_total_cost[
    strategy_costs$strategy == "combined_emb"
  ]
  office_cost <- strategy_costs$expected_total_cost[
    strategy_costs$strategy == "office_emb"
  ]

  if (length(combined_cost) != 1 || length(office_cost) != 1) {
    base::stop(
      "compare_combined_vs_office() requires exactly one 'combined_emb' ",
      "and one 'office_emb' row in strategy_costs."
    )
  }

  incremental_cost <- combined_cost - office_cost
  pct_difference <- if (office_cost == 0) {
    NA_real_
  } else {
    100 * incremental_cost / office_cost
  }

  base::message(
    "Incremental cost of combined_emb vs. office_emb: $",
    base::round(incremental_cost, 2), " (",
    base::round(pct_difference, 1), "%)."
  )

  tibble::tibble(
    combined_emb_cost = combined_cost,
    office_emb_cost = office_cost,
    incremental_cost_combined_vs_office = incremental_cost,
    pct_difference_combined_vs_office = pct_difference,
    combined_is_cost_saving = incremental_cost < 0
  )
}

#' Build the full pairwise strategy-comparison table
#'
#' @param strategy_costs Tibble from
#'   `compute_strategy_costs()$strategy_costs`.
#' @return A tibble with one row per ordered pair of strategies, giving
#'   the absolute and percentage cost difference.
#' @export
build_pairwise_comparison_table <- function(strategy_costs) {
  strategy_pairs <- tidyr::expand_grid(
    strategy_a = strategy_costs$strategy,
    strategy_b = strategy_costs$strategy
  ) %>%
    dplyr::filter(.data$strategy_a != .data$strategy_b)

  strategy_pairs %>%
    dplyr::left_join(
      strategy_costs %>% dplyr::select("strategy", cost_a = "expected_total_cost"),
      by = c("strategy_a" = "strategy")
    ) %>%
    dplyr::left_join(
      strategy_costs %>% dplyr::select("strategy", cost_b = "expected_total_cost"),
      by = c("strategy_b" = "strategy")
    ) %>%
    dplyr::mutate(
      absolute_difference = .data$cost_a - .data$cost_b,
      pct_difference = dplyr::if_else(
        .data$cost_b == 0,
        NA_real_,
        100 * .data$absolute_difference / .data$cost_b
      )
    ) %>%
    dplyr::select(
      "strategy_a", "strategy_b", "cost_a", "cost_b",
      "absolute_difference", "pct_difference"
    )
}
