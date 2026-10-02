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

#' Incremental cost of one strategy relative to another, by name
#'
#' The generic comparison behind [compare_combined_vs_office()]. Useful
#' directly when a project's strategy names aren't `office_emb`/
#' `combined_emb` -- any two `strategy` values present in `strategy_costs`
#' can be compared.
#'
#' @param strategy_costs Tibble from
#'   `compute_strategy_costs()$strategy_costs` or
#'   `compute_multi_strategy_costs()$strategy_costs`.
#' @param strategy_a Character scalar, a value in `strategy_costs$strategy`.
#' @param strategy_b Character scalar, a value in `strategy_costs$strategy`
#'   that `strategy_a` is compared against.
#' @return A one-row tibble: `strategy_a`, `strategy_a_cost`, `strategy_b`,
#'   `strategy_b_cost`, `incremental_cost` (`strategy_a_cost -
#'   strategy_b_cost`), `pct_difference`, `strategy_a_is_cost_saving`.
#' @export
compare_two_strategies <- function(strategy_costs, strategy_a, strategy_b) {
  cost_a <- strategy_costs$expected_total_cost[
    strategy_costs$strategy == strategy_a
  ]
  cost_b <- strategy_costs$expected_total_cost[
    strategy_costs$strategy == strategy_b
  ]

  if (length(cost_a) != 1 || length(cost_b) != 1) {
    base::stop(
      "compare_two_strategies() requires exactly one '", strategy_a,
      "' and one '", strategy_b, "' row in strategy_costs."
    )
  }

  incremental_cost <- cost_a - cost_b
  pct_difference <- if (cost_b == 0) {
    NA_real_
  } else {
    100 * incremental_cost / cost_b
  }

  base::message(
    "Incremental cost of ", strategy_a, " vs. ", strategy_b, ": $",
    base::round(incremental_cost, 2), " (",
    base::round(pct_difference, 1), "%)."
  )

  tibble::tibble(
    strategy_a = strategy_a,
    strategy_a_cost = cost_a,
    strategy_b = strategy_b,
    strategy_b_cost = cost_b,
    incremental_cost = incremental_cost,
    pct_difference = pct_difference,
    strategy_a_is_cost_saving = incremental_cost < 0
  )
}

#' Incremental cost of adding EMB to colonoscopy vs. performing EMB separately
#'
#' A thin, exact-output-preserving wrapper over [compare_two_strategies()].
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
  comparison <- compare_two_strategies(strategy_costs, "combined_emb", "office_emb")

  tibble::tibble(
    combined_emb_cost = comparison$strategy_a_cost,
    office_emb_cost = comparison$strategy_b_cost,
    incremental_cost_combined_vs_office = comparison$incremental_cost,
    pct_difference_combined_vs_office = comparison$pct_difference,
    combined_is_cost_saving = comparison$strategy_a_is_cost_saving
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
