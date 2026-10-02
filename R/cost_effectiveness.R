# Cost-consequence secondary analysis (cost per additional true case detected)
#
# The base case is a cost-minimization analysis: it assumes equivalent
# diagnostic effectiveness across strategies (see
# `docs/methods_notes.md`'s "This is a cost-minimization analysis, not a
# cost-effectiveness analysis"). `R/diagnostic_yield.R::compute_diagnostic_yield()`
# already computes a genuine effectiveness measure -- each strategy's
# overall probability of detecting a true cancer or precancer, using
# published (non-Lynch) Pipelle/D&C sensitivities (Sakna et al. 2023) --
# but that secondary analysis only reports detection probabilities, not
# cost per unit of effectiveness. This file adds that missing piece: a
# deliberately LIGHTER alternative to a full cost-utility analysis (no
# QALYs, no stage-shift/survival model, no utility values -- see
# `docs/methods_notes.md` for why a full cost-utility extension is a much
# larger undertaking this repository does not attempt). It computes
# incremental cost-effectiveness ratios (ICERs) directly from data this
# repository already has: `compute_strategy_costs()`'s healthcare-sector
# cost and `compute_diagnostic_yield()`'s detection probability.
#
# **What an ICER means here:** dollars per additional expected true-positive
# case detected, per patient screened -- e.g., an ICER of $20,000 between
# two strategies means switching from the cheaper to the costlier strategy
# costs an additional $20,000, on average, for every additional expected
# true cancer case detected across a cohort of patients screened. No
# disease-prevalence parameter is needed for this interpretation: dividing
# a cost difference by a detection-*probability* difference already yields
# dollars per additional expected case in a cohort of any size (the
# prevalence cancels out of the ratio).
#
# **Standard health-economic dominance rules, both implemented:**
# - **Strict dominance:** a strategy is dominated if another strategy costs
#   the same or less AND detects the same or more (this repository's real
#   three strategies never happen to trigger this, but the function
#   handles it correctly for scenarios/disease types where they might).
# - **Extended (weak) dominance:** among strategies that survive strict
#   dominance, sorted by cost, ICERs computed strategy-to-strategy along
#   the frontier must increase monotonically. If they do not, the middle
#   strategy is excluded (a combination of its cheaper and costlier
#   neighbors would be more efficient), and the remaining frontier is
#   re-checked.
#
# **Deliberately NOT PSA-integrated**, matching `compute_diagnostic_yield()`'s
# own scope discipline: a deterministic point-estimate secondary analysis,
# not folded into the probabilistic sensitivity analysis.

#' Compute incremental cost-effectiveness ratios from a generic cost/effect table
#'
#' Domain-agnostic: works on any tibble with `strategy`, `cost`, and
#' `effect` columns (higher `effect` is always better).
#'
#' @param cost_effect_table Tibble with columns `strategy` (character),
#'   `cost` (numeric), `effect` (numeric).
#' @return A tibble, one row per strategy, sorted by cost ascending, with
#'   `status` (`"on_frontier"`, `"dominated"`, or `"extendedly_dominated"`)
#'   and `icer` (numeric; `NA` for the cheapest frontier strategy and for
#'   any non-frontier strategy -- an ICER is only defined as the
#'   incremental cost/effect between adjacent frontier strategies).
#' @export
compute_incremental_cost_effectiveness <- function(cost_effect_table) {
  required_columns <- c("strategy", "cost", "effect")
  missing_columns <- base::setdiff(required_columns, base::names(cost_effect_table))
  if (length(missing_columns) > 0) {
    base::stop(
      "cost_effect_table is missing required column(s): ",
      base::paste(missing_columns, collapse = ", ")
    )
  }

  ordered <- cost_effect_table %>% dplyr::arrange(.data$cost)
  n_strategies <- base::nrow(ordered)
  status <- base::rep("on_frontier", n_strategies)

  # Strict dominance: strategy i is dominated if some other strategy j
  # costs the same or less AND detects the same or more, and the two rows
  # are not identical on both dimensions.
  for (i in base::seq_len(n_strategies)) {
    for (j in base::seq_len(n_strategies)) {
      if (i == j) next
      cheaper_or_equal <- ordered$cost[[j]] <= ordered$cost[[i]]
      at_least_as_effective <- ordered$effect[[j]] >= ordered$effect[[i]]
      identical_point <- base::isTRUE(ordered$cost[[j]] == ordered$cost[[i]]) &&
        base::isTRUE(ordered$effect[[j]] == ordered$effect[[i]])
      if (cheaper_or_equal && at_least_as_effective && !identical_point) {
        status[[i]] <- "dominated"
      }
    }
  }

  # Extended dominance: among the surviving frontier, sorted by cost,
  # stepwise ICERs must increase monotonically. Repeat until they do (or
  # fewer than 3 frontier strategies remain, at which point monotonicity
  # is vacuously satisfied).
  repeat {
    frontier_rows <- base::which(status == "on_frontier")
    if (base::length(frontier_rows) < 3) break

    frontier <- ordered[frontier_rows, ] %>% dplyr::arrange(.data$cost)
    step_icers <- base::rep(NA_real_, base::nrow(frontier))
    for (k in 2:base::nrow(frontier)) {
      step_icers[[k]] <- (frontier$cost[[k]] - frontier$cost[[k - 1]]) /
        (frontier$effect[[k]] - frontier$effect[[k - 1]])
    }

    violation_index <- NA_integer_
    for (k in 3:base::nrow(frontier)) {
      if (!base::is.na(step_icers[[k]]) && !base::is.na(step_icers[[k - 1]]) &&
            step_icers[[k]] < step_icers[[k - 1]]) {
        violation_index <- k - 1
        break
      }
    }

    if (base::is.na(violation_index)) break

    extendedly_dominated_strategy <- frontier$strategy[[violation_index]]
    status[ordered$strategy == extendedly_dominated_strategy] <- "extendedly_dominated"
  }

  frontier_rows <- base::which(status == "on_frontier")
  frontier <- ordered[frontier_rows, ] %>% dplyr::arrange(.data$cost)
  icer <- base::rep(NA_real_, base::nrow(frontier))
  if (base::nrow(frontier) >= 2) {
    for (k in 2:base::nrow(frontier)) {
      icer[[k]] <- (frontier$cost[[k]] - frontier$cost[[k - 1]]) /
        (frontier$effect[[k]] - frontier$effect[[k - 1]])
    }
  }
  frontier$icer <- icer

  non_frontier <- ordered[status != "on_frontier", ]
  if (base::nrow(non_frontier) > 0) {
    non_frontier$icer <- NA_real_
  }

  dplyr::bind_rows(frontier, non_frontier) %>%
    dplyr::mutate(status = status[base::match(.data$strategy, ordered$strategy)]) %>%
    dplyr::arrange(.data$cost)
}

#' Compute cost-per-additional-case-detected ICERs for one disease type
#'
#' Thin domain wrapper: assembles the cost/effect table from
#' [compute_strategy_costs()] and [compute_diagnostic_yield()] and passes
#' it to [compute_incremental_cost_effectiveness()].
#'
#' @param model_parameters Tibble from [load_model_parameters()].
#' @param price_index_table Tibble from [load_price_index_table()]
#'   (medical-care CPI, for healthcare-sector costs).
#' @param disease Character scalar, `"cancer"` or `"precancer"`.
#' @return A tibble, one row per strategy: `strategy`, `disease`, `cost`
#'   (healthcare-sector `expected_total_cost`), `effect`
#'   (`detection_probability`), `status`, `icer`.
#' @export
compute_diagnostic_yield_cost_effectiveness <- function(
  model_parameters,
  price_index_table,
  disease = c("cancer", "precancer")
) {
  disease <- base::match.arg(disease)
  base::message(
    "Computing cost-per-additional-case-detected ICERs for disease type: ", disease
  )

  cost_result <- compute_strategy_costs(model_parameters, price_index_table)
  yield_result <- compute_diagnostic_yield(model_parameters, disease = disease)

  cost_effect_table <- yield_result %>%
    dplyr::select("strategy", "disease", "detection_probability") %>%
    dplyr::left_join(
      cost_result$strategy_costs %>%
        dplyr::select("strategy", "expected_total_cost"),
      by = "strategy"
    ) %>%
    dplyr::rename(cost = "expected_total_cost", effect = "detection_probability")

  result <- compute_incremental_cost_effectiveness(
    cost_effect_table %>% dplyr::select("strategy", "cost", "effect")
  ) %>%
    dplyr::left_join(
      cost_effect_table %>% dplyr::select("strategy", "disease"),
      by = "strategy"
    ) %>%
    dplyr::select("strategy", "disease", "cost", "effect", "status", "icer")

  base::message("Cost-effectiveness frontier (lowest to highest cost):")
  purrr::pwalk(
    result,
    function(strategy, disease, cost, effect, status, icer, ...) {
      icer_text <- if (base::is.na(icer)) "NA" else base::paste0("$", base::round(icer, 2))
      base::message(
        "  ", strategy, ": $", base::round(cost, 2), ", detection ",
        base::round(effect, 4), ", ", status, ", ICER ", icer_text
      )
    }
  )

  result
}
