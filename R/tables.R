# Table generation
#
# Formats analysis outputs into plain, human-readable tables and writes
# them to `tables/` as CSV (kept dependency-light -- no `gt`/`kableExtra`
# requirement -- consistent with `colpocleisis_costeff`'s CSV-first
# convention).

#' Save a tibble to `tables/` with a timestamped or fixed filename
#'
#' @param table_data Tibble to save.
#' @param file_name Character scalar, e.g. `"strategy_costs.csv"`.
#' @param output_dir Character scalar directory. Default `"tables"`.
#' @return Invisibly, the path the file was saved to.
#' @export
save_table <- function(table_data, file_name, output_dir = "tables") {
  if (!base::dir.exists(output_dir)) {
    base::message("Creating output directory: ", output_dir)
    base::dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  output_path <- file.path(output_dir, file_name)
  readr::write_csv(table_data, output_path)
  base::message("Saved table to: ", output_path)

  base::invisible(output_path)
}

#' Build the primary human-readable strategy comparison table
#'
#' @param strategy_comparison Tibble from
#'   [compare_strategies_to_cheapest()].
#' @return A tibble formatted for reporting: dollar amounts rounded to
#'   the nearest cent, strategy labels applied.
#' @export
build_strategy_comparison_table <- function(strategy_comparison) {
  strategy_comparison %>%
    dplyr::mutate(
      strategy_label = STRATEGY_LABELS[.data$strategy],
      initial_cost = base::round(.data$initial_cost, 2),
      escalation_probability = base::round(.data$escalation_probability, 4),
      escalation_cost = base::round(.data$escalation_cost, 2),
      expected_total_cost = base::round(.data$expected_total_cost, 2),
      incremental_cost_vs_cheapest = base::round(.data$incremental_cost_vs_cheapest, 2),
      pct_difference_vs_cheapest = base::round(.data$pct_difference_vs_cheapest, 1)
    ) %>%
    dplyr::select(
      "strategy", "strategy_label", "initial_cost", "escalation_probability",
      "escalation_cost", "expected_total_cost", "incremental_cost_vs_cheapest",
      "pct_difference_vs_cheapest", "is_cheapest"
    )
}

#' Build the PSA cost-distribution summary table, joined with cheapest-strategy probabilities
#'
#' Extracted from `analysis/07_manuscript_outputs.R` (2026-08-31) after
#' running that script and finding `n_draws_cheapest`/`pct_draws_cheapest`
#' were `NA` for every row: the inline pivot produced a `strategy` column
#' with a `"_cost"` suffix (`"office_emb_cost"`), which never matched
#' [summarize_probability_cheapest()]'s bare strategy names
#' (`"office_emb"`), so the join silently matched nothing. Pulling this into
#' a standalone, tested function makes that failure mode a regression test
#' rather than something only caught by manually reading a manuscript table.
#'
#' @param probabilistic_estimates Tibble from [run_probabilistic_sensitivity()].
#' @return A tibble with one row per strategy: `strategy`, `mean_cost`,
#'   `sd_cost`, `p2_5`, `p97_5`, `n_draws_cheapest`, `pct_draws_cheapest`.
#' @export
build_psa_summary_table <- function(probabilistic_estimates) {
  cost_summary <- probabilistic_estimates %>%
    tidyr::pivot_longer(
      cols = c("office_emb_cost", "combined_emb_cost", "dnc_cost"),
      names_to = "strategy", values_to = "expected_total_cost"
    ) %>%
    dplyr::mutate(strategy = base::sub("_cost$", "", .data$strategy)) %>%
    dplyr::group_by(.data$strategy) %>%
    dplyr::summarise(
      mean_cost = base::mean(.data$expected_total_cost),
      sd_cost = stats::sd(.data$expected_total_cost),
      p2_5 = stats::quantile(.data$expected_total_cost, 0.025),
      p97_5 = stats::quantile(.data$expected_total_cost, 0.975),
      .groups = "drop"
    )

  cost_summary %>%
    dplyr::left_join(
      summarize_probability_cheapest(probabilistic_estimates),
      by = "strategy"
    ) %>%
    # A strategy absent from summarize_probability_cheapest()'s output
    # (e.g. D&C, in a small/skewed PSA sample) was cheapest in exactly 0
    # draws -- a real zero, not missing data. Left uncorrected, the join
    # above turns that into NA, which reads as "unknown" rather than "never
    # won" in a manuscript table.
    dplyr::mutate(
      n_draws_cheapest = dplyr::coalesce(.data$n_draws_cheapest, 0L),
      pct_draws_cheapest = dplyr::coalesce(.data$pct_draws_cheapest, 0)
    )
}

#' Build a dynamic, model-driven summary sentence
#'
#' Generates the headline sentence from live model output rather than a
#' hard-coded narrative, following the `summary_sentence` pattern in
#' `colpocleisis_costeff`.
#'
#' @param strategy_comparison Tibble from
#'   [compare_strategies_to_cheapest()].
#' @param combined_vs_office Tibble from [compare_combined_vs_office()].
#' @param threshold_estimates Tibble from [run_threshold_analyses()].
#' @return A character scalar.
#' @export
build_summary_sentence <- function(
  strategy_comparison,
  combined_vs_office,
  threshold_estimates
) {
  get_cost <- function(strategy_name) {
    strategy_comparison$expected_total_cost[
      strategy_comparison$strategy == strategy_name
    ]
  }

  minutes_threshold <- threshold_estimates$threshold_value[
    threshold_estimates$parameter == "combined_emb_added_minutes"
  ]

  minutes_clause <- if (length(minutes_threshold) == 1 && !base::is.na(minutes_threshold)) {
    base::paste0(
      "remains the least expensive strategy as long as the incremental ",
      "colonoscopy-suite time stays below approximately ",
      base::round(minutes_threshold, 1), " minutes"
    )
  } else {
    "remained the least expensive strategy across the tested range of incremental colonoscopy-suite time"
  }

  base::paste0(
    "Among a modeled cohort of patients with Lynch syndrome already undergoing ",
    "surveillance colonoscopy, performing endometrial biopsy during the same ",
    "sedated encounter cost an estimated ", scales::dollar(get_cost("combined_emb")),
    " per patient, compared with ", scales::dollar(get_cost("office_emb")),
    " for standalone office biopsy and ", scales::dollar(get_cost("dnc")),
    " for operative dilation and curettage. Adding EMB to colonoscopy cost ",
    scales::dollar(base::abs(combined_vs_office$incremental_cost_combined_vs_office)),
    if (combined_vs_office$combined_is_cost_saving) " less than" else " more than",
    " performing EMB separately (",
    base::round(base::abs(combined_vs_office$pct_difference_combined_vs_office), 1),
    "%). The combined strategy ", minutes_clause, "."
  )
}
