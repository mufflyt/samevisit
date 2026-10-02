# Probabilistic sensitivity analysis (PSA)
#
# `colpocleisis_costeff` did not implement PSA -- only a deterministic
# one-way tornado -- so this module is new, not adapted. It follows the
# distributional convention used by the NIHR Lynch-syndrome
# gynecologic-surveillance economic model (NCBI Bookshelf NBK606810):
# costs are drawn from gamma distributions and probabilities from beta
# distributions.
#
# Because most `low_value`/`high_value` bounds in
# `config/model_parameters.csv` are plausible sensitivity ranges rather
# than formally estimated confidence intervals, this PSA implementation
# is a first-pass scaffold: it treats `low_value`/`high_value` as an
# approximate 95% interval around `base_value` (except where the source
# study reported a true SD, e.g. the per-minute OR/anesthesia costs,
# where `low_value`/`high_value` are mean +/- 1 SD -- treated the same
# way here for simplicity, which will understate their true variance).
# This approximation is documented, not hidden, and should be refined
# once better parameter-level uncertainty data are available.

#' Draw one Monte Carlo sample for a single parameter row
#'
#' @param parameter_row One-row slice of `model_parameters`.
#' @return Numeric scalar (or the original base value for `distribution
#'   == "fixed"`).
#' @export
draw_parameter_sample <- function(parameter_row) {
  distribution_type <- parameter_row$distribution[[1]]
  base_value <- base::as.numeric(parameter_row$base_value[[1]])

  if (distribution_type == "fixed" || base::is.na(distribution_type)) {
    return(base_value)
  }

  low_value <- parameter_row$low_value[[1]]
  high_value <- parameter_row$high_value[[1]]

  if (base::is.na(low_value) || base::is.na(high_value)) {
    return(base_value)
  }

  approx_sd <- (high_value - low_value) / (2 * 1.96)
  if (!base::is.finite(approx_sd) || approx_sd <= 0) {
    return(base_value)
  }

  if (distribution_type == "beta") {
    mean_value <- base::min(base::max(base_value, 1e-6), 1 - 1e-6)
    variance <- base::min(approx_sd^2, mean_value * (1 - mean_value) * 0.99)
    common_term <- (mean_value * (1 - mean_value) / variance) - 1
    shape1 <- mean_value * common_term
    shape2 <- (1 - mean_value) * common_term
    return(stats::rbeta(1, shape1, shape2))
  }

  if (distribution_type == "gamma") {
    explicit_alpha <- parameter_row$gamma_alpha[[1]]
    explicit_rate <- parameter_row$gamma_rate[[1]]
    if (!base::is.na(explicit_alpha) && !base::is.na(explicit_rate)) {
      return(stats::rgamma(1, shape = explicit_alpha, rate = explicit_rate))
    }

    shape_param <- (base_value / approx_sd)^2
    rate_param <- base_value / approx_sd^2
    return(stats::rgamma(1, shape = shape_param, rate = rate_param))
  }

  if (distribution_type == "triangular") {
    return(sample_triangular(low_value, base_value, high_value))
  }

  base_value
}

#' Sample from a triangular distribution
#'
#' A degenerate range (`min_value == max_value`) is treated the same way
#' `draw_parameter_sample()` already treats a degenerate beta/gamma range
#' (`approx_sd <= 0`, see above): fall back to the fixed value rather than
#' dividing by zero. `mode_value - min_value)/(max_value - min_value)` would
#' otherwise be `0/0 = NaN`, and `if (uniform_draw < NaN)` errors with
#' "missing value where TRUE/FALSE needed" -- not a silent wrong number, but
#' not the graceful degenerate-range handling this model uses everywhere
#' else either.
#'
#' @param min_value,mode_value,max_value Numeric scalars.
#' @return One draw.
#' @export
sample_triangular <- function(min_value, mode_value, max_value) {
  if (base::isTRUE(min_value == max_value)) {
    return(mode_value)
  }

  uniform_draw <- stats::runif(1)
  mode_fraction <- (mode_value - min_value) / (max_value - min_value)

  if (uniform_draw < mode_fraction) {
    min_value + base::sqrt(
      uniform_draw * (max_value - min_value) * (mode_value - min_value)
    )
  } else {
    max_value - base::sqrt(
      (1 - uniform_draw) * (max_value - min_value) * (max_value - mode_value)
    )
  }
}

#' Draw one full Monte Carlo parameter set
#'
#' @param model_parameters Tibble from [load_model_parameters()].
#' @return `model_parameters` with `base_value` replaced by one Monte
#'   Carlo draw for every row (fixed/boolean rows are left unchanged).
#' @export
draw_parameter_set <- function(model_parameters) {
  sampled_parameters <- model_parameters
  for (row_index in base::seq_len(nrow(model_parameters))) {
    parameter_row <- model_parameters[row_index, ]
    if (parameter_row$parameter == "combined_requires_preop_office_visit") {
      next
    }
    sampled_parameters$base_value[[row_index]] <- base::as.character(
      draw_parameter_sample(parameter_row)
    )
  }
  sampled_parameters
}

#' Run probabilistic sensitivity analysis
#'
#' @param model_parameters Tibble from [load_model_parameters()].
#' @param price_index_table Tibble from [load_price_index_table()].
#' @param n_simulations Integer number of Monte Carlo draws. Default 1000.
#' @param seed Integer scalar or `NULL`. Default `20260901` (the date PSA
#'   reproducibility was established -- see docs/testing_philosophy.md and
#'   CHANGELOG.md). The RNG state is saved before seeding and restored on
#'   exit, so calling this function does not affect random draws elsewhere
#'   in the caller's session. Pass `NULL` for an unseeded, genuinely random
#'   run (e.g. deliberately exploring draw-to-draw variability); any other
#'   integer reproduces a different but still-fixed sequence of draws.
#' @return A tibble with `n_simulations` rows, one per draw, giving each
#'   strategy's `expected_total_cost` and the combined-vs-office
#'   incremental cost.
#' @export
run_probabilistic_sensitivity <- function(
  model_parameters,
  price_index_table = load_price_index_table(),
  n_simulations = 1000,
  seed = 20260901
) {
  validate_positive(n_simulations, "n_simulations")

  if (!base::is.null(seed)) {
    old_seed <- if (base::exists(".Random.seed", envir = .GlobalEnv)) {
      base::get(".Random.seed", envir = .GlobalEnv)
    } else {
      NULL
    }
    base::on.exit({
      if (!base::is.null(old_seed)) {
        base::assign(".Random.seed", old_seed, envir = .GlobalEnv)
      } else if (base::exists(".Random.seed", envir = .GlobalEnv)) {
        base::rm(".Random.seed", envir = .GlobalEnv)
      }
    }, add = TRUE)
    base::set.seed(seed)
  }

  base::message(
    "Running probabilistic sensitivity analysis: ", n_simulations,
    " Monte Carlo draws",
    if (base::is.null(seed)) " (unseeded)." else base::paste0(" (seed ", seed, ").")
  )

  simulation_rows <- purrr::map(base::seq_len(n_simulations), function(draw_index) {
    if (draw_index %% 200 == 0) {
      base::message("  Draw ", draw_index, " / ", n_simulations)
    }

    sampled_parameters <- draw_parameter_set(model_parameters)
    strategy_result <- compute_strategy_costs(
      sampled_parameters,
      price_index_table = price_index_table
    )
    incremental_result <- compare_combined_vs_office(
      strategy_result$strategy_costs
    )
    # Clinical-outcome columns computed from the SAME per-draw sampled
    # parameters as the cost columns above, not a separate Monte Carlo loop
    # -- see R/diagnostic_yield.R's file-level docblock for why a second PSA
    # framework was deliberately avoided (draws would not otherwise be
    # paired to the same underlying parameter realization).
    clinical_outcomes <- compute_strategy_clinical_outcomes(sampled_parameters)

    dnc_cost <- strategy_result$strategy_costs$expected_total_cost[
      strategy_result$strategy_costs$strategy == "dnc"
    ]

    draw_costs <- c(
      office_emb = incremental_result$office_emb_cost,
      combined_emb = incremental_result$combined_emb_cost,
      dnc = dnc_cost
    )

    outcome_at <- function(strategy_name, column_name) {
      clinical_outcomes[[column_name]][clinical_outcomes$strategy == strategy_name]
    }

    tibble::tibble(
      draw = draw_index,
      office_emb_cost = incremental_result$office_emb_cost,
      combined_emb_cost = incremental_result$combined_emb_cost,
      dnc_cost = dnc_cost,
      incremental_cost_combined_vs_office =
        incremental_result$incremental_cost_combined_vs_office,
      cheapest_strategy = base::names(draw_costs)[
        base::which.min(draw_costs)
      ],
      office_emb_neoplasia_delayed_per_1000 =
        outcome_at("office_emb", "neoplasia_delayed_per_1000"),
      combined_emb_neoplasia_delayed_per_1000 =
        outcome_at("combined_emb", "neoplasia_delayed_per_1000"),
      dnc_neoplasia_delayed_per_1000 =
        outcome_at("dnc", "neoplasia_delayed_per_1000"),
      office_emb_major_ae_per_1000 =
        outcome_at("office_emb", "major_ae_per_1000"),
      combined_emb_major_ae_per_1000 =
        outcome_at("combined_emb", "major_ae_per_1000"),
      dnc_major_ae_per_1000 =
        outcome_at("dnc", "major_ae_per_1000")
    )
  })

  probabilistic_estimates <- dplyr::bind_rows(simulation_rows)

  pct_combined_cost_saving <- 100 * base::mean(
    probabilistic_estimates$incremental_cost_combined_vs_office < 0
  )

  base::message(
    "PSA complete. Combined EMB was cost-saving vs. office EMB in ",
    base::round(pct_combined_cost_saving, 1), "% of draws."
  )

  probabilistic_estimates
}

#' Per-strategy cost mean/SD/95% interval from a set of PSA draws
#'
#' Reshapes the wide per-draw costs from [run_probabilistic_sensitivity()]
#' into one summary row per strategy. Used to attach parameter-evidence
#' uncertainty (as error bars) to a strategy-cost bar chart -- the
#' `model_parameters` passed in may already carry scenario- or
#' locality-specific overrides (see `R/scenarios.R`, `R/geographic_sensitivity.R`),
#' in which case the resulting interval describes uncertainty in the
#' underlying cost/probability evidence AS PRICED at that scenario or
#' locality, not uncertainty about the scenario/locality choice itself
#' (which this repository deliberately treats as deterministic; see
#' `R/geographic_sensitivity.R`'s file-level docblock).
#'
#' @param model_parameters Tibble from [load_model_parameters()].
#' @param price_index_table Tibble from [load_price_index_table()].
#' @param n_simulations Integer number of Monte Carlo draws.
#' @param seed Integer or `NULL`, passed through to
#'   [run_probabilistic_sensitivity()].
#' @return A tibble with one row per strategy: `strategy`, `mean_cost`,
#'   `sd_cost`, `ci_low` (2.5th percentile), `ci_high` (97.5th percentile).
#' @export
summarize_psa_cost_interval <- function(
  model_parameters,
  price_index_table = load_price_index_table(),
  n_simulations = 1000,
  seed = 20260901
) {
  draws <- run_probabilistic_sensitivity(
    model_parameters,
    price_index_table = price_index_table,
    n_simulations = n_simulations,
    seed = seed
  )

  draws %>%
    dplyr::select("office_emb_cost", "combined_emb_cost", "dnc_cost") %>%
    tidyr::pivot_longer(
      dplyr::everything(), names_to = "strategy", values_to = "expected_total_cost"
    ) %>%
    dplyr::mutate(strategy = base::sub("_cost$", "", .data$strategy)) %>%
    dplyr::group_by(.data$strategy) %>%
    dplyr::summarize(
      mean_cost = base::mean(.data$expected_total_cost),
      sd_cost = stats::sd(.data$expected_total_cost),
      ci_low = stats::quantile(.data$expected_total_cost, 0.025)[[1]],
      ci_high = stats::quantile(.data$expected_total_cost, 0.975)[[1]],
      .groups = "drop"
    )
}

#' Metric's 95% PSA interval at one pinned value of a swept parameter
#'
#' Used to draw an uncertainty band around a deterministic threshold-sweep
#' line ([plot_threshold_sweep()]): `parameter_name` is pinned exactly at
#' `parameter_value` (its `distribution` is temporarily forced to
#' `"fixed"`, not merely re-centered, so it does not also vary across
#' draws), while every OTHER parameter is Monte Carlo-sampled exactly as
#' in [run_probabilistic_sensitivity()]. The resulting interval answers
#' "how much could the line move at this x, given uncertainty in every
#' OTHER cited parameter" -- not uncertainty in the swept parameter, which
#' the x-axis already represents directly.
#'
#' @param model_parameters Tibble from [load_model_parameters()].
#' @param parameter_name Character scalar: the swept parameter to pin.
#' @param parameter_value Numeric scalar: the grid value to pin it at.
#' @param price_index_table Tibble from [load_price_index_table()].
#' @param target_metric_fn Function of `strategy_costs` returning a
#'   numeric scalar (see [evaluate_metric_at()]).
#' @param n_simulations Integer Monte Carlo draws at this grid point.
#' @param seed Integer or `NULL`. Same save/restore-on-exit behavior as
#'   [run_probabilistic_sensitivity()].
#' @return A one-row tibble: `parameter_value`, `ci_low`, `ci_high`.
#' @export
evaluate_metric_psa_interval_at <- function(
  model_parameters,
  parameter_name,
  parameter_value,
  price_index_table,
  target_metric_fn,
  n_simulations = 200,
  seed = 20260901
) {
  pinned_parameters <- model_parameters
  pinned_row <- base::which(pinned_parameters$parameter == parameter_name)
  pinned_parameters$base_value[[pinned_row]] <- base::as.character(parameter_value)
  pinned_parameters$distribution[[pinned_row]] <- "fixed"

  if (!base::is.null(seed)) {
    old_seed <- if (base::exists(".Random.seed", envir = .GlobalEnv)) {
      base::get(".Random.seed", envir = .GlobalEnv)
    } else {
      NULL
    }
    base::on.exit({
      if (!base::is.null(old_seed)) {
        base::assign(".Random.seed", old_seed, envir = .GlobalEnv)
      } else if (base::exists(".Random.seed", envir = .GlobalEnv)) {
        base::rm(".Random.seed", envir = .GlobalEnv)
      }
    }, add = TRUE)
    base::set.seed(seed)
  }

  draw_values <- purrr::map_dbl(base::seq_len(n_simulations), function(draw_index) {
    sampled_parameters <- draw_parameter_set(pinned_parameters)
    strategy_result <- compute_strategy_costs(
      sampled_parameters, price_index_table = price_index_table
    )
    target_metric_fn(strategy_result$strategy_costs)
  })

  tibble::tibble(
    parameter_value = parameter_value,
    ci_low = stats::quantile(draw_values, 0.025)[[1]],
    ci_high = stats::quantile(draw_values, 0.975)[[1]]
  )
}

#' Probability each strategy is the least expensive, across PSA draws
#'
#' Answers "combined sampling was the least-cost strategy in N% of
#' simulations" rather than only reporting a single deterministic
#' comparison.
#'
#' @param probabilistic_estimates Tibble from
#'   [run_probabilistic_sensitivity()], which must include a
#'   `cheapest_strategy` column.
#' @return A tibble with one row per strategy: `strategy`, `n_draws_cheapest`,
#'   `pct_draws_cheapest`.
#' @export
summarize_probability_cheapest <- function(probabilistic_estimates) {
  base::message("Summarizing probability each strategy is least expensive.")

  n_total <- base::nrow(probabilistic_estimates)

  summary_tbl <- probabilistic_estimates %>%
    dplyr::count(.data$cheapest_strategy, name = "n_draws_cheapest") %>%
    dplyr::mutate(pct_draws_cheapest = 100 * .data$n_draws_cheapest / n_total) %>%
    dplyr::rename(strategy = "cheapest_strategy") %>%
    dplyr::arrange(dplyr::desc(.data$pct_draws_cheapest))

  purrr::walk2(
    summary_tbl$strategy, summary_tbl$pct_draws_cheapest,
    ~ base::message(
      "  ", .x, " was cheapest in ", base::round(.y, 1), "% of draws."
    )
  )

  summary_tbl
}

#' Probabilistic sensitivity analysis over an arbitrary strategy-cost function
#'
#' A generic Monte Carlo loop: for each draw, samples a full parameter set
#' via [draw_parameter_set()], computes strategy costs with the
#' caller-supplied `strategy_cost_fn`, and applies every function in
#' `metric_fns` to that draw's `strategy_costs`, assembling one row per
#' draw with one column per metric. Unlike [run_probabilistic_sensitivity()]
#' -- which hardcodes the three Lynch-specific strategy names as column
#' names and is paired to [compute_strategy_clinical_outcomes()]'s
#' Lynch-specific clinical-outcome columns -- this function makes no
#' assumption about strategy names or what a "metric" means, so it is
#' usable by a project with a different strategy set (e.g. two strategies
#' instead of three, no shared escalation target).
#'
#' @param model_parameters Tibble from [load_model_parameters()].
#' @param strategy_cost_fn Function of `(model_parameters,
#'   price_index_table = ...)` returning a list with a `strategy_costs`
#'   element, matching [compute_strategy_costs()]'s signature/return
#'   shape (e.g. a function built on [compute_multi_strategy_costs()]).
#' @param metric_fns Named list of functions, each taking one draw's
#'   `strategy_costs` tibble and returning a scalar. Each name becomes a
#'   column in the result. Defaults to an empty list (the result then has
#'   only the `draw` column -- not useful on its own, but kept as an
#'   explicit default rather than requiring the argument, so a caller
#'   who only wants strategy-cost side effects, e.g. for later
#'   inspection, is not forced to supply a metric).
#' @param price_index_table Tibble from [load_price_index_table()].
#' @param n_simulations Integer, number of Monte Carlo draws.
#' @param seed Integer or `NULL`. See [run_probabilistic_sensitivity()]
#'   for the seeding/restore behavior, which this function matches
#'   exactly.
#' @return A tibble with one row per draw: `draw` plus one column per
#'   entry in `metric_fns`.
#' @export
run_probabilistic_sensitivity_generic <- function(
  model_parameters,
  strategy_cost_fn,
  metric_fns = base::list(),
  price_index_table = load_price_index_table(),
  n_simulations = 1000,
  seed = 20260901
) {
  validate_positive(n_simulations, "n_simulations")

  if (!base::is.null(seed)) {
    old_seed <- if (base::exists(".Random.seed", envir = .GlobalEnv)) {
      base::get(".Random.seed", envir = .GlobalEnv)
    } else {
      NULL
    }
    base::on.exit({
      if (!base::is.null(old_seed)) {
        base::assign(".Random.seed", old_seed, envir = .GlobalEnv)
      } else if (base::exists(".Random.seed", envir = .GlobalEnv)) {
        base::rm(".Random.seed", envir = .GlobalEnv)
      }
    }, add = TRUE)
    base::set.seed(seed)
  }

  base::message(
    "Running probabilistic sensitivity analysis: ", n_simulations,
    " Monte Carlo draws",
    if (base::is.null(seed)) " (unseeded)." else base::paste0(" (seed ", seed, ").")
  )

  simulation_rows <- purrr::map(base::seq_len(n_simulations), function(draw_index) {
    if (draw_index %% 200 == 0) {
      base::message("  Draw ", draw_index, " / ", n_simulations)
    }

    sampled_parameters <- draw_parameter_set(model_parameters)
    strategy_result <- strategy_cost_fn(
      sampled_parameters,
      price_index_table = price_index_table
    )

    metric_values <- purrr::map(metric_fns, function(metric_fn) {
      metric_fn(strategy_result$strategy_costs)
    })

    tibble::as_tibble(c(list(draw = draw_index), metric_values))
  })

  probabilistic_estimates <- dplyr::bind_rows(simulation_rows)

  base::message("PSA complete: ", n_simulations, " draws.")

  probabilistic_estimates
}
