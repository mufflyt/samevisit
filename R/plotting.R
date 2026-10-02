# Plotting conventions
#
# `theme_journal()` and the tornado/threshold plotting patterns are
# ported directly from `colpocleisis_costeff/generate_figures.R`, which
# used a minimal `ggplot2::theme_minimal()`-based journal theme, a
# segment-based tornado diagram, and a line-based threshold plot. The
# strategy-cost-plane figure is new (this model is cost-minimization,
# not cost-utility, so there is no QALY axis to plot against).

#' Journal-style ggplot2 theme
#'
#' @return A `ggplot2` theme object.
#' @export
theme_journal <- function() {
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(fill = NA, colour = "grey70"),
      axis.ticks = ggplot2::element_line(colour = "grey70"),
      legend.position = "bottom",
      plot.title = ggplot2::element_text(face = "bold", size = 11),
      plot.subtitle = ggplot2::element_text(size = 8, colour = "grey40", lineheight = 1.2),
      plot.title.position = "plot"
    )
}

#' Display labels for the three strategies
#' @export
STRATEGY_LABELS <- c(
  office_emb = "Office endometrial biopsy (standalone)",
  dnc = "Operative dilation and curettage",
  combined_emb = "Endometrial biopsy with colonoscopy"
)

#' Display labels for tornado-diagram parameters
PARAMETER_LABELS <- c(
  emb_office_professional_cost = "Office endometrial biopsy professional fee",
  emb_failure_lynch = "Office endometrial biopsy failure probability",
  combined_to_dnc_probability = "Combined-arm escalation probability to dilation and curettage",
  combined_emb_added_minutes = "Incremental colonoscopy-suite minutes for combined biopsy",
  direct_room_cost_per_minute = "Procedure-room cost per minute",
  anesthesia_cost_per_minute = "Anesthesia cost per minute",
  emb_pathology_cost = "Endometrial biopsy pathology cost",
  dnc_facility_or_asc_fee = "Dilation and curettage facility or ambulatory surgical center fee",
  coordination_cost = "Scheduling-coordination cost",
  emb_disposable_supply_cost = "Endometrial biopsy disposable supply cost"
)

#' Display labels for scenario-analysis scenario names
#' @export
SCENARIO_LABELS <- c(
  base_case_medicare = "Base case (Medicare)",
  combined_without_preop_visit = "Combined biopsy without separate preoperative visit",
  commercial = "Commercial payer",
  medicaid = "Medicaid payer",
  office_cost_ladabaum_historical = "Office biopsy cost (Ladabaum historical estimate)"
)

#' Bar chart of expected cost per strategy
#'
#' @param strategy_costs Tibble from
#'   `compute_strategy_costs()$strategy_costs`. If it also carries
#'   `ci_low`/`ci_high` columns (from [summarize_psa_cost_interval()]),
#'   a 95% probabilistic-sensitivity-analysis error bar is drawn on each
#'   bar and the dollar-value label is placed above the error bar rather
#'   than above the bare bar.
#' @return A `ggplot` object.
#' @export
plot_strategy_cost_comparison <- function(strategy_costs) {
  has_ci <- base::all(base::c("ci_low", "ci_high") %in% base::names(strategy_costs))

  plot_data <- strategy_costs %>%
    dplyr::mutate(label = STRATEGY_LABELS[.data$strategy]) %>%
    dplyr::arrange(.data$expected_total_cost)

  if (has_ci) {
    # The lower whisker often falls INSIDE the colored bar (the deterministic
    # point estimate need not sit at the PSA distribution's midpoint), where
    # small grey text on a saturated fill is unreadable. Use bold white text
    # centered on the cap when that happens; grey text below the cap when the
    # lower whisker instead falls in the white space below the bar.
    plot_data <- plot_data %>%
      dplyr::mutate(
        ci_low_inside_bar = .data$ci_low < .data$expected_total_cost,
        ci_low_colour = dplyr::if_else(.data$ci_low_inside_bar, "white", "grey35"),
        ci_low_vjust = dplyr::if_else(.data$ci_low_inside_bar, 0.5, 1.4)
      )
  }

  strategy_plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = stats::reorder(.data$label, .data$expected_total_cost),
      y = .data$expected_total_cost,
      fill = .data$label
    )
  ) +
    ggplot2::geom_col(width = 0.6)

  if (has_ci) {
    strategy_plot <- strategy_plot +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = .data$ci_low, ymax = .data$ci_high),
        width = 0.15, linewidth = 0.5, colour = "grey30"
      ) +
      ggplot2::geom_text(
        ggplot2::aes(y = .data$ci_high, label = scales::dollar(.data$ci_high, accuracy = 1)),
        vjust = -0.5, size = 2.6, colour = "grey35"
      ) +
      ggplot2::geom_text(
        ggplot2::aes(
          y = .data$ci_low, label = scales::dollar(.data$ci_low, accuracy = 1),
          vjust = .data$ci_low_vjust, colour = .data$ci_low_colour
        ),
        size = 2.6, fontface = "bold"
      ) +
      ggplot2::scale_colour_identity(guide = "none")
  }

  strategy_plot +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::dollar(.data$expected_total_cost, accuracy = 1)),
      vjust = -0.5, size = 3.2, fontface = "bold"
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::dollar_format(),
      expand = ggplot2::expansion(mult = c(0, 0.2))
    ) +
    ggplot2::scale_fill_brewer(palette = "Set1", guide = "none") +
    ggplot2::labs(
      x = NULL,
      y = "Expected cost per patient ($)",
      caption = if (has_ci) {
        "Error bars: 95% probabilistic-sensitivity-analysis interval (1,000 Monte Carlo draws)."
      } else {
        NULL
      }
    ) +
    theme_journal()
}

#' Tornado diagram from one-way sensitivity results
#'
#' @param sensitivity_estimates Tibble from [run_one_way_sensitivity()].
#' @param base_metric_value Numeric scalar: vertical reference line
#'   (typically `sensitivity_estimates$metric_at_base[[1]]`).
#' @param metric_label Character scalar for the x-axis label.
#' @return A `ggplot` object.
#' @export
plot_tornado <- function(
  sensitivity_estimates,
  base_metric_value = sensitivity_estimates$metric_at_base[[1]],
  metric_label = "Incremental cost, combined biopsy vs. office biopsy ($)"
) {
  tornado_data <- sensitivity_estimates %>%
    dplyr::mutate(
      parameter_label = dplyr::coalesce(
        unname(PARAMETER_LABELS[.data$parameter]), .data$parameter
      ),
      parameter_label = forcats::fct_reorder(.data$parameter_label, .data$spread),
      bar_left = pmin(.data$metric_at_low, .data$metric_at_high),
      bar_right = pmax(.data$metric_at_low, .data$metric_at_high)
    )

  ggplot2::ggplot(tornado_data) +
    ggplot2::geom_segment(
      ggplot2::aes(
        y = .data$parameter_label, yend = .data$parameter_label,
        x = .data$bar_left, xend = .data$bar_right
      ),
      linewidth = 6, colour = "#4A90D9", alpha = 0.8
    ) +
    ggplot2::geom_vline(
      xintercept = base_metric_value, linetype = "dashed", colour = "grey30"
    ) +
    ggplot2::scale_x_continuous(
      labels = scales::dollar_format(),
      expand = ggplot2::expansion(mult = c(0.15, 0.15))
    ) +
    ggplot2::labs(x = metric_label, y = NULL) +
    theme_journal() +
    ggplot2::theme(legend.position = "none")
}

#' Line plot of a metric across a swept parameter range (threshold plot)
#'
#' @param model_parameters Tibble from [load_model_parameters()].
#' @param parameter_name Character scalar parameter to sweep.
#' @param parameter_grid Numeric vector of values to evaluate.
#' @param price_index_table Tibble from [load_price_index_table()].
#' @param target_metric_fn Function of `strategy_costs` returning a
#'   numeric scalar. Defaults to
#'   [metric_combined_vs_office_incremental()].
#' @param x_label,y_label Character scalars for axis labels.
#' @param show_uncertainty_band Logical: draw a shaded 95%
#'   probabilistic-sensitivity-analysis band around the line, showing how
#'   much the line could move at each swept-parameter value given
#'   uncertainty in every OTHER parameter's cited evidence (see
#'   [evaluate_metric_psa_interval_at()]). Default `TRUE`.
#' @param band_grid_points Integer number of points (evenly spaced across
#'   `parameter_grid`'s range) at which the band is evaluated. Coarser
#'   than `parameter_grid` because each point costs its own Monte Carlo
#'   run; `geom_ribbon()` interpolates linearly between them.
#' @param band_n_simulations Integer Monte Carlo draws per band point.
#' @return A `ggplot` object.
#' @export
plot_threshold_sweep <- function(
  model_parameters,
  parameter_name,
  parameter_grid,
  price_index_table = load_price_index_table(),
  target_metric_fn = metric_combined_vs_office_incremental,
  x_label = parameter_name,
  y_label = "Incremental cost, combined biopsy vs. office biopsy ($)",
  show_uncertainty_band = TRUE,
  band_grid_points = 21,
  band_n_simulations = 200
) {
  sweep_values <- purrr::map_dbl(
    parameter_grid,
    ~ evaluate_metric_at(
      model_parameters, parameter_name, .x, price_index_table, target_metric_fn
    )
  )

  sweep_data <- tibble::tibble(
    parameter_value = parameter_grid,
    metric_value = sweep_values
  )

  threshold_plot <- ggplot2::ggplot(
    sweep_data,
    ggplot2::aes(x = .data$parameter_value, y = .data$metric_value)
  )

  if (show_uncertainty_band) {
    band_grid <- base::seq(
      base::min(parameter_grid), base::max(parameter_grid), length.out = band_grid_points
    )
    band_data <- dplyr::bind_rows(purrr::map(band_grid, function(grid_value) {
      evaluate_metric_psa_interval_at(
        model_parameters, parameter_name, grid_value, price_index_table,
        target_metric_fn, n_simulations = band_n_simulations
      )
    }))

    threshold_plot <- threshold_plot +
      ggplot2::geom_ribbon(
        data = band_data,
        ggplot2::aes(x = .data$parameter_value, ymin = .data$ci_low, ymax = .data$ci_high),
        inherit.aes = FALSE, fill = "#4A90D9", alpha = 0.15
      )
  }

  threshold_plot +
    ggplot2::geom_line(linewidth = 0.8, colour = "#4A90D9") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey30") +
    ggplot2::scale_y_continuous(labels = scales::dollar_format()) +
    ggplot2::labs(
      x = x_label, y = y_label,
      caption = if (show_uncertainty_band) {
        "Shaded band: 95% probabilistic-sensitivity-analysis interval from uncertainty in every other parameter."
      } else {
        NULL
      }
    ) +
    theme_journal()
}
