# Validation utilities
#
# Small, composable input-validation helpers used throughout the model.
# Adapted from the validation pattern in `colpocleisis_costeff`
# (`colpocleisis_selective_testing_model.R`), where scalar validators were
# defined inline inside the model function. Here they are pulled out into
# standalone, reusable, individually testable functions.

#' Validate a single probability
#'
#' @param numeric_value Value to check.
#' @param value_name Character scalar used in the error message.
#' @return Invisibly `TRUE` if valid; otherwise raises an error.
#' @export
validate_probability <- function(numeric_value, value_name) {
  if (!base::is.numeric(numeric_value) ||
      length(numeric_value) != 1 ||
      base::is.na(numeric_value) ||
      numeric_value < 0 ||
      numeric_value > 1) {
    base::stop(value_name, " must be one number between 0 and 1.")
  }
  base::invisible(TRUE)
}

#' Validate a non-negative numeric scalar
#'
#' @inheritParams validate_probability
#' @return Invisibly `TRUE` if valid; otherwise raises an error.
#' @export
validate_non_negative <- function(numeric_value, value_name) {
  if (!base::is.numeric(numeric_value) ||
      length(numeric_value) != 1 ||
      base::is.na(numeric_value) ||
      numeric_value < 0) {
    base::stop(value_name, " must be one non-negative number.")
  }
  base::invisible(TRUE)
}

#' Validate a strictly positive numeric scalar
#'
#' @inheritParams validate_probability
#' @return Invisibly `TRUE` if valid; otherwise raises an error.
#' @export
validate_positive <- function(numeric_value, value_name) {
  if (!base::is.numeric(numeric_value) ||
      length(numeric_value) != 1 ||
      base::is.na(numeric_value) ||
      numeric_value <= 0) {
    base::stop(value_name, " must be one positive number.")
  }
  base::invisible(TRUE)
}

#' Validate a single logical scalar
#'
#' @inheritParams validate_probability
#' @return Invisibly `TRUE` if valid; otherwise raises an error.
#' @export
validate_boolean <- function(logical_value, value_name) {
  if (!base::is.logical(logical_value) ||
      length(logical_value) != 1 ||
      base::is.na(logical_value)) {
    base::stop(value_name, " must be one logical value (TRUE/FALSE).")
  }
  base::invisible(TRUE)
}

#' Validate the shape and contents of a model-parameter table
#'
#' Checks that a `model_parameters` tibble (as produced by
#' [load_model_parameters()]) has the required columns, unique parameter
#' names, and internally consistent `low_value`/`base_value`/`high_value`
#' ordering wherever those bounds are supplied.
#'
#' @param model_parameters Tibble produced by [load_model_parameters()].
#' @return Invisibly `TRUE` if valid; otherwise raises an error.
#' @export
validate_model_parameters <- function(model_parameters) {
  required_columns <- c(
    "parameter", "category", "strategy", "description", "base_value",
    "unit", "low_value", "high_value", "distribution", "dollar_year",
    "source", "provisional", "notes", "gamma_alpha", "gamma_rate",
    "evidence_tier"
  )

  missing_columns <- base::setdiff(required_columns, base::names(model_parameters))
  if (length(missing_columns) > 0) {
    base::stop(
      "model_parameters is missing required column(s): ",
      base::paste(missing_columns, collapse = ", ")
    )
  }

  duplicated_names <- model_parameters$parameter[
    base::duplicated(model_parameters$parameter)
  ]
  if (length(duplicated_names) > 0) {
    base::stop(
      "model_parameters has duplicate parameter name(s): ",
      base::paste(base::unique(duplicated_names), collapse = ", ")
    )
  }

  bounded_rows <- model_parameters %>%
    dplyr::filter(
      !base::is.na(.data$low_value),
      !base::is.na(.data$high_value)
    )

  inverted_bounds <- bounded_rows %>%
    dplyr::filter(.data$low_value > .data$high_value)

  if (nrow(inverted_bounds) > 0) {
    base::stop(
      "model_parameters has low_value > high_value for: ",
      base::paste(inverted_bounds$parameter, collapse = ", ")
    )
  }

  # A non-degenerate beta distribution cannot have a genuine mean of exactly
  # 0 or 1. draw_parameter_sample() (R/sensitivity_probabilistic.R) clamps
  # such a base_value to 1e-6/1-1e-6 before moment-matching, which silently
  # produces a near-point-mass distribution -- the parameter never actually
  # varies in PSA despite carrying an explicit low_value/high_value range.
  # This caught office_to_dnc_escalation_fraction (2026-08-31): PSA ran
  # 1,000 draws without that parameter ever meaningfully deviating from 1.0,
  # which meant compute_strategy_clinical_outcomes()'s delayed-neoplasia
  # metric was mechanically silent in every stochastic run. Fixed there by
  # switching to `triangular`, the correct choice for a structural/
  # plausibility range (as opposed to a binomial confidence interval).
  boundary_beta_rows <- model_parameters %>%
    dplyr::mutate(
      numeric_base_value = base::suppressWarnings(base::as.numeric(.data$base_value))
    ) %>%
    dplyr::filter(
      .data$distribution == "beta",
      .data$numeric_base_value %in% base::c(0, 1),
      !base::is.na(.data$low_value),
      !base::is.na(.data$high_value),
      .data$low_value != .data$high_value
    )

  if (nrow(boundary_beta_rows) > 0) {
    base::stop(
      "Non-degenerate beta-distributed parameters cannot have base_value ",
      "exactly 0 or 1: ", base::paste(boundary_beta_rows$parameter, collapse = ", "),
      ". Use a distribution appropriate for boundary-mode structural ",
      "uncertainty, such as triangular."
    )
  }

  base::invisible(TRUE)
}

#' Look up a single base-case parameter value by name
#'
#' @param model_parameters Tibble produced by [load_model_parameters()].
#' @param parameter_name Character scalar parameter name.
#' @param as_numeric Logical. If `TRUE` (default), coerce the returned
#'   `base_value` to numeric. Set to `FALSE` for boolean/text parameters
#'   such as `combined_requires_preop_office_visit`.
#' @return The `base_value` for `parameter_name`.
#' @export
get_parameter_value <- function(model_parameters, parameter_name, as_numeric = TRUE) {
  matched_rows <- model_parameters %>%
    dplyr::filter(.data$parameter == parameter_name)

  if (nrow(matched_rows) != 1) {
    base::stop(
      "Expected exactly one row for parameter '", parameter_name,
      "', found ", nrow(matched_rows), "."
    )
  }

  raw_value <- matched_rows$base_value[[1]]

  if (isTRUE(as_numeric)) {
    return(base::as.numeric(raw_value))
  }

  if (raw_value %in% c("TRUE", "FALSE")) {
    return(base::as.logical(raw_value))
  }

  raw_value
}
