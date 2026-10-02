# Model parameter loading and overriding
#
# Model parameters are stored separately from model logic in
# `config/model_parameters.csv`, following the "separate parameters from
# functions" convention requested for this repository (and a deliberate
# improvement over `colpocleisis_costeff`, where every default was a
# hard-coded function argument). Every row carries a base value, unit,
# plausible low/high bounds, source, dollar year, a `provisional` flag,
# and free-text notes -- so no assumption can silently become
# undocumented.

#' Load the model parameter table
#'
#' @param path Character scalar. Path to the parameter CSV. Defaults to
#'   `config/model_parameters.csv` relative to the current working
#'   directory, which is how the `analysis/` scripts invoke it.
#' @return A validated tibble of model parameters.
#' @export
load_model_parameters <- function(path = "config/model_parameters.csv") {
  base::message("Loading model parameters from: ", path)

  if (!base::file.exists(path)) {
    base::stop("Parameter file not found at: ", path)
  }

  model_parameters <- readr::read_csv(
    path,
    col_types = readr::cols(
      parameter = readr::col_character(),
      category = readr::col_character(),
      strategy = readr::col_character(),
      description = readr::col_character(),
      base_value = readr::col_character(),
      unit = readr::col_character(),
      low_value = readr::col_double(),
      high_value = readr::col_double(),
      distribution = readr::col_character(),
      dollar_year = readr::col_double(),
      source = readr::col_character(),
      provisional = readr::col_logical(),
      notes = readr::col_character(),
      gamma_alpha = readr::col_double(),
      gamma_rate = readr::col_double(),
      evidence_tier = readr::col_character()
    ),
    show_col_types = FALSE
  )

  validate_model_parameters(model_parameters)

  provisional_count <- base::sum(model_parameters$provisional, na.rm = TRUE)
  base::message(
    "  Loaded ", nrow(model_parameters), " parameters (",
    provisional_count, " flagged provisional)."
  )

  model_parameters
}

#' Override one or more base-case parameter values
#'
#' Used by sensitivity, threshold, and scenario analyses to perturb a
#' single parameter (or a small named set of parameters) without mutating
#' the on-disk parameter table.
#'
#' @param model_parameters Tibble produced by [load_model_parameters()].
#' @param overrides Named list, e.g. `list(emb_failure_lynch = 0.24)`.
#'   Names must already exist in `model_parameters$parameter`.
#' @return `model_parameters` with `base_value` replaced for each name in
#'   `overrides`.
#' @export
override_model_parameters <- function(model_parameters, overrides) {
  if (length(overrides) == 0) {
    return(model_parameters)
  }

  unknown_names <- base::setdiff(base::names(overrides), model_parameters$parameter)
  if (length(unknown_names) > 0) {
    base::stop(
      "override_model_parameters() received unknown parameter name(s): ",
      base::paste(unknown_names, collapse = ", ")
    )
  }

  updated_parameters <- model_parameters
  for (parameter_name in base::names(overrides)) {
    row_index <- base::which(updated_parameters$parameter == parameter_name)
    updated_parameters$base_value[[row_index]] <- base::as.character(
      overrides[[parameter_name]]
    )
  }

  updated_parameters
}

#' Convenience accessor: pull every base-case value into a flat named list
#'
#' @param model_parameters Tibble produced by [load_model_parameters()].
#' @param boolean_parameters Character vector of parameter names that
#'   should be kept as logical/character rather than coerced to numeric
#'   (e.g. `combined_requires_preop_office_visit`).
#' @return A named list of base-case values.
#' @export
model_parameters_as_list <- function(
  model_parameters,
  boolean_parameters = c("combined_requires_preop_office_visit")
) {
  parameter_names <- model_parameters$parameter
  values <- stats::setNames(
    base::vector("list", length(parameter_names)),
    parameter_names
  )

  for (parameter_name in parameter_names) {
    is_numeric <- !parameter_name %in% boolean_parameters
    values[[parameter_name]] <- get_parameter_value(
      model_parameters,
      parameter_name,
      as_numeric = is_numeric
    )
  }

  values
}
