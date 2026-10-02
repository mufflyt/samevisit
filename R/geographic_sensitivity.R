# Geographic sensitivity analysis
#
# A thin layer around [override_model_parameters()] and
# [compute_strategy_costs()], not a new cost engine: for each locality, this
# file computes a real CMS GPCI-based multiplier for professional-fee
# parameters and a real CMS wage-index-based multiplier for facility-fee
# parameters, applies them as overrides, and re-runs the existing cost
# engine unchanged. No GPCI or wage-index value is invented here -- the
# locality table (`data/cms_geographic_indices_2026.csv`) and the RVU table
# (`data/cms_pfs_rvus_2026.csv`) are both external CMS inputs, downloaded
# directly from cms.gov (see `docs/data_sources.md` for the exact files and
# URLs).
#
# This is a deliberately **deterministic** analysis, not part of
# `run_probabilistic_sensitivity()`. Geography is not a source of parameter
# uncertainty the way a study's confidence interval is; it is a question of
# whether the base-case conclusion generalizes when the same model is
# priced in a different place. Mixing it into the PSA would conflate "the
# cited studies could be wrong" with "we costed this for the wrong ZIP
# code," which are different claims requiring different evidence.
#
# **The most important check on any run of this analysis is the `national`
# locality**, whose GPCIs and wage index are all 1.0 by construction. It
# must reproduce [compute_strategy_costs()]'s own base-case output exactly
# (see `tests/testthat/test-geographic-sensitivity.R`'s
# INDEPENDENT-CONFIRMATION-flavored check). If it doesn't, stop and find
# which "national" parameter is not actually an unadjusted PFS/OPPS anchor
# before interpreting any locality comparison.

#' Validate required geographic table columns
#'
#' @param input_tbl Input tibble.
#' @param required_columns Required column names.
#' @param input_name Name used in error messages.
#' @return Invisibly TRUE.
#' @export
validate_geographic_columns <- function(input_tbl, required_columns, input_name) {
  missing_columns <- base::setdiff(required_columns, base::names(input_tbl))
  if (base::length(missing_columns) > 0) {
    base::stop(
      input_name, " is missing required column(s): ",
      base::paste(missing_columns, collapse = ", ")
    )
  }
  base::invisible(TRUE)
}

#' Calculate a Medicare PFS geographic multiplier
#'
#' Uses the standard relative-value structure:
#'
#'   (work RVU * work GPCI + PE RVU * PE GPCI + MP RVU * MP GPCI) /
#'   (work RVU + PE RVU + MP RVU)
#'
#' The conversion factor cancels because this function returns a multiplier
#' relative to the national unadjusted payment, not a dollar amount.
#'
#' @param work_rvu Work RVU.
#' @param pe_rvu Practice-expense RVU for the relevant setting.
#' @param mp_rvu Malpractice RVU.
#' @param gpci_work Work GPCI.
#' @param gpci_pe Practice-expense GPCI.
#' @param gpci_mp Malpractice GPCI.
#' @return Numeric geographic multiplier.
#' @export
compute_pfs_geographic_multiplier <- function(
  work_rvu, pe_rvu, mp_rvu, gpci_work, gpci_pe, gpci_mp
) {
  numeric_values <- base::c(work_rvu, pe_rvu, mp_rvu, gpci_work, gpci_pe, gpci_mp)
  if (base::any(!base::is.finite(numeric_values))) {
    base::stop("All RVU and GPCI values must be finite numbers.")
  }
  if (base::any(numeric_values < 0)) {
    base::stop("RVU and GPCI values cannot be negative.")
  }

  national_rvu <- work_rvu + pe_rvu + mp_rvu
  if (national_rvu <= 0) {
    base::stop("Total RVU must be greater than zero.")
  }

  locality_rvu <- work_rvu * gpci_work + pe_rvu * gpci_pe + mp_rvu * gpci_mp
  locality_rvu / national_rvu
}

#' Calculate a facility wage-index multiplier
#'
#' Standard OPPS-style facility geographic adjustment: only the
#' labor-related share of the payment is wage-index-adjusted.
#'
#' @param wage_index CMS wage index for the locality.
#' @param labor_share Fraction of payment treated as labor-related.
#' @return Numeric geographic multiplier.
#' @export
compute_facility_geographic_multiplier <- function(wage_index, labor_share) {
  if (!base::is.finite(wage_index) || wage_index <= 0) {
    base::stop("wage_index must be one positive finite number.")
  }
  validate_probability(labor_share, "labor_share")

  labor_share * wage_index + (1 - labor_share)
}

#' Validate geographic sensitivity inputs
#'
#' @param locality_indices Locality-level GPCI and wage-index table.
#' @param pfs_rvus CPT and place-of-service RVU table.
#' @param professional_mapping Mapping of model parameters to PFS RVUs.
#' @param facility_mapping Mapping of facility parameters to wage indices.
#' @return Invisibly TRUE.
#' @export
validate_geographic_inputs <- function(
  locality_indices, pfs_rvus, professional_mapping, facility_mapping
) {
  validate_geographic_columns(
    locality_indices,
    base::c("locality_id", "locality_label", "gpci_work", "gpci_pe", "gpci_mp"),
    "locality_indices"
  )
  validate_geographic_columns(
    pfs_rvus,
    base::c("cpt", "setting", "work_rvu", "pe_rvu", "mp_rvu"),
    "pfs_rvus"
  )
  validate_geographic_columns(
    professional_mapping,
    base::c("parameter", "cpt", "setting"),
    "professional_mapping"
  )
  validate_geographic_columns(
    facility_mapping,
    base::c("parameter", "index_column", "labor_share"),
    "facility_mapping"
  )

  duplicate_localities <- locality_indices %>%
    dplyr::count(.data$locality_id, name = "n") %>%
    dplyr::filter(.data$n > 1)
  if (base::nrow(duplicate_localities) > 0) {
    base::stop("locality_id must be unique.")
  }

  duplicated_adjustments <- base::intersect(
    professional_mapping$parameter, facility_mapping$parameter
  )
  if (base::length(duplicated_adjustments) > 0) {
    base::stop(
      "Parameter(s) assigned more than one geographic adjustment: ",
      base::paste(duplicated_adjustments, collapse = ", ")
    )
  }

  invalid_labor_share <- facility_mapping %>%
    dplyr::filter(
      !base::is.finite(.data$labor_share) | .data$labor_share < 0 | .data$labor_share > 1
    )
  if (base::nrow(invalid_labor_share) > 0) {
    base::stop("facility_mapping labor_share must be between 0 and 1.")
  }

  missing_index_columns <- base::setdiff(
    base::unique(facility_mapping$index_column), base::names(locality_indices)
  )
  if (base::length(missing_index_columns) > 0) {
    base::stop(
      "Unknown facility wage-index column(s): ",
      base::paste(missing_index_columns, collapse = ", ")
    )
  }

  base::invisible(TRUE)
}

#' Build locality-specific model parameter overrides
#'
#' @param model_parameters Model parameter tibble.
#' @param locality_id Locality identifier.
#' @param locality_indices GPCI and facility wage-index table.
#' @param pfs_rvus CPT/setting-specific Medicare RVUs.
#' @param professional_mapping Mapping from model parameters to PFS RVUs.
#' @param facility_mapping Mapping from model parameters to wage indices.
#' @return List with `overrides` (named list) and `adjustment_audit` (tibble).
#' @export
build_geographic_overrides <- function(
  model_parameters, locality_id, locality_indices, pfs_rvus,
  professional_mapping, facility_mapping
) {
  base::message("Building geographic overrides for locality: ", locality_id)

  validate_geographic_inputs(
    locality_indices = locality_indices, pfs_rvus = pfs_rvus,
    professional_mapping = professional_mapping, facility_mapping = facility_mapping
  )

  locality_row <- locality_indices %>%
    dplyr::filter(.data$locality_id == .env$locality_id)
  if (base::nrow(locality_row) != 1) {
    base::stop("Expected exactly one locality row for: ", locality_id)
  }

  overrides <- base::list()
  audit_rows <- base::list()

  if (base::nrow(professional_mapping) > 0) {
    for (row_index in base::seq_len(base::nrow(professional_mapping))) {
      mapping_row <- professional_mapping[row_index, , drop = FALSE]
      parameter_name <- mapping_row$parameter[[1]]
      cpt_code <- mapping_row$cpt[[1]]
      setting_name <- mapping_row$setting[[1]]

      rvu_row <- pfs_rvus %>%
        dplyr::filter(.data$cpt == .env$cpt_code, .data$setting == .env$setting_name)
      if (base::nrow(rvu_row) != 1) {
        base::stop(
          "Expected exactly one RVU row for CPT ", cpt_code, " in setting ", setting_name, "."
        )
      }

      national_value <- get_parameter_value(model_parameters, parameter_name)
      multiplier <- compute_pfs_geographic_multiplier(
        work_rvu = rvu_row$work_rvu[[1]], pe_rvu = rvu_row$pe_rvu[[1]],
        mp_rvu = rvu_row$mp_rvu[[1]], gpci_work = locality_row$gpci_work[[1]],
        gpci_pe = locality_row$gpci_pe[[1]], gpci_mp = locality_row$gpci_mp[[1]]
      )
      local_value <- national_value * multiplier
      overrides[[parameter_name]] <- local_value
      audit_rows[[base::length(audit_rows) + 1L]] <- tibble::tibble(
        parameter = parameter_name, adjustment_type = "pfs_gpci",
        source_key = base::paste(cpt_code, setting_name, sep = ":"),
        national_value = national_value, multiplier = multiplier, local_value = local_value
      )
      base::message("  PFS adjustment: ", parameter_name, " x ", base::round(multiplier, 4))
    }
  }

  if (base::nrow(facility_mapping) > 0) {
    for (row_index in base::seq_len(base::nrow(facility_mapping))) {
      mapping_row <- facility_mapping[row_index, , drop = FALSE]
      parameter_name <- mapping_row$parameter[[1]]
      index_column <- mapping_row$index_column[[1]]
      labor_share <- mapping_row$labor_share[[1]]

      wage_index <- locality_row[[index_column]][[1]]
      if (!base::is.finite(wage_index)) {
        base::stop("Missing wage index for locality ", locality_id, ": ", index_column)
      }

      national_value <- get_parameter_value(model_parameters, parameter_name)
      multiplier <- compute_facility_geographic_multiplier(
        wage_index = wage_index, labor_share = labor_share
      )
      local_value <- national_value * multiplier
      overrides[[parameter_name]] <- local_value
      audit_rows[[base::length(audit_rows) + 1L]] <- tibble::tibble(
        parameter = parameter_name, adjustment_type = "facility_wage_index",
        source_key = index_column, national_value = national_value,
        multiplier = multiplier, local_value = local_value
      )
      base::message("  Facility adjustment: ", parameter_name, " x ", base::round(multiplier, 4))
    }
  }

  adjustment_audit <- dplyr::bind_rows(audit_rows)
  base::message(
    "Created ", scales::comma(base::length(overrides)), " geographic parameter overrides."
  )

  base::list(overrides = overrides, adjustment_audit = adjustment_audit)
}

#' Run geographic sensitivity analysis
#'
#' @param model_parameters Model parameter tibble.
#' @param locality_indices Locality GPCI/wage-index table.
#' @param pfs_rvus PFS RVU table.
#' @param professional_mapping Professional parameter mapping.
#' @param facility_mapping Facility parameter mapping.
#' @param price_index_table Medical-care price-index table.
#' @return List with `strategy_costs` and `adjustment_audit` (both tibbles,
#'   one row per strategy-locality / adjustment-locality pair).
#' @export
run_geographic_sensitivity <- function(
  model_parameters, locality_indices, pfs_rvus, professional_mapping,
  facility_mapping, price_index_table = load_price_index_table()
) {
  base::message(
    "Running geographic sensitivity for ", scales::comma(base::nrow(locality_indices)),
    " localities."
  )

  validate_geographic_inputs(
    locality_indices = locality_indices, pfs_rvus = pfs_rvus,
    professional_mapping = professional_mapping, facility_mapping = facility_mapping
  )

  locality_runs <- purrr::map(locality_indices$locality_id, function(current_locality) {
    locality_label <- locality_indices %>%
      dplyr::filter(.data$locality_id == current_locality) %>%
      dplyr::pull(.data$locality_label)
    base::message("Processing locality: ", locality_label)

    geographic_bundle <- build_geographic_overrides(
      model_parameters = model_parameters, locality_id = current_locality,
      locality_indices = locality_indices, pfs_rvus = pfs_rvus,
      professional_mapping = professional_mapping, facility_mapping = facility_mapping
    )
    locality_parameters <- override_model_parameters(
      model_parameters, geographic_bundle$overrides
    )
    strategy_bundle <- compute_strategy_costs(
      locality_parameters, price_index_table = price_index_table
    )

    strategy_tbl <- strategy_bundle$strategy_costs %>%
      dplyr::mutate(locality_id = current_locality, locality_label = locality_label, .before = 1)
    audit_tbl <- geographic_bundle$adjustment_audit %>%
      dplyr::mutate(locality_id = current_locality, locality_label = locality_label, .before = 1)

    base::list(strategy_costs = strategy_tbl, adjustment_audit = audit_tbl)
  })

  strategy_tbl <- dplyr::bind_rows(purrr::map(locality_runs, "strategy_costs"))
  audit_tbl <- dplyr::bind_rows(purrr::map(locality_runs, "adjustment_audit"))

  base::message(
    "Geographic sensitivity complete: ", scales::comma(base::nrow(strategy_tbl)),
    " strategy-locality estimates."
  )

  base::list(strategy_costs = strategy_tbl, adjustment_audit = audit_tbl)
}

#' Summarize geographic sensitivity
#'
#' @param geographic_analysis Return value from [run_geographic_sensitivity()].
#' @return List with `locality_summary` (tibble) and `summary_sentence` (character).
#' @export
summarize_geographic_sensitivity <- function(geographic_analysis) {
  base::message("Summarizing geographic sensitivity.")

  locality_summary <- geographic_analysis$strategy_costs %>%
    dplyr::select("locality_id", "locality_label", "strategy", "expected_total_cost") %>%
    tidyr::pivot_wider(
      names_from = "strategy", values_from = "expected_total_cost",
      names_glue = "{strategy}_cost"
    ) %>%
    dplyr::mutate(
      combined_vs_office = .data$combined_emb_cost - .data$office_emb_cost,
      combined_savings_vs_office = .data$office_emb_cost - .data$combined_emb_cost,
      combined_cheaper_than_office = .data$combined_emb_cost < .data$office_emb_cost,
      combined_savings_vs_dnc = .data$dnc_cost - .data$combined_emb_cost
    ) %>%
    dplyr::arrange(.data$locality_label)

  n_localities <- base::nrow(locality_summary)
  n_combined_cheaper <- base::sum(locality_summary$combined_cheaper_than_office)
  minimum_savings <- base::min(locality_summary$combined_savings_vs_office)
  maximum_savings <- base::max(locality_summary$combined_savings_vs_office)

  summary_sentence <- base::paste0(
    "Across ", scales::comma(n_localities),
    " geographic sensitivity localities, combined EMB was less expensive than office EMB in ",
    scales::comma(n_combined_cheaper), " of ", scales::comma(n_localities),
    " localities; savings ranged from $", scales::comma(minimum_savings, accuracy = 0.01),
    " to $", scales::comma(maximum_savings, accuracy = 0.01), " per patient."
  )
  base::message(summary_sentence)

  base::list(locality_summary = locality_summary, summary_sentence = summary_sentence)
}

#' Save geographic sensitivity tables
#'
#' @param geographic_analysis Geographic analysis bundle.
#' @param geographic_summary Geographic summary bundle.
#' @param directory Destination directory.
#' @return Named character vector of saved file paths.
#' @export
save_geographic_sensitivity <- function(
  geographic_analysis, geographic_summary, directory = "tables"
) {
  if (!base::dir.exists(directory)) {
    base::dir.create(directory, recursive = TRUE, showWarnings = FALSE)
    base::message("Created directory: ", base::normalizePath(directory))
  }

  strategy_path <- base::file.path(directory, "geographic_strategy_costs.csv")
  audit_path <- base::file.path(directory, "geographic_adjustment_audit.csv")
  summary_path <- base::file.path(directory, "geographic_sensitivity_summary.csv")

  readr::write_csv(geographic_analysis$strategy_costs, strategy_path)
  base::message("Saved strategy costs: ", base::normalizePath(strategy_path))
  readr::write_csv(geographic_analysis$adjustment_audit, audit_path)
  base::message("Saved adjustment audit: ", base::normalizePath(audit_path))
  readr::write_csv(geographic_summary$locality_summary, summary_path)
  base::message("Saved geographic summary: ", base::normalizePath(summary_path))

  base::c(
    strategy_costs = strategy_path, adjustment_audit = audit_path,
    geographic_summary = summary_path
  )
}
