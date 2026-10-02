# MEPS patient/societal burden estimation
#
# Estimates weighted office-visit total and out-of-pocket cost (2024
# MEPS office-based visit file) and hourly wage (2024 MEPS Jobs file),
# then converts an avoided additional visit into a patient time-cost
# estimate. Not used in the payer-perspective base case; feeds a future
# patient-time/societal-perspective extension. NOTE: verify the MEPS
# variable names below (OBXP24X, OBSF24X, PERWT24F, HRLYWAGE) against the
# current MEPS codebook before use -- they were not independently
# re-verified when this layer was integrated.

#' Read a MEPS data workbook
#'
#' @param path Path to the MEPS `.xlsx` file.
#' @return Tibble of the workbook's first sheet, as read by
#'   `readxl::read_xlsx()`.
#' @export
read_meps_xlsx <- function(path) {
  base::message("Reading MEPS workbook: ", path)

  readxl::read_xlsx(path)
}

#' Estimate weighted MEPS office-visit total and out-of-pocket cost
#'
#' Filters the MEPS office-based visit file to rows with a positive person
#' weight and non-negative payment amounts, then computes the person-weight
#' weighted mean total payment and out-of-pocket payment.
#'
#' @param office_tbl Tibble of the MEPS office-based visit file, with
#'   columns `OBXP24X` (total payment), `OBSF24X` (out-of-pocket amount),
#'   and `PERWT24F` (person weight).
#' @return Tibble with columns `metric` (`"total_payment"` or
#'   `"out_of_pocket"`) and `weighted_mean`.
#' @export
estimate_meps_office_visit_cost <- function(office_tbl) {
  base::message("Estimating weighted MEPS office-visit cost.")

  required_cols <- c(
    "OBXP24X",
    "OBSF24X",
    "PERWT24F"
  )

  missing_cols <- base::setdiff(
    required_cols,
    base::names(office_tbl)
  )

  if (base::length(missing_cols) > 0L) {
    base::stop(
      "MEPS office file missing: ",
      base::paste(missing_cols, collapse = ", ")
    )
  }

  valid_tbl <- office_tbl |>
    dplyr::filter(
      .data$PERWT24F > 0,
      .data$OBXP24X >= 0,
      .data$OBSF24X >= 0
    )

  tibble::tibble(
    metric = c(
      "total_payment",
      "out_of_pocket"
    ),
    weighted_mean = c(
      stats::weighted.mean(
        valid_tbl$OBXP24X,
        valid_tbl$PERWT24F,
        na.rm = TRUE
      ),
      stats::weighted.mean(
        valid_tbl$OBSF24X,
        valid_tbl$PERWT24F,
        na.rm = TRUE
      )
    )
  )
}

#' Estimate the weighted mean hourly wage from the MEPS Jobs file
#'
#' Filters to rows with a positive reported hourly wage and positive person
#' weight, then computes the person-weight weighted mean wage.
#'
#' @param jobs_tbl Tibble of the MEPS Jobs file, with columns `HRLYWAGE`
#'   (reported hourly wage) and `PERWT24F` (person weight).
#' @return One-row tibble with `population` (`"reported_hourly_wage"`),
#'   `n_jobs` (count of jobs used), and `weighted_mean_wage`.
#' @export
estimate_meps_hourly_wage <- function(jobs_tbl) {
  base::message("Estimating weighted MEPS hourly wage.")

  required_cols <- c(
    "HRLYWAGE",
    "PERWT24F"
  )

  missing_cols <- base::setdiff(
    required_cols,
    base::names(jobs_tbl)
  )

  if (base::length(missing_cols) > 0L) {
    base::stop(
      "MEPS jobs file missing: ",
      base::paste(missing_cols, collapse = ", ")
    )
  }

  wage_tbl <- jobs_tbl |>
    dplyr::filter(
      .data$HRLYWAGE > 0,
      .data$PERWT24F > 0
    )

  tibble::tibble(
    population = "reported_hourly_wage",
    n_jobs = base::nrow(wage_tbl),
    weighted_mean_wage = stats::weighted.mean(
      wage_tbl$HRLYWAGE,
      wage_tbl$PERWT24F,
      na.rm = TRUE
    )
  )
}

#' Convert avoided hours into a patient time-cost estimate
#'
#' Adds `avoided_hours` and `patient_time_cost` (`weighted_mean_wage *
#' avoided_hours`) columns to a wage summary tibble.
#'
#' @param wage_summary_tbl Tibble from [estimate_meps_hourly_wage()] (or
#'   equivalent), with a `weighted_mean_wage` column.
#' @param avoided_hours Numeric scalar, hours of patient time avoided.
#' @return `wage_summary_tbl` with `avoided_hours` and `patient_time_cost`
#'   columns added.
#' @export
estimate_patient_time_cost <- function(
    wage_summary_tbl,
    avoided_hours = 4) {
  base::message(
    "Estimating time cost for ",
    avoided_hours,
    " avoided hours."
  )

  wage_summary_tbl |>
    dplyr::mutate(
      avoided_hours = avoided_hours,
      patient_time_cost =
        .data$weighted_mean_wage *
        .data$avoided_hours
    )
}
