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

#' @export
read_meps_xlsx <- function(path) {
  base::message("Reading MEPS workbook: ", path)

  readxl::read_xlsx(path)
}

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
