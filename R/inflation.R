# Inflation adjustment utilities
#
# Costs in this model come from different source years (e.g. the 2014
# ambulatory-OR cost-per-minute study vs. 2026 CMS fee-schedule figures).
# `adjust_for_inflation()` puts every cost on a common `reference_dollar_year`
# basis using a caller-supplied price-index table, keeping the adjustment
# mechanism decoupled from any specific index's actual values.
#
# `data/cpi_medical_care.csv` carries three BLS CPI-U Medical Care values
# (series CUUR0000SAM), all confirmed against the BLS Public Data API on
# 2026-09-13: the 2010 annual average (388.436) and July 2026 (593.781),
# used to cross-check the Ladabaum et al. 2011 office-EMB cost anchor, and
# the 2014 annual average (435.292), used for the Childers/Maggard-Gibbons
# per-minute OR/anesthesia parameters. The 2014 row replaced an
# interpolated placeholder (431.9). `is_placeholder` is kept so any future
# estimated row still triggers the loader's warning.

#' Adjust a cost from its source year to a reference year
#'
#' @param cost_value Numeric scalar. Cost in `source_year` dollars.
#' @param source_year Numeric scalar. Year `cost_value` is denominated in.
#' @param reference_year Numeric scalar. Target year to express the cost
#'   in.
#' @param price_index_table Tibble with columns `year` and `index_value`,
#'   e.g. as read from `data/cpi_medical_care.csv`.
#' @return Numeric scalar: `cost_value` rescaled to `reference_year`
#'   dollars. Returns `cost_value` unchanged (with a message) if
#'   `source_year` equals `reference_year` or is `NA`.
#' @export
adjust_for_inflation <- function(
  cost_value,
  source_year,
  reference_year,
  price_index_table
) {
  validate_non_negative(cost_value, "cost_value")

  if (base::is.na(source_year) || base::isTRUE(source_year == reference_year)) {
    return(cost_value)
  }

  required_columns <- c("year", "index_value")
  missing_columns <- base::setdiff(required_columns, base::names(price_index_table))
  if (length(missing_columns) > 0) {
    base::stop(
      "price_index_table is missing required column(s): ",
      base::paste(missing_columns, collapse = ", ")
    )
  }

  source_index <- price_index_table$index_value[
    price_index_table$year == source_year
  ]
  reference_index <- price_index_table$index_value[
    price_index_table$year == reference_year
  ]

  if (length(source_index) != 1) {
    base::stop(
      "price_index_table has no unique index_value for source_year = ",
      source_year
    )
  }
  if (length(reference_index) != 1) {
    base::stop(
      "price_index_table has no unique index_value for reference_year = ",
      reference_year
    )
  }
  if (!base::is.finite(source_index) || source_index <= 0) {
    base::stop(
      "price_index_table has a non-positive or non-finite index_value (",
      source_index, ") for source_year = ", source_year,
      " -- cannot use as a division denominator."
    )
  }
  if (!base::is.finite(reference_index) || reference_index <= 0) {
    base::stop(
      "price_index_table has a non-positive or non-finite index_value (",
      reference_index, ") for reference_year = ", reference_year, "."
    )
  }

  adjusted_cost <- cost_value * (reference_index / source_index)

  base::message(
    "  Inflation-adjusted $", base::round(cost_value, 2),
    " (", source_year, ") to $", base::round(adjusted_cost, 2),
    " (", reference_year, ")."
  )

  adjusted_cost
}

#' Load a price-index table
#'
#' @param path Character scalar path to a CSV with `year`, `index_value`,
#'   `index_source`, and `is_placeholder` columns.
#' @return A tibble price-index table.
#' @export
load_price_index_table <- function(path = "data/cpi_medical_care.csv") {
  if (!base::file.exists(path)) {
    base::stop("Price index file not found at: ", path)
  }

  price_index_table <- readr::read_csv(
    path,
    col_types = readr::cols(
      year = readr::col_double(),
      index_value = readr::col_double(),
      index_source = readr::col_character(),
      is_placeholder = readr::col_logical()
    ),
    show_col_types = FALSE
  )

  if (base::any(price_index_table$is_placeholder)) {
    base::message(
      "  WARNING: price index table at '", path, "' contains PLACEHOLDER ",
      "index values. Cost figures adjusted with this table are not yet ",
      "suitable for publication. See data-raw/00_get_price_index.R."
    )
  }

  price_index_table
}
