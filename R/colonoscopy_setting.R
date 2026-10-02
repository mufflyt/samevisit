#' CPT/HCPCS codes for the conservative "base" screening colonoscopy definition
#'
#' A narrower subset of `colonoscopy_setting_codes()` used for sensitivity
#' checks, restricted to the base screening/diagnostic colonoscopy codes.
#'
#' @return Character vector of CPT/HCPCS codes.
#' @export
colonoscopy_base_codes <- function() {
  base::c(
    "45378",
    "G0105",
    "G0121"
  )
}

#' CPT/HCPCS codes defining the colonoscopy setting cohort
#'
#' All CPT and HCPCS codes used to identify colonoscopy service lines in the
#' CMS physician/supplier data.
#'
#' @return Character vector of CPT/HCPCS codes.
#' @export
colonoscopy_setting_codes <- function() {
  base::c(
    "45378",
    "45380",
    "45382",
    "45384",
    "45385",
    "45386",
    "45388",
    "45389",
    "45390",
    "G0105",
    "G0121"
  )
}


#' Find the first matching CMS column name among candidates
#'
#' CMS physician/supplier PUF column names change across release years
#' (e.g. "Rndrng_NPI" vs "Rndrng_Prvdr_NPI"). This resolves which candidate
#' name is actually present in a given table.
#'
#' @param table_names Character vector of column names in the target table.
#' @param candidates Character vector of candidate column names to try, in
#'   priority order.
#' @param required Logical; if `TRUE` (default), error when no candidate is
#'   found. If `FALSE`, return `NA_character_` instead.
#' @return Character scalar: the first matching column name, or
#'   `NA_character_` if none match and `required` is `FALSE`.
#' @export
resolve_cms_column <- function(table_names,
                               candidates,
                               required = TRUE) {
  matched <- base::intersect(candidates, table_names)

  if (base::length(matched) > 0L) {
    return(matched[[1]])
  }

  if (required) {
    base::stop(
      "Could not find CMS column. Tried: ",
      base::paste(candidates, collapse = ", ")
    )
  }

  NA_character_
}

#' Pull a column from a CMS table, tolerating a missing column
#'
#' @param cms_tbl Tibble of CMS data.
#' @param column_name Character scalar naming the column to pull, or `NA`
#'   if the column was not found in `cms_tbl`.
#' @param default Value to recycle to `nrow(cms_tbl)` when `column_name` is
#'   `NA`.
#' @return Vector of length `nrow(cms_tbl)`: the named column, or `default`
#'   recycled if `column_name` is `NA`.
#' @export
pull_cms_column <- function(cms_tbl,
                            column_name,
                            default = NA_character_) {
  if (base::is.na(column_name)) {
    return(base::rep(default, base::nrow(cms_tbl)))
  }

  cms_tbl[[column_name]]
}

#' Classify a RUCA code into a rurality group
#'
#' @param ruca_code Numeric or character RUCA (Rural-Urban Commuting Area)
#'   code; coerced to numeric.
#' @return Character vector of the same length as `ruca_code`, one of
#'   "Metropolitan", "Micropolitan", "Small town", "Rural", or "Unknown".
#' @export
classify_ruca_group <- function(ruca_code) {
  numeric_code <- base::suppressWarnings(
    base::as.numeric(ruca_code)
  )

  dplyr::case_when(
    base::is.na(numeric_code) ~ "Unknown",
    numeric_code < 4 ~ "Metropolitan",
    numeric_code < 7 ~ "Micropolitan",
    numeric_code < 10 ~ "Small town",
    numeric_code >= 10 ~ "Rural",
    TRUE ~ "Unknown"
  )
}

#' Detect whether a provider type string denotes an ambulatory surgical center
#'
#' @param provider_type Character vector of CMS provider type descriptions.
#' @return Logical vector of the same length as `provider_type`.
#' @export
is_asc_provider_type <- function(provider_type) {
  normalized <- stringr::str_to_lower(
    stringr::str_squish(provider_type)
  )

  stringr::str_detect(
    normalized,
    "ambulatory surgical center"
  )
}

#' Standardize a raw CMS physician/supplier table to colonoscopy service rows
#'
#' Resolves the CMS PUF's year-varying column names, pulls out the columns
#' needed for colonoscopy setting analysis, filters to colonoscopy
#' CPT/HCPCS codes (`colonoscopy_setting_codes()`), and derives
#' `claim_role`, `place_group`, and `ruca_group`.
#'
#' @param cms_tbl Tibble of raw CMS "Medicare Physician & Other
#'   Practitioners - by Provider and Service" rows for one year.
#' @param data_year Integer or character year the rows in `cms_tbl` belong
#'   to; stored as the `year` column.
#' @return Tibble with one row per provider-service line, with columns
#'   `year`, `provider_npi`, `provider_name`, `provider_type`,
#'   `entity_code`, `street1`, `city`, `state`, `zip5`, `ruca`,
#'   `ruca_description`, `hcpcs_code`, `place_of_service`, `services`,
#'   `beneficiaries`, `beneficiary_day_services`, `submitted_charge`,
#'   `allowed_amount`, `payment_amount`, `claim_role`
#'   ("asc_facility"/"professional"), `place_group`
#'   ("Facility"/"Nonfacility"/"Unknown"), and `ruca_group`.
#' @export
standardize_physician_colonoscopy <- function(cms_tbl,
                                               data_year) {
  base::message(
    "Standardizing CMS physician/supplier colonoscopy rows for ",
    data_year,
    "."
  )

  table_names <- base::names(cms_tbl)

  npi_col <- resolve_cms_column(
    table_names,
    base::c("Rndrng_NPI", "Rndrng_Prvdr_NPI")
  )
  provider_type_col <- resolve_cms_column(
    table_names,
    base::c("Rndrng_Prvdr_Type", "Provider_Type")
  )
  entity_col <- resolve_cms_column(
    table_names,
    base::c("Rndrng_Prvdr_Ent_Cd", "Rndrng_Prvdr_Ent_Type"),
    required = FALSE
  )
  last_name_col <- resolve_cms_column(
    table_names,
    base::c(
      "Rndrng_Prvdr_Last_Org_Name",
      "Rndrng_Prvdr_Last_Org"
    ),
    required = FALSE
  )
  first_name_col <- resolve_cms_column(
    table_names,
    base::c("Rndrng_Prvdr_First_Name"),
    required = FALSE
  )
  street_col <- resolve_cms_column(
    table_names,
    base::c("Rndrng_Prvdr_St1"),
    required = FALSE
  )
  city_col <- resolve_cms_column(
    table_names,
    base::c("Rndrng_Prvdr_City"),
    required = FALSE
  )
  state_col <- resolve_cms_column(
    table_names,
    base::c(
      "Rndrng_Prvdr_State_Abrvtn",
      "Rndrng_Prvdr_State_Abvrtn"
    )
  )
  zip_col <- resolve_cms_column(
    table_names,
    base::c("Rndrng_Prvdr_Zip5", "Rndrng_Prvdr_Zip")
  )
  ruca_col <- resolve_cms_column(
    table_names,
    base::c("Rndrng_Prvdr_RUCA")
  )
  ruca_desc_col <- resolve_cms_column(
    table_names,
    base::c("Rndrng_Prvdr_RUCA_Desc"),
    required = FALSE
  )
  hcpcs_col <- resolve_cms_column(
    table_names,
    base::c("HCPCS_Cd", "HCPCS_Code")
  )
  place_col <- resolve_cms_column(
    table_names,
    base::c("Place_Of_Srvc", "Place_of_Srvc")
  )
  service_col <- resolve_cms_column(
    table_names,
    base::c("Tot_Srvcs", "Line_Srvc_Cnt")
  )
  bene_col <- resolve_cms_column(
    table_names,
    base::c("Tot_Benes", "Bene_Cnt"),
    required = FALSE
  )
  bene_day_col <- resolve_cms_column(
    table_names,
    base::c("Tot_Bene_Day_Srvcs"),
    required = FALSE
  )
  allowed_col <- resolve_cms_column(
    table_names,
    base::c("Avg_Mdcr_Alowd_Amt")
  )
  payment_col <- resolve_cms_column(
    table_names,
    base::c("Avg_Mdcr_Pymt_Amt")
  )
  charge_col <- resolve_cms_column(
    table_names,
    base::c("Avg_Sbmtd_Chrg", "Avg_Sbmtd_Chrg_Amt"),
    required = FALSE
  )

  last_name <- base::as.character(
    pull_cms_column(cms_tbl, last_name_col)
  )
  first_name <- base::as.character(
    pull_cms_column(cms_tbl, first_name_col)
  )

  provider_name <- base::ifelse(
    base::is.na(first_name) | first_name == "",
    last_name,
    base::paste(last_name, first_name)
  )
  provider_name <- stringr::str_squish(provider_name)
  provider_name[provider_name %in% base::c("NA", "")] <- NA_character_

  provider_type <- base::as.character(
    cms_tbl[[provider_type_col]]
  )

  standardized_tbl <- tibble::tibble(
    year = base::as.integer(data_year),
    provider_npi = base::as.character(cms_tbl[[npi_col]]),
    provider_name = provider_name,
    provider_type = provider_type,
    entity_code = base::as.character(
      pull_cms_column(cms_tbl, entity_col)
    ),
    street1 = base::as.character(
      pull_cms_column(cms_tbl, street_col)
    ),
    city = base::as.character(
      pull_cms_column(cms_tbl, city_col)
    ),
    state = base::as.character(cms_tbl[[state_col]]),
    zip5 = base::as.character(cms_tbl[[zip_col]]),
    ruca = base::suppressWarnings(
      base::as.numeric(cms_tbl[[ruca_col]])
    ),
    ruca_description = base::as.character(
      pull_cms_column(cms_tbl, ruca_desc_col)
    ),
    hcpcs_code = base::as.character(cms_tbl[[hcpcs_col]]),
    place_of_service = base::as.character(cms_tbl[[place_col]]),
    services = base::suppressWarnings(
      base::as.numeric(cms_tbl[[service_col]])
    ),
    beneficiaries = base::suppressWarnings(
      base::as.numeric(pull_cms_column(cms_tbl, bene_col))
    ),
    beneficiary_day_services = base::suppressWarnings(
      base::as.numeric(
        pull_cms_column(cms_tbl, bene_day_col)
      )
    ),
    submitted_charge = base::suppressWarnings(
      base::as.numeric(pull_cms_column(cms_tbl, charge_col))
    ),
    allowed_amount = base::suppressWarnings(
      base::as.numeric(cms_tbl[[allowed_col]])
    ),
    payment_amount = base::suppressWarnings(
      base::as.numeric(cms_tbl[[payment_col]])
    )
  ) |>
    dplyr::filter(
      .data$hcpcs_code %in% colonoscopy_setting_codes()
    ) |>
    dplyr::mutate(
      claim_role = dplyr::if_else(
        is_asc_provider_type(.data$provider_type),
        "asc_facility",
        "professional"
      ),
      place_group = dplyr::case_when(
        .data$place_of_service == "F" ~ "Facility",
        .data$place_of_service == "O" ~ "Nonfacility",
        TRUE ~ "Unknown"
      ),
      ruca_group = classify_ruca_group(.data$ruca)
    )

  base::message(
    "Standardized ",
    scales::comma(base::nrow(standardized_tbl)),
    " colonoscopy provider-service rows for ",
    data_year,
    "."
  )

  standardized_tbl
}

#' Summarize the mix of colonoscopy CPT/HCPCS codes by place of service
#'
#' @param physician_tbl Tibble of standardized colonoscopy rows, as
#'   returned by `standardize_physician_colonoscopy()`.
#' @return Tibble with one row per `year`/`hcpcs_code`/`place_group`,
#'   including `observed_services`, `code_total_services`, and
#'   `service_share` (share of each code's services falling in that place
#'   group).
#' @export
summarize_colonoscopy_code_mix <- function(physician_tbl) {
  base::message("Summarizing colonoscopy-coded service mix.")

  physician_tbl |>
    dplyr::filter(.data$claim_role == "professional") |>
    dplyr::group_by(
      .data$year,
      .data$hcpcs_code,
      .data$place_group
    ) |>
    dplyr::summarise(
      observed_services = base::sum(
        .data$services,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::group_by(.data$year, .data$hcpcs_code) |>
    dplyr::mutate(
      code_total_services = base::sum(
        .data$observed_services,
        na.rm = TRUE
      ),
      service_share = .data$observed_services /
        .data$code_total_services
    ) |>
    dplyr::ungroup()
}

#' Summarize place-of-service share using the conservative base-code definition
#'
#' Sensitivity check that repeats the facility-vs-nonfacility share
#' calculation restricted to `colonoscopy_base_codes()`, using
#' beneficiary-day services (falling back to services) as the encounter
#' proxy.
#'
#' @param physician_tbl Tibble of standardized colonoscopy rows, as
#'   returned by `standardize_physician_colonoscopy()`.
#' @return Tibble with one row per `year`/`place_group`, including
#'   `observed_encounter_proxy`, `total_observed_encounter_proxy`, and
#'   `service_share`.
#' @export
summarize_base_code_place <- function(physician_tbl) {
  base::message(
    "Running conservative base/screening-code setting sensitivity."
  )

  base_code_tbl <- physician_tbl |>
    dplyr::filter(
      .data$hcpcs_code %in% colonoscopy_base_codes()
    ) |>
    dplyr::mutate(
      encounter_proxy = dplyr::coalesce(
        .data$beneficiary_day_services,
        .data$services
      )
    ) |>
    dplyr::filter(
      .data$claim_role == "professional",
      .data$place_group %in% base::c(
        "Facility",
        "Nonfacility"
      )
    )

  base_code_tbl |>
    dplyr::group_by(.data$year, .data$place_group) |>
    dplyr::summarise(
      observed_encounter_proxy = base::sum(
        .data$encounter_proxy,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::group_by(.data$year) |>
    dplyr::mutate(
      total_observed_encounter_proxy = base::sum(
        .data$observed_encounter_proxy,
        na.rm = TRUE
      ),
      service_share = .data$observed_encounter_proxy /
        .data$total_observed_encounter_proxy
    ) |>
    dplyr::ungroup()
}


#' Weighted standard deviation
#'
#' @param x Numeric vector of values.
#' @param weights Numeric vector of weights, same length as `x`.
#' @return Numeric scalar weighted standard deviation, or `NA_real_` if
#'   fewer than two finite, positively-weighted observations remain.
#' @export
weighted_sd <- function(x,
                        weights) {
  valid <- base::is.finite(x) &
    base::is.finite(weights) &
    weights > 0

  x_valid <- x[valid]
  weight_valid <- weights[valid]

  if (base::length(x_valid) < 2L) {
    return(NA_real_)
  }

  weighted_mean <- stats::weighted.mean(
    x_valid,
    weight_valid
  )

  variance <- base::sum(
    weight_valid * (x_valid - weighted_mean)^2
  ) / base::sum(weight_valid)

  base::sqrt(variance)
}

#' Summarize colonoscopy services by facility versus nonfacility place of service
#'
#' @param physician_tbl Tibble of standardized colonoscopy rows, as
#'   returned by `standardize_physician_colonoscopy()`.
#' @return Tibble with one row per `year`/`place_group`, including
#'   `observed_services`, `n_provider_service_rows`,
#'   `total_observed_services`, and `service_share`.
#' @export
summarize_colonoscopy_place <- function(physician_tbl) {
  base::message(
    "Summarizing colonoscopy facility versus nonfacility services."
  )

  professional_tbl <- physician_tbl |>
    dplyr::filter(
      .data$claim_role == "professional",
      .data$place_group %in% base::c(
        "Facility",
        "Nonfacility"
      )
    )

  summary_tbl <- professional_tbl |>
    dplyr::group_by(.data$year, .data$place_group) |>
    dplyr::summarise(
      observed_services = base::sum(
        .data$services,
        na.rm = TRUE
      ),
      n_provider_service_rows = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::group_by(.data$year) |>
    dplyr::mutate(
      total_observed_services = base::sum(
        .data$observed_services,
        na.rm = TRUE
      ),
      service_share = .data$observed_services /
        .data$total_observed_services
    ) |>
    dplyr::ungroup()

  base::message("Completed place-of-service summary.")

  summary_tbl
}

#' Summarize colonoscopy setting share by provider state
#'
#' @param physician_tbl Tibble of standardized colonoscopy rows, as
#'   returned by `standardize_physician_colonoscopy()`.
#' @return Tibble with one row per `year`/`state`/`place_group`, including
#'   `observed_services`, `total_observed_services`, and `service_share`
#'   (share within that state-year).
#' @export
summarize_colonoscopy_state <- function(physician_tbl) {
  base::message("Summarizing colonoscopy setting by provider state.")

  professional_tbl <- physician_tbl |>
    dplyr::filter(
      .data$claim_role == "professional",
      .data$place_group %in% base::c(
        "Facility",
        "Nonfacility"
      )
    )

  professional_tbl |>
    dplyr::group_by(
      .data$year,
      .data$state,
      .data$place_group
    ) |>
    dplyr::summarise(
      observed_services = base::sum(
        .data$services,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::group_by(.data$year, .data$state) |>
    dplyr::mutate(
      total_observed_services = base::sum(
        .data$observed_services,
        na.rm = TRUE
      ),
      service_share = .data$observed_services /
        .data$total_observed_services
    ) |>
    dplyr::ungroup()
}

#' Summarize colonoscopy setting share by provider RUCA rurality group
#'
#' @param physician_tbl Tibble of standardized colonoscopy rows, as
#'   returned by `standardize_physician_colonoscopy()`.
#' @return Tibble with one row per `year`/`ruca_group`/`place_group`,
#'   including `observed_services`, `total_observed_services`, and
#'   `service_share` (share within that rurality-year).
#' @export
summarize_colonoscopy_rurality <- function(physician_tbl) {
  base::message("Summarizing colonoscopy setting by provider RUCA group.")

  physician_tbl |>
    dplyr::filter(
      .data$claim_role == "professional",
      .data$place_group %in% base::c(
        "Facility",
        "Nonfacility"
      )
    ) |>
    dplyr::group_by(
      .data$year,
      .data$ruca_group,
      .data$place_group
    ) |>
    dplyr::summarise(
      observed_services = base::sum(
        .data$services,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::group_by(.data$year, .data$ruca_group) |>
    dplyr::mutate(
      total_observed_services = base::sum(
        .data$observed_services,
        na.rm = TRUE
      ),
      service_share = .data$observed_services /
        .data$total_observed_services
    ) |>
    dplyr::ungroup()
}

#' Summarize colonoscopy services by provider specialty
#'
#' @param physician_tbl Tibble of standardized colonoscopy rows, as
#'   returned by `standardize_physician_colonoscopy()`.
#' @return Tibble with one row per `year`/`provider_type`, including
#'   `observed_services`, `unique_providers`, `total_observed_services`,
#'   and `service_share`, sorted by year and descending observed services.
#' @export
summarize_colonoscopy_specialty <- function(physician_tbl) {
  base::message("Summarizing colonoscopy services by provider specialty.")

  physician_tbl |>
    dplyr::filter(.data$claim_role == "professional") |>
    dplyr::group_by(
      .data$year,
      .data$provider_type
    ) |>
    dplyr::summarise(
      observed_services = base::sum(
        .data$services,
        na.rm = TRUE
      ),
      unique_providers = dplyr::n_distinct(
        .data$provider_npi
      ),
      .groups = "drop"
    ) |>
    dplyr::group_by(.data$year) |>
    dplyr::mutate(
      total_observed_services = base::sum(
        .data$observed_services,
        na.rm = TRUE
      ),
      service_share = .data$observed_services /
        .data$total_observed_services
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(
      .data$year,
      dplyr::desc(.data$observed_services)
    )
}

#' Summarize Medicare allowed amounts for colonoscopy services
#'
#' @param physician_tbl Tibble of standardized colonoscopy rows, as
#'   returned by `standardize_physician_colonoscopy()`.
#' @return Tibble with one row per `year`/`place_group`, including
#'   `observed_services`, `weighted_mean_allowed`, `weighted_sd_allowed`,
#'   `median_allowed`, `p25_allowed`, and `p75_allowed`.
#' @export
summarize_colonoscopy_allowed <- function(physician_tbl) {
  base::message("Summarizing colonoscopy Medicare allowed amounts.")

  physician_tbl |>
    dplyr::filter(
      .data$claim_role == "professional",
      .data$place_group %in% base::c(
        "Facility",
        "Nonfacility"
      ),
      base::is.finite(.data$allowed_amount),
      base::is.finite(.data$services),
      .data$services > 0
    ) |>
    dplyr::group_by(.data$year, .data$place_group) |>
    dplyr::summarise(
      observed_services = base::sum(
        .data$services,
        na.rm = TRUE
      ),
      weighted_mean_allowed = stats::weighted.mean(
        .data$allowed_amount,
        .data$services,
        na.rm = TRUE
      ),
      weighted_sd_allowed = weighted_sd(
        .data$allowed_amount,
        .data$services
      ),
      median_allowed = stats::median(
        .data$allowed_amount,
        na.rm = TRUE
      ),
      p25_allowed = stats::quantile(
        .data$allowed_amount,
        probs = 0.25,
        na.rm = TRUE
      ),
      p75_allowed = stats::quantile(
        .data$allowed_amount,
        probs = 0.75,
        na.rm = TRUE
      ),
      .groups = "drop"
    )
}

#' Summarize provider-level concentration of colonoscopy services
#'
#' Computes the Herfindahl-Hirschman Index (HHI) and top-N provider service
#' shares for professional colonoscopy claims, by year.
#'
#' @param physician_tbl Tibble of standardized colonoscopy rows, as
#'   returned by `standardize_physician_colonoscopy()`.
#' @return Tibble with one row per `year`, including `unique_providers`,
#'   `total_observed_services`, `hhi` (0-1 scale), `hhi_10000` (0-10,000
#'   scale), `top_1_share`, `top_10_share`, and `top_50_share`.
#' @export
summarize_colonoscopy_concentration <- function(physician_tbl) {
  base::message("Calculating colonoscopy provider concentration.")

  provider_tbl <- physician_tbl |>
    dplyr::filter(.data$claim_role == "professional") |>
    dplyr::group_by(.data$year, .data$provider_npi) |>
    dplyr::summarise(
      observed_services = base::sum(
        .data$services,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::filter(.data$observed_services > 0) |>
    dplyr::group_by(.data$year) |>
    dplyr::mutate(
      service_share = .data$observed_services /
        base::sum(.data$observed_services)
    ) |>
    dplyr::arrange(
      .data$year,
      dplyr::desc(.data$observed_services)
    ) |>
    dplyr::mutate(rank = dplyr::row_number()) |>
    dplyr::ungroup()

  provider_tbl |>
    dplyr::group_by(.data$year) |>
    dplyr::summarise(
      unique_providers = dplyr::n(),
      total_observed_services = base::sum(
        .data$observed_services
      ),
      hhi = base::sum(.data$service_share^2),
      hhi_10000 = 10000 * base::sum(.data$service_share^2),
      top_1_share = base::sum(
        .data$service_share[.data$rank <= 1L]
      ),
      top_10_share = base::sum(
        .data$service_share[.data$rank <= 10L]
      ),
      top_50_share = base::sum(
        .data$service_share[.data$rank <= 50L]
      ),
      .groups = "drop"
    )
}

#' Build a directory of observed ambulatory surgical centers billing colonoscopy
#'
#' @param physician_tbl Tibble of standardized colonoscopy rows, as
#'   returned by `standardize_physician_colonoscopy()`.
#' @return Tibble with one row per `year`/ASC provider (identified by NPI,
#'   name, and address fields), including `observed_services` and
#'   `weighted_mean_allowed`, sorted by year and descending observed
#'   services.
#' @export
build_asc_colonoscopy_directory <- function(physician_tbl) {
  base::message("Building observed ASC colonoscopy directory.")

  physician_tbl |>
    dplyr::filter(.data$claim_role == "asc_facility") |>
    dplyr::group_by(
      .data$year,
      .data$provider_npi,
      .data$provider_name,
      .data$street1,
      .data$city,
      .data$state,
      .data$zip5,
      .data$ruca_group
    ) |>
    dplyr::summarise(
      observed_services = base::sum(
        .data$services,
        na.rm = TRUE
      ),
      weighted_mean_allowed = stats::weighted.mean(
        .data$allowed_amount,
        .data$services,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::arrange(
      .data$year,
      dplyr::desc(.data$observed_services)
    )
}

#' Estimate the ASC share of facility-setting colonoscopy services
#'
#' The public CMS physician/supplier PUF does not separately break out
#' facility services by facility type, so this estimates the ASC share of
#' observed facility-setting services from the directly billed ASC rows,
#' with the remainder treated as a residual "other facility" share.
#'
#' @param physician_tbl Tibble of standardized colonoscopy rows, as
#'   returned by `standardize_physician_colonoscopy()`.
#' @return Tibble with one row per `year`, including
#'   `professional_facility_services`, `asc_observed_services`,
#'   `residual_other_facility_services`, `asc_share_raw`,
#'   `asc_share_of_facility` (capped at 1), and `suppression_mismatch`
#'   (`TRUE` if observed ASC services exceed observed facility services,
#'   indicating a data suppression artifact).
#' @export
estimate_facility_type_share <- function(physician_tbl) {
  base::message(
    "Estimating ASC versus other-facility share from public PUF rows."
  )

  professional_facility_tbl <- physician_tbl |>
    dplyr::filter(
      .data$claim_role == "professional",
      .data$place_group == "Facility"
    ) |>
    dplyr::group_by(.data$year) |>
    dplyr::summarise(
      professional_facility_services = base::sum(
        .data$services,
        na.rm = TRUE
      ),
      .groups = "drop"
    )

  asc_tbl <- physician_tbl |>
    dplyr::filter(.data$claim_role == "asc_facility") |>
    dplyr::group_by(.data$year) |>
    dplyr::summarise(
      asc_observed_services = base::sum(
        .data$services,
        na.rm = TRUE
      ),
      .groups = "drop"
    )

  dplyr::left_join(
    professional_facility_tbl,
    asc_tbl,
    by = "year"
  ) |>
    dplyr::mutate(
      asc_observed_services = tidyr::replace_na(
        .data$asc_observed_services,
        0
      ),
      residual_other_facility_services = base::pmax(
        .data$professional_facility_services -
          .data$asc_observed_services,
        0
      ),
      asc_share_raw = dplyr::if_else(
        .data$professional_facility_services > 0,
        .data$asc_observed_services /
          .data$professional_facility_services,
        NA_real_
      ),
      asc_share_of_facility = base::pmin(
        .data$asc_share_raw,
        1
      ),
      suppression_mismatch =
        .data$asc_observed_services >
          .data$professional_facility_services
    )
}

#' Decompose observed colonoscopy services into office, ASC, and residual facility settings
#'
#' @param physician_tbl Tibble of standardized colonoscopy rows, as
#'   returned by `standardize_physician_colonoscopy()`.
#' @param data_year Year to filter `physician_tbl` to before decomposing.
#' @return Tibble with one row per `setting` ("Office/nonfacility", "ASC",
#'   "Other facility residual") for `data_year`, including
#'   `observed_services`, `suppression_mismatch` (`TRUE` if observed ASC
#'   services exceed observed facility services), `total_observed_services`,
#'   and `observed_share`.
#' @export
decompose_colonoscopy_observed_settings <- function(physician_tbl,
                                                    data_year) {
  base::message(
    "Decomposing observed office, ASC, and residual facility services for ",
    data_year,
    "."
  )

  office_services <- physician_tbl |>
    dplyr::filter(
      .data$year == data_year,
      .data$claim_role == "professional",
      .data$place_group == "Nonfacility"
    ) |>
    dplyr::summarise(
      value = base::sum(.data$services, na.rm = TRUE)
    ) |>
    dplyr::pull(.data$value)

  facility_services <- physician_tbl |>
    dplyr::filter(
      .data$year == data_year,
      .data$claim_role == "professional",
      .data$place_group == "Facility"
    ) |>
    dplyr::summarise(
      value = base::sum(.data$services, na.rm = TRUE)
    ) |>
    dplyr::pull(.data$value)

  asc_services <- physician_tbl |>
    dplyr::filter(
      .data$year == data_year,
      .data$claim_role == "asc_facility"
    ) |>
    dplyr::summarise(
      value = base::sum(.data$services, na.rm = TRUE)
    ) |>
    dplyr::pull(.data$value)

  mismatch <- asc_services > facility_services
  residual_services <- base::pmax(
    facility_services - asc_services,
    0
  )

  tibble::tibble(
    year = data_year,
    setting = base::c(
      "Office/nonfacility",
      "ASC",
      "Other facility residual"
    ),
    observed_services = base::c(
      office_services,
      asc_services,
      residual_services
    ),
    suppression_mismatch = mismatch
  ) |>
    dplyr::mutate(
      total_observed_services = base::sum(
        .data$observed_services,
        na.rm = TRUE
      ),
      observed_share = .data$observed_services /
        .data$total_observed_services
    )
}


#' Fit a linear trend of facility-setting share over time
#'
#' @param place_tbl Tibble with `place_group`, `year`, `service_share`, and
#'   `total_observed_services` columns, as returned by
#'   `summarize_colonoscopy_place()`.
#' @return A one-row tibble with `first_year`, `last_year`, `first_share`,
#'   `last_share`, `annual_change_pp` (slope in percentage points per
#'   year), `p_value`, and `total_services`. Errors if fewer than three
#'   years of facility-share data are available.
#' @export
fit_colonoscopy_facility_trend <- function(place_tbl) {
  base::message("Fitting facility-share trend across available years.")

  facility_tbl <- place_tbl |>
    dplyr::filter(.data$place_group == "Facility") |>
    dplyr::arrange(.data$year)

  if (base::nrow(facility_tbl) < 3L) {
    base::stop("At least three years are required for trend analysis.")
  }

  trend_fit <- stats::lm(
    service_share ~ year,
    data = facility_tbl
  )

  fit_summary <- base::summary(trend_fit)
  coefficient_tbl <- fit_summary$coefficients
  slope <- coefficient_tbl["year", "Estimate"]
  p_value <- coefficient_tbl["year", "Pr(>|t|)"]

  tibble::tibble(
    first_year = base::min(facility_tbl$year),
    last_year = base::max(facility_tbl$year),
    first_share = facility_tbl$service_share[[1]],
    last_share = facility_tbl$service_share[[
      base::nrow(facility_tbl)
    ]],
    annual_change_pp = slope * 100,
    p_value = p_value,
    total_services = base::sum(
      facility_tbl$total_observed_services,
      na.rm = TRUE
    )
  )
}

#' Format a colonoscopy facility-share trend as a reporting sentence
#'
#' @param trend_tbl One-row tibble as returned by
#'   `fit_colonoscopy_facility_trend()`.
#' @return Character scalar summarizing the trend's direction, start/end
#'   shares, annual change, p-value, and (if present) total observed
#'   services.
#' @export
format_colonoscopy_trend_sentence <- function(trend_tbl) {
  direction <- dplyr::case_when(
    trend_tbl$annual_change_pp[[1]] > 0 ~ "increased",
    trend_tbl$annual_change_pp[[1]] < 0 ~ "decreased",
    TRUE ~ "did not change"
  )

  p_value <- trend_tbl$p_value[[1]]
  p_text <- if (base::is.na(p_value)) {
    "p-value unavailable"
  } else if (p_value < 0.001) {
    "p < 0.001"
  } else {
    base::paste0(
      "p = ",
      base::formatC(
        p_value,
        format = "f",
        digits = 3
      )
    )
  }

  service_text <- ""
  if ("total_services" %in% base::names(trend_tbl)) {
    service_text <- base::paste0(
      " Across ",
      scales::comma(
        base::round(trend_tbl$total_services[[1]])
      ),
      " observed services,"
    )
  }

  base::paste0(
    "From ",
    trend_tbl$first_year[[1]],
    " to ",
    trend_tbl$last_year[[1]],
    ", the facility share of Medicare colonoscopy services ",
    direction,
    " from ",
    scales::percent(
      trend_tbl$first_share[[1]],
      accuracy = 0.1
    ),
    " to ",
    scales::percent(
      trend_tbl$last_share[[1]],
      accuracy = 0.1
    ),
    ".",
    service_text,
    " the estimated annual change was ",
    base::formatC(
      trend_tbl$annual_change_pp[[1]],
      format = "f",
      digits = 2
    ),
    " percentage points (",
    p_text,
    ")."
  )
}

#' Plot colonoscopy place-of-service share over time
#'
#' @param place_tbl Tibble with `year`, `service_share`, and `place_group`
#'   columns, as returned by `summarize_colonoscopy_place()`.
#' @return A ggplot object: a line-and-point chart of service share by
#'   year, one line per place-of-service group.
#' @export
plot_colonoscopy_place_trend <- function(place_tbl) {
  base::message("Creating colonoscopy place-of-service trend figure.")

  ggplot2::ggplot(
    place_tbl,
    ggplot2::aes(
      x = .data$year,
      y = .data$service_share,
      group = .data$place_group,
      linetype = .data$place_group
    )
  ) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(accuracy = 1)
    ) +
    ggplot2::labs(
      x = "Year",
      y = "Share of observed services",
      linetype = "Setting",
      title = "Medicare colonoscopy services by place of service"
    ) +
    ggplot2::theme_minimal()
}

#' Plot facility-setting colonoscopy share by provider state for one year
#'
#' @param state_tbl Tibble with `year`, `place_group`, `state`, and
#'   `service_share` columns, as returned by
#'   `summarize_colonoscopy_state()`.
#' @param data_year Year to filter `state_tbl` to before plotting.
#' @return A ggplot object: a dot plot of facility service share by
#'   state, states ordered by ascending share.
#' @export
plot_state_facility_share <- function(state_tbl,
                                      data_year) {
  base::message(
    "Creating state facility-share figure for ",
    data_year,
    "."
  )

  plotting_tbl <- state_tbl |>
    dplyr::filter(
      .data$year == data_year,
      .data$place_group == "Facility"
    ) |>
    dplyr::arrange(.data$service_share) |>
    dplyr::mutate(
      state = base::factor(.data$state, levels = .data$state)
    )

  ggplot2::ggplot(
    plotting_tbl,
    ggplot2::aes(
      x = .data$service_share,
      y = .data$state
    )
  ) +
    ggplot2::geom_point() +
    ggplot2::scale_x_continuous(
      labels = scales::label_percent(accuracy = 1)
    ) +
    ggplot2::labs(
      x = "Facility share of observed colonoscopy services",
      y = "Provider state",
      title = base::paste(
        data_year,
        "Medicare colonoscopy facility share by provider state"
      )
    ) +
    ggplot2::theme_minimal()
}

#' Save a colonoscopy summary table as a timestamped CSV
#'
#' @param table_tbl Tibble to write to disk.
#' @param directory Directory to write into; created recursively if it
#'   does not exist.
#' @param stem File name stem; the saved file is
#'   `<stem>_<YYYYMMDD_HHMMSS>.csv`.
#' @return Invisibly, the character path the table was written to.
#' @export
save_colonoscopy_table <- function(table_tbl,
                                   directory,
                                   stem) {
  if (!base::dir.exists(directory)) {
    base::dir.create(directory, recursive = TRUE)
  }

  stamp <- base::format(
    base::Sys.time(),
    "%Y%m%d_%H%M%S"
  )
  path <- base::file.path(
    directory,
    base::paste0(stem, "_", stamp, ".csv")
  )

  readr::write_csv(table_tbl, path)
  base::message("Saved colonoscopy table: ", path)

  base::invisible(path)
}

#' Save a colonoscopy figure as a timestamped PNG
#'
#' @param figure_obj ggplot object to save.
#' @param directory Directory to write into; created recursively if it
#'   does not exist.
#' @param stem File name stem; the saved file is
#'   `<stem>_<YYYYMMDD_HHMMSS>.png`.
#' @param width Figure width in inches, passed to `ggplot2::ggsave()`.
#' @param height Figure height in inches, passed to `ggplot2::ggsave()`.
#' @return Invisibly, the character path the figure was written to.
#' @export
save_colonoscopy_figure <- function(figure_obj,
                                    directory,
                                    stem,
                                    width = 8,
                                    height = 6) {
  if (!base::dir.exists(directory)) {
    base::dir.create(directory, recursive = TRUE)
  }

  stamp <- base::format(
    base::Sys.time(),
    "%Y%m%d_%H%M%S"
  )
  path <- base::file.path(
    directory,
    base::paste0(stem, "_", stamp, ".png")
  )

  ggplot2::ggsave(
    filename = path,
    plot = figure_obj,
    width = width,
    height = height,
    dpi = 300
  )
  base::message("Saved colonoscopy figure: ", path)

  base::invisible(path)
}

#' Find the most recent cached CMS colonoscopy extract for a year
#'
#' @param cache_dir Directory to search for cached files.
#' @param data_year Year the cached file name must match.
#' @return Character scalar path to the most recent matching cache file
#'   (by timestamp in the file name), or `NA_character_` if `cache_dir`
#'   does not exist or no matching file is found.
#' @export
latest_colonoscopy_cache_path <- function(cache_dir,
                                          data_year) {
  if (!base::dir.exists(cache_dir)) {
    return(NA_character_)
  }

  pattern <- base::paste0(
    "^cms_colonoscopy_",
    data_year,
    "_[0-9]{8}_[0-9]{6}\\.csv$"
  )

  paths <- base::list.files(
    cache_dir,
    pattern = pattern,
    full.names = TRUE
  )

  if (base::length(paths) == 0L) {
    return(NA_character_)
  }

  base::sort(paths, decreasing = TRUE)[[1]]
}

#' Save standardized CMS colonoscopy rows as a timestamped cache CSV
#'
#' @param colonoscopy_tbl Tibble of standardized colonoscopy rows to cache.
#' @param cache_dir Directory to write into; created recursively if it
#'   does not exist.
#' @param data_year Year the rows belong to; used in the cache file name.
#' @return Invisibly, the character path the cache file was written to.
#' @export
save_colonoscopy_cache <- function(colonoscopy_tbl,
                                   cache_dir,
                                   data_year) {
  if (!base::dir.exists(cache_dir)) {
    base::dir.create(cache_dir, recursive = TRUE)
  }

  stamp <- base::format(
    base::Sys.time(),
    "%Y%m%d_%H%M%S"
  )
  path <- base::file.path(
    cache_dir,
    base::paste0(
      "cms_colonoscopy_",
      data_year,
      "_",
      stamp,
      ".csv"
    )
  )

  readr::write_csv(colonoscopy_tbl, path)
  base::message("Saved CMS colonoscopy cache: ", path)

  base::invisible(path)
}

#' Fetch (or load cached) standardized CMS colonoscopy rows for one year
#'
#' Reads from the most recent matching cache file when available and
#' `use_cache` is `TRUE`; otherwise queries the CMS data API for each
#' colonoscopy HCPCS code, standardizes the result with
#' `standardize_physician_colonoscopy()`, and writes a fresh cache file.
#'
#' @param data_year Year to fetch.
#' @param title_pattern Character scalar CMS dataset title pattern to
#'   search for; defaults to the "Medicare Physician & Other Practitioners
#'   - by Provider and Service" dataset title when `NULL`.
#' @param cache_dir Directory to read/write the cache file in.
#' @param use_cache Logical; if `TRUE` (default), return a cached extract
#'   when one exists instead of re-fetching from the CMS API.
#' @return Tibble of standardized colonoscopy provider-service rows for
#'   `data_year` (cached, read from CSV, or freshly standardized).
#' @export
fetch_colonoscopy_physician_year <- function(
    data_year,
    title_pattern = NULL,
    cache_dir = "data-raw/cms_colonoscopy",
    use_cache = TRUE) {
  cached_path <- latest_colonoscopy_cache_path(
    cache_dir,
    data_year
  )

  if (use_cache && !base::is.na(cached_path)) {
    base::message(
      "Reading cached CMS colonoscopy rows: ",
      cached_path
    )

    return(
      readr::read_csv(
        cached_path,
        show_col_types = FALSE
      )
    )
  }

  if (base::is.null(title_pattern)) {
    title_pattern <- base::paste(
      "Medicare Physician & Other Practitioners -",
      "by Provider and Service"
    )
  }

  base::message(
    "Fetching public CMS colonoscopy rows for ",
    data_year,
    "."
  )

  version_uuid <- cms_find_dataset_uuid(
    title_pattern = title_pattern,
    data_year = data_year
  )

  raw_tbl <- purrr::map_dfr(
    colonoscopy_setting_codes(),
    function(code) {
      cms_query_hcpcs(
        uuid = version_uuid,
        hcpcs_code = code
      )
    }
  )

  standardized_tbl <- standardize_physician_colonoscopy(
    raw_tbl,
    data_year = data_year
  )

  save_colonoscopy_cache(
    standardized_tbl,
    cache_dir = cache_dir,
    data_year = data_year
  )

  standardized_tbl
}

#' Fetch standardized CMS colonoscopy rows across multiple years
#'
#' @param years Integer vector of years to fetch; defaults to `2019:2024`.
#' @param cache_dir Directory to read/write per-year cache files in.
#' @param use_cache Logical; if `TRUE` (default), reuse cached per-year
#'   extracts when available instead of re-fetching from the CMS API.
#' @return Tibble of standardized colonoscopy provider-service rows for
#'   all requested years, row-bound together.
#' @export
fetch_colonoscopy_physician_years <- function(
    years = 2019:2024,
    cache_dir = "data-raw/cms_colonoscopy",
    use_cache = TRUE) {
  base::message(
    "Fetching CMS colonoscopy data for years: ",
    base::paste(years, collapse = ", ")
  )

  purrr::map_dfr(
    years,
    function(data_year) {
      fetch_colonoscopy_physician_year(
        data_year = data_year,
        cache_dir = cache_dir,
        use_cache = use_cache
      )
    }
  )
}
