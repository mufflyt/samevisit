#' @export
colonoscopy_base_codes <- function() {
  base::c(
    "45378",
    "G0105",
    "G0121"
  )
}

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

#' @export
pull_cms_column <- function(cms_tbl,
                            column_name,
                            default = NA_character_) {
  if (base::is.na(column_name)) {
    return(base::rep(default, base::nrow(cms_tbl)))
  }

  cms_tbl[[column_name]]
}

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
