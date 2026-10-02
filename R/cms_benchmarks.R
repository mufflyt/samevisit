#' @export
cms_or_else <- function(value,
                        fallback) {
  if (base::is.null(value) || base::length(value) == 0L) {
    return(fallback)
  }

  value
}

#' @export
cms_distribution_uuid <- function(download_url) {
  matched <- stringr::str_match(
    download_url,
    "/dataset/([^/]+)/data"
  )

  uuid <- matched[, 2]

  if (base::is.na(uuid)) {
    base::stop(
      "Could not extract CMS version UUID from: ",
      download_url
    )
  }

  uuid
}

#' @export
cms_resolve_version_uuid <- function(catalog_payload,
                                     title_pattern,
                                     data_year = 2024L) {
  dataset_list <- catalog_payload$dataset

  matched_datasets <- purrr::keep(
    dataset_list,
    function(dataset_item) {
      dataset_title <- cms_or_else(
        dataset_item$title,
        ""
      )

      stringr::str_detect(
        dataset_title,
        stringr::regex(
          title_pattern,
          ignore_case = TRUE
        )
      )
    }
  )

  if (base::length(matched_datasets) != 1L) {
    base::stop(
      "CMS title matched ",
      base::length(matched_datasets),
      " datasets: ",
      title_pattern
    )
  }

  distribution_list <- matched_datasets[[1]]$distribution
  year_pattern <- base::paste0(
    "(^|[^0-9])",
    data_year,
    "([^0-9]|$)"
  )

  year_candidates <- purrr::keep(
    distribution_list,
    function(distribution_item) {
      format_value <- cms_or_else(
        distribution_item$format,
        ""
      )
      distribution_title <- cms_or_else(
        distribution_item$title,
        ""
      )
      temporal_value <- cms_or_else(
        distribution_item$temporal,
        ""
      )
      access_url <- cms_or_else(
        distribution_item$accessURL,
        ""
      )

      base::identical(format_value, "API") &&
        (
          stringr::str_detect(
            distribution_title,
            year_pattern
          ) ||
            stringr::str_detect(
              temporal_value,
              year_pattern
            )
        ) &&
        stringr::str_detect(
          access_url,
          "/data-api/v1/dataset/"
        )
    }
  )

  latest_candidates <- purrr::keep(
    year_candidates,
    function(distribution_item) {
      description <- cms_or_else(
        distribution_item$description,
        ""
      )

      base::identical(
        base::tolower(description),
        "latest"
      )
    }
  )

  selected_candidates <- if (
    base::length(latest_candidates) == 1L
  ) {
    latest_candidates
  } else {
    year_candidates
  }

  if (base::length(selected_candidates) != 1L) {
    base::stop(
      "Expected one CMS API distribution for ",
      data_year,
      "; found ",
      base::length(selected_candidates),
      "."
    )
  }

  access_url <- selected_candidates[[1]]$accessURL
  uuid <- cms_distribution_uuid(access_url)

  base::message(
    "Resolved CMS ",
    data_year,
    " version UUID: ",
    uuid
  )

  uuid
}

#' @export
cms_find_dataset_uuid <- function(title_pattern,
                                  data_year = 2024L) {
  catalog_url <- "https://data.cms.gov/data.json"

  base::message("Reading CMS Open Data catalog: ", catalog_url)

  response <- httr2::request(catalog_url) |>
    httr2::req_user_agent(
      "emb-colonoscopy-research/0.1"
    ) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_timeout(seconds = 60) |>
    httr2::req_perform()

  catalog_payload <- httr2::resp_body_json(
    response,
    simplifyVector = FALSE
  )

  cms_resolve_version_uuid(
    catalog_payload,
    title_pattern = title_pattern,
    data_year = data_year
  )
}

#' @export
cms_extract_rows <- function(payload) {
  if (base::is.data.frame(payload)) {
    return(tibble::as_tibble(payload))
  }

  if (!base::is.null(payload$data) &&
      base::is.data.frame(payload$data)) {
    return(tibble::as_tibble(payload$data))
  }

  if (!base::is.null(payload$data) &&
      base::is.list(payload$data)) {
    return(dplyr::bind_rows(payload$data))
  }

  if (base::is.list(payload) &&
      base::length(payload) > 0L &&
      base::all(
        purrr::map_lgl(
          payload,
          base::is.list
        )
      )) {
    return(dplyr::bind_rows(payload))
  }

  base::stop("Unrecognized CMS API response.")
}

#' Guard against the CMS API's silent-unfiltered-response failure mode
#'
#' The CMS data-api silently ignores a filter condition on a field that
#' does not exist in the target dataset, returning the ENTIRE unfiltered
#' table rather than erroring (confirmed empirically: an OPPS "by
#' Provider and Service" query filtered on "HCPCS_Cd" -- a column that
#' dataset does not have, since it is organized by APC code instead --
#' silently returned 116,182 unfiltered rows; see docs/evidence_layers.md
#' and docs/appendix.md). Pulled out as a pure function so this guard can
#' be unit-tested without a live network call.
#'
#' @param page_tbl Tibble of one page of CMS API results.
#' @param hcpcs_field Character scalar naming the HCPCS/CPT code column to
#'   check for in `page_tbl`.
#' @return Invisibly, `TRUE` if the field is present; errors otherwise.
#' @export
validate_cms_filter_field <- function(page_tbl, hcpcs_field) {
  if (base::nrow(page_tbl) == 0L) {
    return(base::invisible(TRUE))
  }

  if (!hcpcs_field %in% base::names(page_tbl)) {
    base::stop(
      "CMS dataset has no '", hcpcs_field, "' field to filter on -- got ",
      "columns: ", base::paste(base::names(page_tbl), collapse = ", "),
      ". This dataset may be organized by a different code (e.g. APC ",
      "rather than HCPCS); do not treat this result as filtered."
    )
  }

  base::invisible(TRUE)
}

#' @export
cms_query_hcpcs <- function(uuid,
                            hcpcs_code,
                            hcpcs_field = "HCPCS_Cd",
                            page_size = 5000L) {
  base_url <- base::paste0(
    "https://data.cms.gov/data-api/v1/dataset/",
    uuid,
    "/data"
  )

  base::message(
    "Querying CMS HCPCS ",
    hcpcs_code,
    " from version ",
    uuid,
    "."
  )

  offset <- 0L
  page_list <- base::list()

  repeat {
    request_obj <- httr2::request(base_url)

    query_args <- base::list(
      size = page_size,
      offset = offset,
      `filter[filter-1][condition][path]` =
        hcpcs_field,
      `filter[filter-1][condition][operator]` =
        "=",
      `filter[filter-1][condition][value]` =
        hcpcs_code
    )

    request_obj <- rlang::exec(
      httr2::req_url_query,
      request_obj,
      !!!query_args
    )

    response <- request_obj |>
      httr2::req_user_agent(
        "emb-colonoscopy-research/0.1"
      ) |>
      httr2::req_retry(max_tries = 3) |>
      httr2::req_timeout(seconds = 60) |>
      httr2::req_perform()

    payload <- httr2::resp_body_json(
      response,
      simplifyVector = TRUE
    )

    page_tbl <- cms_extract_rows(payload)

    if (offset == 0L) {
      validate_cms_filter_field(page_tbl, hcpcs_field)
    }

    if (base::nrow(page_tbl) == 0L) {
      break
    }

    page_list[[base::length(page_list) + 1L]] <-
      page_tbl

    if (base::nrow(page_tbl) < page_size) {
      break
    }

    offset <- offset + page_size
  }

  combined_tbl <- dplyr::bind_rows(page_list)

  base::message(
    "CMS HCPCS ",
    hcpcs_code,
    " rows: ",
    scales::comma(base::nrow(combined_tbl))
  )

  combined_tbl
}

#' @export
cms_sampling_benchmarks <- function(
    physician_uuid,
    hcpcs_codes = sampling_codebook()$code) {
  base::message("Building CMS professional benchmarks.")

  cms_tbl <- purrr::map_dfr(
    hcpcs_codes,
    function(code) {
      cms_query_hcpcs(
        uuid = physician_uuid,
        hcpcs_code = code
      )
    }
  )

  numeric_candidates <- base::c(
    "Avg_Sbmtd_Chrg",
    "Avg_Mdcr_Alowd_Amt",
    "Avg_Mdcr_Pymt_Amt",
    "Tot_Srvcs"
  )

  keep_numeric <- base::intersect(
    numeric_candidates,
    base::names(cms_tbl)
  )

  cms_tbl |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(keep_numeric),
        base::as.numeric
      )
    )
}

#' @export
summarize_cms_benchmarks <- function(cms_tbl) {
  base::message("Summarizing CMS benchmark costs.")

  required_cols <- base::c(
    "HCPCS_Cd",
    "Avg_Mdcr_Alowd_Amt",
    "Tot_Srvcs"
  )

  missing_cols <- base::setdiff(
    required_cols,
    base::names(cms_tbl)
  )

  if (base::length(missing_cols) > 0L) {
    base::stop(
      "CMS table missing: ",
      base::paste(missing_cols, collapse = ", ")
    )
  }

  cms_tbl |>
    dplyr::group_by(.data$HCPCS_Cd) |>
    dplyr::summarise(
      n_provider_service_rows = dplyr::n(),
      total_services = base::sum(
        .data$Tot_Srvcs,
        na.rm = TRUE
      ),
      service_weighted_mean_allowed = stats::weighted.mean(
        .data$Avg_Mdcr_Alowd_Amt,
        .data$Tot_Srvcs,
        na.rm = TRUE
      ),
      mean_allowed = base::mean(
        .data$Avg_Mdcr_Alowd_Amt,
        na.rm = TRUE
      ),
      sd_allowed = stats::sd(
        .data$Avg_Mdcr_Alowd_Amt,
        na.rm = TRUE
      ),
      median_allowed = stats::median(
        .data$Avg_Mdcr_Alowd_Amt,
        na.rm = TRUE
      ),
      p25_allowed = stats::quantile(
        .data$Avg_Mdcr_Alowd_Amt,
        0.25,
        na.rm = TRUE
      ),
      p75_allowed = stats::quantile(
        .data$Avg_Mdcr_Alowd_Amt,
        0.75,
        na.rm = TRUE
      ),
      .groups = "drop"
    )
}

#' @export
summarize_cms_facility_benchmarks <- function(
    cms_facility_tbl,
    code_field = "HCPCS_Cd",
    payment_field = "Avg_Tot_Pymt_Amt") {
  base::message("Summarizing CMS facility benchmarks.")

  required_cols <- base::c(
    code_field,
    payment_field
  )

  missing_cols <- base::setdiff(
    required_cols,
    base::names(cms_facility_tbl)
  )

  if (base::length(missing_cols) > 0L) {
    base::stop(
      "CMS facility table missing: ",
      base::paste(missing_cols, collapse = ", ")
    )
  }

  cms_facility_tbl |>
    dplyr::group_by(
      code = .data[[code_field]]
    ) |>
    dplyr::summarise(
      n_rows = dplyr::n(),
      mean_payment = base::mean(
        base::as.numeric(.data[[payment_field]]),
        na.rm = TRUE
      ),
      sd_payment = stats::sd(
        base::as.numeric(.data[[payment_field]]),
        na.rm = TRUE
      ),
      median_payment = stats::median(
        base::as.numeric(.data[[payment_field]]),
        na.rm = TRUE
      ),
      p25_payment = stats::quantile(
        base::as.numeric(.data[[payment_field]]),
        0.25,
        na.rm = TRUE
      ),
      p75_payment = stats::quantile(
        base::as.numeric(.data[[payment_field]]),
        0.75,
        na.rm = TRUE
      ),
      .groups = "drop"
    )
}
