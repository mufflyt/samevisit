#' @export
normalize_hpt_names <- function(column_names) {
  column_names |>
    stringr::str_replace("^\\ufeff", "") |>
    stringr::str_trim() |>
    stringr::str_replace_all("\\s*\\|\\s*", "|")
}

#' @export
read_hpt_prefix <- function(path_or_url,
                            n_lines = 12L) {
  base::message("Inspecting HPT MRF header: ", path_or_url)

  if (base::file.exists(path_or_url)) {
    connection <- base::file(
      path_or_url,
      open = "rt",
      encoding = "UTF-8"
    )
    base::on.exit(base::close(connection), add = TRUE)

    return(
      base::readLines(
        connection,
        n = n_lines,
        warn = FALSE
      )
    )
  }

  request_obj <- httr2::request(path_or_url) |>
    httr2::req_user_agent(
      "emb-colonoscopy-research/0.1"
    ) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_timeout(seconds = 60)

  response <- httr2::req_perform_connection(request_obj)
  base::on.exit(base::close(response), add = TRUE)

  httr2::resp_stream_lines(
    response,
    lines = n_lines,
    max_size = 1024 * 1024
  )
}

#' @export
detect_hpt_charge_header <- function(lines) {
  cleaned <- lines |>
    stringr::str_replace("^\\ufeff", "") |>
    stringr::str_trim()

  matches <- base::which(
    stringr::str_detect(
      cleaned,
      stringr::regex(
        "^\\\"?description\\\"?\\s*,",
        ignore_case = TRUE
      )
    )
  )

  if (base::length(matches) == 0L) {
    base::stop(
      "Could not find the CMS HPT charge-table header."
    )
  }

  matches[[1]]
}

#' @export
read_hpt_csv <- function(path_or_url) {
  prefix_lines <- read_hpt_prefix(path_or_url)
  header_line <- detect_hpt_charge_header(prefix_lines)
  skip_rows <- header_line - 1L

  base::message(
    "Reading HPT charge table after ",
    scales::comma(skip_rows),
    " metadata rows."
  )

  hpt_tbl <- duckplyr::read_csv_duckdb(
    path = path_or_url,
    prudence = "stingy",
    options = base::list(
      header = TRUE,
      skip = skip_rows,
      all_varchar = TRUE,
      ignore_errors = TRUE
    )
  )

  base::names(hpt_tbl) <- normalize_hpt_names(
    base::names(hpt_tbl)
  )

  hpt_tbl
}

#' @export
find_hpt_column <- function(column_names,
                            patterns,
                            required = TRUE) {
  matches <- purrr::map(
    patterns,
    function(pattern) {
      column_names[
        stringr::str_detect(
          column_names,
          stringr::regex(
            pattern,
            ignore_case = TRUE
          )
        )
      ]
    }
  ) |>
    base::unlist() |>
    base::unique()

  if (base::length(matches) == 0L && required) {
    base::stop(
      "Could not locate HPT column: ",
      base::paste(patterns, collapse = " | ")
    )
  }

  if (base::length(matches) == 0L) {
    return(NA_character_)
  }

  matches[[1]]
}

#' @export
hpt_code_columns <- function(column_names) {
  normalized <- normalize_hpt_names(column_names)

  column_names[
    stringr::str_detect(
      normalized,
      stringr::regex(
        "^code(\\|[0-9]+)?$",
        ignore_case = TRUE
      )
    )
  ]
}

#' @export
hpt_target_code <- function(service_tbl,
                            code_columns,
                            codes) {
  purrr::pmap_chr(
    service_tbl[code_columns],
    function(...) {
      values <- base::as.character(base::c(...))
      matches <- values[values %in% codes]

      if (base::length(matches) == 0L) {
        return(NA_character_)
      }

      matches[[1]]
    }
  )
}

#' @export
materialize_hpt_target_rows <- function(hpt_tbl,
                                        codes) {
  column_names <- base::names(hpt_tbl)
  code_columns <- hpt_code_columns(column_names)

  if (base::length(code_columns) == 0L) {
    base::stop("HPT MRF contains no recognized code columns.")
  }

  base::message(
    "Filtering HPT MRF across ",
    scales::comma(base::length(code_columns)),
    " code columns."
  )

  service_tbl <- hpt_tbl |>
    dplyr::filter(
      dplyr::if_any(
        dplyr::all_of(code_columns),
        ~ .x %in% codes
      )
    ) |>
    dplyr::collect() |>
    tibble::as_tibble()

  if (base::nrow(service_tbl) == 0L) {
    return(service_tbl)
  }

  service_tbl$target_code <- hpt_target_code(
    service_tbl,
    code_columns,
    codes
  )

  base::message(
    "Materialized ",
    scales::comma(base::nrow(service_tbl)),
    " target HPT service rows."
  )

  service_tbl
}

#' @export
hpt_get_column <- function(service_tbl,
                           column_name) {
  if (base::is.na(column_name)) {
    return(
      base::rep(
        NA_character_,
        base::nrow(service_tbl)
      )
    )
  }

  base::as.character(service_tbl[[column_name]])
}

#' @export
extract_hpt_tall_prices <- function(service_tbl) {
  column_names <- base::names(service_tbl)

  payer_col <- find_hpt_column(
    column_names,
    "^payer_name$"
  )
  plan_col <- find_hpt_column(
    column_names,
    "^plan_name$"
  )
  negotiated_col <- find_hpt_column(
    column_names,
    "^standard_charge\\|negotiated_dollar$",
    required = FALSE
  )
  median_col <- find_hpt_column(
    column_names,
    "^median_amount$",
    required = FALSE
  )
  p10_col <- find_hpt_column(
    column_names,
    "^10th_percentile$",
    required = FALSE
  )
  p90_col <- find_hpt_column(
    column_names,
    "^90th_percentile$",
    required = FALSE
  )

  service_tbl |>
    dplyr::transmute(
      code = .data$target_code,
      payer_name = hpt_get_column(
        service_tbl,
        payer_col
      ),
      plan_name = hpt_get_column(
        service_tbl,
        plan_col
      ),
      negotiated_dollar = base::suppressWarnings(
        base::as.numeric(
          hpt_get_column(service_tbl, negotiated_col)
        )
      ),
      allowed_p10 = base::suppressWarnings(
        base::as.numeric(
          hpt_get_column(service_tbl, p10_col)
        )
      ),
      allowed_median = base::suppressWarnings(
        base::as.numeric(
          hpt_get_column(service_tbl, median_col)
        )
      ),
      allowed_p90 = base::suppressWarnings(
        base::as.numeric(
          hpt_get_column(service_tbl, p90_col)
        )
      )
    )
}

#' @export
parse_hpt_wide_column <- function(column_name) {
  normalized <- normalize_hpt_names(column_name)
  parts <- base::strsplit(normalized, "\\|", fixed = FALSE)[[1]]

  if (base::length(parts) == 4L &&
      parts[[1]] == "standard_charge" &&
      parts[[4]] == "negotiated_dollar") {
    return(
      tibble::tibble(
        column_name = column_name,
        payer_name = parts[[2]],
        plan_name = parts[[3]],
        metric = "negotiated_dollar"
      )
    )
  }

  if (base::length(parts) == 3L &&
      parts[[1]] %in% base::c(
        "median_amount",
        "10th_percentile",
        "90th_percentile"
      )) {
    metric <- dplyr::case_when(
      parts[[1]] == "median_amount" ~ "allowed_median",
      parts[[1]] == "10th_percentile" ~ "allowed_p10",
      TRUE ~ "allowed_p90"
    )

    return(
      tibble::tibble(
        column_name = column_name,
        payer_name = parts[[2]],
        plan_name = parts[[3]],
        metric = metric
      )
    )
  }

  tibble::tibble(
    column_name = base::character(),
    payer_name = base::character(),
    plan_name = base::character(),
    metric = base::character()
  )
}

#' @export
hpt_wide_metadata <- function(column_names) {
  purrr::map_dfr(
    column_names,
    parse_hpt_wide_column
  ) |>
    dplyr::distinct(
      .data$column_name,
      .data$payer_name,
      .data$plan_name,
      .data$metric
    )
}

#' @export
hpt_wide_metric_column <- function(metadata_tbl,
                                   payer_name,
                                   plan_name,
                                   metric) {
  matched <- metadata_tbl |>
    dplyr::filter(
      .data$payer_name == .env$payer_name,
      .data$plan_name == .env$plan_name,
      .data$metric == .env$metric
    ) |>
    dplyr::pull(.data$column_name)

  if (base::length(matched) == 0L) {
    return(NA_character_)
  }

  matched[[1]]
}

#' @export
extract_hpt_wide_prices <- function(service_tbl) {
  metadata_tbl <- hpt_wide_metadata(
    base::names(service_tbl)
  )

  if (base::nrow(metadata_tbl) == 0L) {
    base::stop(
      "HPT wide-format MRF has no payer price columns."
    )
  }

  payer_plan_tbl <- metadata_tbl |>
    dplyr::distinct(
      .data$payer_name,
      .data$plan_name
    )

  price_tbl <- purrr::pmap_dfr(
    payer_plan_tbl,
    function(payer_name,
             plan_name) {
      negotiated_col <- hpt_wide_metric_column(
        metadata_tbl,
        payer_name,
        plan_name,
        "negotiated_dollar"
      )
      median_col <- hpt_wide_metric_column(
        metadata_tbl,
        payer_name,
        plan_name,
        "allowed_median"
      )
      p10_col <- hpt_wide_metric_column(
        metadata_tbl,
        payer_name,
        plan_name,
        "allowed_p10"
      )
      p90_col <- hpt_wide_metric_column(
        metadata_tbl,
        payer_name,
        plan_name,
        "allowed_p90"
      )

      tibble::tibble(
        code = service_tbl$target_code,
        payer_name = payer_name,
        plan_name = plan_name,
        negotiated_dollar = base::suppressWarnings(
          base::as.numeric(
            hpt_get_column(service_tbl, negotiated_col)
          )
        ),
        allowed_p10 = base::suppressWarnings(
          base::as.numeric(
            hpt_get_column(service_tbl, p10_col)
          )
        ),
        allowed_median = base::suppressWarnings(
          base::as.numeric(
            hpt_get_column(service_tbl, median_col)
          )
        ),
        allowed_p90 = base::suppressWarnings(
          base::as.numeric(
            hpt_get_column(service_tbl, p90_col)
          )
        )
      )
    }
  )

  price_tbl |>
    dplyr::filter(
      !(
        base::is.na(.data$negotiated_dollar) &
          base::is.na(.data$allowed_p10) &
          base::is.na(.data$allowed_median) &
          base::is.na(.data$allowed_p90)
      )
    )
}

#' @export
extract_hpt_sampling_prices <- function(
    hpt_tbl,
    codes = base::c("58100", "58120", "58558", "88305")) {
  base::message("Extracting sampling prices from HPT MRF.")

  base::names(hpt_tbl) <- normalize_hpt_names(
    base::names(hpt_tbl)
  )

  service_tbl <- materialize_hpt_target_rows(
    hpt_tbl,
    codes
  )

  if (base::nrow(service_tbl) == 0L) {
    return(
      tibble::tibble(
        code = base::character(),
        payer_name = base::character(),
        plan_name = base::character(),
        negotiated_dollar = base::double(),
        allowed_p10 = base::double(),
        allowed_median = base::double(),
        allowed_p90 = base::double()
      )
    )
  }

  column_names <- base::names(service_tbl)
  is_tall <- base::all(
    base::c("payer_name", "plan_name") %in% column_names
  )
  is_wide <- base::any(
    stringr::str_detect(
      column_names,
      stringr::regex(
        "^standard_charge\\|.+\\|.+\\|negotiated_dollar$",
        ignore_case = TRUE
      )
    )
  )

  if (is_tall) {
    base::message("Detected CMS HPT tall CSV format.")
    return(extract_hpt_tall_prices(service_tbl))
  }

  if (is_wide) {
    base::message("Detected CMS HPT wide CSV format.")
    return(extract_hpt_wide_prices(service_tbl))
  }

  base::stop("Could not identify CMS HPT tall or wide format.")
}

#' @export
summarize_hpt_prices <- function(price_tbl) {
  base::message("Summarizing commercial HPT prices.")

  price_tbl |>
    dplyr::group_by(.data$code) |>
    dplyr::summarise(
      n_price_rows = dplyr::n(),
      mean_negotiated = base::mean(
        .data$negotiated_dollar,
        na.rm = TRUE
      ),
      sd_negotiated = stats::sd(
        .data$negotiated_dollar,
        na.rm = TRUE
      ),
      median_negotiated = stats::median(
        .data$negotiated_dollar,
        na.rm = TRUE
      ),
      p25_negotiated = stats::quantile(
        .data$negotiated_dollar,
        0.25,
        na.rm = TRUE
      ),
      p75_negotiated = stats::quantile(
        .data$negotiated_dollar,
        0.75,
        na.rm = TRUE
      ),
      mean_allowed_median = base::mean(
        .data$allowed_median,
        na.rm = TRUE
      ),
      sd_allowed_median = stats::sd(
        .data$allowed_median,
        na.rm = TRUE
      ),
      median_allowed_median = stats::median(
        .data$allowed_median,
        na.rm = TRUE
      ),
      p25_allowed_median = stats::quantile(
        .data$allowed_median,
        0.25,
        na.rm = TRUE
      ),
      p75_allowed_median = stats::quantile(
        .data$allowed_median,
        0.75,
        na.rm = TRUE
      ),
      .groups = "drop"
    )
}

#' @export
read_hpt_manifest <- function(path) {
  base::message("Reading HPT manifest: ", path)

  manifest_tbl <- readr::read_csv(
    file = path,
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character())
  )

  required_cols <- base::c(
    "hospital_name",
    "hospital_state",
    "mrf_url"
  )

  missing_cols <- base::setdiff(
    required_cols,
    base::names(manifest_tbl)
  )

  if (base::length(missing_cols) > 0L) {
    base::stop(
      "HPT manifest missing: ",
      base::paste(missing_cols, collapse = ", ")
    )
  }

  manifest_tbl
}

#' @export
extract_one_hpt_manifest_row <- function(manifest_row,
                                         codes) {
  hospital_name <- manifest_row$hospital_name[[1]]
  hospital_state <- manifest_row$hospital_state[[1]]
  mrf_url <- manifest_row$mrf_url[[1]]

  base::message("Processing HPT hospital: ", hospital_name)

  base::tryCatch(
    {
      hpt_tbl <- read_hpt_csv(mrf_url)
      price_tbl <- extract_hpt_sampling_prices(
        hpt_tbl,
        codes = codes
      )

      if (base::nrow(price_tbl) == 0L) {
        return(
          base::list(
            prices = price_tbl,
            failure = tibble::tibble(
              hospital_name = hospital_name,
              hospital_state = hospital_state,
              mrf_url = mrf_url,
              failure_type = "no_target_codes",
              error_message = NA_character_
            )
          )
        )
      }

      price_tbl <- price_tbl |>
        dplyr::mutate(
          hospital_name = hospital_name,
          hospital_state = hospital_state,
          mrf_url = mrf_url
        )

      base::list(
        prices = price_tbl,
        failure = tibble::tibble()
      )
    },
    error = function(error_condition) {
      base::list(
        prices = tibble::tibble(),
        failure = tibble::tibble(
          hospital_name = hospital_name,
          hospital_state = hospital_state,
          mrf_url = mrf_url,
          failure_type = "parser_or_access_failure",
          error_message = base::conditionMessage(
            error_condition
          )
        )
      )
    }
  )
}

#' @export
extract_hpt_manifest_prices_safe <- function(
    manifest_tbl,
    codes = base::c("58100", "58120", "58558", "88305")) {
  base::message(
    "Processing ",
    scales::comma(base::nrow(manifest_tbl)),
    " HPT manifest hospitals."
  )

  extraction_list <- purrr::map(
    base::seq_len(base::nrow(manifest_tbl)),
    function(row_id) {
      extract_one_hpt_manifest_row(
        manifest_tbl[row_id, , drop = FALSE],
        codes
      )
    }
  )

  price_tbl <- purrr::map_dfr(
    extraction_list,
    "prices"
  )
  failure_tbl <- purrr::map_dfr(
    extraction_list,
    "failure"
  )

  base::message(
    "Extracted ",
    scales::comma(base::nrow(price_tbl)),
    " commercial-price rows."
  )
  base::message(
    "HPT extraction audit rows: ",
    scales::comma(base::nrow(failure_tbl)),
    "."
  )

  base::list(
    prices = price_tbl,
    failures = failure_tbl
  )
}

#' @export
extract_hpt_manifest_prices <- function(
    manifest_tbl,
    codes = base::c("58100", "58120", "58558", "88305")) {
  extraction <- extract_hpt_manifest_prices_safe(
    manifest_tbl,
    codes
  )

  if (base::nrow(extraction$failures) > 0L) {
    base::stop(
      "HPT extraction produced ",
      base::nrow(extraction$failures),
      " audit failures."
    )
  }

  extraction$prices
}
