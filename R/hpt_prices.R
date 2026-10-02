#' Normalize hospital price-transparency MRF column names
#'
#' Strips a leading UTF-8 BOM, trims whitespace, and collapses stray
#' whitespace around the `|` delimiter used in CMS HPT wide-format column
#' names (e.g. `"standard_charge | Payer | Plan | negotiated_dollar"`).
#'
#' @param column_names Character vector of raw column names.
#' @return Character vector of normalized column names, same length as input.
#' @export
normalize_hpt_names <- function(column_names) {
  column_names |>
    stringr::str_replace("^\\ufeff", "") |>
    stringr::str_trim() |>
    stringr::str_replace_all("\\s*\\|\\s*", "|")
}

#' Read the first lines of a hospital price-transparency MRF
#'
#' Reads local files directly; for remote URLs, streams just the
#' requested number of lines over HTTP rather than downloading the whole
#' file. Used to inspect the CMS metadata preamble that precedes the
#' actual charge table in an HPT machine-readable file.
#'
#' @param path_or_url Local file path or remote URL of the HPT MRF.
#' @param n_lines Number of lines to read from the start of the file.
#' @return Character vector of up to `n_lines` lines.
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

#' Locate the charge-table header row in an HPT MRF prefix
#'
#' CMS hospital price-transparency CSVs begin with a block of metadata
#' rows before the actual charge table header (which starts with a
#' `"description",` field). This finds that header row's position among
#' the supplied lines.
#'
#' @param lines Character vector of lines read from the start of an HPT MRF
#'   (e.g. via [read_hpt_prefix()]).
#' @return Integer position (1-based) of the charge-table header line.
#'   Errors if no such line is found.
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

#' Read a CMS hospital price-transparency MRF CSV, skipping its preamble
#'
#' Detects how many leading metadata rows precede the charge table (via
#' [read_hpt_prefix()] and [detect_hpt_charge_header()]), then reads the
#' charge table itself with DuckDB, treating every column as character and
#' tolerating malformed rows. Column names are normalized with
#' [normalize_hpt_names()] after reading.
#'
#' @param path_or_url Local file path or remote URL of the HPT MRF CSV.
#' @return A duckplyr/data frame of the charge table, with all columns
#'   character and normalized names.
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

#' Find an HPT column name matching one of several patterns
#'
#' @param column_names Character vector of column names to search.
#' @param patterns Character vector of regular expressions (matched
#'   case-insensitively) tried in order.
#' @param required If `TRUE`, errors when no column matches; if `FALSE`,
#'   returns `NA_character_` instead.
#' @return The first matching column name, or `NA_character_` if none match
#'   and `required` is `FALSE`.
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

#' Identify billing-code columns in an HPT MRF
#'
#' Matches normalized column names of the form `code` or `code|N` (CMS
#' HPT files may list multiple code columns, e.g. `code|1`, `code|2`, for
#' different code types).
#'
#' @param column_names Character vector of column names to search.
#' @return Character vector of the original (non-normalized) column names
#'   that are code columns.
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

#' Pick the matching target code for each HPT service row
#'
#' For each row, scans the given code columns and returns the first value
#' that is one of the target `codes`.
#'
#' @param service_tbl Tibble of HPT service rows.
#' @param code_columns Character vector of column names in `service_tbl`
#'   that hold billing codes (e.g. from [hpt_code_columns()]).
#' @param codes Character vector of target billing codes to match against.
#' @return Character vector, same length as `nrow(service_tbl)`, of the
#'   matched code for each row (`NA_character_` if none of its code columns
#'   match).
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

#' Filter an HPT MRF down to rows for the target billing codes
#'
#' Finds the code columns, filters (lazily, then collects) to rows where
#' any code column matches one of `codes`, and adds a `target_code` column
#' recording which code matched.
#'
#' @param hpt_tbl HPT MRF table (e.g. from [read_hpt_csv()]).
#' @param codes Character vector of target billing codes to keep.
#' @return A tibble of matching rows with a `target_code` column added;
#'   zero rows if no rows matched. Errors if `hpt_tbl` has no recognized
#'   code columns.
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

#' Safely pull a possibly-absent column from an HPT service table
#'
#' @param service_tbl Tibble to pull the column from.
#' @param column_name Name of the column to pull, or `NA` if the column
#'   does not exist in `service_tbl`.
#' @return Character vector, length `nrow(service_tbl)`: the column's
#'   values coerced to character, or all `NA_character_` if `column_name`
#'   is `NA`.
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

#' Extract commercial prices from a tall-format HPT MRF
#'
#' Tall-format CMS HPT files carry one `payer_name`/`plan_name` pair per
#' row, with separate columns for the negotiated dollar amount and the
#' allowed-amount percentiles.
#'
#' @param service_tbl Tibble of HPT service rows already filtered to
#'   target codes (must include a `target_code` column, e.g. from
#'   [materialize_hpt_target_rows()]).
#' @return Tibble with columns `code`, `payer_name`, `plan_name`,
#'   `negotiated_dollar`, `allowed_p10`, `allowed_median`, `allowed_p90`.
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

#' Parse a wide-format HPT MRF column name into payer/plan/metric metadata
#'
#' Wide-format CMS HPT files encode payer, plan, and price metric in the
#' column name itself, either as
#' `standard_charge|PAYER|PLAN|negotiated_dollar` or as
#' `{median_amount,10th_percentile,90th_percentile}|PAYER|PLAN`.
#'
#' @param column_name A single (normalized) column name to parse.
#' @return A one-row tibble with columns `column_name`, `payer_name`,
#'   `plan_name`, `metric` if `column_name` matches one of the recognized
#'   patterns; a zero-row tibble with those same columns otherwise.
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

#' Build payer/plan/metric metadata for all wide-format HPT columns
#'
#' Applies [parse_hpt_wide_column()] to every column name and collapses
#' the results to distinct payer/plan/metric combinations.
#'
#' @param column_names Character vector of column names from a wide-format
#'   HPT MRF.
#' @return Tibble with columns `column_name`, `payer_name`, `plan_name`,
#'   `metric`, one row per distinct combination found.
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

#' Look up the wide-format column for a given payer/plan/metric
#'
#' @param metadata_tbl Metadata tibble from [hpt_wide_metadata()].
#' @param payer_name Payer name to match.
#' @param plan_name Plan name to match.
#' @param metric Price metric to match (e.g. `"negotiated_dollar"`,
#'   `"allowed_median"`, `"allowed_p10"`, `"allowed_p90"`).
#' @return The matching column name, or `NA_character_` if no row matches.
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

#' Extract commercial prices from a wide-format HPT MRF
#'
#' Builds payer/plan/metric column metadata, then for each distinct
#' payer/plan combination pulls the negotiated-dollar and allowed-amount
#' percentile columns into a tidy row per combination.
#'
#' @param service_tbl Tibble of HPT service rows already filtered to
#'   target codes (must include a `target_code` column, e.g. from
#'   [materialize_hpt_target_rows()]).
#' @return Tibble with columns `code`, `payer_name`, `plan_name`,
#'   `negotiated_dollar`, `allowed_p10`, `allowed_median`, `allowed_p90`,
#'   excluding rows where all four price columns are `NA`. Errors if no
#'   payer price columns are found.
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

#' Extract commercial prices for sampled billing codes from an HPT MRF
#'
#' Top-level entry point for price extraction: normalizes column names,
#' filters the table to the target `codes`, detects whether the MRF is in
#' CMS's tall or wide CSV format, and dispatches to
#' [extract_hpt_tall_prices()] or [extract_hpt_wide_prices()] accordingly.
#'
#' @param hpt_tbl HPT MRF table (e.g. from [read_hpt_csv()]).
#' @param codes Character vector of target billing codes to extract prices
#'   for; defaults to the CPT/HCPCS sampling codes used in this project
#'   (`"58100"`, `"58120"`, `"58558"`, `"88305"`).
#' @return Tibble with columns `code`, `payer_name`, `plan_name`,
#'   `negotiated_dollar`, `allowed_p10`, `allowed_median`, `allowed_p90`;
#'   zero rows (with that schema) if no rows match the target codes.
#'   Errors if the MRF is neither recognizable tall nor wide format.
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

#' Summarize extracted commercial HPT prices by billing code
#'
#' @param price_tbl Tibble of extracted prices (e.g. from
#'   [extract_hpt_sampling_prices()]), with `code`, `negotiated_dollar`,
#'   and `allowed_median` columns.
#' @return Tibble with one row per `code` and columns `n_price_rows`,
#'   `mean_negotiated`, `sd_negotiated`, `median_negotiated`,
#'   `p25_negotiated`, `p75_negotiated`, `mean_allowed_median`,
#'   `sd_allowed_median`, `median_allowed_median`, `p25_allowed_median`,
#'   `p75_allowed_median`.
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

#' Read the manifest of hospitals to scrape HPT prices from
#'
#' @param path Path to the manifest CSV.
#' @return Tibble of the manifest, all columns read as character. Errors
#'   if any of the required columns (`hospital_name`, `hospital_state`,
#'   `mrf_url`) are missing.
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

#' Extract HPT prices for a single manifest hospital, without raising errors
#'
#' Reads and parses the hospital's MRF and extracts sampling prices for
#' `codes`. Any failure (download/parse error, or no rows for the target
#' codes) is captured as an audit record rather than raised, so a batch
#' run (see [extract_hpt_manifest_prices_safe()]) can continue past one
#' hospital's failure.
#'
#' @param manifest_row Single-row tibble with `hospital_name`,
#'   `hospital_state`, and `mrf_url` columns (one row of a manifest from
#'   [read_hpt_manifest()]).
#' @param codes Character vector of target billing codes.
#' @return A list with elements `prices` (tibble of extracted prices,
#'   tagged with hospital metadata, empty on failure) and `failure`
#'   (zero- or one-row tibble recording `hospital_name`, `hospital_state`,
#'   `mrf_url`, `failure_type`, and `error_message`; empty on success).
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

#' Extract HPT prices for every hospital in a manifest, auditing failures
#'
#' Runs [extract_one_hpt_manifest_row()] over every row of `manifest_tbl`
#' and combines the results, so that individual hospital failures are
#' recorded in an audit table rather than stopping the whole run.
#'
#' @param manifest_tbl Hospital manifest tibble (e.g. from
#'   [read_hpt_manifest()]).
#' @param codes Character vector of target billing codes; defaults to the
#'   CPT/HCPCS sampling codes used in this project (`"58100"`, `"58120"`,
#'   `"58558"`, `"88305"`).
#' @return A list with elements `prices` (combined tibble of extracted
#'   prices across all hospitals) and `failures` (combined audit tibble of
#'   per-hospital failures).
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

#' Extract HPT prices for a manifest, erroring on any extraction failure
#'
#' Wraps [extract_hpt_manifest_prices_safe()] but, unlike that function,
#' raises an error if any hospital in the manifest produced an audit
#' failure rather than returning the audit table for inspection.
#'
#' @param manifest_tbl Hospital manifest tibble (e.g. from
#'   [read_hpt_manifest()]).
#' @param codes Character vector of target billing codes; defaults to the
#'   CPT/HCPCS sampling codes used in this project (`"58100"`, `"58120"`,
#'   `"58558"`, `"88305"`).
#' @return Tibble of extracted prices across all hospitals. Errors if any
#'   hospital produced an audit failure.
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
