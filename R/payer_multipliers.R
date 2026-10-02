# Refresh the empirical payer multipliers from hpt_prices
#
# The Medicaid and commercial scenarios (R/scenarios.R) scale each
# reimbursement input by a `payer_multiplier_<payer>_<parameter>` row of
# config/model_parameters.csv. Each row is the median, across hospitals, of
# the within-hospital ratio of that payer's professional-fee rate to the same
# hospital's traditional Medicare rate for one CPT code, computed by the
# hpt_prices pipeline (github.com/mufflyt/hpt_prices,
# analysis/14_emb_payer_ratios.R, which writes payer_to_medicare_ratios.csv).
#
# [refresh_payer_multipliers()] copies those ratios into the eight rows:
# `base_value` (median ratio), `low_value`/`high_value` (interhospital 25th
# and 75th percentiles), the hpt_prices commit cited in `source`, and the
# hospital count in `notes`. Only a row whose values change is rewritten,
# field by field with minimal CSV quoting, so every other byte of the file
# (line endings, quoting, other rows) is preserved. Run it through
# analysis/17_refresh_payer_multipliers.R, then rerun the scenario analysis
# and manuscript outputs (analysis/05, 07, and 11).

#' CPT code whose ratio each reimbursement input takes
#' @export
payer_multiplier_codes <- function() {
  codes <- base::c(
    emb_office_professional_cost = "58100",
    emb_pathology_cost = "88305",
    dc_professional_cost = "58120",
    office_visit_em_cost = "99213"
  )
  # the four inputs the payer scenarios scale, and no others
  stopifnot(base::setequal(base::names(codes), REIMBURSEMENT_PARAMETER_NAMES))
  codes
}

#' Payer types each reimbursement input's multiplier is computed for
#'
#' Parallel to [payer_multiplier_codes()]: the payer types scanned when
#' refreshing or building `payer_multiplier_<payer>_<parameter>` rows.
#'
#' @return Character vector: `"medicaid"`, `"commercial"`.
#' @export
payer_multiplier_payers <- function() {
  base::c("medicaid", "commercial")
}

#' Read hpt_prices' payer_to_medicare_ratios.csv
#'
#' @param path Path to the file.
#' @return Tibble with code, fee_type, payer_type, n_hospitals, median_ratio,
#'   p25_ratio, p75_ratio (ratios numeric).
#' @export
read_payer_ratios <- function(path) {
  if (!base::file.exists(path)) {
    base::stop("Payer ratio file not found: ", path,
               "\nSet HPT_PAYER_RATIOS to hpt_prices' output/payer_to_medicare_ratios.csv.", call. = FALSE)
  }
  ratios <- readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE)
  required <- base::c("code", "fee_type", "payer_type", "n_hospitals", "median_ratio", "p25_ratio", "p75_ratio")
  missing <- base::setdiff(required, base::names(ratios))
  if (base::length(missing) > 0L) {
    base::stop("Payer ratio file is missing column(s): ", base::paste(missing, collapse = ", "), call. = FALSE)
  }
  ratios |>
    dplyr::mutate(dplyr::across(base::c("median_ratio", "p25_ratio", "p75_ratio"), base::as.numeric),
                  n_hospitals = base::as.integer(.data$n_hospitals))
}

#' Split one CSV line into its fields, verbatim (no trimming, "NA" kept as text)
#'
#' @param line Character scalar, one raw CSV line.
#' @param n_fields Optional integer; if supplied, errors unless the parsed
#'   line has exactly this many fields.
#' @return Character vector of the line's fields, in order.
#' @export
csv_line_fields <- function(line, n_fields = NULL) {
  fields <- readr::read_csv(
    I(line), col_names = FALSE, col_types = readr::cols(.default = readr::col_character()),
    na = base::character(), trim_ws = FALSE, show_col_types = FALSE, progress = FALSE
  )
  fields <- base::unlist(fields[1, ], use.names = FALSE)
  if (!base::is.null(n_fields) && base::length(fields) != n_fields) {
    base::stop("Could not parse a model_parameters.csv row into ", n_fields, " fields: ",
               base::substr(line, 1L, 80L), call. = FALSE)
  }
  fields
}

#' Join fields into one CSV line with minimal quoting (the file's own style)
#'
#' @param fields Character vector of fields to join.
#' @return Character scalar, one CSV line.
#' @export
csv_line_join <- function(fields) {
  needs_quotes <- stringr::str_detect(fields, "[\",\r\n]")
  quoted <- base::ifelse(needs_quotes, base::paste0("\"", stringr::str_replace_all(fields, "\"", "\"\""), "\""), fields)
  base::paste(quoted, collapse = ",")
}

#' Copy hpt_prices payer ratios into config/model_parameters.csv
#'
#' @param params_path config/model_parameters.csv.
#' @param ratios_path hpt_prices' payer_to_medicare_ratios.csv.
#' @param hpt_commit Short hpt_prices commit the ratios come from (cited in
#'   `source`).
#' @param dry_run When TRUE, report the changes and write nothing.
#' @return Tibble, one row per multiplier: parameter, n_hospitals, and the old
#'   and new base/low/high values, plus `changed`. The file is rewritten only
#'   when `dry_run` is FALSE and at least one row changed.
#' @export
refresh_payer_multipliers <- function(params_path, ratios_path, hpt_commit, dry_run = FALSE) {
  if (base::missing(hpt_commit) || !base::is.character(hpt_commit) || !stringr::str_detect(hpt_commit, "^[0-9a-f]{7,40}$")) {
    base::stop("hpt_commit must be the hpt_prices commit the ratios came from (7-40 hex characters).", call. = FALSE)
  }
  ratios <- read_payer_ratios(ratios_path)

  raw <- base::readChar(params_path, base::file.info(params_path)$size, useBytes = TRUE)
  eol <- if (stringr::str_detect(raw, "\r\n")) "\r\n" else "\n"
  ends_with_eol <- base::endsWith(raw, eol)
  lines <- base::strsplit(raw, eol, fixed = TRUE)[[1]]
  header <- csv_line_fields(lines[[1]])
  col <- stats::setNames(base::seq_along(header), header)
  for (needed in base::c("parameter", "base_value", "low_value", "high_value", "source", "notes")) {
    if (!needed %in% header) base::stop("config/model_parameters.csv has no '", needed, "' column.", call. = FALSE)
  }

  codes <- payer_multiplier_codes()
  report <- base::list()
  for (payer in payer_multiplier_payers()) {
    for (parameter_name in base::names(codes)) {
      parameter <- payer_multiplier_parameter(payer, parameter_name)
      line_index <- base::which(base::startsWith(lines, base::paste0(parameter, ",")))
      if (base::length(line_index) != 1L) {
        base::stop("Expected one '", parameter, "' row in ", params_path, ", found ", base::length(line_index), ".", call. = FALSE)
      }
      ratio <- ratios |>
        dplyr::filter(.data$code == codes[[parameter_name]], .data$fee_type == "professional", .data$payer_type == payer)
      if (base::nrow(ratio) != 1L || base::anyNA(ratio[, base::c("median_ratio", "p25_ratio", "p75_ratio")])) {
        base::stop("No usable professional ", payer, " ratio for CPT ", codes[[parameter_name]], " (", parameter,
                   ") in ", ratios_path, ".", call. = FALSE)
      }

      original <- lines[[line_index]]
      fields <- csv_line_fields(original, n_fields = base::length(header))
      if (!base::identical(csv_line_join(fields), original)) {
        base::stop("The '", parameter, "' row does not use minimal CSV quoting; refusing to rewrite it.", call. = FALSE)
      }
      old <- fields[col[base::c("base_value", "low_value", "high_value")]]
      new_fields <- fields
      new_fields[col[["base_value"]]] <- base::sprintf("%.3f", ratio$median_ratio)
      new_fields[col[["low_value"]]] <- base::sprintf("%.3f", ratio$p25_ratio)
      new_fields[col[["high_value"]]] <- base::sprintf("%.3f", ratio$p75_ratio)
      new_fields[col[["source"]]] <- stringr::str_replace(fields[col[["source"]]], "@ [0-9a-f]{7,40}", base::paste("@", hpt_commit))
      new_fields[col[["notes"]]] <- stringr::str_replace(fields[col[["notes"]]], "Median across [0-9]+ hospitals",
                                                         base::paste("Median across", ratio$n_hospitals, "hospitals"))
      changed <- !base::identical(new_fields, fields)
      if (changed) {
        lines[[line_index]] <- csv_line_join(new_fields)
      }
      report[[base::length(report) + 1L]] <- tibble::tibble(
        parameter = parameter, n_hospitals = ratio$n_hospitals,
        base_old = old[[1]], base_new = new_fields[col[["base_value"]]],
        low_old = old[[2]], low_new = new_fields[col[["low_value"]]],
        high_old = old[[3]], high_new = new_fields[col[["high_value"]]],
        changed = changed
      )
    }
  }
  report <- dplyr::bind_rows(report)

  if (!dry_run && base::any(report$changed)) {
    out <- base::paste0(base::paste(lines, collapse = eol), if (ends_with_eol) eol else "")
    tmp <- base::paste0(params_path, ".tmp")
    con <- base::file(tmp, open = "wb")
    base::writeChar(out, con, eos = NULL, useBytes = TRUE)
    base::close(con)
    base::file.rename(tmp, params_path)
  }
  report
}
