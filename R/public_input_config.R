#' Quote a value as an R string literal
#'
#' @param value Character vector of values to quote.
#' @return Character vector, same length as `value`, with each element
#'   double-quoted and escaped as a valid R string literal.
#' @export
quote_r_string <- function(value) {
  base::encodeString(
    value,
    quote = "\""
  )
}

#' Set environment variables pointing to the public input files
#'
#' @param office_xlsx Path to the MEPS office-visit XLSX workbook.
#' @param jobs_xlsx Path to the MEPS jobs XLSX workbook.
#' @param hpt_manifest Path to the HPT MRF manifest CSV.
#' @return Invisibly `TRUE`. Sets `MEPS_OFFICE_XLSX`, `MEPS_JOBS_XLSX`,
#'   and `HPT_MRF_MANIFEST` in the current R session's environment as a
#'   side effect.
#' @export
set_public_input_env <- function(office_xlsx,
                                 jobs_xlsx,
                                 hpt_manifest) {
  base::message("Setting public-input environment variables.")

  base::Sys.setenv(
    MEPS_OFFICE_XLSX = office_xlsx,
    MEPS_JOBS_XLSX = jobs_xlsx,
    HPT_MRF_MANIFEST = hpt_manifest
  )

  base::invisible(TRUE)
}

#' Write an R config file recording public input file locations
#'
#' Resolves each path to an absolute path, writes an R script to `path`
#' that calls `Sys.setenv()` with those absolute paths (so the config can
#' be `source()`d later to restore the environment variables), and also
#' sets those environment variables in the current session via
#' [set_public_input_env()].
#'
#' @param office_xlsx Path to the MEPS office-visit XLSX workbook.
#' @param jobs_xlsx Path to the MEPS jobs XLSX workbook.
#' @param hpt_manifest Path to the HPT MRF manifest CSV.
#' @param path Path to write the generated R config script to.
#' @return Invisibly, `path`. Writes the config file and sets session
#'   environment variables as side effects.
#' @export
write_public_input_config <- function(
    office_xlsx,
    jobs_xlsx,
    hpt_manifest,
    path = "config/public_inputs.R") {
  base::message("Writing public-input R config: ", path)

  parent_dir <- base::dirname(path)

  if (!base::dir.exists(parent_dir)) {
    base::dir.create(parent_dir, recursive = TRUE)
  }

  office_path <- base::normalizePath(
    office_xlsx,
    winslash = "/",
    mustWork = FALSE
  )

  jobs_path <- base::normalizePath(
    jobs_xlsx,
    winslash = "/",
    mustWork = FALSE
  )

  manifest_path <- base::normalizePath(
    hpt_manifest,
    winslash = "/",
    mustWork = FALSE
  )

  lines <- base::c(
    "base::Sys.setenv(",
    base::paste0(
      "  MEPS_OFFICE_XLSX = ",
      quote_r_string(office_path),
      ","
    ),
    base::paste0(
      "  MEPS_JOBS_XLSX = ",
      quote_r_string(jobs_path),
      ","
    ),
    base::paste0(
      "  HPT_MRF_MANIFEST = ",
      quote_r_string(manifest_path)
    ),
    ")"
  )

  base::writeLines(lines, path)

  set_public_input_env(
    office_xlsx = office_path,
    jobs_xlsx = jobs_path,
    hpt_manifest = manifest_path
  )

  base::message("Saved public-input config: ", path)

  base::invisible(path)
}
