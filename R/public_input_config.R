#' @export
quote_r_string <- function(value) {
  base::encodeString(
    value,
    quote = "\""
  )
}

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
