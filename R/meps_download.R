#' Registry of 2024 MEPS public-use file download URLs
#'
#' @return Tibble with columns `key` (`"office"` or `"jobs"`),
#'   `source_page` (the AHRQ download page URL), and `download_url` (the
#'   ZIP file URL).
#' @export
meps_2024_resources <- function() {
  base::message("Building 2024 MEPS public-resource registry.")

  tibble::tribble(
    ~key, ~source_page, ~download_url,
    "office",
    base::paste0(
      "https://meps.ahrq.gov/mepsweb/data_stats/",
      "download_data_files_detail.jsp?",
      "cboPufNumber=HC-254G&prfricon=yes"
    ),
    base::paste0(
      "https://meps.ahrq.gov/mepsweb/data_files/",
      "pufs/h254g/h254gxlsx.zip"
    ),
    "jobs",
    base::paste0(
      "https://meps.ahrq.gov/mepsweb/data_stats/",
      "download_data_files_detail.jsp?",
      "cboPufNumber=HC-253&prfricon=yes"
    ),
    base::paste0(
      "https://meps.ahrq.gov/mepsweb/data_files/",
      "pufs/h253/h253xlsx.zip"
    )
  )
}

#' Download a public file to a local destination
#'
#' Creates the destination directory if needed. Skips the download (and
#' returns the existing file) if `destination` already exists and
#' `overwrite` is `FALSE`.
#'
#' @param url URL to download.
#' @param destination Local file path to save the download to.
#' @param overwrite If `TRUE`, re-download even when `destination` already
#'   exists.
#' @return `destination`, invisibly on the early-exit path, after the file
#'   exists on disk. Errors if the download does not create the file.
#' @export
download_public_file <- function(url,
                                 destination,
                                 overwrite = FALSE) {
  base::message("Downloading public file: ", url)
  base::message("Destination: ", destination)

  if (base::file.exists(destination) && !overwrite) {
    base::message("Using existing file: ", destination)
    return(destination)
  }

  parent_dir <- base::dirname(destination)

  if (!base::dir.exists(parent_dir)) {
    base::dir.create(parent_dir, recursive = TRUE)
  }

  request_obj <- httr2::request(url) |>
    httr2::req_user_agent(
      "emb-colonoscopy-research/0.1"
    ) |>
    httr2::req_retry(max_tries = 4) |>
    httr2::req_timeout(seconds = 180)

  httr2::req_perform(
    request_obj,
    path = destination
  )

  if (!base::file.exists(destination)) {
    base::stop("Download did not create: ", destination)
  }

  base::message("Downloaded: ", destination)

  destination
}

#' Compute the SHA-256 checksum of a file
#'
#' @param path Path to the file to hash. Errors if the file does not exist.
#' @return Character scalar: the hex-encoded SHA-256 hash.
#' @export
sha256_file <- function(path) {
  if (!base::file.exists(path)) {
    base::stop("Cannot hash missing file: ", path)
  }

  connection <- base::file(path, open = "rb")
  base::on.exit(base::close(connection), add = TRUE)

  hash_raw <- openssl::sha256(connection)

  base::as.character(hash_raw)
}

#' Required columns for a 2024 MEPS public-use file
#'
#' @param file_type Which MEPS file: `"office"` or `"jobs"` (matched via
#'   [base::match.arg()]).
#' @return Character vector of required column names for `file_type`.
#' @export
meps_required_columns <- function(file_type) {
  file_type <- base::match.arg(
    file_type,
    base::c("office", "jobs")
  )

  if (file_type == "office") {
    return(
      base::c(
        "DUPERSID",
        "OBXP24X",
        "OBSF24X",
        "PERWT24F",
        "VARSTR",
        "VARPSU"
      )
    )
  }

  base::c(
    "DUPERSID",
    "HRLYWAGE",
    "PERWT24F",
    "VARSTR",
    "VARPSU"
  )
}

#' Validate that a MEPS table has its required columns
#'
#' @param meps_tbl Tibble/data frame to check.
#' @param file_type Which MEPS file `meps_tbl` is from: `"office"` or
#'   `"jobs"` (see [meps_required_columns()]).
#' @return `TRUE` if all required columns are present. Errors listing the
#'   missing columns otherwise.
#' @export
validate_meps_columns <- function(meps_tbl,
                                  file_type) {
  required_cols <- meps_required_columns(file_type)
  missing_cols <- base::setdiff(
    required_cols,
    base::names(meps_tbl)
  )

  if (base::length(missing_cols) > 0L) {
    base::stop(
      "MEPS ",
      file_type,
      " file is missing required columns: ",
      base::paste(missing_cols, collapse = ", ")
    )
  }

  base::message(
    "Validated MEPS ",
    file_type,
    " columns: ",
    base::paste(required_cols, collapse = ", ")
  )

  TRUE
}

#' Locate the single XLSX workbook in a directory
#'
#' Searches recursively, ignoring Excel's temporary lock files (which
#' start with `~$`).
#'
#' @param directory Directory to search.
#' @return Path to the single matching `.xlsx` file. Errors if zero or
#'   more than one match is found.
#' @export
find_single_xlsx <- function(directory) {
  candidates <- base::list.files(
    directory,
    pattern = "\\.xlsx$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )

  candidates <- candidates[
    !base::grepl("^~\\$", base::basename(candidates))
  ]

  if (base::length(candidates) != 1L) {
    base::stop(
      "Expected exactly one XLSX in ",
      directory,
      "; found ",
      base::length(candidates),
      "."
    )
  }

  candidates[[1]]
}

#' Extract a MEPS XLSX workbook from its downloaded ZIP
#'
#' Skips extraction (and returns the existing workbook) if `extract_dir`
#' already contains exactly one `.xlsx` file and `overwrite` is `FALSE`.
#'
#' @param zip_path Path to the downloaded ZIP archive.
#' @param extract_dir Directory to extract into (created if needed).
#' @param overwrite If `TRUE`, re-extract even when a workbook already
#'   exists in `extract_dir`.
#' @return Path to the extracted (or pre-existing) `.xlsx` workbook.
#' @export
extract_meps_xlsx <- function(zip_path,
                              extract_dir,
                              overwrite = FALSE) {
  base::message("Extracting MEPS ZIP: ", zip_path)

  if (!base::dir.exists(extract_dir)) {
    base::dir.create(extract_dir, recursive = TRUE)
  }

  existing <- base::list.files(
    extract_dir,
    pattern = "\\.xlsx$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )

  if (base::length(existing) == 1L && !overwrite) {
    base::message("Using extracted workbook: ", existing[[1]])
    return(existing[[1]])
  }

  utils::unzip(
    zipfile = zip_path,
    exdir = extract_dir,
    overwrite = overwrite
  )

  xlsx_path <- find_single_xlsx(extract_dir)

  base::message("Extracted workbook: ", xlsx_path)

  xlsx_path
}

#' Validate that a MEPS workbook has its required columns
#'
#' Reads only the header row of the workbook before checking it.
#'
#' @param path Path to the MEPS `.xlsx` workbook.
#' @param file_type Which MEPS file `path` is: `"office"` or `"jobs"` (see
#'   [meps_required_columns()]).
#' @return `TRUE` if all required columns are present; errors otherwise.
#' @export
validate_meps_xlsx <- function(path,
                               file_type) {
  base::message("Inspecting MEPS workbook: ", path)

  header_tbl <- readxl::read_xlsx(
    path,
    n_max = 1
  )

  validate_meps_columns(
    header_tbl,
    file_type = file_type
  )
}

#' Download, extract, and validate the 2024 MEPS public-use files
#'
#' Downloads the office-visit and jobs 2024 MEPS ZIP files (see
#' [meps_2024_resources()]) into per-resource subdirectories of
#' `directory`, extracts each to an XLSX workbook, validates each
#' workbook's required columns, and writes a provenance CSV recording the
#' download URLs, file paths, SHA-256 checksums, and download timestamp.
#'
#' @param directory Root directory to download and extract files into.
#' @param overwrite If `TRUE`, re-download and re-extract even when files
#'   already exist.
#' @return Tibble with one row per resource and columns `key`,
#'   `source_page`, `download_url`, `zip_path`, `xlsx_path`, `sha256`,
#'   `downloaded_at`. Also writes a timestamped provenance CSV to
#'   `directory` as a side effect.
#' @export
download_meps_2024 <- function(
    directory = "data-raw/meps/2024",
    overwrite = FALSE) {
  base::message("Starting 2024 MEPS public downloads.")
  base::message("MEPS root directory: ", directory)

  resources_tbl <- meps_2024_resources()
  downloaded_at <- base::format(
    base::Sys.time(),
    "%Y-%m-%dT%H:%M:%S%z"
  )

  input_tbl <- purrr::pmap_dfr(
    resources_tbl,
    function(key,
             source_page,
             download_url) {
      resource_dir <- base::file.path(directory, key)
      zip_path <- base::file.path(
        resource_dir,
        base::basename(download_url)
      )

      download_public_file(
        url = download_url,
        destination = zip_path,
        overwrite = overwrite
      )

      xlsx_path <- extract_meps_xlsx(
        zip_path = zip_path,
        extract_dir = base::file.path(
          resource_dir,
          "xlsx"
        ),
        overwrite = overwrite
      )

      validate_meps_xlsx(
        xlsx_path,
        file_type = key
      )

      tibble::tibble(
        key = key,
        source_page = source_page,
        download_url = download_url,
        zip_path = zip_path,
        xlsx_path = xlsx_path,
        sha256 = sha256_file(zip_path),
        downloaded_at = downloaded_at
      )
    }
  )

  stamp <- base::format(
    base::Sys.time(),
    "%Y%m%d_%H%M%S"
  )

  provenance_path <- base::file.path(
    directory,
    base::paste0(
      "meps_2024_provenance_",
      stamp,
      ".csv"
    )
  )

  readr::write_csv(input_tbl, provenance_path)

  base::message("Saved MEPS provenance: ", provenance_path)

  input_tbl
}
