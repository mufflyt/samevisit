#' Normalize raw column names to snake_case
#'
#' Lowercases, collapses runs of non-alphanumeric characters to a single
#' underscore, trims leading/trailing underscores, then de-duplicates any
#' resulting collisions with [base::make.unique()].
#'
#' @param column_names Character vector of raw column names.
#' @return Character vector of normalized, unique column names, same length
#'   as `column_names`.
#' @export
normalize_public_names <- function(column_names) {
  normalized <- column_names |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z0-9]+", "_") |>
    stringr::str_replace_all("^_+|_+$", "")

  base::make.unique(normalized, sep = "_")
}

#' Apply column-name normalization to a raw CMS hospital frame
#'
#' Pulled out of [download_cms_hospital_frame()] as a pure function so it
#' can be unit-tested without a live download. CMS's real "City/Town"
#' header normalizes to "city_town" (the "/" becomes an underscore
#' separator, same as the space in "Facility ID"), not "citytown" --
#' confirmed against a live download on 2026-08-28, when this repository's
#' own synthetic test fixtures had baked in the same wrong assumption
#' ("citytown", no underscore) as the original implementation, so neither
#' caught it; only running against real data did. Every downstream
#' function in this file expects the canonical name "citytown", so rename
#' at this ingestion boundary rather than touching every call site.
#'
#' @param hospital_tbl A tibble/data.frame with raw CMS column headers.
#' @return `hospital_tbl` with normalized, `citytown`-corrected names.
#' @export
normalize_cms_hospital_frame_names <- function(hospital_tbl) {
  base::names(hospital_tbl) <- normalize_public_names(
    base::names(hospital_tbl)
  )

  if ("city_town" %in% base::names(hospital_tbl) &&
      !"citytown" %in% base::names(hospital_tbl)) {
    hospital_tbl <- dplyr::rename(hospital_tbl, citytown = "city_town")
  }

  hospital_tbl
}

#' Fetch CMS provider-data metastore metadata for a dataset
#'
#' @param identifier CMS provider-data dataset identifier. Defaults to the
#'   Hospital General Information dataset.
#' @return A list, the parsed JSON metadata response (via
#'   `httr2::resp_body_json(simplifyVector = FALSE)`), including a
#'   `distribution` field listing available file formats.
#' @export
cms_provider_dataset_metadata <- function(
    identifier = "xubh-q36u") {
  metadata_url <- base::paste0(
    "https://data.cms.gov/provider-data/api/1/",
    "metastore/schemas/dataset/items/",
    identifier
  )

  base::message("Reading CMS provider metadata: ", metadata_url)

  response <- httr2::request(metadata_url) |>
    httr2::req_user_agent(
      "emb-colonoscopy-research/0.1"
    ) |>
    httr2::req_retry(max_tries = 4) |>
    httr2::req_timeout(seconds = 60) |>
    httr2::req_perform()

  httr2::resp_body_json(
    response,
    simplifyVector = FALSE
  )
}

#' Extract the CSV download URL from CMS dataset metadata
#'
#' Finds the single `distribution` entry in `metadata` whose media type is
#' `"text/csv"` or whose `downloadURL` ends in `.csv`, and errors if there
#' is not exactly one such entry.
#'
#' @param metadata List of CMS provider-data metadata, as returned by
#'   [cms_provider_dataset_metadata()].
#' @return Character scalar, the CSV distribution's `downloadURL`.
#' @export
cms_csv_distribution_url <- function(metadata) {
  distributions <- metadata$distribution

  if (base::length(distributions) == 0L) {
    base::stop("CMS metadata contains no distributions.")
  }

  csv_distributions <- purrr::keep(
    distributions,
    function(item) {
      media_type <- item$mediaType %||% ""
      download_url <- item$downloadURL %||% ""

      base::identical(media_type, "text/csv") ||
        stringr::str_detect(
          download_url,
          stringr::regex("\\.csv($|\\?)", ignore_case = TRUE)
        )
    }
  )

  if (base::length(csv_distributions) != 1L) {
    base::stop(
      "Expected one CMS CSV distribution; found ",
      base::length(csv_distributions),
      "."
    )
  }

  csv_distributions[[1]]$downloadURL
}

`%||%` <- function(x, y) {
  if (base::is.null(x) || base::length(x) == 0L) {
    return(y)
  }

  x
}

#' Download and validate the CMS hospital sampling frame
#'
#' Looks up the CMS Hospital General Information CSV via
#' [cms_provider_dataset_metadata()] and [cms_csv_distribution_url()],
#' downloads it, normalizes its column names, checks that the columns
#' this pipeline depends on are present, and writes a provenance record
#' (source URL, local path, sha256, download timestamp) alongside it.
#'
#' @param directory Directory to save the downloaded CSV and its
#'   provenance file into; created if it does not exist.
#' @param identifier CMS provider-data dataset identifier to download.
#' @return Tibble of the downloaded hospital frame with normalized column
#'   names, all columns read as character.
#' @export
download_cms_hospital_frame <- function(
    directory = "data-raw/cms/hospitals",
    identifier = "xubh-q36u") {
  base::message("Starting CMS hospital sampling-frame download.")

  metadata <- cms_provider_dataset_metadata(identifier)
  csv_url <- cms_csv_distribution_url(metadata)

  if (!base::dir.exists(directory)) {
    base::dir.create(directory, recursive = TRUE)
  }

  stamp <- base::format(
    base::Sys.time(),
    "%Y%m%d_%H%M%S"
  )

  csv_path <- base::file.path(
    directory,
    base::paste0(
      "hospital_general_information_",
      stamp,
      ".csv"
    )
  )

  download_public_file(
    url = csv_url,
    destination = csv_path,
    overwrite = TRUE
  )

  hospital_tbl <- readr::read_csv(
    csv_path,
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character())
  )

  hospital_tbl <- normalize_cms_hospital_frame_names(hospital_tbl)

  required_cols <- base::c(
    "facility_id",
    "facility_name",
    "citytown",
    "state",
    "hospital_type",
    "hospital_ownership"
  )

  missing_cols <- base::setdiff(
    required_cols,
    base::names(hospital_tbl)
  )

  if (base::length(missing_cols) > 0L) {
    base::stop(
      "CMS hospital frame is missing: ",
      base::paste(missing_cols, collapse = ", ")
    )
  }

  provenance_tbl <- tibble::tibble(
    identifier = identifier,
    title = metadata$title %||% NA_character_,
    modified = metadata$modified %||% NA_character_,
    released = metadata$released %||% NA_character_,
    download_url = csv_url,
    local_path = csv_path,
    sha256 = sha256_file(csv_path),
    downloaded_at = base::format(
      base::Sys.time(),
      "%Y-%m-%dT%H:%M:%S%z"
    )
  )

  provenance_path <- base::file.path(
    directory,
    base::paste0(
      "hospital_general_information_provenance_",
      stamp,
      ".csv"
    )
  )

  readr::write_csv(provenance_tbl, provenance_path)

  base::message("Saved CMS hospital frame: ", csv_path)
  base::message("Saved CMS provenance: ", provenance_path)

  hospital_tbl
}

#' Map a US state abbreviation to its census region
#'
#' @param state Character vector of two-letter US state/territory
#'   abbreviations.
#' @return Character vector, one of `"Northeast"`, `"Midwest"`, `"South"`,
#'   `"West"`, or `NA_character_` for an unrecognized abbreviation. Same
#'   length as `state`.
#' @export
census_region_from_state <- function(state) {
  northeast <- base::c(
    "CT", "ME", "MA", "NH", "RI", "VT",
    "NJ", "NY", "PA"
  )

  midwest <- base::c(
    "IL", "IN", "MI", "OH", "WI",
    "IA", "KS", "MN", "MO", "NE", "ND", "SD"
  )

  south <- base::c(
    "DE", "DC", "FL", "GA", "MD", "NC", "SC", "VA", "WV",
    "AL", "KY", "MS", "TN", "AR", "LA", "OK", "TX"
  )

  west <- base::c(
    "AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY",
    "AK", "CA", "HI", "OR", "WA"
  )

  dplyr::case_when(
    state %in% northeast ~ "Northeast",
    state %in% midwest ~ "Midwest",
    state %in% south ~ "South",
    state %in% west ~ "West",
    TRUE ~ NA_character_
  )
}

#' Classify a CMS hospital ownership string into a coarse group
#'
#' @param ownership Character vector of raw CMS "Hospital Ownership"
#'   values (e.g. "Government - State", "Voluntary non-profit - Church",
#'   "Proprietary").
#' @return Character vector, one of `"government"`, `"nonprofit"`,
#'   `"for_profit"`, or `NA_character_` if none of those patterns match.
#'   Same length as `ownership`.
#' @export
ownership_group_from_text <- function(ownership) {
  dplyr::case_when(
    stringr::str_detect(
      ownership,
      stringr::regex("government", ignore_case = TRUE)
    ) ~ "government",
    stringr::str_detect(
      ownership,
      stringr::regex("voluntary|non-profit", ignore_case = TRUE)
    ) ~ "nonprofit",
    stringr::str_detect(
      ownership,
      stringr::regex("proprietary", ignore_case = TRUE)
    ) ~ "for_profit",
    TRUE ~ NA_character_
  )
}

#' Add census region and ownership group columns to a hospital frame
#'
#' @param hospital_tbl Tibble with `state` and `hospital_ownership`
#'   columns, e.g. from [download_cms_hospital_frame()].
#' @return `hospital_tbl` with two added columns: `census_region` (from
#'   [census_region_from_state()]) and `ownership_group` (from
#'   [ownership_group_from_text()]).
#' @export
classify_hpt_hospitals <- function(hospital_tbl) {
  base::message("Classifying hospitals for HPT sampling.")

  hospital_tbl |>
    dplyr::mutate(
      census_region = census_region_from_state(.data$state),
      ownership_group = ownership_group_from_text(
        .data$hospital_ownership
      )
    )
}

#' Draw a stratified sample of hospitals for HPT discovery
#'
#' Classifies hospitals via [classify_hpt_hospitals()], restricts to
#' "Acute Care Hospitals" with a known census region and ownership group,
#' then samples `per_stratum` hospitals from each of the 12
#' census-region x ownership-group strata using a fixed seed for
#' reproducibility. Errors if any stratum has fewer than `per_stratum`
#' eligible hospitals, or if the final sample size is not exactly
#' `per_stratum * 12`.
#'
#' @param hospital_tbl Tibble of hospitals, e.g. from
#'   [download_cms_hospital_frame()].
#' @param per_stratum Number of hospitals to sample from each
#'   region x ownership stratum.
#' @param seed Integer seed passed to [withr::with_seed()] for
#'   reproducible sampling.
#' @return Tibble of sampled hospitals, `per_stratum * 12` rows, with
#'   `census_region` and `ownership_group` columns added, sorted by
#'   region, ownership group, and facility ID.
#' @export
sample_hpt_hospitals <- function(hospital_tbl,
                                 per_stratum = 10L,
                                 seed = 20260828L) {
  base::message(
    "Sampling ",
    per_stratum,
    " hospitals per region x ownership stratum."
  )
  base::message("HPT sampling seed: ", seed)

  classified_tbl <- classify_hpt_hospitals(hospital_tbl) |>
    dplyr::filter(
      .data$hospital_type == "Acute Care Hospitals",
      !base::is.na(.data$census_region),
      !base::is.na(.data$ownership_group)
    ) |>
    dplyr::arrange(
      .data$census_region,
      .data$ownership_group,
      .data$facility_id
    )

  stratum_counts <- classified_tbl |>
    dplyr::count(
      .data$census_region,
      .data$ownership_group,
      name = "available_n"
    )

  undersized <- stratum_counts |>
    dplyr::filter(.data$available_n < per_stratum)

  if (base::nrow(undersized) > 0L) {
    base::stop(
      "At least one HPT sampling stratum has fewer than ",
      per_stratum,
      " eligible hospitals."
    )
  }

  sampled_tbl <- withr::with_seed(
    seed,
    classified_tbl |>
      dplyr::group_by(
        .data$census_region,
        .data$ownership_group
      ) |>
      dplyr::slice_sample(n = per_stratum) |>
      dplyr::ungroup() |>
      dplyr::arrange(
        .data$census_region,
        .data$ownership_group,
        .data$facility_id
      )
  )

  expected_n <- per_stratum * 12L

  if (base::nrow(sampled_tbl) != expected_n) {
    base::stop(
      "Expected ",
      expected_n,
      " sampled hospitals; found ",
      base::nrow(sampled_tbl),
      "."
    )
  }

  base::message(
    "Sampled ",
    scales::comma(base::nrow(sampled_tbl)),
    " hospitals."
  )

  sampled_tbl
}

#' Write the HPT hospital sample and domain template to disk
#'
#' Writes `sample_tbl` to a canonical `hpt_hospital_sample.csv` and a
#' timestamped audit copy. Also writes a `hpt_hospital_domains.csv`
#' template (one row per sampled hospital, with blank `website_domain`
#' and `domain_source` columns to be filled in later) unless that file
#' already exists and `overwrite_domains` is `FALSE`, in which case the
#' existing domain file is left untouched.
#'
#' @param sample_tbl Tibble of sampled hospitals, e.g. from
#'   [sample_hpt_hospitals()].
#' @param config_dir Directory to write the sample and domain files
#'   into; created if it does not exist.
#' @param overwrite_domains If `TRUE`, regenerate the domain template
#'   even if `hpt_hospital_domains.csv` already exists.
#' @return A list with `sample_path`, `sample_audit_path`, and
#'   `domain_path`, the paths written.
#' @export
write_hpt_sample_files <- function(
    sample_tbl,
    config_dir = "config",
    overwrite_domains = FALSE) {
  if (!base::dir.exists(config_dir)) {
    base::dir.create(config_dir, recursive = TRUE)
  }

  stamp <- base::format(
    base::Sys.time(),
    "%Y%m%d_%H%M%S"
  )

  sample_audit_path <- base::file.path(
    config_dir,
    base::paste0("hpt_hospital_sample_", stamp, ".csv")
  )

  sample_path <- base::file.path(
    config_dir,
    "hpt_hospital_sample.csv"
  )

  domain_path <- base::file.path(
    config_dir,
    "hpt_hospital_domains.csv"
  )

  readr::write_csv(sample_tbl, sample_audit_path)
  readr::write_csv(sample_tbl, sample_path)

  if (!base::file.exists(domain_path) || overwrite_domains) {
    domain_tbl <- sample_tbl |>
      dplyr::transmute(
        facility_id = .data$facility_id,
        facility_name = .data$facility_name,
        citytown = .data$citytown,
        state = .data$state,
        census_region = .data$census_region,
        ownership_group = .data$ownership_group,
        website_domain = "",
        domain_source = ""
      )

    readr::write_csv(domain_tbl, domain_path)
    base::message("Saved HPT domain template: ", domain_path)
  } else {
    base::message("Preserving existing HPT domain file: ", domain_path)
  }

  base::message("Saved HPT sample: ", sample_path)
  base::message("Saved HPT sample audit: ", sample_audit_path)

  base::list(
    sample_path = sample_path,
    sample_audit_path = sample_audit_path,
    domain_path = domain_path
  )
}

#' Normalize a hospital website domain string
#'
#' Trims whitespace, strips a leading `http://`/`https://` scheme, and
#' strips any path or trailing slashes, leaving a bare host (e.g.
#' `"www.example.org"`).
#'
#' @param domain Character vector of raw domain or URL strings.
#' @return Character vector of normalized bare hostnames, same length as
#'   `domain`.
#' @export
normalize_hpt_domain <- function(domain) {
  normalized <- domain |>
    stringr::str_trim() |>
    stringr::str_replace(
      stringr::regex("^https?://", ignore_case = TRUE),
      ""
    ) |>
    stringr::str_replace("/.*$", "") |>
    stringr::str_replace("/+$", "")

  normalized
}

#' Look up the first value for a given key in parallel key/value vectors
#'
#' Used to pull a single named field (e.g. `"mrf-url"`) out of one
#' cms-hpt.txt location block, where `key` and `value` are the parsed
#' key and value columns for that block.
#'
#' @param key Character vector of keys.
#' @param value Character vector of values, same length as `key`.
#' @param target Character scalar key to look up.
#' @return Character scalar, the first `value` where `key == target`, or
#'   `NA_character_` if `target` does not occur in `key`.
#' @export
hpt_field_value <- function(key,
                            value,
                            target) {
  matches <- value[key == target]

  if (base::length(matches) == 0L) {
    return(NA_character_)
  }

  matches[[1]]
}

#' Parse cms-hpt.txt content into a tibble of discovered locations
#'
#' cms-hpt.txt is a plain-text `key: value` format grouped into blocks,
#' each block starting with a `location-name` line. This splits
#' `hpt_text` into lines, parses each `key: value` pair, groups
#' consecutive pairs into blocks on `location-name`, and pulls out the
#' fields this pipeline cares about via [hpt_field_value()].
#'
#' @param hpt_text Character scalar, the raw contents of a cms-hpt.txt
#'   file.
#' @return Tibble with one row per location block and columns
#'   `location_name`, `source_page_url`, `mrf_url`, `contact_name`,
#'   `contact_email`. Zero rows if no `location-name` block is found.
#' @export
parse_cms_hpt_text <- function(hpt_text) {
  base::message("Parsing cms-hpt.txt content.")

  lines <- base::strsplit(
    hpt_text,
    split = "\\r?\\n"
  )[[1]]

  parsed <- stringr::str_match(
    lines,
    "^\\s*([^:]+)\\s*:\\s*(.*?)\\s*$"
  )

  pairs_tbl <- tibble::tibble(
    key = stringr::str_to_lower(parsed[, 2]),
    value = parsed[, 3]
  ) |>
    dplyr::filter(!base::is.na(.data$key)) |>
    dplyr::mutate(
      key = stringr::str_trim(.data$key),
      value = stringr::str_trim(.data$value),
      location_block = base::cumsum(
        .data$key == "location-name"
      )
    ) |>
    dplyr::filter(.data$location_block > 0L)

  if (base::nrow(pairs_tbl) == 0L) {
    return(
      tibble::tibble(
        location_name = base::character(),
        source_page_url = base::character(),
        mrf_url = base::character(),
        contact_name = base::character(),
        contact_email = base::character()
      )
    )
  }

  pairs_tbl |>
    dplyr::group_by(.data$location_block) |>
    dplyr::summarise(
      location_name = hpt_field_value(
        .data$key,
        .data$value,
        "location-name"
      ),
      source_page_url = hpt_field_value(
        .data$key,
        .data$value,
        "source-page-url"
      ),
      mrf_url = hpt_field_value(
        .data$key,
        .data$value,
        "mrf-url"
      ),
      contact_name = hpt_field_value(
        .data$key,
        .data$value,
        "contact-name"
      ),
      contact_email = hpt_field_value(
        .data$key,
        .data$value,
        "contact-email"
      ),
      .groups = "drop"
    ) |>
    dplyr::select(-"location_block")
}

#' Fetch a hospital's cms-hpt.txt discovery file
#'
#' Normalizes `domain` via [normalize_hpt_domain()] and tries
#' `https://<domain>/cms-hpt.txt` then `http://<domain>/cms-hpt.txt`,
#' returning the first successful response.
#'
#' @param domain Character scalar hospital website domain (or URL; will
#'   be normalized).
#' @return A list with `url` (the URL that succeeded), `status` (HTTP
#'   status code), and `text` (the response body). Errors, including the
#'   last request's error message, if both URLs fail.
#' @export
fetch_cms_hpt_text <- function(domain) {
  normalized_domain <- normalize_hpt_domain(domain)

  if (!base::nzchar(normalized_domain)) {
    base::stop("Hospital website domain is blank.")
  }

  candidate_urls <- base::c(
    base::paste0(
      "https://",
      normalized_domain,
      "/cms-hpt.txt"
    ),
    base::paste0(
      "http://",
      normalized_domain,
      "/cms-hpt.txt"
    )
  )

  last_error <- NULL

  for (candidate_url in candidate_urls) {
    base::message("Trying HPT discovery URL: ", candidate_url)

    fetched <- base::tryCatch(
      {
        response <- httr2::request(candidate_url) |>
          httr2::req_user_agent(
            "emb-colonoscopy-research/0.1"
          ) |>
          httr2::req_retry(max_tries = 3) |>
          httr2::req_timeout(seconds = 30) |>
          httr2::req_perform()

        base::list(
          url = candidate_url,
          status = httr2::resp_status(response),
          text = httr2::resp_body_string(response)
        )
      },
      error = function(error_condition) {
        last_error <<- base::conditionMessage(
          error_condition
        )
        NULL
      }
    )

    if (!base::is.null(fetched)) {
      return(fetched)
    }
  }

  base::stop(
    "Could not retrieve cms-hpt.txt for ",
    normalized_domain,
    ". Last error: ",
    last_error
  )
}

#' Normalize a hospital name for fuzzy matching
#'
#' Uppercases, replaces any non-alphanumeric, non-space character with a
#' space, and squishes repeated/leading/trailing whitespace.
#'
#' @param name Character vector of hospital names.
#' @return Character vector of normalized names, same length as `name`.
#' @export
normalize_hospital_name <- function(name) {
  name |>
    stringr::str_to_upper() |>
    stringr::str_replace_all("[^A-Z0-9 ]", " ") |>
    stringr::str_squish()
}

#' Split a hospital name into matching tokens
#'
#' Normalizes `name` via [normalize_hospital_name()], splits on
#' whitespace, and drops common generic stop words (e.g. "HOSPITAL",
#' "MEDICAL", "CENTER", "HEALTH", "SYSTEM") that would otherwise inflate
#' similarity between unrelated hospitals.
#'
#' @param name Character scalar hospital name.
#' @return Character vector of uppercase tokens, with stop words removed.
#' @export
hospital_name_tokens <- function(name) {
  stop_words <- base::c(
    "HOSPITAL",
    "MEDICAL",
    "CENTER",
    "CENTRE",
    "HEALTH",
    "SYSTEM",
    "THE",
    "OF"
  )

  tokens <- normalize_hospital_name(name) |>
    stringr::str_split("\\s+") |>
    base::unlist()

  base::setdiff(tokens, stop_words)
}

#' Score the similarity of two hospital names
#'
#' Tokenizes both names via [hospital_name_tokens()] and computes the
#' Jaccard similarity (intersection over union) of the two token sets.
#'
#' @param reference_name Character scalar, the hospital name to match
#'   against.
#' @param candidate_name Character scalar, the candidate hospital name.
#' @return Numeric scalar in `[0, 1]`; `0` if both names tokenize to no
#'   tokens at all.
#' @export
hospital_name_score <- function(reference_name,
                                candidate_name) {
  reference_tokens <- hospital_name_tokens(reference_name)
  candidate_tokens <- hospital_name_tokens(candidate_name)

  union_tokens <- base::union(
    reference_tokens,
    candidate_tokens
  )

  if (base::length(union_tokens) == 0L) {
    return(0)
  }

  base::length(
    base::intersect(reference_tokens, candidate_tokens)
  ) / base::length(union_tokens)
}

#' Select the HPT location row matching a given facility
#'
#' Restricts `discovered_tbl` to rows with a non-blank `mrf_url`. If
#' exactly one such row remains, returns it directly (with
#' `name_match_score` set to 1). Otherwise scores every remaining row's
#' `location_name` against `facility_name` via [hospital_name_score()]
#' and returns the unique top-scoring row, provided its score is at
#' least `min_score`; errors if no row reaches `min_score` or if the top
#' score is tied between multiple rows.
#'
#' @param discovered_tbl Tibble of candidate locations, e.g. from
#'   [parse_cms_hpt_text()].
#' @param facility_name Character scalar, the CMS facility name to match
#'   against.
#' @param min_score Minimum [hospital_name_score()] required to accept a
#'   match when more than one candidate location exists.
#' @return One-row tibble (a subset of `discovered_tbl`'s rows) with an
#'   added `name_match_score` column.
#' @export
select_hpt_location <- function(discovered_tbl,
                                facility_name,
                                min_score = 0.40) {
  valid_tbl <- discovered_tbl |>
    dplyr::filter(
      !base::is.na(.data$mrf_url),
      base::nzchar(.data$mrf_url)
    )

  if (base::nrow(valid_tbl) == 0L) {
    base::stop("cms-hpt.txt did not contain an MRF URL.")
  }

  if (base::nrow(valid_tbl) == 1L) {
    return(
      valid_tbl |>
        dplyr::mutate(name_match_score = 1)
    )
  }

  scored_tbl <- valid_tbl |>
    dplyr::mutate(
      name_match_score = purrr::map_dbl(
        .data$location_name,
        ~ hospital_name_score(
          facility_name,
          .x
        )
      )
    ) |>
    dplyr::arrange(dplyr::desc(.data$name_match_score))

  top_score <- scored_tbl$name_match_score[[1]]
  top_n <- base::sum(
    scored_tbl$name_match_score == top_score
  )

  if (top_score < min_score || top_n != 1L) {
    base::stop(
      "Could not uniquely match HPT location for: ",
      facility_name
    )
  }

  scored_tbl |>
    dplyr::slice(1L)
}

#' Infer a machine-readable file's format from its URL
#'
#' @param mrf_url Character vector of MRF URLs. Query strings and
#'   fragments are stripped before checking the extension.
#' @return Character vector, one of `"csv"`, `"json"`, or `"unknown"`.
#'   Same length as `mrf_url`.
#' @export
infer_mrf_format <- function(mrf_url) {
  clean_url <- stringr::str_remove(mrf_url, "[?#].*$")

  dplyr::case_when(
    stringr::str_detect(
      clean_url,
      stringr::regex("\\.csv$", ignore_case = TRUE)
    ) ~ "csv",
    stringr::str_detect(
      clean_url,
      stringr::regex("\\.json$", ignore_case = TRUE)
    ) ~ "json",
    TRUE ~ "unknown"
  )
}

#' Check whether an MRF URL responds to an HTTP HEAD request
#'
#' @param mrf_url Character scalar URL to probe.
#' @return Integer scalar HTTP status code, or `NA_integer_` if the
#'   request errors (e.g. timeout, DNS failure, connection refused).
#' @export
check_mrf_head <- function(mrf_url) {
  checked <- base::tryCatch(
    {
      response <- httr2::request(mrf_url) |>
        httr2::req_method("HEAD") |>
        httr2::req_user_agent(
          "emb-colonoscopy-research/0.1"
        ) |>
        httr2::req_timeout(seconds = 30) |>
        httr2::req_perform()

      httr2::resp_status(response)
    },
    error = function(error_condition) {
      NA_integer_
    }
  )

  checked
}

#' Resolve one hospital's machine-readable price-transparency file
#'
#' Orchestrates the full per-hospital discovery pipeline: if
#' `website_domain` is blank, returns immediately with
#' `resolution_status = "missing_domain"`; otherwise fetches the
#' hospital's cms-hpt.txt ([fetch_cms_hpt_text()]), parses it
#' ([parse_cms_hpt_text()]), selects the matching location
#' ([select_hpt_location()]), infers the MRF format
#' ([infer_mrf_format()]), and probes the MRF URL
#' ([check_mrf_head()]). Any error along that path is caught and
#' reported as `resolution_status = "failed"` with `error_message` set,
#' rather than propagated.
#'
#' @param facility_id CMS facility ID (CCN).
#' @param facility_name CMS facility name, used for fuzzy location
#'   matching.
#' @param citytown Facility city/town, passed through to the output.
#' @param state Facility state, passed through to the output.
#' @param census_region Facility census region, passed through to the
#'   output.
#' @param ownership_group Facility ownership group, passed through to
#'   the output.
#' @param website_domain Hospital website domain (or URL); `NA` or blank
#'   short-circuits to `resolution_status = "missing_domain"`.
#' @return One-row tibble with `facility_id`, `facility_name`,
#'   `citytown`, `state`, `census_region`, `ownership_group`,
#'   `website_domain`, `resolution_status` (`"resolved"`,
#'   `"missing_domain"`, or `"failed"`), `cms_hpt_url`, `location_name`,
#'   `source_page_url`, `mrf_url`, `mrf_format`, `mrf_head_status`,
#'   `name_match_score`, and `error_message`.
#' @export
resolve_one_hpt_hospital <- function(facility_id,
                                     facility_name,
                                     citytown,
                                     state,
                                     census_region,
                                     ownership_group,
                                     website_domain) {
  base::message(
    "Resolving HPT MRF for ",
    facility_name,
    " (",
    facility_id,
    ")."
  )

  if (base::is.na(website_domain) ||
      !base::nzchar(website_domain)) {
    return(
      tibble::tibble(
        facility_id = facility_id,
        facility_name = facility_name,
        citytown = citytown,
        state = state,
        census_region = census_region,
        ownership_group = ownership_group,
        website_domain = website_domain,
        resolution_status = "missing_domain",
        cms_hpt_url = NA_character_,
        location_name = NA_character_,
        source_page_url = NA_character_,
        mrf_url = NA_character_,
        mrf_format = NA_character_,
        mrf_head_status = NA_integer_,
        name_match_score = NA_real_,
        error_message = "website_domain is blank"
      )
    )
  }

  base::tryCatch(
    {
      fetched <- fetch_cms_hpt_text(website_domain)
      discovered_tbl <- parse_cms_hpt_text(fetched$text)
      selected_tbl <- select_hpt_location(
        discovered_tbl,
        facility_name = facility_name
      )

      tibble::tibble(
        facility_id = facility_id,
        facility_name = facility_name,
        citytown = citytown,
        state = state,
        census_region = census_region,
        ownership_group = ownership_group,
        website_domain = normalize_hpt_domain(
          website_domain
        ),
        resolution_status = "resolved",
        cms_hpt_url = fetched$url,
        location_name = selected_tbl$location_name[[1]],
        source_page_url = selected_tbl$source_page_url[[1]],
        mrf_url = selected_tbl$mrf_url[[1]],
        mrf_format = infer_mrf_format(
          selected_tbl$mrf_url[[1]]
        ),
        mrf_head_status = check_mrf_head(
          selected_tbl$mrf_url[[1]]
        ),
        name_match_score = selected_tbl$name_match_score[[1]],
        error_message = NA_character_
      )
    },
    error = function(error_condition) {
      tibble::tibble(
        facility_id = facility_id,
        facility_name = facility_name,
        citytown = citytown,
        state = state,
        census_region = census_region,
        ownership_group = ownership_group,
        website_domain = normalize_hpt_domain(
          website_domain
        ),
        resolution_status = "failed",
        cms_hpt_url = NA_character_,
        location_name = NA_character_,
        source_page_url = NA_character_,
        mrf_url = NA_character_,
        mrf_format = NA_character_,
        mrf_head_status = NA_integer_,
        name_match_score = NA_real_,
        error_message = base::conditionMessage(
          error_condition
        )
      )
    }
  )
}

#' Resolve MRF locations for an entire HPT hospital sample
#'
#' Left-joins `sample_tbl` to `domains_tbl` on `facility_id`, then calls
#' [resolve_one_hpt_hospital()] for every row via [purrr::pmap_dfr()].
#' Splits the combined results into a cleaned, renamed `manifest` of
#' only the successfully resolved hospitals and a `failures` table of
#' everything else.
#'
#' @param sample_tbl Tibble of sampled hospitals with `facility_id`,
#'   `facility_name`, `citytown`, `state`, `census_region`, and
#'   `ownership_group` columns.
#' @param domains_tbl Tibble with `facility_id` and `website_domain`
#'   columns, e.g. from the HPT domain template file.
#' @return A list with `manifest` (tibble of resolved hospitals, with
#'   columns `hospital_name`, `hospital_state`, `facility_id`,
#'   `citytown`, `census_region`, `ownership_group`, `website_domain`,
#'   `cms_hpt_url`, `location_name`, `source_page_url`, `mrf_url`,
#'   `mrf_format`, `mrf_head_status`, `name_match_score`) and `failures`
#'   (tibble of rows where `resolution_status != "resolved"`, in the
#'   shape returned by [resolve_one_hpt_hospital()]).
#' @export
resolve_hpt_manifest <- function(sample_tbl,
                                 domains_tbl) {
  base::message("Joining HPT sample to hospital domains.")

  required_domain_cols <- base::c(
    "facility_id",
    "website_domain"
  )

  missing_cols <- base::setdiff(
    required_domain_cols,
    base::names(domains_tbl)
  )

  if (base::length(missing_cols) > 0L) {
    base::stop(
      "HPT domain file is missing: ",
      base::paste(missing_cols, collapse = ", ")
    )
  }

  joined_tbl <- sample_tbl |>
    dplyr::select(
      .data$facility_id,
      .data$facility_name,
      .data$citytown,
      .data$state,
      .data$census_region,
      .data$ownership_group
    ) |>
    dplyr::left_join(
      domains_tbl |>
        dplyr::select(
          .data$facility_id,
          .data$website_domain
        ),
      by = "facility_id",
      relationship = "one-to-one"
    )

  resolution_tbl <- purrr::pmap_dfr(
    joined_tbl,
    resolve_one_hpt_hospital
  )

  manifest_tbl <- resolution_tbl |>
    dplyr::filter(.data$resolution_status == "resolved") |>
    dplyr::transmute(
      hospital_name = .data$facility_name,
      hospital_state = .data$state,
      facility_id = .data$facility_id,
      citytown = .data$citytown,
      census_region = .data$census_region,
      ownership_group = .data$ownership_group,
      website_domain = .data$website_domain,
      cms_hpt_url = .data$cms_hpt_url,
      location_name = .data$location_name,
      source_page_url = .data$source_page_url,
      mrf_url = .data$mrf_url,
      mrf_format = .data$mrf_format,
      mrf_head_status = .data$mrf_head_status,
      name_match_score = .data$name_match_score
    )

  failure_tbl <- resolution_tbl |>
    dplyr::filter(.data$resolution_status != "resolved")

  base::list(
    manifest = manifest_tbl,
    failures = failure_tbl
  )
}

#' Write the HPT MRF manifest and failure log to disk
#'
#' Writes `resolution$manifest` to a canonical `hpt_mrf_manifest.csv`
#' and a timestamped audit copy, and `resolution$failures` to a
#' timestamped failure log.
#'
#' @param resolution List with `manifest` and `failures` tibbles, as
#'   returned by [resolve_hpt_manifest()].
#' @param config_dir Directory to write the files into; created if it
#'   does not exist.
#' @return A list with `manifest_path`, `manifest_audit_path`, and
#'   `failure_path`, the paths written.
#' @export
write_hpt_resolution_files <- function(
    resolution,
    config_dir = "config") {
  if (!base::dir.exists(config_dir)) {
    base::dir.create(config_dir, recursive = TRUE)
  }

  stamp <- base::format(
    base::Sys.time(),
    "%Y%m%d_%H%M%S"
  )

  manifest_path <- base::file.path(
    config_dir,
    "hpt_mrf_manifest.csv"
  )

  manifest_audit_path <- base::file.path(
    config_dir,
    base::paste0("hpt_mrf_manifest_", stamp, ".csv")
  )

  failure_path <- base::file.path(
    config_dir,
    base::paste0("hpt_mrf_failures_", stamp, ".csv")
  )

  readr::write_csv(resolution$manifest, manifest_path)
  readr::write_csv(
    resolution$manifest,
    manifest_audit_path
  )
  readr::write_csv(resolution$failures, failure_path)

  base::message("Saved HPT manifest: ", manifest_path)
  base::message("Saved HPT manifest audit: ", manifest_audit_path)
  base::message("Saved HPT failure log: ", failure_path)

  base::list(
    manifest_path = manifest_path,
    manifest_audit_path = manifest_audit_path,
    failure_path = failure_path
  )
}

#' Location of the TPAFS historical HPT URL index
#'
#' This index is used only to seed hospital website-domain guesses
#' ([build_hpt_domain_hints()]), not as a source of prices.
#'
#' @return One-row tibble with `source`, `source_page`, and
#'   `download_url` columns describing the TPAFS transparency-data
#'   `machine_readable_links.csv` resource.
#' @export
hpt_historical_index_resource <- function() {
  tibble::tibble(
    source = "TPAFS transparency-data",
    source_page = base::paste0(
      "https://github.com/TPAFS/transparency-data/blob/main/",
      "price_transparency/hospitals/machine_readable_links.csv"
    ),
    download_url = base::paste0(
      "https://raw.githubusercontent.com/TPAFS/",
      "transparency-data/main/price_transparency/hospitals/",
      "machine_readable_links.csv"
    )
  )
}

#' Download the TPAFS historical HPT URL index
#'
#' Downloads the CSV at [hpt_historical_index_resource()], validates
#' that it has the columns this pipeline depends on, and writes a
#' provenance record (source, local path, sha256, download timestamp)
#' alongside it.
#'
#' @param directory Directory to save the downloaded CSV and its
#'   provenance file into; created if it does not exist.
#' @return Tibble of the downloaded index, all columns read as
#'   character, with at least `ccn`, `machine_readable_page`,
#'   `supplemental_url`, and `machine_readable_url` columns.
#' @export
download_hpt_historical_index <- function(
    directory = "data-raw/hpt/index") {
  base::message("Downloading historical HPT URL index.")
  base::message(
    "This index is used only for domain discovery, not prices."
  )

  resource_tbl <- hpt_historical_index_resource()

  if (!base::dir.exists(directory)) {
    base::dir.create(directory, recursive = TRUE)
  }

  stamp <- base::format(
    base::Sys.time(),
    "%Y%m%d_%H%M%S"
  )

  index_path <- base::file.path(
    directory,
    base::paste0("hpt_url_index_", stamp, ".csv")
  )

  download_public_file(
    url = resource_tbl$download_url[[1]],
    destination = index_path,
    overwrite = TRUE
  )

  index_tbl <- readr::read_csv(
    index_path,
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character())
  )

  required_cols <- base::c(
    "ccn",
    "machine_readable_page",
    "supplemental_url",
    "machine_readable_url"
  )

  missing_cols <- base::setdiff(
    required_cols,
    base::names(index_tbl)
  )

  if (base::length(missing_cols) > 0L) {
    base::stop(
      "Historical HPT index is missing: ",
      base::paste(missing_cols, collapse = ", ")
    )
  }

  provenance_tbl <- tibble::tibble(
    source = resource_tbl$source[[1]],
    source_page = resource_tbl$source_page[[1]],
    download_url = resource_tbl$download_url[[1]],
    local_path = index_path,
    sha256 = sha256_file(index_path),
    downloaded_at = base::format(
      base::Sys.time(),
      "%Y-%m-%dT%H:%M:%S%z"
    )
  )

  provenance_path <- base::file.path(
    directory,
    base::paste0("hpt_url_index_provenance_", stamp, ".csv")
  )

  readr::write_csv(provenance_tbl, provenance_path)

  base::message("Saved historical HPT index: ", index_path)
  base::message("Saved HPT index provenance: ", provenance_path)

  index_tbl
}

#' Extract the lowercase hostname from an http(s) URL
#'
#' @param url Character scalar URL.
#' @return Character scalar lowercase hostname, or `NA_character_` if
#'   `url` is `NA`, blank, or does not match `^https?://`.
#' @export
extract_url_hostname <- function(url) {
  if (base::is.na(url) || !base::nzchar(url)) {
    return(NA_character_)
  }

  matched <- stringr::str_match(
    url,
    stringr::regex(
      "^https?://([^/:?#]+)",
      ignore_case = TRUE
    )
  )

  hostname <- matched[, 2]

  if (base::is.na(hostname)) {
    return(NA_character_)
  }

  stringr::str_to_lower(hostname)
}

#' Build per-hospital website-domain hints from the historical HPT index
#'
#' For each CMS certification number (`ccn`) in `index_tbl`, extracts a
#' hostname from `machine_readable_page`, `supplemental_url`, and
#' `machine_readable_url` (via [extract_url_hostname()]) and coalesces
#' them in that priority order to pick one `website_domain` hint per
#' hospital, recording which column it came from in `domain_source`.
#' Rows with a blank `ccn` or no derivable hostname are dropped, and
#' only the first row per `ccn` is kept.
#'
#' @param index_tbl Tibble with `ccn`, `machine_readable_page`,
#'   `supplemental_url`, and `machine_readable_url` columns, e.g. from
#'   [download_hpt_historical_index()].
#' @return Tibble with one row per `ccn`, columns `facility_id`,
#'   `website_domain`, and `domain_source` (one of
#'   `"historical_machine_readable_page"`,
#'   `"historical_supplemental_url"`, or `"historical_mrf_host"`).
#' @export
build_hpt_domain_hints <- function(index_tbl) {
  base::message("Building HPT domain hints by CMS certification number.")

  index_tbl |>
    dplyr::filter(
      !base::is.na(.data$ccn),
      base::nzchar(.data$ccn)
    ) |>
    dplyr::mutate(
      page_domain = purrr::map_chr(
        .data$machine_readable_page,
        extract_url_hostname
      ),
      supplemental_domain = purrr::map_chr(
        .data$supplemental_url,
        extract_url_hostname
      ),
      mrf_domain = purrr::map_chr(
        .data$machine_readable_url,
        extract_url_hostname
      ),
      website_domain = dplyr::coalesce(
        .data$page_domain,
        .data$supplemental_domain,
        .data$mrf_domain
      ),
      domain_source = dplyr::case_when(
        !base::is.na(.data$page_domain) ~
          "historical_machine_readable_page",
        !base::is.na(.data$supplemental_domain) ~
          "historical_supplemental_url",
        !base::is.na(.data$mrf_domain) ~
          "historical_mrf_host",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::filter(
      !base::is.na(.data$website_domain),
      base::nzchar(.data$website_domain)
    ) |>
    dplyr::arrange(.data$ccn) |>
    dplyr::group_by(.data$ccn) |>
    dplyr::slice(1L) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      facility_id = .data$ccn,
      website_domain = .data$website_domain,
      domain_source = .data$domain_source
    )
}

#' Prefill blank website domains from the historical HPT index
#'
#' Reads the domain file at `domain_path`, builds historical hints via
#' [build_hpt_domain_hints()], and fills in `website_domain` (and
#' `domain_source`) for any row where `website_domain` is currently
#' blank, leaving rows that already have a domain untouched (their
#' `domain_source` is set to `"manual"` if not already populated).
#' Writes the updated table back to `domain_path`.
#'
#' @param domain_path Path to an existing HPT domain CSV (e.g. from
#'   [write_hpt_sample_files()]) with `facility_id` and `website_domain`
#'   columns.
#' @param index_tbl Tibble of the historical HPT URL index, e.g. from
#'   [download_hpt_historical_index()].
#' @return Tibble of the updated domain file (also written to
#'   `domain_path`), with `website_domain` and `domain_source` columns
#'   filled in where hints were available.
#' @export
prefill_hpt_domains <- function(domain_path,
                                index_tbl) {
  base::message("Prefilling HPT domains from historical URL hints.")

  domain_tbl <- readr::read_csv(
    domain_path,
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character())
  )

  if (!"domain_source" %in% base::names(domain_tbl)) {
    domain_tbl <- domain_tbl |>
      dplyr::mutate(domain_source = NA_character_)
  }

  hint_tbl <- build_hpt_domain_hints(index_tbl) |>
    dplyr::rename(
      hinted_domain = .data$website_domain,
      hinted_source = .data$domain_source
    )

  updated_tbl <- domain_tbl |>
    dplyr::left_join(
      hint_tbl,
      by = "facility_id",
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      domain_blank = base::is.na(.data$website_domain) |
        !base::nzchar(.data$website_domain),
      website_domain = dplyr::if_else(
        .data$domain_blank,
        .data$hinted_domain,
        .data$website_domain
      ),
      domain_source = dplyr::if_else(
        .data$domain_blank,
        .data$hinted_source,
        dplyr::coalesce(
          .data$domain_source,
          "manual"
        )
      )
    ) |>
    dplyr::select(
      -dplyr::any_of(
        base::c(
          "hinted_domain",
          "hinted_source",
          "domain_blank"
        )
      )
    )

  readr::write_csv(updated_tbl, domain_path)

  auto_n <- base::sum(
    stringr::str_detect(
      updated_tbl$domain_source,
      "^historical_"
    ),
    na.rm = TRUE
  )

  base::message(
    "Historical index supplied ",
    scales::comma(auto_n),
    " domain hints."
  )
  base::message("Updated HPT domain file: ", domain_path)

  updated_tbl
}

#' Read a frozen HPT hospital sample from disk
#'
#' @param path Path to a frozen HPT sample CSV, e.g.
#'   `config/hpt_hospital_sample.csv`.
#' @return Tibble of the sample, all columns read as character, with at
#'   least `facility_id`, `facility_name`, `citytown`, `state`,
#'   `census_region`, and `ownership_group` columns. Errors if any of
#'   those required columns is missing.
#' @export
read_hpt_sample <- function(path) {
  base::message("Reading frozen HPT hospital sample: ", path)

  sample_tbl <- readr::read_csv(
    path,
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character())
  )

  required_cols <- base::c(
    "facility_id",
    "facility_name",
    "citytown",
    "state",
    "census_region",
    "ownership_group"
  )

  missing_cols <- base::setdiff(
    required_cols,
    base::names(sample_tbl)
  )

  if (base::length(missing_cols) > 0L) {
    base::stop(
      "Frozen HPT sample is missing: ",
      base::paste(missing_cols, collapse = ", ")
    )
  }

  sample_tbl
}

#' Load the frozen HPT sample, or create one if none exists
#'
#' If `config_dir/hpt_hospital_sample.csv` already exists and `resample`
#' is `FALSE`, reuses it via [read_hpt_sample()]; otherwise draws a new
#' sample via [sample_hpt_hospitals()]. Either way, (re-)writes the
#' sample files via [write_hpt_sample_files()] without overwriting an
#' existing domain file.
#'
#' @param hospital_tbl Tibble of hospitals to sample from if a new
#'   sample is needed, e.g. from [download_cms_hospital_frame()].
#' @param config_dir Directory the frozen sample and domain files live
#'   in (and are written to).
#' @param per_stratum Number of hospitals per stratum, passed to
#'   [sample_hpt_hospitals()] if resampling.
#' @param seed Seed passed to [sample_hpt_hospitals()] if resampling.
#' @param resample If `TRUE`, draw a new sample even if a frozen one
#'   already exists.
#' @return A list with `sample` (the sample tibble) and `files` (the
#'   list of written file paths from [write_hpt_sample_files()]).
#' @export
load_or_create_hpt_sample <- function(
    hospital_tbl,
    config_dir = "config",
    per_stratum = 10L,
    seed = 20260828L,
    resample = FALSE) {
  sample_path <- base::file.path(
    config_dir,
    "hpt_hospital_sample.csv"
  )

  if (base::file.exists(sample_path) && !resample) {
    base::message(
      "Reusing frozen HPT sample; no resampling requested."
    )
    sample_tbl <- read_hpt_sample(sample_path)
  } else {
    sample_tbl <- sample_hpt_hospitals(
      hospital_tbl,
      per_stratum = per_stratum,
      seed = seed
    )
  }

  sample_files <- write_hpt_sample_files(
    sample_tbl,
    config_dir = config_dir,
    overwrite_domains = FALSE
  )

  base::list(
    sample = sample_tbl,
    files = sample_files
  )
}
