# Procedure codebook and canonical claims schema
#
# Part of the empirical "evidence layer" (APCD/CMS/HPT/MEPS), which
# estimates strategy costs directly from claims and public reimbursement
# data rather than from the literature-sourced parameters in
# `config/model_parameters.csv`. See `docs/evidence_layers.md`.

#' HCPCS/CPT codebook mapping each billing code to its clinical concept
#' @export
sampling_codebook <- function() {
  base::message("Building procedure codebook.")

  tibble::tribble(
    ~concept, ~code,
    "emb", "58100",
    "dc", "58120",
    "hysteroscopy_sampling", "58558",
    "pathology", "88305",
    "anesthesia_colonoscopy", "00811",
    "anesthesia_colonoscopy", "00812",
    "anesthesia_gynecology", "00952",
    "colonoscopy", "45378",
    "colonoscopy", "45380",
    "colonoscopy", "45381",
    "colonoscopy", "45384",
    "colonoscopy", "45385"
  )
}

#' Look up the billing code(s) for one or more clinical concepts
#'
#' @param concept Character vector of concept names to match against
#'   [sampling_codebook()]'s `concept` column.
#' @return Character vector of unique matching HCPCS/CPT codes.
#' @export
sampling_code_vector <- function(concept) {
  sampling_codebook() |>
    dplyr::filter(.data$concept %in% .env$concept) |>
    dplyr::pull(.data$code) |>
    unique()
}
