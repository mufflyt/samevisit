#' Evidence-layer provenance table
#'
#' One row per data layer (CMS professional/facility, hospital price
#' transparency, MEPS, hospital cost reports), documenting source,
#' release, role, and implementation status -- the evidence-layer analog
#' of `config/model_parameters.csv`'s per-parameter provenance. An APCD
#' claims-linkage layer was designed and prototyped (with a working
#' episode-construction and rescue-linkage engine) but was dropped: it
#' requires an approved state APCD data use agreement this project does
#' not currently have, and the model does not need patient-linked claims
#' to answer its core question -- see docs/evidence_layers.md.
#' @export
evidence_layer_catalog <- function() {
  base::message("Building evidence-layer provenance table.")

  tibble::tribble(
    ~layer, ~source, ~release, ~role, ~status,
    "medicare_professional",
    "CMS Physician & Other Practitioners",
    "2024",
    "Professional allowed and payment benchmarks",
    "implemented",
    "medicare_facility",
    "CMS Outpatient Hospitals (OPPS)",
    "2023",
    "Hospital outpatient facility payment benchmark",
    "implemented",
    "commercial",
    "Hospital Price Transparency",
    "2026 schema",
    "Commercial negotiated and allowed amounts",
    "implemented, requires a real MRF manifest",
    "patient_societal",
    "MEPS",
    "2024",
    "Visit payment, out-of-pocket, and wage inputs",
    "implemented, requires local MEPS files",
    "hospital_cost_reports",
    "HCRIS (Healthcare Cost Report Information System)",
    "not yet selected",
    "Hospital cost-to-charge ratios for a hospital-cost-basis sensitivity analysis",
    "not implemented -- documented next step"
  )
}
