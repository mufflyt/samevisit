# Strategy cost functions
#
# Each endometrial-sampling strategy is modeled as a one-step decision
# tree: an initial attempt, which either succeeds, or fails and escalates
# to operative D&C. This mirrors how MD Anderson's combined-screening
# program actually behaves in practice (Nebgen et al. 2014 explicitly
# describe escalation to hysteroscopy/D&C after inadequate combined
# sampling), and it gives office-first and combined-first strategies a
# structurally identical -- and therefore directly comparable -- shape:
#
#   E(cost) = initial_cost + P(escalation) * E(cost_dnc)
#
# D&C is modeled as the deterministic reference arm with no escalation
# branch of its own (a simplifying assumption; see docs/methods_notes.md).
#
# The **incremental-cost principle** governs the combined-EMB arm: no
# colonoscopy base cost, GI professional fee, or baseline sedation cost
# is charged to this strategy, because the Lynch patient is assumed to be
# undergoing that colonoscopy regardless of whether EMB is added.
# `colonoscopy_anesthesia_episode_cost` is deliberately never referenced
# in `compute_combined_emb_strategy_cost()` -- see
# `tests/testthat/test-strategy-costs.R` for a unit test that enforces
# this.

#' Look up a base-case value and inflation-adjust it if needed
#'
#' @param model_parameters Tibble from [load_model_parameters()].
#' @param parameter_name Character scalar.
#' @param price_index_table Tibble from [load_price_index_table()].
#' @param reference_year Numeric scalar target year.
#' @return Numeric scalar, adjusted to `reference_year` dollars.
#' @export
get_adjusted_cost_parameter <- function(
  model_parameters,
  parameter_name,
  price_index_table,
  reference_year
) {
  matched_row <- model_parameters %>%
    dplyr::filter(.data$parameter == parameter_name)

  if (nrow(matched_row) != 1) {
    base::stop("Expected exactly one row for parameter '", parameter_name, "'.")
  }

  raw_value <- base::as.numeric(matched_row$base_value[[1]])
  source_year <- matched_row$dollar_year[[1]]

  adjust_for_inflation(
    cost_value = raw_value,
    source_year = source_year,
    reference_year = reference_year,
    price_index_table = price_index_table
  )
}

#' Compute the D&C (operative) strategy cost
#'
#' Recovery-room/PACU time is deliberately NOT a separate component here.
#' Per MedPAC's payment-basics documentation of the ASC payment system
#' (methodologically linked to OPPS): "Medicare pays for facility
#' services provided in ASCs -- such as nursing, recovery care,
#' anesthetics, drugs, and other supplies -- using a payment system that
#' is primarily linked to `OPPS`... Within each APC, CMS packages most
#' ancillary items and services with the primary service." Recovery-room
#' cost is therefore already inside `dnc_facility_or_asc_fee`; adding a
#' separate `dnc_recovery_room_cost` component would double-count it.
#' `dnc_recovery_room_cost` is kept in `config/model_parameters.csv` only
#' as a documented, explicitly-excluded reference value (same pattern as
#' `colonoscopy_anesthesia_episode_cost` for the combined arm) -- see
#' `tests/testthat/test-strategy-costs.R` for the regression test that
#' enforces this.
#'
#' @inheritParams get_adjusted_cost_parameter
#' @return A list with `components` (tibble) and `expected_total_cost`
#'   (numeric scalar; identical to `initial_cost` since D&C has no
#'   escalation branch in the base-case model).
#' @export
compute_dnc_strategy_cost <- function(
  model_parameters,
  price_index_table,
  reference_year
) {
  base::message("Computing D&C (operative) strategy cost.")

  # adverse_event_cost_partial_perforation_only: expected cost of managing a
  # uterine perforation, weighted by management-state probability, per
  # docs/ae_cost_evidence_table.md. This is a DELIBERATE PARTIAL/LOWER-BOUND
  # estimate: only the two management states with a fully-sourced cost
  # (observation, $0; diagnostic laparoscopy without conversion, professional
  # + OPPS facility fee) are included. Immediate laparotomy and
  # laparoscopy-converted-to-laparotomy (28.8% of perforations combined) are
  # excluded because CPT 49000 carries OPPS status indicator C (inpatient-only)
  # -- no OPPS/ASC facility rate exists for it, and pricing it would require
  # MS-DRG inpatient costing this repository has not built. The unspecified
  # 5.8% (Ben-Baruch/Menczer 1982's own abstract does not account for all 52
  # patients) is also excluded. Severe hemorrhage is not represented at all:
  # no management-pathway source was found for it. See
  # docs/ae_cost_evidence_table.md for the full accounting and what would
  # close each gap.
  perforation_probability <- get_parameter_value(
    model_parameters, "dnc_perforation_probability"
  )
  observation_fraction <- get_parameter_value(
    model_parameters, "dnc_perforation_management_observation_fraction"
  )
  laparoscopy_only_fraction <- get_parameter_value(
    model_parameters, "dnc_perforation_management_laparoscopy_only_fraction"
  )
  laparoscopy_professional_cost <- get_parameter_value(
    model_parameters, "dnc_perforation_laparoscopy_professional_cost"
  )
  laparoscopy_facility_cost <- get_parameter_value(
    model_parameters, "dnc_perforation_laparoscopy_facility_cost"
  )
  adverse_event_cost_partial <- perforation_probability * (
    observation_fraction * 0 +
      laparoscopy_only_fraction *
        (laparoscopy_professional_cost + laparoscopy_facility_cost)
  )

  components <- tibble::tibble(
    strategy = "dnc",
    component = c(
      "professional_fee",
      "pathology",
      "facility_fee",
      "preop_clinic_visit",
      "anesthesia",
      "adverse_event_cost_partial_perforation_only"
    ),
    amount = c(
      get_parameter_value(model_parameters, "dc_professional_cost"),
      get_parameter_value(model_parameters, "emb_pathology_cost"),
      get_parameter_value(model_parameters, "dnc_facility_or_asc_fee"),
      get_parameter_value(model_parameters, "dnc_preop_clinic_visit_cost"),
      get_parameter_value(model_parameters, "dnc_anesthesia_cost"),
      adverse_event_cost_partial
    )
  )

  initial_cost <- base::sum(components$amount)

  base::message("  D&C total cost: $", base::round(initial_cost, 2))

  list(
    components = components,
    escalation_probability = 0,
    escalation_cost = 0,
    initial_cost = initial_cost,
    expected_total_cost = initial_cost
  )
}

#' Compute the standalone office EMB strategy cost
#'
#' Disposable supplies (Pipelle curette, pelvic exam pack, tenaculum, etc.)
#' are deliberately NOT a separate component here. CMS's CY2026 Direct PE
#' Inputs file (CMS-1832-F) itemizes these exact supplies as inputs to CPT
#' 58100's *nonfacility* practice-expense RVU (`nf_quantity = 1` for every
#' item), which is exactly the setting `emb_office_professional_cost`
#' prices. Adding `emb_disposable_supply_cost` here would double-count
#' supplies already inside the professional fee. See
#' `tests/testthat/test-strategy-costs.R` for the regression test that
#' enforces this, and `docs/data_sources.md` for the itemized supply list.
#'
#' @inheritParams get_adjusted_cost_parameter
#' @param dnc_expected_cost Numeric scalar. Expected D&C cost, used as the
#'   rescue-procedure cost for failed office EMB attempts.
#' @return A list with `components`, `escalation_probability`,
#'   `escalation_cost`, `repeat_attempt_probability`, `repeat_visit_cost`,
#'   `initial_cost`, and `expected_total_cost`.
#' @export
compute_office_emb_strategy_cost <- function(
  model_parameters,
  dnc_expected_cost,
  price_index_table,
  reference_year
) {
  base::message("Computing office EMB (standalone) strategy cost.")

  components <- tibble::tibble(
    strategy = "office_emb",
    component = c(
      "office_visit",
      "professional_fee",
      "pathology"
    ),
    amount = c(
      get_parameter_value(model_parameters, "office_visit_em_cost"),
      get_parameter_value(model_parameters, "emb_office_professional_cost"),
      get_parameter_value(model_parameters, "emb_pathology_cost")
    )
  )

  initial_cost <- base::sum(components$amount)

  # emb_failure_lynch: pooled 13.3% (22/166) across three Lynch-specific,
  # confirmed genuinely Pipelle-specific surveillance studies -- Elmasry et
  # al. 2009 (Fam Cancer 8:431-439, PMID 19526324) 5/25, from Table 3's
  # "Pipelle done" column; Lecuru et al. 2008 (Int J Gynecol Cancer
  # 18:1326-1331, PMID 18217965) 12/116; Woolderink et al. 2020 (BMC Womens
  # Health 20:54, PMID 32183830, open access) 5/25, from Table 2. See
  # config/model_parameters.csv's own row and docs/data_sources.md's "Full
  # primary-source verification" section for the complete per-study
  # breakdown -- these are table-derived counts, not a single quotable
  # narrative sentence in any of the three papers.
  failure_probability <- get_parameter_value(model_parameters, "emb_failure_lynch")

  # office_repeat_attempt_fraction / office_repeat_attempt_success_probability:
  # reproduce Yi et al. 2018's (Gynecol Oncol 150:112-118, PMID 29747864)
  # actual decision-tree structure for a failed Pipelle attempt, in place of
  # the single-parameter "escalation fraction" this repository used before
  # 2026-09-02 (office_to_dnc_escalation_fraction, now superseded -- see
  # config/model_parameters.csv). Yi's tree: on failure, the physician either
  # moves directly to D&C (95%, office_repeat_attempt_fraction's complement)
  # or attempts a second sampling procedure (5%); a repeat attempt succeeds
  # 75% of the time (Kandil et al. 2014, PMID 25083966: of 1,120 insufficient
  # endometrial samples, 38% had a second sampling procedure by 12-month
  # follow-up, adequate in 75% of those -- REPLACED the original Adambekov
  # et al. 2017-based 25% figure 2026-09-02; see
  # config/model_parameters.csv's own row for the reconstructed-CI caveat)
  # or, per Yi's own text, "the physician will then move to the D&C route"
  # -- i.e. Yi's tree has NO branch where a failure is simply left
  # unresolved. This repository's clinical-outcome function
  # (compute_strategy_clinical_outcomes(), R/diagnostic_yield.R) mirrors that:
  # office_unresolved_probability is 0 under this structure, not a residual
  # PSA artifact of a single escalation-fraction parameter.
  repeat_attempt_fraction <- get_parameter_value(
    model_parameters, "office_repeat_attempt_fraction"
  )
  repeat_attempt_success_probability <- get_parameter_value(
    model_parameters, "office_repeat_attempt_success_probability"
  )

  repeat_attempt_probability <- failure_probability * repeat_attempt_fraction
  repeat_visit_cost <- repeat_attempt_probability * initial_cost

  # P(escalates to D&C) = P(no repeat attempted) + P(repeat attempted AND fails)
  #                      = failure_probability * (1 - repeat_attempt_fraction * repeat_attempt_success_probability)
  escalation_probability <- failure_probability *
    (1 - repeat_attempt_fraction * repeat_attempt_success_probability)
  escalation_cost <- escalation_probability * dnc_expected_cost

  expected_total_cost <- initial_cost + repeat_visit_cost + escalation_cost

  base::message(
    "  Office EMB initial cost: $", base::round(initial_cost, 2),
    "; repeat-attempt probability: ", base::round(repeat_attempt_probability, 4),
    "; escalation probability: ", base::round(escalation_probability, 4),
    "; expected total: $", base::round(expected_total_cost, 2)
  )

  list(
    components = components,
    escalation_probability = escalation_probability,
    escalation_cost = escalation_cost,
    repeat_attempt_probability = repeat_attempt_probability,
    repeat_visit_cost = repeat_visit_cost,
    initial_cost = initial_cost,
    expected_total_cost = expected_total_cost
  )
}

#' Compute the colonoscopy-combined EMB strategy cost
#'
#' Only costs incremental to an already-planned surveillance colonoscopy
#' are included (the incremental-cost principle). Room and anesthesia
#' minutes use the marginal/direct per-minute rates
#' (`direct_room_cost_per_minute`, `anesthesia_cost_per_minute`), not the
#' fully-loaded `procedure_room_cost_per_minute` scenario value.
#'
#' The incremental professional fee uses
#' `emb_office_professional_cost_facility`, NOT
#' `emb_office_professional_cost` -- the EMB portion of this arm is
#' performed in the facility/endoscopy-suite setting where the colonoscopy
#' itself takes place, not the physician's own office, and CMS's live PUF
#' data show a real ~38% gap between facility ($60.05) and nonfacility
#' ($97.03) allowed amounts for CPT 58100. `emb_disposable_supply_cost` IS
#' still charged here (unlike office_emb): CMS's Direct PE Inputs file
#' shows `f_quantity = 0` for every disposable-supply line item under the
#' facility-setting PE calculation for CPT 58100, i.e. those supplies are
#' not priced into the facility-rate professional fee, so they remain a
#' genuine incremental cost when EMB is added to the colonoscopy. See
#' `docs/data_sources.md` for the full verification chain.
#'
#' @inheritParams compute_office_emb_strategy_cost
#' @return A list with `components`, `escalation_probability`,
#'   `escalation_cost`, `initial_cost`, and `expected_total_cost`.
#' @export
compute_combined_emb_strategy_cost <- function(
  model_parameters,
  dnc_expected_cost,
  price_index_table,
  reference_year
) {
  base::message("Computing colonoscopy-combined EMB strategy cost.")

  # combined_emb_added_minutes: Huang et al. 2011 (PMC3014510), an MD
  # Anderson feasibility study, n=42. Quoted directly in Nebgen et al. 2014
  # (PMC4389779), which cites this same figure: "the EMBx added a median
  # time of 5 minutes (range, 1-12 minutes)."
  added_minutes <- get_parameter_value(model_parameters, "combined_emb_added_minutes")

  # direct_room_cost_per_minute / anesthesia_cost_per_minute: Childers CP,
  # Maggard-Gibbons M. Understanding Costs of Care in the Operating Room.
  # JAMA Surg 2014 -- a U.S. ambulatory-OR cost-accounting study (FY2014
  # dollars, inflation-adjusted below). Not colonoscopy-suite-specific
  # (flagged tier C in config/model_parameters.csv); used as a marginal/
  # direct per-minute cost proxy for this arm's incremental minutes.
  direct_room_cost_per_minute <- get_adjusted_cost_parameter(
    model_parameters, "direct_room_cost_per_minute", price_index_table, reference_year
  )
  anesthesia_cost_per_minute <- get_adjusted_cost_parameter(
    model_parameters, "anesthesia_cost_per_minute", price_index_table, reference_year
  )

  incremental_room_cost <- added_minutes * direct_room_cost_per_minute
  incremental_anesthesia_time_cost <- added_minutes * anesthesia_cost_per_minute

  components <- tibble::tibble(
    strategy = "combined_emb",
    component = c(
      "incremental_professional_fee",
      "pathology",
      "supplies",
      "incremental_room_time",
      "incremental_anesthesia_time",
      "incremental_anesthesia_drug",
      "coordination"
    ),
    amount = c(
      get_parameter_value(model_parameters, "emb_office_professional_cost_facility"),
      get_parameter_value(model_parameters, "emb_pathology_cost"),
      get_parameter_value(model_parameters, "emb_disposable_supply_cost"),
      incremental_room_cost,
      incremental_anesthesia_time_cost,
      # combined_emb_anesthesia_drug_increment_cost: base case $0, per
      # Nebgen et al. 2014 (PMC4389779): "No increases in anesthetic
      # medication dosing or change in type of sedation offered were
      # necessitated by the addition of the EMBx."
      get_parameter_value(
        model_parameters, "combined_emb_anesthesia_drug_increment_cost"
      ),
      get_parameter_value(model_parameters, "coordination_cost")
    )
  )

  requires_preop_visit <- get_parameter_value(
    model_parameters, "combined_requires_preop_office_visit", as_numeric = FALSE
  )
  if (isTRUE(requires_preop_visit)) {
    components <- dplyr::bind_rows(
      components,
      tibble::tibble(
        strategy = "combined_emb",
        component = "preop_office_visit",
        amount = get_parameter_value(model_parameters, "office_visit_em_cost")
      )
    )
  }

  initial_cost <- base::sum(components$amount)

  # combined_to_dnc_probability: 1.8% (2/111), a directly-observed
  # per-encounter escalation rate -- Nebgen et al. 2014 (PMC4389779).
  # CORRECTED 2026-08-31 from an earlier 3.6% (2/55), a denominator
  # mismatch: the paper's own text -- "Two women (3.6%) had cervical
  # stenosis and underwent hysteroscopy with dilation and curettage" --
  # sits among unambiguously patient-level percentages (age, race, parity,
  # all computed over its 55 patients), not its own 111-visit denominator
  # used elsewhere in the same paper for encounter-level rates ("EMBx...
  # detected endometrial cancer in 0.9% (1/111) of surveillance visits").
  # This function prices a single encounter, so 2/111 is the correct
  # denominator. See docs/data_sources.md's "CORRECTED:
  # combined_to_dnc_probability's denominator" section for the full audit.
  escalation_probability <- get_parameter_value(
    model_parameters, "combined_to_dnc_probability"
  )
  escalation_cost <- escalation_probability * dnc_expected_cost
  expected_total_cost <- initial_cost + escalation_cost

  base::message(
    "  Combined EMB initial cost: $", base::round(initial_cost, 2),
    " (", added_minutes, " incremental minutes); escalation probability: ",
    base::round(escalation_probability, 4),
    "; expected total: $", base::round(expected_total_cost, 2)
  )

  list(
    components = components,
    escalation_probability = escalation_probability,
    escalation_cost = escalation_cost,
    initial_cost = initial_cost,
    expected_total_cost = expected_total_cost
  )
}

#' Compute expected costs for an arbitrary named set of strategies
#'
#' The generic strategy-cost engine behind [compute_strategy_costs()].
#' Every function in `strategy_fns` must follow the strategy-cost
#' function contract: given `(model_parameters, price_index_table,
#' reference_year)` -- or, if that strategy is named in `rescue_strategy`'s
#' place as the *target* of another strategy's escalation,
#' `(model_parameters, rescue_cost, price_index_table, reference_year)`,
#' matching [compute_office_emb_strategy_cost()]'s and
#' [compute_combined_emb_strategy_cost()]'s existing signature -- it must
#' return `list(components, escalation_probability, escalation_cost,
#' initial_cost, expected_total_cost)`, where `components` is a tibble
#' with `strategy`/`component`/`amount` columns (see
#' [compute_dnc_strategy_cost()] for a worked example of the contract).
#'
#' @param strategy_fns Named list of strategy-cost functions following
#'   the contract above. Names become the `strategy` column's values.
#' @param model_parameters Tibble from [load_model_parameters()].
#' @param price_index_table Tibble from [load_price_index_table()].
#'   Defaults to loading `data/cpi_medical_care.csv`.
#' @param reference_year Numeric scalar. Defaults to the model's
#'   `reference_dollar_year` parameter.
#' @param rescue_strategy Character scalar naming one entry of
#'   `strategy_fns` that every *other* strategy may escalate to (e.g. a
#'   failed office attempt escalating to an operative rescue procedure).
#'   That entry is computed first, with no rescue cost of its own, and
#'   its `expected_total_cost` is passed as the second positional
#'   argument to every other strategy function. `NULL` (the default)
#'   means no strategy has a shared escalation target -- every strategy
#'   function is called with just `(model_parameters, price_index_table,
#'   reference_year)`, the shape a comparison with no third "rescue" arm
#'   needs.
#' @return A named list with `strategy_costs` (tibble, one row per
#'   strategy) and `cost_components` (long tibble, one row per
#'   strategy-component).
#' @export
compute_multi_strategy_costs <- function(
  strategy_fns,
  model_parameters,
  price_index_table = load_price_index_table(),
  reference_year = get_parameter_value(model_parameters, "reference_dollar_year"),
  rescue_strategy = NULL
) {
  base::message(
    "Computing strategy costs (reference year: ", reference_year, ")."
  )

  strategy_results <- base::list()

  if (!base::is.null(rescue_strategy)) {
    strategy_results[[rescue_strategy]] <- strategy_fns[[rescue_strategy]](
      model_parameters, price_index_table, reference_year
    )
    rescue_cost <- strategy_results[[rescue_strategy]]$expected_total_cost
  }

  other_names <- base::setdiff(base::names(strategy_fns), rescue_strategy)
  for (strategy_name in other_names) {
    strategy_results[[strategy_name]] <- if (base::is.null(rescue_strategy)) {
      strategy_fns[[strategy_name]](model_parameters, price_index_table, reference_year)
    } else {
      strategy_fns[[strategy_name]](
        model_parameters, rescue_cost, price_index_table, reference_year
      )
    }
  }
  # preserve strategy_fns' original ordering rather than rescue-then-others
  strategy_results <- strategy_results[base::names(strategy_fns)]

  strategy_costs <- tibble::tibble(
    strategy = base::names(strategy_results),
    initial_cost = base::unname(purrr::map_dbl(strategy_results, "initial_cost")),
    escalation_probability = base::unname(purrr::map_dbl(
      strategy_results, "escalation_probability"
    )),
    escalation_cost = base::unname(purrr::map_dbl(strategy_results, "escalation_cost")),
    expected_total_cost = base::unname(
      purrr::map_dbl(strategy_results, "expected_total_cost")
    )
  ) %>%
    dplyr::arrange(.data$expected_total_cost)

  cost_components <- dplyr::bind_rows(
    purrr::map(strategy_results, "components")
  )

  base::message("Strategy cost ranking (lowest to highest expected cost):")
  purrr::walk2(
    strategy_costs$strategy, strategy_costs$expected_total_cost,
    ~ base::message("  ", .x, ": $", base::round(.y, 2))
  )

  list(
    strategy_costs = strategy_costs,
    cost_components = cost_components
  )
}

#' Compute expected costs for all three strategies
#'
#' A thin, exact-output-preserving wrapper over
#' [compute_multi_strategy_costs()], orchestrating
#' [compute_dnc_strategy_cost()] (the rescue strategy every failed
#' sampling attempt may escalate to), [compute_office_emb_strategy_cost()],
#' and [compute_combined_emb_strategy_cost()].
#'
#' @param model_parameters Tibble from [load_model_parameters()].
#' @param price_index_table Tibble from [load_price_index_table()].
#'   Defaults to loading `data/cpi_medical_care.csv`.
#' @param reference_year Numeric scalar. Defaults to the model's
#'   `reference_dollar_year` parameter.
#' @return A named list with `strategy_costs` (tibble, one row per
#'   strategy) and `cost_components` (long tibble, one row per
#'   strategy-component).
#' @export
compute_strategy_costs <- function(
  model_parameters,
  price_index_table = load_price_index_table(),
  reference_year = get_parameter_value(model_parameters, "reference_dollar_year")
) {
  compute_multi_strategy_costs(
    strategy_fns = base::list(
      dnc = compute_dnc_strategy_cost,
      office_emb = compute_office_emb_strategy_cost,
      combined_emb = compute_combined_emb_strategy_cost
    ),
    model_parameters = model_parameters,
    price_index_table = price_index_table,
    reference_year = reference_year,
    rescue_strategy = "dnc"
  )
}
