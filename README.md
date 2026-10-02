# samevisit <img src="man/figures/logo.png" align="right" height="139" alt="samevisit hex logo: two overlapping circles on a navy hexagon" />

<!-- badges: start -->
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R >= 4.1.0](https://img.shields.io/badge/R-%3E%3D%204.1.0-blue.svg)](https://cran.r-project.org/)
[![GitHub last commit](https://img.shields.io/github/last-commit/mufflyt/samevisit.svg)](https://github.com/mufflyt/samevisit/commits/main)
<!-- badges: end -->

Decision-analytic cost modeling for comparing a standalone procedure against
the same procedure performed during an already-scheduled encounter ("same
visit") instead of as its own separate visit: cost components, escalation/
rescue probabilities, deterministic and probabilistic sensitivity analysis,
and threshold (root-finding) analysis.

The question this package exists to answer, in general form: *a patient is
already going to have procedure A. Procedure B could be done separately, or
folded into the same encounter as A. Given the real costs on both sides --
not just the obvious professional fee, but coordination overhead, added
anesthesia/OR time, and the probability and cost of each path's failure mode
-- which is actually cheaper?*

## Install

```r
# install.packages("remotes")
remotes::install_github("mufflyt/samevisit")
```

## Two layers: a worked application, and a generic API

This package was extracted from
[`mufflyt/emb_colonoscopy`](https://github.com/mufflyt/emb_colonoscopy), a
cost-minimization model comparing standalone office endometrial biopsy,
operative dilation and curettage (D&C), and endometrial biopsy performed
during an already-scheduled surveillance colonoscopy, for endometrial cancer
surveillance in Lynch syndrome. Most of `R/` is still that specific
application's functions and parameter names (`compute_strategy_costs()`,
`compute_office_emb_strategy_cost()`, `emb_failure_lynch`, `dnc_*`, etc.) --
useful directly if you're working on that exact clinical question, and
useful as a fully worked, heavily-documented example otherwise.

Underneath that, a **generic layer** lets a *different* same-visit-vs.-
separate-visit comparison plug in its own strategies without touching this
package's code or copying its files.

### The strategy-cost function contract

Every piece of the generic layer operates on a `strategy_costs` tibble
(`strategy`, `initial_cost`, `escalation_probability`, `escalation_cost`,
`expected_total_cost`) built from *strategy-cost functions you write*, one
per strategy. Each must return:

```r
list(
  components = tibble::tibble(strategy = "<name>", component = <chr>, amount = <dbl>),
  escalation_probability = <dbl scalar>,  # 0 if this strategy never escalates
  escalation_cost = <dbl scalar>,         # 0 if escalation_probability is 0
  initial_cost = <dbl scalar>,
  expected_total_cost = <dbl scalar>      # initial_cost + escalation_probability * escalation_cost
)
```

Your function's signature is `function(model_parameters, price_index_table,
reference_year)` for a strategy with no escalation target, or
`function(model_parameters, rescue_cost, price_index_table, reference_year)`
for a strategy that may escalate to a shared "rescue" strategy (this
package's own D&C arm, which both the office-biopsy and combined-biopsy
arms may escalate to on a failed sample, is the worked example --
see `R/strategy_costs.R`'s `compute_dnc_strategy_cost()`).

### A minimal worked example

A two-strategy comparison with no shared escalation target -- the shape a
standalone-vs.-combined-procedure comparison with no third "rescue" arm
needs:

```r
library(samevisit)

parameters <- tibble::tibble(
  parameter = c("standalone_fee", "combined_fee", "reference_dollar_year"),
  category = c("professional_fee", "professional_fee", "structural"),
  strategy = c("standalone", "combined", "structural"),
  description = c("standalone professional fee", "combined professional fee", "reference year"),
  base_value = c(500, 300, 2026),
  unit = c("USD_per_procedure", "USD_per_procedure", "year"),
  low_value = c(400, 200, NA), high_value = c(600, 400, NA),
  distribution = c("gamma", "gamma", "fixed"),
  dollar_year = c(2026, 2026, NA), source = c("example", "example", "example"),
  provisional = FALSE, notes = "",
  gamma_alpha = NA_real_, gamma_rate = NA_real_,
  evidence_tier = c("D", "D", "structural")
)

compute_standalone <- function(model_parameters, price_index_table, reference_year) {
  fee <- get_parameter_value(model_parameters, "standalone_fee")
  list(
    components = tibble::tibble(strategy = "standalone", component = "fee", amount = fee),
    escalation_probability = 0, escalation_cost = 0,
    initial_cost = fee, expected_total_cost = fee
  )
}
compute_combined <- function(model_parameters, price_index_table, reference_year) {
  fee <- get_parameter_value(model_parameters, "combined_fee")
  list(
    components = tibble::tibble(strategy = "combined", component = "fee", amount = fee),
    escalation_probability = 0, escalation_cost = 0,
    initial_cost = fee, expected_total_cost = fee
  )
}
my_strategy_costs <- function(model_parameters, price_index_table = NULL) {
  compute_multi_strategy_costs(
    strategy_fns = list(standalone = compute_standalone, combined = compute_combined),
    model_parameters = model_parameters, price_index_table = price_index_table,
    reference_year = 2026, rescue_strategy = NULL  # no shared escalation target
  )
}

result <- my_strategy_costs(parameters)
compare_two_strategies(result$strategy_costs, "combined", "standalone")
#> incremental_cost: -200 (combined is $200 cheaper)
```

From here, the rest of the generic layer plugs into the same two functions:

```r
# One-way sensitivity on any parameter, using your own strategy-cost function
run_one_way_sensitivity(
  parameters, parameter_names = c("standalone_fee", "combined_fee"),
  target_metric_fn = function(sc) compare_two_strategies(sc, "combined", "standalone")$incremental_cost,
  strategy_cost_fn = my_strategy_costs
)

# Monte Carlo probabilistic sensitivity analysis
run_probabilistic_sensitivity_generic(
  parameters, strategy_cost_fn = my_strategy_costs,
  metric_fns = list(
    incremental_cost = function(sc) compare_two_strategies(sc, "combined", "standalone")$incremental_cost
  ),
  n_simulations = 1000
)

# Threshold: how far can standalone_fee move before combined stops being cheaper?
find_parameter_threshold(
  parameters, parameter_name = "standalone_fee",
  target_metric_fn = function(sc) compare_two_strategies(sc, "combined", "standalone")$incremental_cost,
  strategy_cost_fn = my_strategy_costs,
  search_lower = 0, search_upper = 2000
)
```

If one of your strategies *is* a shared escalation target -- e.g. a failed
standalone attempt escalates to a third, more invasive procedure that a
combined attempt might also escalate to -- name it in
`compute_multi_strategy_costs()`'s `rescue_strategy` argument instead of
`NULL`; it's computed first and its `expected_total_cost` is passed as the
second positional argument to every other strategy function (matching
`compute_office_emb_strategy_cost()`'s signature in this package's own
Lynch-specific code).

### What's already generic, with zero wrapping needed

`load_model_parameters()`/`override_model_parameters()`/`get_parameter_value()`,
`adjust_for_inflation()`/`load_price_index_table()`, every `validate_*()`,
`compare_strategies_to_cheapest()` and `build_pairwise_comparison_table()`
(both operate on any `strategy`/`expected_total_cost` tibble already, no
hardcoded names), `theme_journal()`/`save_table()`, and the parameter-draw
machinery (`draw_parameter_sample()`/`sample_triangular()`/
`draw_parameter_set()`) all work as-is, regardless of your strategy names.

### What's still missing

- No generic equivalent of a secondary clinical-outcome or cost-effectiveness
  estimate (this package's own `compute_strategy_clinical_outcomes()`/
  `compute_diagnostic_yield()` and `run_probabilistic_sensitivity()` stay
  Lynch-specific -- too tightly coupled to this model's own clinical-outcome
  columns to generalize as a thin wrapper).
- No generic threshold-analysis convenience layer (`run_threshold_analyses()`
  and its 4 named wrappers in this package are this model's own questions) --
  compose your own directly from `find_parameter_threshold()` +
  `strategy_cost_fn`.
- This has only been verified against a synthetic toy model (above), not yet
  against a second project's actual parameter table and real strategy
  functions.

## Status

**Vignettes are not included here.** `emb_colonoscopy`'s vignettes
demonstrate this package's functions against that project's own
`config/model_parameters.csv` and `data/*.csv` fixtures, which live in that
repository, not this one -- see
[`emb_colonoscopy/vignettes/`](https://github.com/mufflyt/emb_colonoscopy/tree/main/vignettes)
and [`emb_colonoscopy/docs/vignettes/`](https://github.com/mufflyt/emb_colonoscopy/tree/main/docs/vignettes)
for runnable, documented examples, and
[`emb_colonoscopy/docs/samevisit_generic_api.md`](https://github.com/mufflyt/emb_colonoscopy/blob/main/docs/samevisit_generic_api.md)
for the fuller generic-API writeup this README summarizes.

**Not yet CRAN-ready.** No `R CMD check` NOTE/WARNING cleanup has been done
beyond what's needed for a clean local install (e.g. several functions write
to caller-specified relative paths rather than a package-safe location). See
`emb_colonoscopy`'s own `docs/reuse_mapping.md` for the fuller account of
what was and wasn't done in this extraction and in genericizing it.

## Where this came from

See [`mufflyt/emb_colonoscopy`](https://github.com/mufflyt/emb_colonoscopy)
for the worked application (Lynch syndrome endometrial surveillance), its
test suite, and its own copy of this package's source (kept in sync
manually, for now -- `emb_colonoscopy` does not yet depend on this repo as
an external package). The generic layer was added after checking it against
a second, unrelated same-visit cost model
([`mufflyt/iud_bariatric`](https://github.com/mufflyt/iud_bariatric),
standalone vs. bariatric-surgery-combined IUD insertion) that had
independently reimplemented near-duplicate versions of this package's files
-- see `emb_colonoscopy/docs/reuse_mapping.md` for that account.

## License

MIT
