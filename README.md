# samevisit

Decision-analytic cost modeling for comparing a standalone procedure against
the same procedure performed during an already-scheduled encounter ("same
visit") instead of as its own separate visit: cost components, escalation/
rescue probabilities, deterministic and probabilistic sensitivity analysis,
and threshold analysis.

## Status

This package was extracted from
[`mufflyt/emb_colonoscopy`](https://github.com/mufflyt/emb_colonoscopy), a
cost-minimization model comparing standalone office endometrial biopsy,
operative dilation and curettage, and endometrial biopsy performed during an
already-scheduled surveillance colonoscopy, for endometrial cancer
surveillance in Lynch syndrome.

**Read this before using it for a new project.** The function and parameter
names in `R/` are still that specific application's names (`compute_strategy_costs()`,
`emb_failure_lynch`, `dnc_*`, etc.) -- generalizing the naming to match this
package's broader framing is a deliberate follow-up, not yet done. What *does*
generalize today is the underlying machinery: per-strategy cost-component
breakdowns, an escalation-to-a-more-invasive-strategy branch, deterministic
one-way sensitivity/tornado analysis, probabilistic sensitivity analysis
(Monte Carlo over gamma/beta/triangular parameter distributions), and
threshold (root-finding) analysis -- all driven by an external parameter
table rather than hardcoded values, so a new same-visit-vs.-separate-visit
comparison can plausibly be built by supplying a differently-shaped parameter
table and composing the same functions, without touching this package's code.

**Vignettes are not included here.** `emb_colonoscopy`'s vignettes
demonstrate this package's functions against that project's own
`config/model_parameters.csv` and `data/*.csv` fixtures, which live in that
repository, not this one -- see
[`emb_colonoscopy/vignettes/`](https://github.com/mufflyt/emb_colonoscopy/tree/main/vignettes)
and [`emb_colonoscopy/docs/vignettes/`](https://github.com/mufflyt/emb_colonoscopy/tree/main/docs/vignettes)
for runnable, documented examples.

**Not yet CRAN-ready.** No `R CMD check` NOTE/WARNING cleanup has been done
beyond what's needed for a clean local install (e.g. several functions write
to caller-specified relative paths rather than a package-safe location). See
`emb_colonoscopy`'s own `docs/reuse_mapping.md` for the fuller account of
what was and wasn't done in this first extraction.

## Install

```r
# install.packages("remotes")
remotes::install_github("mufflyt/samevisit")
```

## Where this came from

See [`mufflyt/emb_colonoscopy`](https://github.com/mufflyt/emb_colonoscopy)
for the worked application (Lynch syndrome endometrial surveillance), its
test suite, and its own copy of this package's source (kept in sync
manually, for now -- `emb_colonoscopy` does not yet depend on this repo as
an external package).

## License

MIT
