#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' Tidy-eval data/env pronouns
#'
#' `.data`/`.env` are used throughout this package's dplyr pipelines
#' (e.g. `.data$strategy`) to resolve a bare column name against the
#' data mask rather than the calling environment -- see
#' `docs/appendix.md`'s "Bug 1: data-masking name collision" for the
#' real bug this pattern guards against. Imported here so every file
#' that uses them is covered by one `NAMESPACE` entry rather than each
#' file re-importing them.
#'
#' @importFrom rlang .data .env
#' @keywords internal
#' @name tidyeval-pronouns
NULL
