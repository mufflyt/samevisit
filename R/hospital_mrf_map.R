# Static map of hospitals with usable MRF-derived payer-rate data
#
# Aggregates the two real-hospital price-transparency MRF samples
# (`data/gyn_onc_hospital_payer_rates.csv`, 74 FREIDA gynecologic-oncology-
# fellowship hospitals; `data/colonoscopy_multi_hospital_rates.csv`, the
# original 6-hospital convenience sample) and plots one approximate point
# per hospital at its home city's real coordinates
# (`data/gyn_onc_hospital_cities.csv`). Those coordinates are Census Bureau
# Gazetteer Files internal-point coordinates for each hospital's home city
# (2024 Places file; NYC-borough hospitals fall back to the Gazetteer
# Counties file; La Jolla, CA -- an unincorporated San Diego neighborhood
# with no Census place of its own -- falls back to San Diego city), not a
# geocoded street address -- see that CSV's `coordinate_source` column for
# exactly which applies to each row, and `docs/mrf_hospital_data_overview.md`
# for the full hospital-level list. City-level precision means hospitals
# that share a city (e.g. four NYC hospitals) plot at the same point; the
# map jitters them with a fixed seed so every hospital is visible, and
# says so in its own subtitle.

#' Classify FREIDA-sample hospitals as full/partial/zero usable data
#'
#' A hospital-code row counts as "usable" if its `medicare_confidence` or
#' `commercial_confidence` is anything other than `not_found`,
#' `inconclusive`, or `not_applicable` -- the same rule used throughout
#' `docs/mrf_hospital_data_overview.md` and `docs/data_sources.md`.
#'
#' @param hospital_payer_rates Tibble from
#'   `readr::read_csv("data/gyn_onc_hospital_payer_rates.csv")`.
#' @return A tibble with one row per hospital: `hospital`, `state`,
#'   `codes_usable`, `codes_total`, `has_usable_data` (boolean).
#' @export
classify_freida_hospitals <- function(hospital_payer_rates) {
  not_usable <- c("not_found", "inconclusive", "not_applicable")

  hospital_payer_rates %>%
    dplyr::mutate(
      row_usable = !(.data$medicare_confidence %in% not_usable) |
        !(.data$commercial_confidence %in% not_usable)
    ) %>%
    dplyr::group_by(.data$hospital, .data$state) %>%
    dplyr::summarise(
      codes_usable = base::sum(.data$row_usable),
      codes_total = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(has_usable_data = .data$codes_usable > 0)
}

#' Build one row per hospital with usable MRF data, with approximate coordinates
#'
#' Unions the 74-hospital FREIDA sample's hospitals with any usable data
#' (full or partial) with the original 6-hospital convenience sample (all 6
#' have usable data), de-duplicating any hospital that appears in both
#' (UCHealth University of Colorado Hospital, Emory University Hospital,
#' and NYU Langone Hospitals (Tisch)), then joins each to its approximate
#' city-level coordinates from `data/gyn_onc_hospital_cities.csv`.
#'
#' @param hospital_payer_rates Tibble from
#'   `data/gyn_onc_hospital_payer_rates.csv`.
#' @param colonoscopy_hospital_rates Tibble from
#'   `data/colonoscopy_multi_hospital_rates.csv` (see
#'   [load_colonoscopy_hospital_rates_table()]).
#' @param hospital_cities Tibble from
#'   `data/gyn_onc_hospital_cities.csv` (`hospital`, `city`, `state`, `lat`,
#'   `lon`, `coordinate_source`). Defaults to reading that file.
#' @return A tibble with one row per hospital: `hospital`, `state`,
#'   `coverage` (`"full"` if every attempted code was usable, else
#'   `"partial"`), `lat`, `lon`, `coordinate_source`.
#' @export
build_hospital_mrf_points <- function(
  hospital_payer_rates,
  colonoscopy_hospital_rates,
  hospital_cities = readr::read_csv(
    "data/gyn_onc_hospital_cities.csv", show_col_types = FALSE
  )
) {
  freida_classified <- classify_freida_hospitals(hospital_payer_rates) %>%
    dplyr::filter(.data$has_usable_data) %>%
    dplyr::mutate(
      coverage = base::ifelse(.data$codes_usable == .data$codes_total, "full", "partial")
    ) %>%
    dplyr::select("hospital", "state", "coverage")

  colonoscopy_only <- colonoscopy_hospital_rates %>%
    dplyr::select(hospital = "hospital", state = "state_abbr") %>%
    dplyr::mutate(coverage = "full")

  dplyr::bind_rows(freida_classified, colonoscopy_only) %>%
    dplyr::distinct(.data$hospital, .data$state, .keep_all = TRUE) %>%
    dplyr::inner_join(
      hospital_cities %>% dplyr::select("hospital", "lat", "lon", "coordinate_source"),
      by = "hospital"
    )
}

#' Static jittered point map of hospitals with usable MRF-derived payer-rate data
#'
#' Needs the `maps` and `mapproj` packages installed -- `ggplot2::map_data()`
#' and `ggplot2::coord_map()` call into them internally, even though no
#' function from either is called directly in this file, which is why
#' they're imported explicitly here rather than left undeclared.
#'
#' @param hospital_points Tibble from [build_hospital_mrf_points()].
#' @return A `ggplot` object.
#' @importFrom maps map
#' @importFrom mapproj mapproject
#' @export
plot_hospital_mrf_map <- function(hospital_points) {
  us_states <- ggplot2::map_data("state")
  jitter_position <- ggplot2::position_jitter(width = 0.25, height = 0.25, seed = 1)

  ggplot2::ggplot() +
    ggplot2::geom_polygon(
      data = us_states,
      ggplot2::aes(x = .data$long, y = .data$lat, group = .data$group),
      fill = "grey93", colour = "white", linewidth = 0.2
    ) +
    ggplot2::geom_point(
      data = hospital_points,
      ggplot2::aes(x = .data$lon, y = .data$lat, colour = .data$coverage),
      position = jitter_position, size = 2.2, alpha = 0.8
    ) +
    ggplot2::scale_colour_manual(
      values = c(full = "#2c7fb8", partial = "#a1dab4"),
      labels = c(full = "Usable data for every code attempted", partial = "Usable data for some codes"),
      name = NULL
    ) +
    ggplot2::coord_map("albers", lat0 = 29.5, lat1 = 45.5, xlim = c(-125, -66), ylim = c(24, 50)) +
    ggplot2::labs(
      title = "Hospitals with usable price-transparency (MRF) payer-rate data",
      subtitle = stringr::str_wrap(
        base::paste0(
          base::nrow(hospital_points), " hospitals, plotted at their home city's",
          " approximate coordinates (Census Bureau Gazetteer Files, not a geocoded",
          " street address) and jittered slightly where multiple hospitals share",
          " a city -- see docs/mrf_hospital_data_overview.md for the full",
          " hospital-level list"
        ),
        width = 95
      )
    ) +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 11),
      plot.subtitle = ggplot2::element_text(size = 7.5, colour = "grey40", lineheight = 1.2),
      plot.title.position = "plot",
      legend.position = "bottom"
    )
}
