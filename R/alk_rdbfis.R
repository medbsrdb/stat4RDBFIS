#' Age-Length Key estimation
#'
#' Create an age-length key (ALK) from sampling data and apply it to the
#' observed numbers-at-length in order to estimate numbers-at-age.
#'
#' The function works on the cleaned sampling template and follows the same
#' filtering logic used in the other stat4RDBFIS modules. Length classes are
#' grouped into bins of user-defined width (default 10 mm). The ALK is computed
#' as P(age | length class) and then applied to the corresponding
#' `number_at_length` values.
#'
#' @param sampling_df A data.frame containing the cleaned sampling template.
#' @param SP Optional character vector of species codes. Default is `NULL`.
#' @param GSA Optional character vector of GSA codes. Default is `NULL`.
#' @param YEAR Optional numeric vector of years. Default is `NULL`.
#' @param length_bin Numeric. Width of the length bin in mm. Default is `10`.
#' @param out_dir Character. Output directory where csv files and plots are
#'   written.
#'
#' @return A list with components:
#' - `alk_long`: ALK in long format.
#' - `alk_wide`: ALK in wide format.
#' - `naa_long`: Numbers-at-age by length bin.
#' - `naa`: Numbers-at-age in long format, aggregated by age.
#' - `naa_wide`: Numbers-at-age in wide format.
#' - `coverage_summary`: Coverage diagnostics for the ALK application.
#'
#' The function also writes these files to `out_dir`:
#' - `alk_long_*.csv`: ALK in long format.
#' - `alk_wide_*.csv`: ALK in wide format.
#' - `naa_long_*.csv`: Estimated numbers-at-age by length bin.
#' - `naa_wide_*.csv`: Estimated numbers-at-age in wide format.
#' - `coverage_summary_*.csv`: Coverage diagnostics.
#' - `alk_heatmap_*.png`: ALK heatmap.
#' - `alk_lfd_*.png`: Length-frequency plot used for ALK application.
#' @author stat4RDBFIS Core Team
#' @export
alk_rdbfis <- function(sampling_df,
                SP = NULL,
                GSA = NULL,
                YEAR = NULL,
                length_bin = 10,
                out_dir = getwd()) {
  
  required_cols <- c(
    "flag_country",
    "year",
    "area",
    "species",
    "sex",
    "length_class",
    "number_at_length",
    "age"
  )
  
  missing_cols <- setdiff(required_cols, names(sampling_df))
  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns in sampling_df: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  if (!is.numeric(length_bin) || length(length_bin) != 1 || is.na(length_bin) || length_bin <= 0) {
    stop("length_bin must be a single positive numeric value.")
  }
  
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  
  dat <- sampling_df |>
    dplyr::mutate(
      length_mm = suppressWarnings(as.numeric(.data$length_class)),
      length_bin_mm = floor(.data$length_mm / length_bin) * length_bin
    )
  
  dat <- dplyr::filter(dat, .data$sex %in% c('F', 'M'))
  
  if (!is.null(SP)) {
    dat <- dplyr::filter(dat, .data$species %in% SP)
  }
  if (!is.null(GSA)) {
    dat <- dplyr::filter(dat, .data$area %in% GSA)
  }
  if (!is.null(YEAR)) {
    dat <- dplyr::filter(dat, .data$year %in% YEAR)
  }
  
  if (nrow(dat) == 0) {
    stop("No rows remain after filtering.")
  }
  
  suffix_parts <- c()
  
  if (!is.null(GSA)) {
    suffix_parts <- c(suffix_parts, paste(GSA, collapse = "-"))
  }
  if (!is.null(YEAR)) {
    suffix_parts <- c(suffix_parts, paste(YEAR, collapse = "-"))
  }
  if (!is.null(SP)) {
    suffix_parts <- c(suffix_parts, paste(SP, collapse = "-"))
  }
  
  suffix <- if (length(suffix_parts) > 0) {
    paste0("_", paste(suffix_parts, collapse = "_"))
  } else {
    ""
  }
  
  aged_data <- dat |>
    dplyr::filter(!is.na(.data$age), !is.na(.data$length_bin_mm), !is.na(.data$number_at_length)) |>
    dplyr::mutate(age = as.integer(.data$age))
  
  if (nrow(aged_data) == 0) {
    stop("No aged fish found after filtering.")
  }
  
  alk_counts <- aged_data |>
    dplyr::group_by(
      .data$year, .data$area, .data$species, .data$sex,
      .data$length_bin_mm, .data$age
    ) |>
    dplyr::summarise(
      n_aged = sum(.data$number_at_length, na.rm = TRUE),
      .groups = "drop"
    )
  
  alk_totals <- alk_counts |>
    dplyr::group_by(
      .data$year, .data$area, .data$species, .data$sex,
      .data$length_bin_mm
    ) |>
    dplyr::summarise(
      n_aged_length = sum(.data$n_aged, na.rm = TRUE),
      .groups = "drop"
    )
  
  alk_long <- alk_counts |>
    dplyr::left_join(
      alk_totals,
      by = c("year", "area", "species", "sex", "length_bin_mm")
    ) |>
    dplyr::mutate(
      alk = dplyr::if_else(.data$n_aged_length > 0,
                           .data$n_aged / .data$n_aged_length,
                           NA_real_)
    ) |>
    dplyr::arrange(
      .data$year, .data$area, .data$species, .data$sex,
      .data$length_bin_mm, .data$age
    )
  
  alk_wide <- alk_long |>
    dplyr::select(
      .data$year, .data$area, .data$species, .data$sex,
      .data$length_bin_mm, .data$age, .data$alk
    ) |>
    dplyr::mutate(age = paste0("age_", .data$age)) |>
    tidyr::pivot_wider(
      names_from = .data$age,
      values_from = .data$alk,
      values_fill = 0
    ) |>
    dplyr::arrange(
      .data$year, .data$area, .data$species, .data$sex,
      .data$length_bin_mm
    )
  
  lfd <- dat |>
    dplyr::filter(!is.na(.data$length_bin_mm), !is.na(.data$number_at_length)) |>
    dplyr::group_by(
      .data$year, .data$area, .data$species, .data$sex,
      .data$length_bin_mm
    ) |>
    dplyr::summarise(
      n_length = sum(.data$number_at_length, na.rm = TRUE),
      .groups = "drop"
    )
  
  if (nrow(lfd) == 0) {
    stop("No length-frequency data found after filtering.")
  }
  
  naa_long <- lfd |>
    dplyr::left_join(
      alk_long |>
        dplyr::select(
          .data$year, .data$area, .data$species, .data$sex,
          .data$length_bin_mm, .data$age, .data$alk
        ),
      by = c("year", "area", "species", "sex", "length_bin_mm")
    ) |>
    dplyr::mutate(
      alk = tidyr::replace_na(.data$alk, 0),
      estimated_n_age_at_length = .data$n_length * .data$alk
    ) |>
    dplyr::arrange(
      .data$year, .data$area, .data$species, .data$sex,
      .data$length_bin_mm, .data$age
    )
  
  naa <- naa_long |>
    dplyr::filter(!is.na(.data$age)) |>
    dplyr::group_by(
      .data$year, .data$area, .data$species, .data$sex, .data$age
    ) |>
    dplyr::summarise(
      estimated_n_age = sum(.data$estimated_n_age_at_length, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(
      .data$year, .data$area, .data$species, .data$sex, .data$age
    )
  
  naa_wide <- naa |>
    dplyr::mutate(age = paste0("age_", .data$age)) |>
    tidyr::pivot_wider(
      names_from = .data$age,
      values_from = .data$estimated_n_age,
      values_fill = 0
    ) |>
    dplyr::arrange(.data$year, .data$area, .data$species, .data$sex)
  
  alk_coverage <- naa_long |>
    dplyr::group_by(
      .data$year, .data$area, .data$species, .data$sex,
      .data$length_bin_mm, .data$n_length
    ) |>
    dplyr::summarise(
      alk_available = any(!is.na(.data$age) & .data$alk > 0),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      uncovered_n_length = ifelse(.data$alk_available, 0, .data$n_length)
    )
  
  coverage_summary <- alk_coverage |>
    dplyr::group_by(.data$year, .data$area, .data$species, .data$sex) |>
    dplyr::summarise(
      total_n_length = sum(.data$n_length, na.rm = TRUE),
      uncovered_n_length = sum(.data$uncovered_n_length, na.rm = TRUE),
      coverage_prop = dplyr::if_else(
        .data$total_n_length > 0,
        1 - (.data$uncovered_n_length / .data$total_n_length),
        NA_real_
      ),
      .groups = "drop"
    )
  
  p_alk_heatmap <- ggplot2::ggplot(
    alk_long,
    ggplot2::aes(x = .data$length_bin_mm, y = .data$age, fill = .data$alk)
  ) +
    ggplot2::geom_tile() +
    ggplot2::facet_grid(year + area ~ species + sex, scales = "free_x") +
    ggplot2::labs(
      title = "Age-Length Key",
      x = "Length class (mm)",
      y = "Age",
      fill = "P(age|length)"
    ) +
    ggplot2::theme_bw()
  
  p_lfd <- ggplot2::ggplot(
    lfd,
    ggplot2::aes(x = .data$length_bin_mm, y = .data$n_length)
  ) +
    ggplot2::geom_col() +
    ggplot2::facet_grid(year + area ~ species + sex, scales = "free_x") +
    ggplot2::labs(
      title = "Length-frequency distributions used for ALK application",
      x = "Length class (mm)",
      y = "N"
    ) +
    ggplot2::theme_bw()
  
  utils::write.csv(
    alk_long,
    file.path(out_dir, paste0("alk_long", suffix, ".csv")),
    row.names = FALSE
  )
  utils::write.csv(
    alk_wide,
    file.path(out_dir, paste0("alk_wide", suffix, ".csv")),
    row.names = FALSE
  )
  utils::write.csv(
    naa_long,
    file.path(out_dir, paste0("naa_long", suffix, ".csv")),
    row.names = FALSE
  )
  utils::write.csv(
    naa_wide,
    file.path(out_dir, paste0("naa_wide", suffix, ".csv")),
    row.names = FALSE
  )
  utils::write.csv(
    coverage_summary,
    file.path(out_dir, paste0("coverage_summary", suffix, ".csv")),
    row.names = FALSE
  )
  
  ggplot2::ggsave(
    filename = file.path(out_dir, paste0("alk_heatmap", suffix, ".png")),
    plot = p_alk_heatmap,
    width = 12,
    height = 8,
    units = "in",
    dpi = 300
  )
  
  ggplot2::ggsave(
    filename = file.path(out_dir, paste0("alk_lfd", suffix, ".png")),
    plot = p_lfd,
    width = 12,
    height = 8,
    units = "in",
    dpi = 300
  )
  
  invisible(list(
    alk_long = alk_long,
    alk_wide = alk_wide,
    naa_long = naa_long,
    naa = naa,
    naa_wide = naa_wide,
    coverage_summary = coverage_summary
  ))
}
