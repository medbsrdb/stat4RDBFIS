#' Discard ratio estimation
#'
#' Estimate discard ratios from cleaned RCG sampling data.
#'
#' This function is part of the stat4RDBFIS analytical workflow after quality
#' control and crosschecks. It uses the cleaned sampling template in memory and
#' estimates discard ratios by country, year, area, metier, species, and sampling
#' aggregation level.
#'
#' @param sampling_df data.frame. Clean sampling data already loaded in memory.
#' @param out_dir character. Output folder path (created if missing).
#' @param MS character vector or NULL. Subset by flag_country (ISO3).
#' @param GSA character vector or NULL. Subset by GSA code.
#' @param YEAR integer vector or NULL. Subset by year.
#' @param SP character vector or NULL. Subset by ASFIS species code.
#' @param GEAR character vector or NULL. Subset by gear code (first 3 chars of EU L6 gear).
#' @param R integer. Number of bootstrap replicates.
#' @param conf numeric. Confidence level for percentile bootstrap CI.
#' @param seed integer. Random seed for reproducibility.
#'
#' @return A data.frame with discard ratio estimates and bootstrap confidence
#'   intervals. The function also writes the same table to `out_dir`.
#'
#' @author stat4RDBFIS Core Team
#' @export discard_ratio_rdbfis
discard_ratio_rdbfis <- function(
    sampling_df,
    out_dir = file.path("Consistency_Checks", "discard_ratio"),
    MS = NULL,
    GSA = NULL,
    YEAR = NULL,
    SP = NULL,
    GEAR = NULL,
    R = 2000,
    conf = 0.95,
    seed = 123
) {
  on.exit(closeAllConnections(), add = TRUE)

  for (pkg in c("dplyr", "tidyr", "readr", "boot", "tibble")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(
        "Required package not installed: ", pkg,
        "\n  Install with: install.packages(\"", pkg, "\")"
      )
    }
  }

  `%>%`      <- dplyr::`%>%`
  filter     <- dplyr::filter
  mutate     <- dplyr::mutate
  distinct   <- dplyr::distinct
  pull       <- dplyr::pull
  transmute  <- dplyr::transmute
  group_by   <- dplyr::group_by
  summarise  <- dplyr::summarise
  ungroup    <- dplyr::ungroup
  arrange    <- dplyr::arrange
  bind_rows  <- dplyr::bind_rows
  n_distinct <- dplyr::n_distinct
  select     <- dplyr::select
  tibble     <- tibble::tibble

  cat("========================================\n")
  cat("discard_ratio_rdbfis: Discard Ratio\n")
  cat("========================================\n\n")

  if (missing(sampling_df) || is.null(sampling_df)) {
    stop("ERROR: 'sampling_df' is missing or NULL.")
  }

  if (!inherits(sampling_df, "data.frame")) {
    stop("ERROR: 'sampling_df' must be a data.frame already loaded in memory.")
  }

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  suffix_parts <- c()
  if (!is.null(MS))   suffix_parts <- c(suffix_parts, paste(MS, collapse = "-"))
  if (!is.null(GSA))  suffix_parts <- c(suffix_parts, paste(GSA, collapse = "-"))
  if (!is.null(YEAR)) suffix_parts <- c(suffix_parts, paste(YEAR, collapse = "-"))
  if (!is.null(SP))   suffix_parts <- c(suffix_parts, paste(SP, collapse = "-"))
  if (!is.null(GEAR)) suffix_parts <- c(suffix_parts, paste(GEAR, collapse = "-"))

  file_suffix <- if (length(suffix_parts) > 0) {
    paste0("_", paste(suffix_parts, collapse = "_"))
  } else {
    ""
  }

  cat("Loading and normalizing sampling data...\n")

  dat <- sampling_df
  names(dat) <- trimws(names(dat))

  required_cols <- c(
    "sampling_type", "flag_country", "year", "area", "species",
    "fishing_activity_category_eu_l6", "catch_category",
    "weight", "aggregation_level"
  )

  missing_cols <- setdiff(required_cols, names(dat))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  dat <- dat %>%
    mutate(
      flag_country = toupper(as.character(flag_country)),
      area = toupper(as.character(area)),
      species = toupper(as.character(species)),
      fishing_activity_category_eu_l6 = toupper(as.character(fishing_activity_category_eu_l6)),
      catch_category = as.character(catch_category),
      sampling_type = toupper(as.character(sampling_type)),
      aggregation_level = dplyr::case_when(
        aggregation_level %in% c(TRUE, "TRUE", "T", "t")   ~ "T",
        aggregation_level %in% c(FALSE, "FALSE", "F", "f") ~ "H",
        TRUE ~ toupper(as.character(aggregation_level))
      ),
      year = suppressWarnings(as.numeric(year)),
      weight = suppressWarnings(as.numeric(weight))
    )

  cat("  \u2713 Loaded:", nrow(dat), "records\n")

  if (!is.null(MS)) {
    before <- nrow(dat)
    dat <- dat %>% filter(flag_country %in% MS)
    cat("  \u2713 Country filter:", before, "\u2192", nrow(dat), "records\n")
  }

  if (!is.null(GSA)) {
    before <- nrow(dat)
    dat <- dat %>% filter(area %in% GSA)
    cat("  \u2713 Area filter:", before, "\u2192", nrow(dat), "records\n")
  }

  if (!is.null(YEAR)) {
    before <- nrow(dat)
    dat <- dat %>% filter(year %in% YEAR)
    cat("  \u2713 Year filter:", before, "\u2192", nrow(dat), "records\n")
  }

  if (!is.null(SP)) {
    before <- nrow(dat)
    dat <- dat %>% filter(species %in% SP)
    cat("  \u2713 Species filter:", before, "\u2192", nrow(dat), "records\n")
  }

  if (nrow(dat) == 0) {
    stop("ERROR: sampling dataset is empty after filtering.")
  }

  agg_levels <- dat %>%
    distinct(aggregation_level) %>%
    pull(aggregation_level)

  agg_levels <- agg_levels[!is.na(agg_levels)]

  if (length(agg_levels) == 0) {
    stop("No non-missing values found in aggregation_level.")
  }

  valid_levels <- c("T", "H")
  invalid_levels <- setdiff(agg_levels, valid_levels)

  if (length(invalid_levels) > 0) {
    stop(
      "Unsupported aggregation_level values found: ",
      paste(invalid_levels, collapse = ", "),
      ". Expected only 'T' (trip) or 'H' (haul)."
    )
  }

  if (length(agg_levels) > 1) {
    warning(
      "Multiple aggregation levels found: ",
      paste(agg_levels, collapse = ", "),
      ". They will be processed separately and combined."
    )
  }

  ensure_catch_cols <- function(df) {
    if (!"Lan" %in% names(df)) df$Lan <- 0
    if (!"Dis" %in% names(df)) df$Dis <- 0
    df
  }

  estimate_ratios <- function(df) {
    dis <- sum(df$Dis / 1000, na.rm = TRUE)
    lan <- sum(df$Lan / 1000, na.rm = TRUE)
    tot <- dis + lan

    ratio_dis_total <- if (tot > 0) dis / tot else NA_real_
    ratio_dis_lan   <- if (lan > 0) dis / lan else NA_real_

    tibble(
      discards = dis,
      landings = lan,
      total = tot,
      discard_ratio_total = ratio_dis_total,
      discard_ratio_landings = ratio_dis_lan
    )
  }

  boot_ratio_fun <- function(data, indices) {
    d <- data[indices, , drop = FALSE]

    dis <- sum(d$Dis, na.rm = TRUE)
    lan <- sum(d$Lan, na.rm = TRUE)
    tot <- dis + lan

    ratio_dis_total <- if (tot > 0) dis / tot else NA_real_
    ratio_dis_lan   <- if (lan > 0) dis / lan else NA_real_

    c(ratio_dis_total, ratio_dis_lan)
  }

  get_boot_ci <- function(boot_obj, index, conf = 0.95) {
    out <- tryCatch(
      boot::boot.ci(boot_obj, type = "perc", conf = conf, index = index),
      error = function(e) NULL
    )

    if (is.null(out) || is.null(out$percent)) {
      c(NA_real_, NA_real_)
    } else {
      out$percent[4:5]
    }
  }

  process_one_level <- function(dat_sub, agg_value) {
    psu_col <- if (agg_value == "T") {
      if (!"trip_code" %in% names(dat_sub)) {
        stop("aggregation_level = 'T' but trip_code column is missing.")
      }
      "trip_code"
    } else if (agg_value == "H") {
      if (!"station_code" %in% names(dat_sub)) {
        stop("aggregation_level = 'H' but station_code column is missing.")
      }
      "station_code"
    } else {
      stop("Unsupported aggregation_level: ", agg_value)
    }

    dat_filt <- dat_sub %>%
      mutate(
        gear = substr(fishing_activity_category_eu_l6, 1, 3),
        metier = fishing_activity_category_eu_l6
      ) %>%
      filter(
        catch_category %in% c("Lan", "Dis"),
        sampling_type %in% c("S")
      )

    if (!is.null(GEAR)) {
      dat_filt <- dat_filt %>% filter(gear %in% GEAR)
    }

    dat_filt <- dat_filt %>%
      transmute(
        flag_country,
        year,
        area,
        gear,
        species,
        psu = .data[[psu_col]],
        catch_category,
        weight,
        length_class,
        number_at_length
      )

    if (nrow(dat_filt) == 0) {
      return(tibble())
    }

    psu_level <- dat_filt %>%
      select(-number_at_length, -length_class) %>%
      distinct() %>%
      group_by(flag_country, year, area, metier, species, psu, catch_category) %>%
      summarise(weight = sum(weight, na.rm = TRUE), .groups = "drop") %>%
      tidyr::pivot_wider(
        names_from = catch_category,
        values_from = weight,
        values_fill = list(weight = 0)
      ) %>%
      ensure_catch_cols() %>%
      mutate(
        Lan = tidyr::replace_na(Lan, 0),
        Dis = tidyr::replace_na(Dis, 0),
        Total = Lan + Dis
      )

    if (nrow(psu_level) == 0) {
      return(tibble())
    }

    group_id <- interaction(
      psu_level$flag_country,
      psu_level$year,
      psu_level$area,
      psu_level$metier,
      psu_level$species,
      drop = TRUE,
      lex.order = TRUE
    )

    groups <- split(psu_level, group_id)

    results_list <- lapply(groups, function(df) {
      keys <- df[1, c("flag_country", "year", "area", "metier", "species"), drop = FALSE]
      n_psu <- n_distinct(df$psu)
      est <- estimate_ratios(df)

      if (n_psu < 2) {
        out_one <- cbind(
          data.frame(
            aggregation_level = agg_value,
            psu_type = ifelse(agg_value == "T", "trip_code", "station_code"),
            stringsAsFactors = FALSE
          ),
          keys,
          data.frame(
            n_psu = n_psu,
            discards = est$discards,
            landings = est$landings,
            total = est$total,
            discard_ratio_total = est$discard_ratio_total,
            discard_ratio_total_lwr = NA_real_,
            discard_ratio_total_upr = NA_real_,
            discard_ratio_landings = est$discard_ratio_landings,
            discard_ratio_landings_lwr = NA_real_,
            discard_ratio_landings_upr = NA_real_,
            stringsAsFactors = FALSE
          )
        )
        return(out_one)
      }

      set.seed(seed)
      b <- boot::boot(data = df, statistic = boot_ratio_fun, R = R)

      ci_total <- get_boot_ci(b, index = 1, conf = conf)
      ci_land  <- get_boot_ci(b, index = 2, conf = conf)

      out_one <- cbind(
        data.frame(
          aggregation_level = agg_value,
          psu_type = ifelse(agg_value == "T", "trip_code", "station_code"),
          stringsAsFactors = FALSE
        ),
        keys,
        data.frame(
          n_psu = n_psu,
          discards = est$discards,
          landings = est$landings,
          total = est$total,
          discard_ratio_total = est$discard_ratio_total,
          discard_ratio_total_lwr = ci_total[1],
          discard_ratio_total_upr = ci_total[2],
          discard_ratio_landings = est$discard_ratio_landings,
          discard_ratio_landings_lwr = ci_land[1],
          discard_ratio_landings_upr = ci_land[2],
          stringsAsFactors = FALSE
        )
      )

      out_one
    })

    bind_rows(results_list) %>%
      arrange(flag_country, year, area, metier, species)
  }

  out <- lapply(agg_levels, function(a) {
    dat_sub <- dat %>% filter(aggregation_level == a)
    process_one_level(dat_sub, a)
  }) %>%
    bind_rows() %>%
    arrange(flag_country, year, area, metier, species)

  output_file <- file.path(
    out_dir,
    paste0("discard_ratio_estimates", file_suffix, ".csv")
  )

  readr::write_csv(out, output_file)

  cat("\nOutput written to:\n")
  cat("  \u2713 ", output_file, "\n", sep = "")

  out
}
