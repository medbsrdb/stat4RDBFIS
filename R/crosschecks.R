#' Cross-Check Validation (CL vs CS) for LFD Raising - stat4RDBFIS
#'
#' Cross-validate clean landings and sampling data for raising readiness.
#' This function is designed as the third QC step in the stat4RDBFIS workflow.
#' It stops when no common strata are found, because raising would be impossible.
#'
#' @param sampling_df data.frame. Clean sampling data, typically
#'   \code{samplings_qc(...)\$sampling_df}.
#' @param landings_df data.frame. Clean landings data, typically
#'   \code{landings_qc(...)\$landings_df}.
#' @param out_dir character. Output directory where clean data, diagnostics,
#'   and charts are written.
#' @param validation_lists_path character or \code{NULL}. Path to
#'   \code{validation_lists.json}. When \code{NULL}, the bundled package copy
#'   is used.
#' @param MS character vector or NULL. Optional subset of \code{flag_country}
#'   ISO3 codes.
#' @param GSA character vector or NULL. Optional subset of GSA codes.
#' @param SP character vector or NULL. Optional subset of ASFIS species codes.
#' @param YEAR integer vector or NULL. Optional subset of years.
#'
#' @return A named list with elements \code{landings_clean},
#'   \code{sampling_clean}, \code{errors}, \code{warnings}, \code{summary},
#'   \code{output_dir}, and \code{output_suffix}. \code{sampling_clean} is
#'   \code{NULL} when no common strata are found.
#'
#' @details
#' Records flagged as \code{ERROR} are excluded from the clean output used for
#' raising. Records flagged as \code{WARNING} are retained and reported for
#' manual review. Trip-level comparisons involving landings use
#' \code{harbour + month + area + fishing_activity_category_eu_l6} as a proxy
#' trip identifier because the CL template does not provide \code{trip_code};
#' this may generate warnings that require analyst review when multiple vessels
#' share the same proxy combination.
#'
#' @seealso \code{\link{landings_qc}}, \code{\link{samplings_qc}}
#' @family qc functions
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' qc_cross <- crosschecks(
#'   sampling_df = qc_samp$sampling_df,
#'   landings_df = qc_land$landings_df,
#'   validation_lists_path = system.file(
#'     "extdata", "validation_lists.json", package = "stat4RDBFIS"
#'   )
#' )
#' }
#'
#' @author stat4RDBFIS Core Team
#' @export

crosschecks <- function(
    sampling_df,
    landings_df,
    out_dir               = file.path("Consistency_Checks", "qc_outputs_crosscheck"),
    validation_lists_path = NULL,
    MS      = NULL,
    GSA          = NULL,
    YEAR          = NULL,
    SP      = NULL
) {
  # crosschecks: Cross-Checks & Quality Control for LFD Raising (stat4RDBFIS)
  # Cross-validates RCG landings vs sampling data for LFD raising eligibility.
  # ERROR   -> record excluded from raising
  # WARNING -> record kept, flagged for manual review
  
  # DEPENDENCIES ----
  for (pkg in c("dplyr", "tidyr", "readr", "stringr", "lubridate", "jsonlite", "rlang")) {
    if (!requireNamespace(pkg, quietly = TRUE))
      stop("Required package not installed: ", pkg,
           "\n  Install with: install.packages(\"", pkg, "\")")
  }
  # Source-safe pipe bridge: keeps the tutorial workflow working when files are sourced.
  `%>%` <- dplyr::`%>%`
  raw_cat <- base::cat
  cat <- function(..., file = "", sep = " ", fill = FALSE, labels = NULL, append = FALSE) {
    parts <- lapply(list(...), as.character)
    msg <- paste(vapply(parts, paste, collapse = sep, FUN.VALUE = character(1)), collapse = sep)
    msg_clean <- msg
    msg_clean <- gsub("(\\[WARN\\])\\s+WARNING\\s*", "\\1 ", msg_clean)
    msg_clean <- gsub("(\\[ERROR\\])\\s+ERROR\\s*", "\\1 ", msg_clean)
    msg_clean <- gsub("(\\[OK\\])\\s+OK\\s*", "\\1 ", msg_clean)
    msg_trim <- trimws(gsub("[\r\n]+$", "", msg_clean))
    keep <- identical(msg, "\n") ||
      msg_trim == "" ||
      grepl("^(=+|crosschecks:|stat4RDBFIS QC Module - Activity 10 RDBFIS III|Loading validation lists\\.\\.\\.|Writing output files\\.\\.\\.|SECTION\\b|crosschecks Completed|[0-9]+\\.[0-9]+\\b)", msg_trim) ||
      grepl("^\\[OK\\]|^\\[WARN\\]|^\\[ERROR\\]", msg_trim) ||
      grepl("^\\s+\\[OK\\]|^\\s+\\[WARN\\]|^\\s+\\[ERROR\\]", msg_clean)
    drop <- grepl("\\.(csv|png)\\b", msg_trim, ignore.case = TRUE) ||
      grepl("SECTION 5: WRITE FINAL OUTPUTS|Landings loaded:|Sampling loaded:|Deriving quarter|These records will have|Filter countries:|Filter areas:|Filter years:|Filter species:|After filtering:|Output suffix|Sampling landings retained|Landings strata:|Sampling strata:|Total strata:|Likely causes:|These strata will be flagged again|Removed sampling records:|Outputs saved to|Raising diagnostics:|Details :|No trips matched|Verifying common strata|Sampling-only strata|Landings-only strata|Common strata \\(overlap\\):|Strata in sampling|Strata in landings|Rule: RF|RF > 50|coverage < 1%|< 3 distinct length classes|^--- INPUT ---|^--- OUTPUT ---|^--- QC RESULTS BY SECTION ---|Landings removed:|Sampling Lan rem:|Sampling Dis rem:|Total errors|Total warnings|No checks triggered|Section [0-9]", msg_trim)
    if (keep && !drop)
      raw_cat(msg_clean, file = file, sep = "", fill = fill, labels = labels, append = append)
  }
  
  # Cross-check tolerances (fixed per spec)
  weight_tolerance        <- 0.10
  pooled_n_tolerance      <- 1
  pooled_weight_tolerance <- 0.05
  
  # 0.2 Startup  ----
  cat("================================================================================\n")
  cat("crosschecks: Cross-Checks & Quality Control for LFD Raising\n")
  cat("stat4RDBFIS QC Module - Activity 10 RDBFIS III\n")
  cat("================================================================================\n\n")
  
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  raising_diag_dir <- file.path(out_dir, "raising_diagnostics")
  dir.create(raising_diag_dir, recursive = TRUE, showWarnings = FALSE)
  
  # 0.3 Optional validation lists  ----
  validation_lists_path <- stat4rdbfis_extdata_path(
    "validation_lists.json",
    validation_lists_path
  )
  cat("Loading validation lists...\n")
  VLISTS <- load_validation_lists(validation_lists_path)
  cat("  [OK] Validation lists loaded\n\n")
  
  # 0.4 Helper functions  ----
  
  
  print_section <- function(title) {
    cat("\n================================================================================\n")
    cat(title, "\n")
    cat("================================================================================\n\n")
  }
  
  # 0.5 Load datasets and add deterministic row IDs  ----
  print_section("SECTION 0: LOAD DATA & APPLY FILTERS")
  
  
  landings_path <- "<in-memory landings_df>"
  sampling_path <- "<in-memory sampling_df>"
  
  if (missing(landings_df) || is.null(landings_df)) stop("ERROR: landings_df is missing or NULL")
  if (missing(sampling_df) || is.null(sampling_df)) stop("ERROR: sampling_df is missing or NULL")
  
  # NOTE: The tutorial loads data with proper types in SECTION 0.
  # For cross-check computations we MUST keep numeric fields numeric.
  .to_num <- function(x) {
    if (is.numeric(x)) return(x)
    x <- as.character(x)
    x <- gsub("\\s+", "", x)
    # allow both comma and dot decimals; drop thousand separators and stray characters
    x <- gsub(",", ".", x, fixed = TRUE)
    suppressWarnings(as.numeric(gsub("[^0-9.+-]", "", x)))
  }

  landings <- landings_df %>%
    dplyr::mutate(.row_id = dplyr::row_number()) %>%
    dplyr::mutate(
      official_landings_weight = .to_num(.data$official_landings_weight),
      official_landings_value  = .to_num(.data$official_landings_value)
    )
  
  sampling <- sampling_df %>%
    dplyr::mutate(.row_id = dplyr::row_number()) %>%
    dplyr::mutate(
      individual_weight = .to_num(.data$individual_weight),
      subsample_weight  = .to_num(.data$subsample_weight),
      weight            = .to_num(.data$weight)
    )
  
  # Ensure year is numeric for filtering (landings_df from QC may have year as character)
  landings <- landings %>% dplyr::mutate(year = suppressWarnings(as.numeric(year)))
  sampling <- sampling %>% dplyr::mutate(year = suppressWarnings(as.numeric(year)))
  
  n_landings_original <- nrow(landings)
  n_sampling_original <- nrow(sampling)
  
  cat("Landings loaded:", formatC(n_landings_original, format="d", big.mark=","), "records\n")
  cat("Sampling loaded:", formatC(n_sampling_original, format="d", big.mark=","), "records\n\n")
  
  # 0.6 Derive temporal fields in sampling  ----
  cat("Deriving quarter and month from sampling date...\n")
  
  # Check if date column exists
  if (!"date" %in% names(sampling)) {
    stop("ERROR: Column 'date' not found in sampling data.\n",
         "  Verify that you are loading the correct file: ", sampling_path, "\n",
         "  The sampling file should be the OUTPUT of Script 2 (sampling_clean_for_qc.csv)")
  }
  
  sampling <- sampling %>%
    dplyr::mutate(
      date_parsed = lubridate::parse_date_time(!!rlang::sym("date"), 
                                               orders = c("dmy", "ymd", "dmy HMS", "ymd HMS"), 
                                               quiet = TRUE),
      quarter_from_date = lubridate::quarter(date_parsed),
      month_from_date = lubridate::month(date_parsed)
    )
  
  # Check for unparsed dates
  bad_dates <- sampling %>% dplyr::filter(is.na(date_parsed) & !is.na(!!rlang::sym("date")) & !!rlang::sym("date") != "")
  if (nrow(bad_dates) > 0) {
    pct_bad <- round(100 * nrow(bad_dates) / nrow(sampling), 1)
    cat("  [WARN] Check 0.6:", nrow(bad_dates), "sampling records with non-parsable date\n")
  }
  
  cat("  [OK] Done\n\n")
  
  # 0.7 Apply optional filters  ----
  filters_applied <- FALSE
  
  if (!is.null(MS)) {
    landings <- landings %>% dplyr::filter(flag_country %in% MS)
    sampling <- sampling %>% dplyr::filter(flag_country %in% MS)
    cat("  Filter countries:", paste(MS, collapse = ", "), "\n")
    filters_applied <- TRUE
  }
  
  if (!is.null(GSA)) {
    landings <- landings %>% dplyr::filter(area %in% GSA)
    sampling <- sampling %>% dplyr::filter(area %in% GSA)
    cat("  Filter areas:", paste(GSA, collapse = ", "), "\n")
    filters_applied <- TRUE
  }
  
  if (!is.null(YEAR)) {
    landings <- landings %>% dplyr::filter(year %in% YEAR)
    sampling <- sampling %>% dplyr::filter(year %in% YEAR)
    cat("  Filter years:", paste(YEAR, collapse = ", "), "\n")
    filters_applied <- TRUE
  }
  if (!is.null(SP)) {
    landings <- landings %>% dplyr::filter(species %in% SP)
    sampling <- sampling %>% dplyr::filter(species %in% SP)
    cat("  Filter species:", paste(SP, collapse = ", "), "\n")
    filters_applied <- TRUE
  }
  
  if (filters_applied) {
    cat("\n  After filtering:\n")
    cat("    Landings:", formatC(nrow(landings), format="d", big.mark=","), "records\n")
    cat("    Sampling:", formatC(nrow(sampling), format="d", big.mark=","), "records\n\n")
  }
  
  # 0.8 Output suffix  ----
  suffix_parts <- c()
  if (!is.null(MS))   suffix_parts <- c(suffix_parts, paste0("MS-",   paste(MS,   collapse = "-")))
  if (!is.null(GSA))  suffix_parts <- c(suffix_parts, paste0("GSA-",  paste(GSA,  collapse = "-")))
  if (!is.null(YEAR)) suffix_parts <- c(suffix_parts, paste0("YEAR-", paste(YEAR, collapse = "-")))
  if (!is.null(SP))   suffix_parts <- c(suffix_parts, paste0("SP-",   paste(SP,   collapse = "-")))

  file_suffix <- if (length(suffix_parts) > 0) paste0("_", paste(suffix_parts, collapse = "_")) else ""
  if (file_suffix != "") cat("Output suffix:", file_suffix, "\n\n")

  obsolete_crosscheck_chart <- file.path(raising_diag_dir, paste0("qc_summary_chart", file_suffix, ".png"))
  if (file.exists(obsolete_crosscheck_chart)) file.remove(obsolete_crosscheck_chart)
  obsolete_strata_diag <- file.path(out_dir, paste0("crosscheck_strata_diagnostics", file_suffix, ".csv"))
  if (file.exists(obsolete_strata_diag)) file.remove(obsolete_strata_diag)
  strata_diag_file <- file.path(raising_diag_dir, paste0("crosscheck_strata_diagnostics", file_suffix, ".csv"))

  apply_crosscheck_scope <- function(log_tbl) {
    if (nrow(log_tbl) == 0) {
      log_tbl$scope <- character()
      return(log_tbl)
    }
    log_tbl %>%
      dplyr::mutate(
          scope = dplyr::coalesce(
            scope,
            dplyr::case_when(
            check_id %in% c("2.3", "2.8", "2.12", "3.3", "3.8", "3.12") ~ "haul",
            check_id %in% c("2.5", "2.11", "3.5", "3.11", "3.13", "4.7") ~ "trip",
            check_id %in% c("4.0", "4.1", "4.2", "4.3", "4.4", "4.5", "4.6", "4.8", "4.9", "4.10") ~ "stratum",
            TRUE ~ "record"
          )
        )
      )
  }
  
  if (nrow(landings) == 0) stop("ERROR: landings dataset is empty after filtering.")
  if (nrow(sampling) == 0) stop("ERROR: sampling dataset is empty after filtering.")
  
  # SECTION 1: INTERNAL LANDINGS CHECKS ----
  
  print_section("SECTION 1: INTERNAL LANDINGS CHECKS")
  
  check_log    <- dplyr::tibble()
  all_errors   <- dplyr::tibble()
  all_warnings <- dplyr::tibble()
  format_qc_issue_output <- function(df) {
    if (!"original_row_id" %in% names(df)) {
      if (".row_id" %in% names(df)) {
        df <- dplyr::rename(df, original_row_id = .row_id)
      } else {
        df <- dplyr::mutate(df, original_row_id = NA_integer_)
      }
    }
    dplyr::relocate(df, original_row_id)
  }
  n_landings_removed <- 0L
  n_sampling_landings_removed <- 0L
  n_sampling_discards_removed <- 0L
  
  # 1.1 Quarter correction from month (landings)  ----
  # Per RCG spec: 'month' is an OPTIONAL field in the CL template.
  # Rule: if month is present and falls outside 1-12 -> ERROR (invalid month).
  #       if month is present and valid -> derive expected_quarter and compare.
  #       if month is absent (NA) -> keep the declared quarter unchanged (no error).
  cat("1.1 Quarter correction from month (landings)\n")

  landings <- landings %>%
    dplyr::mutate(
      expected_quarter = dplyr::case_when(
        month %in% 1:3  ~ 1L,
        month %in% 4:6  ~ 2L,
        month %in% 7:9  ~ 3L,
        month %in% 10:12 ~ 4L,
        TRUE            ~ NA_integer_   # covers both NA month and out-of-range values
      ),
      quarter_declared = if ("quarter" %in% names(landings)) as.integer(quarter) else NA_integer_
    )

  # Only flag records where month IS present (not NA) but is not valid (1-12)
  invalid_month_landings <- landings %>%
    dplyr::filter(!is.na(month) & is.na(expected_quarter))

  if (nrow(invalid_month_landings) > 0) {
    cat("  [ERROR] ERROR:", nrow(invalid_month_landings),
        "records with an out-of-range month value (must be 1-12 when present)\n")
    check_log <- dplyr::bind_rows(check_log,
      log_check(invalid_month_landings, "1.1", "ERROR",
                "Invalid month value in landings (out of range 1-12); quarter cannot be derived"))
    all_errors <- dplyr::bind_rows(all_errors,
      invalid_month_landings %>%
        dplyr::mutate(check_id = "1.1", severity = "ERROR") %>%
        dplyr::select(-dplyr::any_of(c(".row_id", "quarter_declared", "expected_quarter"))))
    landings <- landings %>% dplyr::filter(!(.row_id %in% invalid_month_landings$.row_id))
    n_landings_removed <- n_landings_removed + nrow(invalid_month_landings)
  } else {
    cat("  [OK] month valid (or absent - field is optional)\n")
  }

  # When month is present and valid, check whether the declared quarter matches
  invalid_quarter_landings <- landings %>%
    dplyr::filter(!is.na(expected_quarter) &
           !is.na(quarter_declared) &
           quarter_declared != expected_quarter)

  if (nrow(invalid_quarter_landings) > 0) {
    cat("  [WARN] WARNING:", nrow(invalid_quarter_landings),
        "records with quarter-month mismatch; quarter corrected from month\n")
    check_log <- dplyr::bind_rows(check_log,
      log_check(invalid_quarter_landings, "1.2", "WARNING",
                "Landings quarter corrected from month (declared quarter did not match month)"))
    all_warnings <- dplyr::bind_rows(all_warnings,
      invalid_quarter_landings %>%
        dplyr::mutate(check_id = "1.2", severity = "WARNING") %>%
        dplyr::select(-dplyr::any_of(c(".row_id", "quarter_declared", "expected_quarter"))))
  } else {
    cat("  [OK] no quarter-month mismatch\n")
  }

  # When month is present and valid -> use expected_quarter.
  # When month is absent (NA) -> preserve the original declared quarter.
  landings <- landings %>%
    dplyr::mutate(quarter = dplyr::if_else(
      !is.na(expected_quarter), expected_quarter, quarter_declared
    )) %>%
    dplyr::select(-expected_quarter, -quarter_declared)
  cat("  Landings remaining:", formatC(nrow(landings), format = "d", big.mark = ","), "\n\n")
  
  # 1.3 Weight and value validation (ERROR)  ----
  cat("1.3 Weight and value validation (landings)\n")
  
  invalid_weight_value_landings <- landings %>%
    dplyr::filter(is.na(official_landings_weight) | official_landings_weight <= 0 |
             is.na(official_landings_value) | official_landings_value < 0)
  
  if (nrow(invalid_weight_value_landings) > 0) {
    cat("  [ERROR] Check 1.3:", nrow(invalid_weight_value_landings),
        "landings records with invalid weight/value\n")
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_weight_value_landings, "1.3", "ERROR", "Invalid landings weight or value"))
    all_errors <- dplyr::bind_rows(all_errors, invalid_weight_value_landings %>% dplyr::mutate(check_id = "1.3", severity = "ERROR") %>% dplyr::select(-dplyr::any_of(c(".row_id", "quarter_declared", "expected_quarter"))))
    landings <- landings %>% dplyr::filter(!(.row_id %in% invalid_weight_value_landings$.row_id))
    n_landings_removed <- n_landings_removed + nrow(invalid_weight_value_landings)
  } else {
    cat("  [OK] No issues detected\n")
  }
  cat("  Landings remaining:", formatC(nrow(landings), format="d", big.mark=","), "\n\n")
  
  # 1.4 Exact duplicates (ERROR)  ----
  cat("1.4 Exact duplicates (landings)\n")
  
  dup_cols <- c("flag_country", "year", "quarter", "month", "area", "species",
                "fishing_activity_category_national", "fishing_activity_category_eu_l6", "harbour")
  
  landings_dup <- landings %>%
    dplyr::group_by(dplyr::across(all_of(dup_cols))) %>%
    dplyr::mutate(n_same = dplyr::n(), same_wt = dplyr::n_distinct(official_landings_weight) == 1,
           same_val = dplyr::n_distinct(official_landings_value) == 1) %>%
    dplyr::ungroup()
  
  invalid_duplicates_landings <- landings_dup %>%
    dplyr::filter(n_same > 1 & same_wt & same_val) %>%
    dplyr::group_by(dplyr::across(all_of(dup_cols))) %>%
    dplyr::slice(-1) %>%
    dplyr::ungroup()
  
  if (nrow(invalid_duplicates_landings) > 0) {
    cat("  [ERROR] Check 1.4:", nrow(invalid_duplicates_landings),
        "duplicate landings records\n")
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_duplicates_landings, "1.4", "ERROR", "Exact duplicate landings records"))
    all_errors <- dplyr::bind_rows(all_errors, invalid_duplicates_landings %>% dplyr::mutate(check_id = "1.4", severity = "ERROR") %>% dplyr::select(-dplyr::any_of(c(".row_id", "quarter_declared", "expected_quarter"))))
    landings <- landings %>% dplyr::filter(!(.row_id %in% invalid_duplicates_landings$.row_id))
    n_landings_removed <- n_landings_removed + nrow(invalid_duplicates_landings)
  } else {
    cat("  [OK] No issues detected\n")
  }
  
  cat("  Landings remaining:", formatC(nrow(landings), format="d", big.mark=","), "\n\n")
  
  # 1.5 Harbour missing (WARNING)  ----
  cat("1.5 Missing harbour (landings)\n")
  
  if (!("harbour" %in% names(landings))) {
    cat("  [OK] Check 1.5: harbour check skipped\n")
  } else {
    invalid_harbour_landings <- landings %>%
      dplyr::filter(is.na(harbour) | trimws(as.character(harbour)) == "")
    if (nrow(invalid_harbour_landings) > 0) {
      cat("  [WARN] Check 1.5:", nrow(invalid_harbour_landings),
          "landings records with missing harbour\n")
      check_log <- dplyr::bind_rows(check_log, log_check(invalid_harbour_landings, "1.5", "WARNING", "Missing harbour (landings)"))
      all_warnings <- dplyr::bind_rows(all_warnings, invalid_harbour_landings %>%
                                  dplyr::mutate(check_id = "1.5", severity = "WARNING") %>%
                                  dplyr::select(-dplyr::any_of(c(".row_id", "quarter_declared", "expected_quarter"))))
    } else {
      cat("  [OK] No issues detected\n")
    }
  }
  cat("  Landings remaining:", formatC(nrow(landings), format="d", big.mark=","), "\n\n")
  
  # SECTION 2: INTERNAL SAMPLING LANDINGS CHECKS (Lan) ----

  print_section('SECTION 2: INTERNAL SAMPLING LANDINGS CHECKS (catch_category = "Lan")')

  sampling_landings <- sampling %>% dplyr::filter(catch_category == "Lan")
  cat("Sampling landings:", formatC(nrow(sampling_landings), format="d", big.mark=","), "records\n\n")

  # 2.1 NA in critical LFD fields (ERROR)  ----
  cat("2.1 NA in critical LFD fields (sampling Lan)\n")

  invalid_lfd_fields_lan <- sampling_landings %>%
    dplyr::filter(is.na(length_class) | is.na(length_code) |
             is.na(number_at_length) | number_at_length <= 0)

  if (nrow(invalid_lfd_fields_lan) > 0) {
    cat("  [ERROR] Check 2.1:", nrow(invalid_lfd_fields_lan),
        "sampling Lan records missing critical LFD fields\n")
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_lfd_fields_lan, "2.1", "ERROR", "NA in critical LFD fields (sampling Lan)"))
    all_errors <- dplyr::bind_rows(all_errors, invalid_lfd_fields_lan %>%
                              dplyr::mutate(check_id = "2.1", severity = "ERROR") %>%
                              dplyr::select(-dplyr::any_of(c(".row_id", "quarter_declared", "expected_quarter"))))
    sampling_landings <- sampling_landings %>% dplyr::filter(!(.row_id %in% invalid_lfd_fields_lan$.row_id))
    n_sampling_landings_removed <- n_sampling_landings_removed + nrow(invalid_lfd_fields_lan)
  } else {
    cat("  [OK] No issues detected\n")
  }
  cat("  Sampling landings remaining:", formatC(nrow(sampling_landings), format="d", big.mark=","), "\n\n")

  # 2.2 Registration flags (ERROR)  ----
  cat("2.2 Registration flags consistency (sampling Lan)\n")

  invalid_registration_lan <- sampling_landings %>%
    dplyr::filter(catch_registration == "Non" & species_registration != "Non")

  if (nrow(invalid_registration_lan) > 0) {
    cat("  [ERROR] Check 2.2:", nrow(invalid_registration_lan),
        "sampling Lan records with inconsistent registration flags\n")
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_registration_lan, "2.2", "ERROR", "Registration flag violation (sampling Lan)"))
    all_errors <- dplyr::bind_rows(all_errors, invalid_registration_lan %>%
                              dplyr::mutate(check_id = "2.2", severity = "ERROR") %>%
                              dplyr::select(-dplyr::any_of(c(".row_id", "quarter_declared", "expected_quarter"))))
    sampling_landings <- sampling_landings %>% dplyr::filter(!(.row_id %in% invalid_registration_lan$.row_id))
    n_sampling_landings_removed <- n_sampling_landings_removed + nrow(invalid_registration_lan)
  } else {
    cat("  [OK] No issues detected\n")
  }
  cat("  Sampling landings remaining:", formatC(nrow(sampling_landings), format="d", big.mark=","), "\n\n")

  # 2.3 Individual vs Subsample weight consistency (haul-level, WARNING)  ----
  cat("2.3 Individual vs subsample weight consistency (haul-level, sampling Lan)\n")

  haul_key <- c("flag_country", "year", "trip_code", "station_code", "area",
                "fishing_activity_category_eu_l6", "species", "catch_category")

  invalid_weight_sample_lan <- sampling_landings %>%
    dplyr::filter(!is.na(individual_weight) | !is.na(subsample_weight)) %>%
    dplyr::group_by(dplyr::across(all_of(haul_key))) %>%
    dplyr::summarise(
      total_individual_weight = sum(individual_weight, na.rm = TRUE),
      total_subsample_weight = collapse_repeated_weight(subsample_weight),
      has_individual = any(!is.na(individual_weight) & individual_weight > 0),
      has_subsample = any(!is.na(subsample_weight) & subsample_weight > 0),
      .groups = "drop"
    ) %>%
    dplyr::mutate(total_subsample_weight = ifelse(is.na(total_subsample_weight), 0, total_subsample_weight)) %>%
    dplyr::filter(has_individual & has_subsample & total_subsample_weight > 0) %>%
    dplyr::mutate(
      weight_diff_pct = abs(total_individual_weight - total_subsample_weight) / total_subsample_weight
    ) %>%
    dplyr::filter(weight_diff_pct > weight_tolerance)

  if (nrow(invalid_weight_sample_lan) > 0) {
    cat("  [WARN] Check 2.3:", nrow(invalid_weight_sample_lan),
        "sampling Lan hauls with individual/subsample mismatch\n")
    check_log    <- dplyr::bind_rows(check_log, log_check(invalid_weight_sample_lan, "2.3", "WARNING",
                                                   "Individual vs subsample weight mismatch (haul-level, sampling Lan)"))
    all_warnings <- dplyr::bind_rows(all_warnings, invalid_weight_sample_lan %>% dplyr::mutate(check_id = "2.3", severity = "WARNING"))
  } else {
    cat("  [OK] No issues detected\n")
  }
  cat("  Sampling landings remaining:", formatC(nrow(sampling_landings), format="d", big.mark=","), "\n\n")

  # 2.4 Subsample exceeds weight (ERROR)  ----
  cat("2.4 Subsample exceeds total weight (sampling Lan)\n")

  invalid_subsample_exceeds_lan <- sampling_landings %>%
    dplyr::filter(!is.na(subsample_weight) & !is.na(weight)) %>%
    dplyr::filter(subsample_weight > weight * (1 + weight_tolerance))

  if (nrow(invalid_subsample_exceeds_lan) > 0) {
    cat("  [ERROR] Check 2.4:", nrow(invalid_subsample_exceeds_lan),
        "sampling Lan records with subsample > total weight\n")
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_subsample_exceeds_lan, "2.4", "ERROR", "Subsample exceeds total weight (sampling Lan)"))
    all_errors <- dplyr::bind_rows(all_errors, invalid_subsample_exceeds_lan %>%
                              dplyr::mutate(check_id = "2.4", severity = "ERROR") %>%
                              dplyr::select(-dplyr::any_of(c(".row_id", "quarter_declared", "expected_quarter"))))
    sampling_landings <- sampling_landings %>% dplyr::filter(!(.row_id %in% invalid_subsample_exceeds_lan$.row_id))
    n_sampling_landings_removed <- n_sampling_landings_removed + nrow(invalid_subsample_exceeds_lan)
  } else {
    cat("  [OK] No issues detected\n")
  }
  cat("  Sampling landings remaining:", formatC(nrow(sampling_landings), format="d", big.mark=","), "\n\n")

  # 2.5 Effort consistency (WARNING)  ----
  cat("2.5 Effort consistency (sampling Lan)\n")

  invalid_effort_lan <- sampling_landings %>%
    dplyr::filter(!is.na(days_at_sea) & !is.na(n_sets_hauls)) %>%
    dplyr::filter(days_at_sea > 1 & n_sets_hauls == 1)

  if (nrow(invalid_effort_lan) > 0) {
    cat("  [WARN] Check 2.5:", nrow(invalid_effort_lan),
        "sampling Lan records with inconsistent effort fields\n")
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_effort_lan, "2.5", "WARNING", "Multi-day trip with single haul (sampling Lan)"))
    all_warnings <- dplyr::bind_rows(all_warnings, invalid_effort_lan %>%
                                dplyr::mutate(check_id = "2.5", severity = "WARNING") %>%
                                dplyr::select(-dplyr::any_of(c(".row_id", "quarter_declared", "expected_quarter"))))
  } else {
    cat("  [OK] No issues detected\n")
  }
  cat("  Sampling landings remaining:", formatC(nrow(sampling_landings), format="d", big.mark=","), "\n\n")

  # 2.6 Exact duplicates with fish_id (ERROR)  ----
  cat("2.6 Exact duplicates with fish_id (sampling Lan)\n")

  invalid_number_at_length_lan <- sampling_landings %>%
    dplyr::filter(!is.na(fish_id)) %>%
    dplyr::group_by(flag_country, year, trip_code, fish_id) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::mutate(concat = paste0(length_class, "_", ifelse(is.na(individual_weight), "NA", individual_weight), "_",
                           ifelse(is.na(age), "NA", age), "_", ifelse(is.na(sex), "NA", sex), "_",
                           ifelse(is.na(maturity_stage), "NA", maturity_stage), "_", number_at_length),
           all_same = dplyr::n_distinct(concat) == 1) %>%
    dplyr::filter(all_same) %>%
    dplyr::group_by(flag_country, year, trip_code, fish_id) %>%
    dplyr::slice(-1) %>%
    dplyr::ungroup() %>%
    dplyr::select(-concat, -all_same)

  if (nrow(invalid_number_at_length_lan) > 0) {
    cat("  [ERROR] Check 2.6:", nrow(invalid_number_at_length_lan),
        "duplicate sampling Lan records with fish_id\n")
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_number_at_length_lan, "2.6", "ERROR", "Exact duplicate fish_id records (sampling Lan)"))
    all_errors <- dplyr::bind_rows(all_errors, invalid_number_at_length_lan %>%
                              dplyr::mutate(check_id = "2.6", severity = "ERROR") %>%
                              dplyr::select(-dplyr::any_of(c(".row_id", "quarter_declared", "expected_quarter"))))
    sampling_landings <- sampling_landings %>% dplyr::filter(!(.row_id %in% invalid_number_at_length_lan$.row_id))
    n_sampling_landings_removed <- n_sampling_landings_removed + nrow(invalid_number_at_length_lan)
  } else {
    cat("  [OK] No issues detected\n")
  }
  cat("  Sampling landings remaining:", formatC(nrow(sampling_landings), format="d", big.mark=","), "\n\n")

  # 2.7 Possible duplicates without fish_id (WARNING)  ----
  cat("2.7 Possible duplicates without fish_id (sampling Lan)\n")

  all_fields <- c("sampling_type", "flag_country", "year", "trip_code", "harbour", "n_sets_hauls",
                  "days_at_sea", "sampling_method", "aggregation_level", "station_code",
                  "duration_fishing_operation", "initial_latitude", "initial_longitude",
                  "final_latitude", "final_longitude", "depth_fishing_operation", "water_depth",
                  "catch_registration", "species_registration", "date", "area",
                  "fishing_activity_category_national", "fishing_activity_category_eu_l6",
                  "species", "catch_category", "weight", "subsample_weight", "sex",
                  "maturity_method", "maturity_scale", "maturity_stage", "ageing_method", "age",
                  "length_code", "length_class", "number_at_length",
                  "commercial_size_category_scale", "commercial_size_category",
                  "individual_weight", "dbid")

  existing_fields <- all_fields[all_fields %in% names(sampling_landings)]

  invalid_pooled_lan <- sampling_landings %>%
    dplyr::filter(is.na(fish_id)) %>%
    dplyr::group_by(dplyr::across(all_of(existing_fields))) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::ungroup()

  if (nrow(invalid_pooled_lan) > 0) {
    cat("  [WARN] Check 2.7:", nrow(invalid_pooled_lan),
        "potential duplicate pooled sampling Lan records\n")
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_pooled_lan, "2.7", "WARNING", "Possible duplicates without fish_id (sampling Lan)"))
    all_warnings <- dplyr::bind_rows(all_warnings, invalid_pooled_lan %>%
                                dplyr::mutate(check_id = "2.7", severity = "WARNING") %>%
                                dplyr::select(-dplyr::any_of(c(".row_id", "quarter_declared", "expected_quarter"))))
  } else {
    cat("  [OK] No issues detected\n")
  }
  cat("  Sampling landings remaining:", formatC(nrow(sampling_landings), format="d", big.mark=","), "\n\n")

  # 2.8 Individual vs pooled double counting (ERROR)  ----
  cat("2.8 Individual vs pooled double counting (sampling Lan)\n")

  haul_key_pooled <- c("flag_country", "year", "trip_code", "station_code", "area",
                       "fishing_activity_category_eu_l6", "species", "catch_category",
                       "sex", "length_code", "length_class")

  invalid_trip_haul_keys_lan <- sampling_landings %>%
    dplyr::group_by(dplyr::across(all_of(haul_key_pooled))) %>%
    dplyr::summarise(
      has_indiv = any(number_at_length == 1 & !is.na(individual_weight)),
      has_pooled = any(number_at_length > 1),
      total_indiv_n = sum(ifelse(number_at_length == 1, 1, 0), na.rm = TRUE),
      total_indiv_wt = sum(ifelse(number_at_length == 1, individual_weight, 0), na.rm = TRUE),
      pooled_n = max(ifelse(number_at_length > 1, number_at_length, 0), na.rm = TRUE),
      pooled_wt = max(ifelse(number_at_length > 1 & !is.na(subsample_weight), subsample_weight, 0), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(has_indiv & has_pooled & pooled_wt > 0) %>%
    dplyr::mutate(
      n_diff = abs(total_indiv_n - pooled_n),
      wt_pct_diff = abs(total_indiv_wt - pooled_wt) / pmax(pooled_wt, 0.001),
      is_duplicate = (n_diff <= pooled_n_tolerance) & (wt_pct_diff <= pooled_weight_tolerance)
    ) %>%
    dplyr::filter(is_duplicate) %>%
    dplyr::select(all_of(haul_key_pooled))

  invalid_trip_haul_lan <- sampling_landings %>%
    dplyr::inner_join(invalid_trip_haul_keys_lan, by = haul_key_pooled) %>%
    dplyr::filter(number_at_length > 1)

  if (nrow(invalid_trip_haul_lan) > 0) {
    cat("  [ERROR] Check 2.8:", nrow(invalid_trip_haul_lan),
        "sampling Lan records with individual/pooled double counting\n")
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_trip_haul_lan, "2.8", "ERROR", "Individual vs pooled double counting (sampling Lan)"))
    all_errors <- dplyr::bind_rows(all_errors, invalid_trip_haul_lan %>%
                              dplyr::mutate(check_id = "2.8", severity = "ERROR") %>%
                              dplyr::select(-dplyr::any_of(c(".row_id", "quarter_declared", "expected_quarter"))))
    sampling_landings <- sampling_landings %>% dplyr::filter(!(.row_id %in% invalid_trip_haul_lan$.row_id))
    n_sampling_landings_removed <- n_sampling_landings_removed + nrow(invalid_trip_haul_lan)
  } else {
    cat("  [OK] No issues detected\n")
  }
  cat("  Sampling landings remaining:", formatC(nrow(sampling_landings), format="d", big.mark=","), "\n\n")

  # 2.9 fish_id conflicts (ERROR)  ----
  cat("2.9 fish_id conflicts (sampling Lan)\n")

  fishid_basic <- sampling_landings %>%
    dplyr::filter(!is.na(fish_id)) %>%
    dplyr::group_by(flag_country, year, trip_code, fish_id) %>%
    dplyr::filter(dplyr::n() > 1 & (dplyr::n_distinct(species, na.rm = TRUE) > 1 | dplyr::n_distinct(sex, na.rm = TRUE) > 1 |
                        dplyr::n_distinct(length_code, na.rm = TRUE) > 1)) %>%
    dplyr::ungroup()

  fishid_length <- sampling_landings %>%
    dplyr::filter(!is.na(fish_id)) %>%
    dplyr::mutate(length_num = suppressWarnings(as.numeric(length_class))) %>%
    dplyr::filter(!is.na(length_num)) %>%
    dplyr::group_by(flag_country, year, trip_code, fish_id) %>%
    dplyr::summarise(
      range = if (dplyr::n_distinct(length_num) > 1) {
        max(length_num, na.rm = TRUE) - min(length_num, na.rm = TRUE)
      } else {
        0
      },
      conflict = range > 50,
      .groups = "drop"
    ) %>%
    dplyr::filter(conflict) %>%
    dplyr::inner_join(sampling_landings, by = c("flag_country", "year", "trip_code", "fish_id")) %>%
    dplyr::select(-range, -conflict)

  fishid_age <- sampling_landings %>%
    dplyr::filter(!is.na(fish_id)) %>%
    dplyr::mutate(age_num = suppressWarnings(as.numeric(age))) %>%
    dplyr::filter(!is.na(age_num)) %>%
    dplyr::group_by(flag_country, year, trip_code, fish_id) %>%
    dplyr::summarise(
      range = if (dplyr::n_distinct(age_num) > 1) {
        max(age_num, na.rm = TRUE) - min(age_num, na.rm = TRUE)
      } else {
        0
      },
      conflict = range > 2,
      .groups = "drop"
    ) %>%
    dplyr::filter(conflict) %>%
    dplyr::inner_join(sampling_landings, by = c("flag_country", "year", "trip_code", "fish_id")) %>%
    dplyr::select(-range, -conflict)

  invalid_fishid_lan <- dplyr::bind_rows(fishid_basic, fishid_length, fishid_age) %>% dplyr::distinct()

  if (nrow(invalid_fishid_lan) > 0) {
    cat("  [ERROR] Check 2.9:", nrow(invalid_fishid_lan),
        "sampling Lan records with fish_id conflicts\n")
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_fishid_lan, "2.9", "ERROR", "fish_id conflicts (sampling Lan)"))
    all_errors <- dplyr::bind_rows(all_errors, invalid_fishid_lan %>%
                              dplyr::mutate(check_id = "2.9", severity = "ERROR") %>%
                              dplyr::select(-dplyr::any_of(c(".row_id", "quarter_declared", "expected_quarter"))))
    sampling_landings <- sampling_landings %>% dplyr::filter(!(.row_id %in% invalid_fishid_lan$.row_id))
    n_sampling_landings_removed <- n_sampling_landings_removed + nrow(invalid_fishid_lan)
  } else {
    cat("  [OK] No issues detected\n")
  }
  cat("  Sampling landings remaining:", formatC(nrow(sampling_landings), format="d", big.mark=","), "\n\n")


  # 2.10 Maturity scale-stage cross-check (WARNING)  ----
  cat("2.10 Maturity scale-stage cross-check (sampling Lan)\n")

  if (is.null(VLISTS) || is.null(VLISTS$maturity_stages_by_scale)) {
    cat("  [OK] Check 2.10: maturity check skipped\n")
  } else {

    invalid_maturity_method_lan <- sampling_landings %>%
      dplyr::filter((is.na(maturity_method) | maturity_method == "") &
               ((!is.na(maturity_scale) & maturity_scale != "") |
                  (!is.na(maturity_stage) & maturity_stage != "")))

    invalid_maturity_pairs_lan <- sampling_landings %>%
      dplyr::filter(!is.na(maturity_scale) & !is.na(maturity_stage) &
               maturity_scale != "" & maturity_stage != "") %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        valid_combination =
          !is.na(maturity_scale) && maturity_scale != "" &&
          isTRUE(maturity_scale %in% names(VLISTS$maturity_stages_by_scale)) &&
          !is.na(maturity_stage) && maturity_stage != "" &&
          maturity_stage %in% VLISTS$maturity_stages_by_scale[[maturity_scale]]
      ) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!valid_combination) %>%
      dplyr::select(-valid_combination)

    invalid_maturity_lan <- dplyr::bind_rows(invalid_maturity_method_lan, invalid_maturity_pairs_lan) %>% dplyr::distinct()

    if (nrow(invalid_maturity_lan) > 0) {
      cat("  [WARN] Check 2.10:", nrow(invalid_maturity_lan),
          "sampling Lan records with incompatible maturity scale/stage\n")
      check_log <- dplyr::bind_rows(check_log, log_check(invalid_maturity_lan, "2.10", "WARNING", "Maturity scale-stage inconsistency (sampling Lan)"))
        all_warnings <- dplyr::bind_rows(all_warnings, invalid_maturity_lan %>%
                                dplyr::mutate(check_id = "2.10", severity = "WARNING") %>%
                                dplyr::select(-dplyr::any_of(c(".row_id", "quarter_declared", "expected_quarter"))))
    } else {
      cat("  [OK] No issues detected\n")
    }
  }
  cat("  Sampling landings remaining:", formatC(nrow(sampling_landings), format="d", big.mark=","), "\n\n")

  # 2.11 Trip aggregation_level='T' with multiple weights (ERROR) ----
  cat("2.11 Trip aggregation consistency (aggregation_level='T') (sampling Lan)\n")

  trip_multi_w_sampling_landings <- sampling_landings %>%
    dplyr::filter(aggregation_level == "T") %>%
    dplyr::group_by(year, quarter_from_date, trip_code, species, fishing_activity_category_eu_l6, commercial_size_category) %>%
    dplyr::filter(dplyr::n_distinct(weight, na.rm = TRUE) > 1) %>%
    dplyr::ungroup()

  if (nrow(trip_multi_w_sampling_landings) > 0) {
    n_trips <- dplyr::n_distinct(trip_multi_w_sampling_landings$trip_code)
    cat("  [ERROR] Check 2.11:", nrow(trip_multi_w_sampling_landings),
        "sampling Lan records with ambiguous trip-level weight\n")
    check_log <- dplyr::bind_rows(check_log, log_check(trip_multi_w_sampling_landings, "2.11", "ERROR",
                           "aggregation_level='T' with multiple weights per trip_code - raising impossible"))
    all_errors <- dplyr::bind_rows(all_errors, trip_multi_w_sampling_landings %>%
                              dplyr::mutate(check_id = "2.11",
                                            check_message = "aggregation_level='T' with multiple weights per trip_code") %>%
                              dplyr::select(-dplyr::any_of(c(".row_id", "quarter_declared", "expected_quarter"))))
    sampling_landings <- sampling_landings %>% dplyr::filter(!(.row_id %in% trip_multi_w_sampling_landings$.row_id))
    n_sampling_landings_removed <- n_sampling_landings_removed + nrow(trip_multi_w_sampling_landings)
  } else {
    cat("  [OK] No issues detected\n")
  }
  cat("  sampling Lan remaining:", formatC(nrow(sampling_landings), format="d", big.mark=","), "\n\n")

  # 2.12 Haul aggregation_level='H' with multiple weights (ERROR) ----
  cat("2.12 Haul aggregation consistency (aggregation_level='H') (sampling Lan)\n")

  haul_multi_w_sampling_landings <- sampling_landings %>%
    dplyr::filter(aggregation_level == "H") %>%
    dplyr::group_by(year, quarter_from_date, trip_code, station_code, species, fishing_activity_category_eu_l6, commercial_size_category) %>%
    dplyr::filter(dplyr::n_distinct(weight, na.rm = TRUE) > 1) %>%
    dplyr::ungroup()

  if (nrow(haul_multi_w_sampling_landings) > 0) {
    n_hauls <- dplyr::n_distinct(haul_multi_w_sampling_landings$station_code)
    cat("  [ERROR] Check 2.12:", nrow(haul_multi_w_sampling_landings),
        "sampling Lan records with ambiguous haul-level weight\n")
    check_log <- dplyr::bind_rows(check_log, log_check(haul_multi_w_sampling_landings, "2.12", "ERROR",
                           "aggregation_level='H' with multiple weights per haul - raising impossible"))
    all_errors <- dplyr::bind_rows(all_errors, haul_multi_w_sampling_landings %>%
                              dplyr::mutate(check_id = "2.12",
                                            check_message = "aggregation_level='H' with multiple weights per trip_code + station_code") %>%
                              dplyr::select(-dplyr::any_of(c(".row_id", "quarter_declared", "expected_quarter"))))
    sampling_landings <- sampling_landings %>% dplyr::filter(!(.row_id %in% haul_multi_w_sampling_landings$.row_id))
    n_sampling_landings_removed <- n_sampling_landings_removed + nrow(haul_multi_w_sampling_landings)
  } else {
    cat("  [OK] No issues detected\n")
  }
  cat("  sampling Lan remaining:", formatC(nrow(sampling_landings), format="d", big.mark=","), "\n\n")

  # SECTION 3: INTERNAL SAMPLING DISCARDS CHECKS (Dis) ----

  print_section('SECTION 3: INTERNAL SAMPLING DISCARDS CHECKS (catch_category = "Dis")')

  sampling_discards <- sampling %>% dplyr::filter(catch_category == "Dis")
  cat("Sampling discards:", formatC(nrow(sampling_discards), format="d", big.mark=","), "records\n\n")

  # 3.1 NA in critical LFD fields (ERROR)  ----
  cat("3.1 NA in critical LFD fields (sampling Dis)\n")

  invalid_lfd_fields_dis <- sampling_discards %>%
    dplyr::filter(is.na(length_class) | is.na(length_code) |
             is.na(number_at_length) | number_at_length <= 0)

  if (nrow(invalid_lfd_fields_dis) > 0) {
    cat("  [ERROR] Check 3.1:", nrow(invalid_lfd_fields_dis),
        "sampling Dis records missing critical LFD fields\n")
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_lfd_fields_dis, "3.1", "ERROR", "NA in critical LFD fields (sampling Dis)"))
    all_errors <- dplyr::bind_rows(all_errors, invalid_lfd_fields_dis %>%
                              dplyr::mutate(check_id = "3.1", severity = "ERROR") %>%
                              dplyr::select(-dplyr::any_of(c(".row_id", "quarter_declared", "expected_quarter"))))
    sampling_discards <- sampling_discards %>% dplyr::filter(!(.row_id %in% invalid_lfd_fields_dis$.row_id))
    n_sampling_discards_removed <- n_sampling_discards_removed + nrow(invalid_lfd_fields_dis)
  } else {
    cat("  [OK] No issues detected\n")
  }
  cat("  Sampling discards remaining:", formatC(nrow(sampling_discards), format="d", big.mark=","), "\n\n")

  # 3.2 Registration flags (ERROR)  ----
  cat("3.2 Registration flags consistency (sampling Dis)\n")

  invalid_registration_dis <- sampling_discards %>%
    dplyr::filter(catch_registration == "Non" & species_registration != "Non")

  if (nrow(invalid_registration_dis) > 0) {
    cat("  [ERROR] Check 3.2:", nrow(invalid_registration_dis),
        "sampling Dis records with inconsistent registration flags\n")
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_registration_dis, "3.2", "ERROR", "Registration flag violation (sampling Dis)"))
    all_errors <- dplyr::bind_rows(all_errors, invalid_registration_dis %>%
                              dplyr::mutate(check_id = "3.2", severity = "ERROR") %>%
                              dplyr::select(-dplyr::any_of(c(".row_id", "quarter_declared", "expected_quarter"))))
    sampling_discards <- sampling_discards %>% dplyr::filter(!(.row_id %in% invalid_registration_dis$.row_id))
    n_sampling_discards_removed <- n_sampling_discards_removed + nrow(invalid_registration_dis)
  } else {
    cat("  [OK] No issues detected\n")
  }
  cat("  Sampling discards remaining:", formatC(nrow(sampling_discards), format="d", big.mark=","), "\n\n")

  # 3.3 Individual vs subsample weight (haul-level, WARNING)  ----
  cat("3.3 Individual vs subsample weight consistency (haul-level, sampling Dis)\n")

  invalid_weight_sample_dis <- sampling_discards %>%
    dplyr::filter(!is.na(individual_weight) | !is.na(subsample_weight)) %>%
    dplyr::group_by(dplyr::across(all_of(haul_key))) %>%
    dplyr::summarise(
      total_individual_weight = sum(individual_weight, na.rm = TRUE),
      total_subsample_weight = collapse_repeated_weight(subsample_weight),
      has_individual = any(!is.na(individual_weight) & individual_weight > 0),
      has_subsample = any(!is.na(subsample_weight) & subsample_weight > 0),
      .groups = "drop"
    ) %>%
    dplyr::mutate(total_subsample_weight = ifelse(is.na(total_subsample_weight), 0, total_subsample_weight)) %>%
    dplyr::filter(has_individual & has_subsample & total_subsample_weight > 0) %>%
    dplyr::mutate(weight_diff_pct = abs(total_individual_weight - total_subsample_weight) / total_subsample_weight) %>%
    dplyr::filter(weight_diff_pct > weight_tolerance)

  if (nrow(invalid_weight_sample_dis) > 0) {
    cat("  [WARN] Check 3.3:", nrow(invalid_weight_sample_dis),
        "sampling Dis hauls with individual/subsample mismatch\n")
    check_log    <- dplyr::bind_rows(check_log, log_check(invalid_weight_sample_dis, "3.3", "WARNING",
                                                   "Individual vs subsample weight mismatch (haul-level, sampling Dis)"))
    all_warnings <- dplyr::bind_rows(all_warnings, invalid_weight_sample_dis %>% dplyr::mutate(check_id = "3.3", severity = "WARNING"))
  } else {
    cat("  [OK] No issues detected\n")
  }
  cat("  Sampling discards remaining:", formatC(nrow(sampling_discards), format="d", big.mark=","), "\n\n")

  # 3.4 Subsample exceeds weight (ERROR)  ----
  cat("3.4 Subsample exceeds total weight (sampling Dis)\n")

  invalid_subsample_exceeds_dis <- sampling_discards %>%
    dplyr::filter(!is.na(subsample_weight) & !is.na(weight)) %>%
    dplyr::filter(subsample_weight > weight * (1 + weight_tolerance))

  if (nrow(invalid_subsample_exceeds_dis) > 0) {
    cat("  [ERROR] Check 3.4:", nrow(invalid_subsample_exceeds_dis),
        "sampling Dis records with subsample > total weight\n")
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_subsample_exceeds_dis, "3.4", "ERROR", "Subsample exceeds total weight (sampling Dis)"))
    all_errors <- dplyr::bind_rows(all_errors, invalid_subsample_exceeds_dis %>%
                              dplyr::mutate(check_id = "3.4", severity = "ERROR") %>%
                              dplyr::select(-dplyr::any_of(c(".row_id", "quarter_declared", "expected_quarter"))))
    sampling_discards <- sampling_discards %>% dplyr::filter(!(.row_id %in% invalid_subsample_exceeds_dis$.row_id))
    n_sampling_discards_removed <- n_sampling_discards_removed + nrow(invalid_subsample_exceeds_dis)
  } else {
    cat("  [OK] No issues detected\n")
  }
  cat("  Sampling discards remaining:", formatC(nrow(sampling_discards), format="d", big.mark=","), "\n\n")

  # 3.5 Effort consistency (WARNING)  ----
  cat("3.5 Effort consistency (sampling Dis)\n")

  invalid_effort_dis <- sampling_discards %>%
    dplyr::filter(!is.na(days_at_sea) & !is.na(n_sets_hauls)) %>%
    dplyr::filter(days_at_sea > 1 & n_sets_hauls == 1)

  if (nrow(invalid_effort_dis) > 0) {
    cat("  [WARN] Check 3.5:", nrow(invalid_effort_dis),
        "sampling Dis records with inconsistent effort fields\n")
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_effort_dis, "3.5", "WARNING", "Multi-day trip with single haul (sampling Dis)"))
    all_warnings <- dplyr::bind_rows(all_warnings, invalid_effort_dis %>%
                                dplyr::mutate(check_id = "3.5", severity = "WARNING") %>%
                                dplyr::select(-dplyr::any_of(c(".row_id", "quarter_declared", "expected_quarter"))))
  } else {
    cat("  [OK] No issues detected\n")
  }
  cat("  Sampling discards remaining:", formatC(nrow(sampling_discards), format="d", big.mark=","), "\n\n")

  # 3.6 Exact duplicates with fish_id (ERROR)  ----
  cat("3.6 Exact duplicates with fish_id (sampling Dis)\n")

  invalid_number_at_length_dis <- sampling_discards %>%
    dplyr::filter(!is.na(fish_id)) %>%
    dplyr::group_by(flag_country, year, trip_code, fish_id) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::mutate(concat = paste0(length_class, "_", ifelse(is.na(individual_weight), "NA", individual_weight), "_",
                           ifelse(is.na(age), "NA", age), "_", ifelse(is.na(sex), "NA", sex), "_",
                           ifelse(is.na(maturity_stage), "NA", maturity_stage), "_", number_at_length),
           all_same = dplyr::n_distinct(concat) == 1) %>%
    dplyr::filter(all_same) %>%
    dplyr::group_by(flag_country, year, trip_code, fish_id) %>%
    dplyr::slice(-1) %>%
    dplyr::ungroup() %>%
    dplyr::select(-concat, -all_same)

  if (nrow(invalid_number_at_length_dis) > 0) {
    cat("  [ERROR] Check 3.6:", nrow(invalid_number_at_length_dis),
        "duplicate sampling Dis records with fish_id\n")
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_number_at_length_dis, "3.6", "ERROR", "Exact duplicate fish_id records (sampling Dis)"))
    all_errors <- dplyr::bind_rows(all_errors, invalid_number_at_length_dis %>%
                              dplyr::mutate(check_id = "3.6", severity = "ERROR") %>%
                              dplyr::select(-dplyr::any_of(c(".row_id", "quarter_declared", "expected_quarter"))))
    sampling_discards <- sampling_discards %>% dplyr::filter(!(.row_id %in% invalid_number_at_length_dis$.row_id))
    n_sampling_discards_removed <- n_sampling_discards_removed + nrow(invalid_number_at_length_dis)
  } else {
    cat("  [OK] No issues detected\n")
  }
  cat("  Sampling discards remaining:", formatC(nrow(sampling_discards), format="d", big.mark=","), "\n\n")

  # 3.7 Possible duplicates without fish_id (WARNING)  ----
  cat("3.7 Possible duplicates without fish_id (sampling Dis)\n")

  existing_fields_dis <- all_fields[all_fields %in% names(sampling_discards)]

  invalid_pooled_dis <- sampling_discards %>%
    dplyr::filter(is.na(fish_id)) %>%
    dplyr::group_by(dplyr::across(all_of(existing_fields_dis))) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::ungroup()

  if (nrow(invalid_pooled_dis) > 0) {
    cat("  [WARN] Check 3.7:", nrow(invalid_pooled_dis),
        "potential duplicate pooled sampling Dis records\n")
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_pooled_dis, "3.7", "WARNING", "Possible duplicates without fish_id (sampling Dis)"))
    all_warnings <- dplyr::bind_rows(all_warnings, invalid_pooled_dis %>%
                                dplyr::mutate(check_id = "3.7", severity = "WARNING") %>%
                                dplyr::select(-dplyr::any_of(c(".row_id", "quarter_declared", "expected_quarter"))))
  } else {
    cat("  [OK] No issues detected\n")
  }
  cat("  Sampling discards remaining:", formatC(nrow(sampling_discards), format="d", big.mark=","), "\n\n")

  # 3.8 Individual vs pooled double counting (ERROR)  ----
  cat("3.8 Individual vs pooled double counting (sampling Dis)\n")

  invalid_trip_haul_keys_dis <- sampling_discards %>%
    dplyr::group_by(dplyr::across(all_of(haul_key_pooled))) %>%
    dplyr::summarise(
      has_indiv = any(number_at_length == 1 & !is.na(individual_weight)),
      has_pooled = any(number_at_length > 1),
      total_indiv_n = sum(ifelse(number_at_length == 1, 1, 0), na.rm = TRUE),
      total_indiv_wt = sum(ifelse(number_at_length == 1, individual_weight, 0), na.rm = TRUE),
      pooled_n = max(ifelse(number_at_length > 1, number_at_length, 0), na.rm = TRUE),
      pooled_wt = max(ifelse(number_at_length > 1 & !is.na(subsample_weight), subsample_weight, 0), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(has_indiv & has_pooled & pooled_wt > 0) %>%
    dplyr::mutate(
      n_diff = abs(total_indiv_n - pooled_n),
      wt_pct_diff = abs(total_indiv_wt - pooled_wt) / pmax(pooled_wt, 0.001),
      is_duplicate = (n_diff <= pooled_n_tolerance) & (wt_pct_diff <= pooled_weight_tolerance)
    ) %>%
    dplyr::filter(is_duplicate) %>%
    dplyr::select(all_of(haul_key_pooled))

  invalid_trip_haul_dis <- sampling_discards %>%
    dplyr::inner_join(invalid_trip_haul_keys_dis, by = haul_key_pooled) %>%
    dplyr::filter(number_at_length > 1)

  if (nrow(invalid_trip_haul_dis) > 0) {
    cat("  [ERROR] Check 3.8:", nrow(invalid_trip_haul_dis),
        "sampling Dis records with individual/pooled double counting\n")
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_trip_haul_dis, "3.8", "ERROR", "Individual vs pooled double counting (sampling Dis)"))
    all_errors <- dplyr::bind_rows(all_errors, invalid_trip_haul_dis %>%
                              dplyr::mutate(check_id = "3.8", severity = "ERROR") %>%
                              dplyr::select(-dplyr::any_of(c(".row_id", "quarter_declared", "expected_quarter"))))
    sampling_discards <- sampling_discards %>% dplyr::filter(!(.row_id %in% invalid_trip_haul_dis$.row_id))
    n_sampling_discards_removed <- n_sampling_discards_removed + nrow(invalid_trip_haul_dis)
  } else {
    cat("  [OK] No issues detected\n")
  }
  cat("  Sampling discards remaining:", formatC(nrow(sampling_discards), format="d", big.mark=","), "\n\n")

  # 3.9 fish_id conflicts (ERROR)  ----
  cat("3.9 fish_id conflicts (sampling Dis)\n")

  fishid_basic_dis <- sampling_discards %>%
    dplyr::filter(!is.na(fish_id)) %>%
    dplyr::group_by(flag_country, year, trip_code, fish_id) %>%
    dplyr::filter(dplyr::n() > 1 & (dplyr::n_distinct(species, na.rm = TRUE) > 1 | dplyr::n_distinct(sex, na.rm = TRUE) > 1 |
                        dplyr::n_distinct(length_code, na.rm = TRUE) > 1)) %>%
    dplyr::ungroup()

  fishid_length_dis <- sampling_discards %>%
    dplyr::filter(!is.na(fish_id)) %>%
    dplyr::mutate(length_num = suppressWarnings(as.numeric(length_class))) %>%
    dplyr::filter(!is.na(length_num)) %>%
    dplyr::group_by(flag_country, year, trip_code, fish_id) %>%
    dplyr::summarise(
      range = if (dplyr::n_distinct(length_num) > 1) {
        max(length_num, na.rm = TRUE) - min(length_num, na.rm = TRUE)
      } else {
        0
      },
      conflict = range > 50,
      .groups = "drop"
    ) %>%
    dplyr::filter(conflict) %>%
    dplyr::inner_join(sampling_discards, by = c("flag_country", "year", "trip_code", "fish_id")) %>%
    dplyr::select(-range, -conflict)

  fishid_age_dis <- sampling_discards %>%
    dplyr::filter(!is.na(fish_id)) %>%
    dplyr::mutate(age_num = suppressWarnings(as.numeric(age))) %>%
    dplyr::filter(!is.na(age_num)) %>%
    dplyr::group_by(flag_country, year, trip_code, fish_id) %>%
    dplyr::summarise(
      range = if (dplyr::n_distinct(age_num) > 1) {
        max(age_num, na.rm = TRUE) - min(age_num, na.rm = TRUE)
      } else {
        0
      },
      conflict = range > 2,
      .groups = "drop"
    ) %>%
    dplyr::filter(conflict) %>%
    dplyr::inner_join(sampling_discards, by = c("flag_country", "year", "trip_code", "fish_id")) %>%
    dplyr::select(-range, -conflict)

  invalid_fishid_dis <- dplyr::bind_rows(fishid_basic_dis, fishid_length_dis, fishid_age_dis) %>% dplyr::distinct()

  if (nrow(invalid_fishid_dis) > 0) {
    cat("  [ERROR] Check 3.9:", nrow(invalid_fishid_dis),
        "sampling Dis records with fish_id conflicts\n")
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_fishid_dis, "3.9", "ERROR", "fish_id conflicts (sampling Dis)"))
    all_errors <- dplyr::bind_rows(all_errors, invalid_fishid_dis %>%
                              dplyr::mutate(check_id = "3.9", severity = "ERROR") %>%
                              dplyr::select(-dplyr::any_of(c(".row_id", "quarter_declared", "expected_quarter"))))
    sampling_discards <- sampling_discards %>% dplyr::filter(!(.row_id %in% invalid_fishid_dis$.row_id))
    n_sampling_discards_removed <- n_sampling_discards_removed + nrow(invalid_fishid_dis)
  } else {
    cat("  [OK] No issues detected\n")
  }
  cat("  Sampling discards remaining:", formatC(nrow(sampling_discards), format="d", big.mark=","), "\n\n")


  # 3.10 Maturity scale-stage cross-check (WARNING)  ----
  cat("3.10 Maturity scale-stage cross-check (sampling Dis)\n")

  if (is.null(VLISTS) || is.null(VLISTS$maturity_stages_by_scale)) {
    cat("  [OK] Check 3.10: maturity check skipped\n")
  } else {

    invalid_maturity_method_dis <- sampling_discards %>%
      dplyr::filter((is.na(maturity_method) | maturity_method == "") &
               ((!is.na(maturity_scale) & maturity_scale != "") |
                  (!is.na(maturity_stage) & maturity_stage != "")))

    invalid_maturity_pairs_dis <- sampling_discards %>%
      dplyr::filter(!is.na(maturity_scale) & !is.na(maturity_stage) &
               maturity_scale != "" & maturity_stage != "") %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        valid_combination =
          !is.na(maturity_scale) && maturity_scale != "" &&
          isTRUE(maturity_scale %in% names(VLISTS$maturity_stages_by_scale)) &&
          !is.na(maturity_stage) && maturity_stage != "" &&
          maturity_stage %in% VLISTS$maturity_stages_by_scale[[maturity_scale]]
      ) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!valid_combination) %>%
      dplyr::select(-valid_combination)

    invalid_maturity_dis <- dplyr::bind_rows(invalid_maturity_method_dis, invalid_maturity_pairs_dis) %>% dplyr::distinct()

    if (nrow(invalid_maturity_dis) > 0) {
      cat("  [WARN] Check 3.10:", nrow(invalid_maturity_dis),
          "sampling Dis records with incompatible maturity scale/stage\n")
      check_log <- dplyr::bind_rows(check_log, log_check(invalid_maturity_dis, "3.10", "WARNING", "Maturity scale-stage inconsistency (sampling Dis)"))
        all_warnings <- dplyr::bind_rows(all_warnings, invalid_maturity_dis %>%
                                dplyr::mutate(check_id = "3.10", severity = "WARNING") %>%
                                dplyr::select(-dplyr::any_of(c(".row_id", "quarter_declared", "expected_quarter"))))
    } else {
      cat("  [OK] No issues detected\n")
    }
  }
  cat("  Sampling discards remaining:", formatC(nrow(sampling_discards), format="d", big.mark=","), "\n\n")

  # 3.11 Trip aggregation_level='T' with multiple weights (ERROR) ----
  cat("3.11 Trip aggregation consistency (aggregation_level='T') (sampling Dis)\n")

  trip_multi_w_sampling_discards <- sampling_discards %>%
    dplyr::filter(aggregation_level == "T") %>%
    dplyr::group_by(year, quarter_from_date, trip_code, species, fishing_activity_category_eu_l6, commercial_size_category) %>%
    dplyr::filter(dplyr::n_distinct(weight, na.rm = TRUE) > 1) %>%
    dplyr::ungroup()

  if (nrow(trip_multi_w_sampling_discards) > 0) {
    n_trips <- dplyr::n_distinct(trip_multi_w_sampling_discards$trip_code)
    cat("  [ERROR] Check 3.11:", nrow(trip_multi_w_sampling_discards),
        "sampling Dis records with ambiguous trip-level weight\n")
    check_log <- dplyr::bind_rows(check_log, log_check(trip_multi_w_sampling_discards, "3.11", "ERROR",
                           "aggregation_level='T' with multiple weights per trip_code - raising impossible"))
    all_errors <- dplyr::bind_rows(all_errors, trip_multi_w_sampling_discards %>%
                              dplyr::mutate(check_id = "3.11",
                                            check_message = "aggregation_level='T' with multiple weights per trip_code") %>%
                              dplyr::select(-dplyr::any_of(c(".row_id", "quarter_declared", "expected_quarter"))))
    sampling_discards <- sampling_discards %>% dplyr::filter(!(.row_id %in% trip_multi_w_sampling_discards$.row_id))
    n_sampling_discards_removed <- n_sampling_discards_removed + nrow(trip_multi_w_sampling_discards)
  } else {
    cat("  [OK] No issues detected\n")
  }
  cat("  sampling Dis remaining:", formatC(nrow(sampling_discards), format="d", big.mark=","), "\n\n")

  # 3.12 Haul aggregation_level='H' with multiple weights (ERROR) ----
  cat("3.12 Haul aggregation consistency (aggregation_level='H') (sampling Dis)\n")

  haul_multi_w_sampling_discards <- sampling_discards %>%
    dplyr::filter(aggregation_level == "H") %>%
    dplyr::group_by(year, quarter_from_date, trip_code, station_code, species, fishing_activity_category_eu_l6, commercial_size_category) %>%
    dplyr::filter(dplyr::n_distinct(weight, na.rm = TRUE) > 1) %>%
    dplyr::ungroup()

  if (nrow(haul_multi_w_sampling_discards) > 0) {
    n_hauls <- dplyr::n_distinct(haul_multi_w_sampling_discards$station_code)
    cat("  [ERROR] Check 3.12:", nrow(haul_multi_w_sampling_discards),
        "sampling Dis records with ambiguous haul-level weight\n")
    check_log <- dplyr::bind_rows(check_log, log_check(haul_multi_w_sampling_discards, "3.12", "ERROR",
                           "aggregation_level='H' with multiple weights per haul - raising impossible"))
    all_errors <- dplyr::bind_rows(all_errors, haul_multi_w_sampling_discards %>%
                              dplyr::mutate(check_id = "3.12",
                                            check_message = "aggregation_level='H' with multiple weights per trip_code + station_code") %>%
                              dplyr::select(-dplyr::any_of(c(".row_id", "quarter_declared", "expected_quarter"))))
    sampling_discards <- sampling_discards %>% dplyr::filter(!(.row_id %in% haul_multi_w_sampling_discards$.row_id))
    n_sampling_discards_removed <- n_sampling_discards_removed + nrow(haul_multi_w_sampling_discards)
  } else {
    cat("  [OK] No issues detected\n")
  }
  cat("  sampling Dis remaining:", formatC(nrow(sampling_discards), format="d", big.mark=","), "\n\n")

  # 3.13 Intra-trip Lan/Dis consistency (WARNING) ----
  # Trips that have both Lan and Dis records must have the same area,
  # gear (fishing_activity_category_eu_l6), and quarter within each category.
  cat("3.13 Intra-trip Lan/Dis consistency\n")

  trips_lan <- sampling_landings %>%
    dplyr::distinct(flag_country, year, trip_code, area,
                    fishing_activity_category_eu_l6, quarter_from_date)

  trips_dis <- sampling_discards %>%
    dplyr::distinct(flag_country, year, trip_code, area,
                    fishing_activity_category_eu_l6, quarter_from_date)

  # Keep only trips present in both categories
  common_trips <- trips_lan %>%
    dplyr::select(flag_country, year, trip_code) %>%
    dplyr::inner_join(
      trips_dis %>% dplyr::select(flag_country, year, trip_code),
      by = c("flag_country", "year", "trip_code")
    ) %>%
    dplyr::distinct()

  if (nrow(common_trips) == 0) {
    cat("  [OK] Check 3.13: intra-trip Lan/Dis consistency skipped\n")
  } else {
    trips_lan_common <- trips_lan %>%
      dplyr::semi_join(common_trips, by = c("flag_country", "year", "trip_code"))
    trips_dis_common <- trips_dis %>%
      dplyr::semi_join(common_trips, by = c("flag_country", "year", "trip_code"))

    inconsistent_trips <- trips_lan_common %>%
      dplyr::full_join(trips_dis_common,
                       by = c("flag_country", "year", "trip_code"),
                       suffix = c("_lan", "_dis")) %>%
      dplyr::filter(
        area_lan != area_dis |
        fishing_activity_category_eu_l6_lan != fishing_activity_category_eu_l6_dis |
        quarter_from_date_lan != quarter_from_date_dis
      )

    if (nrow(inconsistent_trips) > 0) {
      cat("  [WARN] Check 3.13:", nrow(inconsistent_trips),
          "trip(s) with Lan/Dis inconsistency\n")
      for (i in seq_len(nrow(inconsistent_trips))) {
        r <- inconsistent_trips[i, ]
        cat(sprintf("    trip_code=%s | Lan: area=%s gear=%s Q=%s | Dis: area=%s gear=%s Q=%s\n",
                    r$trip_code,
                    r$area_lan, r$fishing_activity_category_eu_l6_lan, r$quarter_from_date_lan,
                    r$area_dis, r$fishing_activity_category_eu_l6_dis, r$quarter_from_date_dis))
      }
      check_log <- dplyr::bind_rows(check_log,
                             log_check(inconsistent_trips, "3.13", "WARNING",
                                       "Intra-trip Lan/Dis inconsistency in area, gear or quarter"))
      all_warnings <- dplyr::bind_rows(all_warnings,
                                inconsistent_trips %>%
                                  dplyr::mutate(check_id = "3.13", severity = "WARNING"))
    } else {
      cat("  [OK] No issues detected\n")
    }
  }
  cat("\n")

  # SECTION 4: CROSS-CHECKS (Sampling Lan vs Landings) ----
  
  print_section("SECTION 4: CROSS-CHECKS (SAMPLING LANDINGS vs LANDINGS)")
  
  # 4.0 Stratum key-matching pre-check (ERROR if zero overlap) ----
  # Purpose: Before any cross-check maths, verify that at least one stratum
  # (country + year + area + gear + species) is shared between the sampling
  # and landings datasets. Zero overlap means raising factors cannot be computed
  # for ANY record - it is always a data-linkage problem and must block execution.
  cat("4.0 Stratum key-matching pre-check\n")
  cat("  Verifying common strata (country x year x area x gear x species)...\n")
  
  stratum_keys_sampling <- sampling_landings %>%
    dplyr::distinct(flag_country, year, area,
                    fishing_activity_category_eu_l6, species) %>%
    dplyr::mutate(stratum_key = paste(flag_country, year, area,
                                      fishing_activity_category_eu_l6, species, sep = "|"))
  
  stratum_keys_landings <- landings %>%
    dplyr::distinct(flag_country, year, area,
                    fishing_activity_category_eu_l6, species) %>%
    dplyr::mutate(stratum_key = paste(flag_country, year, area,
                                      fishing_activity_category_eu_l6, species, sep = "|"))
  
  common_strata   <- intersect(stratum_keys_sampling$stratum_key,
                               stratum_keys_landings$stratum_key)
  sampling_only   <- setdiff(stratum_keys_sampling$stratum_key,
                             stratum_keys_landings$stratum_key)
  landings_only   <- setdiff(stratum_keys_landings$stratum_key,
                             stratum_keys_sampling$stratum_key)
  
  cat("  Strata in sampling   :", nrow(stratum_keys_sampling), "\n")
  cat("  Strata in landings   :", nrow(stratum_keys_landings), "\n")
  cat("  Common strata (overlap):", length(common_strata), "\n")
  cat("  Sampling-only strata :", length(sampling_only), "(no landings -> RF = 0)\n")
  cat("  Landings-only strata :", length(landings_only), "(no sampling -> cannot expand)\n")
  
  if (length(common_strata) == 0) {
    # Zero overlap: raising is completely impossible - ERROR and stop
    cat("  [ERROR] ERROR 4.0: ZERO common strata between sampling and landings files.\n")
    cat("    Raising factors cannot be computed for ANY record.\n")
    cat("    Likely causes:\n")
    cat("      - Different year(s), area(s), gear(s), or species between files\n")
    cat("      - Wrong file(s) selected\n")
    cat("      - Stratum key columns (flag_country / area / fishing_activity_category_eu_l6 / species) contain mismatches\n")
    cat("\n  === EXECUTION HALTED: correct stratum mismatches before proceeding ===\n")
    check_log <- dplyr::bind_rows(check_log,
                           log_check(sampling_landings, "4.0", "ERROR",
                                     "Zero common strata between sampling and landings: raising impossible"))
    all_errors <- format_qc_issue_output(all_errors)
    all_warnings <- format_qc_issue_output(all_warnings)
    out <- list(
      landings_clean  = landings,
      sampling_clean  = NULL,
      errors          = all_errors,
      warnings        = all_warnings,
      summary         = check_log,
      output_dir      = out_dir,
      output_suffix   = if (exists("file_suffix")) file_suffix else ""
    )
    readr::write_csv(landings %>% dplyr::select(-dplyr::any_of(".row_id")),
                     file.path(out_dir, paste0("landings_clean", file_suffix, ".csv")))
    readr::write_csv(tibble::tibble(),
                     file.path(out_dir, paste0("sampling_clean", file_suffix, ".csv")))
    readr::write_csv(all_errors,
                     file.path(out_dir, paste0("crosscheck_errors", file_suffix, ".csv")))
    readr::write_csv(all_warnings,
                     file.path(out_dir, paste0("crosscheck_warnings", file_suffix, ".csv")))
    overlap_diagnostics <- dplyr::bind_rows(
      stratum_keys_sampling %>%
        dplyr::transmute(flag_country, year, area, fishing_activity_category_eu_l6, species,
                         overlap_status = "sampling_only"),
      stratum_keys_landings %>%
        dplyr::transmute(flag_country, year, area, fishing_activity_category_eu_l6, species,
                         overlap_status = "landings_only")
    )
    readr::write_csv(overlap_diagnostics, strata_diag_file)
    check_log <- apply_crosscheck_scope(check_log)
    readr::write_csv(check_log,
                     file.path(out_dir, paste0("crosscheck_summary", file_suffix, ".csv")))
    return(out)
  } else if (length(sampling_only) > 0) {
    # Partial overlap: warn but continue - those strata will get RF = 0
    cat("  [WARN] Check 4.0:", length(sampling_only),
        "sampling strata without landings match\n")
    check_log <- dplyr::bind_rows(check_log,
                           log_check(
                             sampling_landings %>%
                               dplyr::mutate(stratum_key = paste(flag_country, year, area,
                                                                 fishing_activity_category_eu_l6,
                                                                 species, sep = "|")) %>%
                               dplyr::filter(stratum_key %in% sampling_only) %>%
                               dplyr::select(-stratum_key),
                             "4.0", "WARNING",
                             paste0(length(sampling_only),
                                    " sampling strata with no landings match (partial overlap)")))
    all_warnings <- dplyr::bind_rows(all_warnings,
                              sampling_landings %>%
                                dplyr::mutate(stratum_key = paste(flag_country, year, area,
                                                                  fishing_activity_category_eu_l6,
                                                                  species, sep = "|"),
                                              check_id = "4.0",
                                              check_message = "Sampling stratum has no landings match") %>%
                                dplyr::filter(stratum_key %in% sampling_only) %>%
                                dplyr::select(-stratum_key))
    cat("  [OK] Pre-check 4.0: partial overlap -", length(common_strata),
        "common strata found, proceeding with cross-checks.\n\n")
  } else {
    cat("  [OK] Check 4.0: all sampling strata matched in landings - OK\n\n")
  }
  
  # 4.1 Quarter construction for cross-checks (sampling Lan)  ----
  # quarter_cc is used directly for Lan cross-checks, but we also populate the
  # same derived quarter on Dis records so the final sampling_clean output keeps
  # a coherent quarter field for both catch categories.
  cat("4.1 Quarter construction for cross-checks (sampling Lan)\n")
  
  quarter_from_rdbfis_date <- function(x) {
    parsed <- suppressWarnings(as.Date(x, format = "%d/%m/%Y"))
    suppressWarnings(as.integer(ceiling(as.integer(format(parsed, "%m")) / 3)))
  }

  sampling_landings <- sampling_landings %>%
    dplyr::mutate(
      quarter_cc = dplyr::coalesce(
        as.integer(quarter_from_date),
        quarter_from_rdbfis_date(date)
      )
    )

  sampling_discards <- sampling_discards %>%
    dplyr::mutate(
      quarter_cc = dplyr::coalesce(
        as.integer(quarter_from_date),
        quarter_from_rdbfis_date(date)
      )
    )
  
  invalid_cross_weight_stratum <- sampling_landings %>%
    dplyr::filter(is.na(quarter_cc))
  
  if (nrow(invalid_cross_weight_stratum) > 0) {
    cat("  [WARN] Check 4.1:", nrow(invalid_cross_weight_stratum),
        "sampling Lan records with missing or non-parsable date\n")
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_cross_weight_stratum, "4.1", "WARNING", "Quarter not available for cross-checks (date missing or not parsable)"))
    all_warnings <- dplyr::bind_rows(all_warnings, invalid_cross_weight_stratum %>%
                                dplyr::mutate(check_id = "4.1", severity = "WARNING") %>%
                                dplyr::select(-dplyr::any_of(c(".row_id", "quarter_declared", "expected_quarter"))))
  } else {
    cat("  [OK] No issues detected\n")
  }
  
  sampling_landings_for_cross <- sampling_landings %>% dplyr::filter(!is.na(quarter_cc))
  
  cat("  Sampling landings retained for cross-checks:", formatC(nrow(sampling_landings_for_cross), format="d", big.mark=","), "
  
  ")
  # Aggregate by stratum  ----
  cat("Aggregation by stratum (flag_country x year x quarter x area x gear x species)\n")
  
  landings_strata <- landings %>%
    dplyr::group_by(flag_country, year, quarter, area, fishing_activity_category_eu_l6, species) %>%
    dplyr::summarise(
      total_landings_weight = sum(official_landings_weight, na.rm = TRUE),
      n_landings_records = dplyr::n(),
      .groups = "drop"
    )
  
  sampling_strata <- sampling_landings_for_cross %>%
    dplyr::mutate(quarter = quarter_cc) %>%
    dplyr::group_by(flag_country, year, quarter, area, fishing_activity_category_eu_l6, species,
             trip_code, station_code) %>%
    dplyr::summarise(
      has_LFD = any(!is.na(length_class) & number_at_length > 0),
      haul_sample_weight = collapse_repeated_weight(subsample_weight),
      .groups = "drop"
    ) %>%
    dplyr::mutate(haul_sample_weight = ifelse(is.na(haul_sample_weight), 0, haul_sample_weight)) %>%
    dplyr::group_by(flag_country, year, quarter, area, fishing_activity_category_eu_l6, species) %>%
    dplyr::summarise(
      has_LFD = any(has_LFD),
      total_sample_weight = sum(haul_sample_weight, na.rm = TRUE),
      n_sampling_records = dplyr::n(),
      .groups = "drop"
    )
  
  cross_strata <- landings_strata %>%
    dplyr::full_join(sampling_strata,
              by = c("flag_country", "year", "quarter", "area", "fishing_activity_category_eu_l6", "species")) %>%
    dplyr::mutate(
      total_landings_weight = tidyr::replace_na(total_landings_weight, 0),
      has_LFD = tidyr::replace_na(has_LFD, FALSE),
      total_sample_weight = tidyr::replace_na(total_sample_weight, 0)
    )
  
  cat("  Landings strata:", formatC(nrow(landings_strata), format="d", big.mark=","), "\n")
  cat("  Sampling strata:", formatC(nrow(sampling_strata), format="d", big.mark=","), "\n")
  cat("  Total strata:", formatC(nrow(cross_strata), format="d", big.mark=","), "\n\n")

  # 4.2 RF high-side plausibility pre-check (WARNING) ----
  # Raising factor (RF) = official_landings_weight_kg / total_sample_weight_kg.
  # Large RF means very small sample coverage relative to landings.
  # Interpretation used below: RF > 50 means coverage < 2%.
  # Sample weight > landings is treated separately in check 4.5.
  # RF > 50  -> sample is tiny relative to landings: suspect weight/unit error
  #              or a stratum mismatch. The low-side imbalance is handled in 4.5.
  cat("4.2 RF high-side plausibility pre-check (stratum-level)\n")
  cat("  Rule: RF = landings_kg / sample_weight_kg; flag RF > 50 (coverage < 2%)\n")

  rf_strata <- cross_strata %>%
    dplyr::filter(total_sample_weight > 0 & total_landings_weight > 0) %>%
    dplyr::mutate(
      total_sample_weight_kg = total_sample_weight / 1000,
      coverage_frac = total_sample_weight_kg / total_landings_weight,
      coverage_pct = 100 * coverage_frac,
      rf = total_landings_weight / total_sample_weight_kg
    )

  rf_invalid <- rf_strata %>% dplyr::filter(rf > 50)

  if (nrow(rf_invalid) > 0) {
    cat("  [WARN] Check 4.2:", nrow(rf_invalid),
        "strata with extreme RF / very low sample coverage\n")
    check_log <- dplyr::bind_rows(check_log,
                           log_check(rf_invalid %>% dplyr::select(-total_sample_weight_kg, -coverage_frac, -coverage_pct, -rf),
                                     "4.2", "WARNING",
                                     "Extreme raising factor (RF > 50; sample coverage < 2%) - check weight units or stratum matching"))
    all_warnings <- dplyr::bind_rows(all_warnings,
                              rf_invalid %>%
                                dplyr::select(-total_sample_weight_kg, -coverage_frac, -coverage_pct, -rf) %>%
                                dplyr::mutate(check_id = "4.2", severity = "WARNING"))
  } else {
    cat("  [OK] No issues detected\n")
  }
  cat("\n")

  # 4.3 Sampling with zero landings (WARNING)  ----
  cat("4.3 Sampling with ZERO landings (sampling Lan)\n")
  
  invalid_cross_weight_strata <- cross_strata %>%
    dplyr::filter(has_LFD & total_landings_weight == 0) %>%
    dplyr::select(flag_country, year, quarter, area, fishing_activity_category_eu_l6, species)
  
  if (nrow(invalid_cross_weight_strata) > 0) {
    cat("  [WARN] Check 4.3:", nrow(invalid_cross_weight_strata),
        "sampling strata without matching landings\n")
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_cross_weight_strata, "4.3", "WARNING", "Sampling with zero landings (stratum-level)"))
    all_warnings <- dplyr::bind_rows(all_warnings, invalid_cross_weight_strata %>%
                                dplyr::mutate(check_id = "4.3", severity = "WARNING") %>%
                                dplyr::select(-dplyr::any_of(c(".row_id", "quarter_declared", "expected_quarter"))))
  } else {
    cat("  [OK] No issues detected\n")
  }
  cat("\n")
  
  # 4.4 Landings without sampling (WARNING)  ----
  cat("4.4 Landings without sampling (coverage gap)\n")
  
  invalid_cross_quarter <- cross_strata %>%
    dplyr::filter(total_landings_weight > 0 & !has_LFD)
  
  if (nrow(invalid_cross_quarter) > 0) {
    cat("  [WARN] Check 4.4:", nrow(invalid_cross_quarter),
        "landings strata without sampling coverage\n")
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_cross_quarter, "4.4", "WARNING", "Landings without sampling (coverage gap)"))
    all_warnings <- dplyr::bind_rows(all_warnings, invalid_cross_quarter %>%
                                dplyr::mutate(check_id = "4.4", severity = "WARNING") %>%
                                dplyr::select(-dplyr::any_of(c(".row_id", "quarter_declared", "expected_quarter"))))
  } else {
    cat("  [OK] No issues detected\n")
  }
  cat("\n")
  
  # 4.5 Sample weight exceeds landings (ERROR)  ----
  # Units: subsample_weight is in grams (RCG Sampling template),
  #        official_landings_weight is in kg (RCG Landings template).
  #        Division by 1000 converts grams to kg for comparison.
  cat("4.5 Sample weight exceeds landings (sampling Lan)\n")
  
  invalid_cross_area_strata <- cross_strata %>%
    dplyr::filter(total_sample_weight > 0 & total_landings_weight > 0) %>%
    dplyr::filter(total_sample_weight/1000 > total_landings_weight * (1 + weight_tolerance)) %>%
    dplyr::select(flag_country, year, quarter, area, fishing_activity_category_eu_l6, species,
           total_landings_weight, total_sample_weight)
  
  if (nrow(invalid_cross_area_strata) > 0) {
    cat("  [ERROR] Check 4.5:", nrow(invalid_cross_area_strata),
        "strata where sample weight exceeds landings\n")
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_cross_area_strata, "4.5", "ERROR", "Sample weight exceeds landings (stratum-level)"))
    
    invalid_cross_area_rows <- sampling_landings %>%
      dplyr::mutate(quarter = quarter_cc) %>%
      dplyr::inner_join(invalid_cross_area_strata %>% dplyr::select(flag_country, year, quarter, area, fishing_activity_category_eu_l6, species),
                 by = c("flag_country","year","quarter","area","fishing_activity_category_eu_l6","species"))
    
    all_errors <- dplyr::bind_rows(all_errors, invalid_cross_area_rows %>%
                              dplyr::mutate(check_id = "4.5", severity = "ERROR") %>%
                              dplyr::select(-dplyr::any_of(c(".row_id", "quarter_declared", "expected_quarter"))))
    sampling_landings <- sampling_landings %>% dplyr::filter(!(.row_id %in% invalid_cross_area_rows$.row_id))
    n_sampling_landings_removed <- n_sampling_landings_removed + nrow(invalid_cross_area_rows)
  } else {
    cat("  [OK] No issues detected\n")
  }
  cat("\n")
  
  # 4.6 Month mismatch (WARNING)  ----
  cat("4.6 Month mismatch (sampling Lan month not in landings)\n")

  month_landings <- landings %>%
    dplyr::select(flag_country, year, quarter, month, area, fishing_activity_category_eu_l6, species) %>%
    dplyr::distinct() %>%
    dplyr::mutate(in_landings = TRUE)

  month_sampling <- sampling_landings %>%
    dplyr::mutate(quarter = quarter_from_date, month = month_from_date) %>%
    dplyr::select(flag_country, year, quarter, month, area, fishing_activity_category_eu_l6, species) %>%
    dplyr::distinct() %>%
    dplyr::mutate(in_sampling = TRUE)

  invalid_cross_month <- month_sampling %>%
    dplyr::left_join(month_landings,
              by = c("flag_country","year","quarter","month","area","fishing_activity_category_eu_l6","species")) %>%
    dplyr::filter(is.na(in_landings))

  if (nrow(invalid_cross_month) > 0) {
    cat("  [WARN] Check 4.6:", nrow(invalid_cross_month),
        "month combinations missing from landings\n")
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_cross_month, "4.6", "WARNING", "Sampling month not present in landings months"))
    all_warnings <- dplyr::bind_rows(all_warnings, invalid_cross_month %>% dplyr::mutate(check_id = "4.6", severity = "WARNING") %>% dplyr::select(-dplyr::any_of(c(".row_id", "quarter_declared", "expected_quarter"))))
  } else {
    cat("  [OK] No issues detected\n")
  }
  cat("\n")

  # 4.7 Trip-level species completeness (WARNING, bidirectional)  ----
  cat("4.7 Trip-level species completeness (landings vs sampling Lan)\n")

  if (!("harbour" %in% names(landings)) || !("harbour" %in% names(sampling_landings))) {
    cat("  [OK] Check 4.7: trip-level species completeness skipped\n")
  } else {
    trips_landings <- landings %>%
      dplyr::filter(!is.na(harbour)) %>%
      dplyr::mutate(harbour = as.character(harbour)) %>%
      dplyr::group_by(flag_country, year, month, harbour, area, fishing_activity_category_eu_l6) %>%
      dplyr::summarise(landings_species = list(unique(species)),
                n_species_landings = dplyr::n_distinct(species),
                .groups = "drop")

    trips_sampling <- sampling_landings %>%
      dplyr::filter(!is.na(harbour)) %>%
      dplyr::mutate(month = month_from_date, harbour = as.character(harbour)) %>%
      dplyr::group_by(flag_country, year, month, harbour, area, fishing_activity_category_eu_l6) %>%
      dplyr::summarise(sampling_species = list(unique(species)),
                n_species_sampling = dplyr::n_distinct(species),
                .groups = "drop")

    trip_comparison <- trips_landings %>%
      dplyr::inner_join(trips_sampling,
                 by = c("flag_country","year","month","harbour","area","fishing_activity_category_eu_l6"))

    if (nrow(trip_comparison) > 0) {

      invalid_cross_species_in_sampling <- trip_comparison %>%
        dplyr::rowwise() %>%
        dplyr::mutate(missing = list(setdiff(unlist(sampling_species), unlist(landings_species))),
               n_missing = length(unlist(missing))) %>%
        dplyr::filter(n_missing > 0) %>%
        tidyr::unnest(missing) %>%
        dplyr::select(flag_country, year, month, harbour, area, fishing_activity_category_eu_l6,
               species = missing, n_species_landings, n_species_sampling)

      if (nrow(invalid_cross_species_in_sampling) > 0) {
        check_log <- dplyr::bind_rows(check_log, log_check(invalid_cross_species_in_sampling, "4.7", "WARNING", "Species in sampling not in landings (trip-level)"))
        all_warnings <- dplyr::bind_rows(all_warnings, invalid_cross_species_in_sampling %>%
                                    dplyr::mutate(check_id = "4.7", severity = "WARNING") %>%
                                    dplyr::select(-dplyr::any_of(c(".row_id", "quarter_declared", "expected_quarter"))))
      }

      invalid_cross_species_in_landings <- trip_comparison %>%
        dplyr::rowwise() %>%
        dplyr::mutate(missing = list(setdiff(unlist(landings_species), unlist(sampling_species))),
               n_missing = length(unlist(missing))) %>%
        dplyr::filter(n_missing > 0) %>%
        tidyr::unnest(missing) %>%
        dplyr::select(flag_country, year, month, harbour, area, fishing_activity_category_eu_l6,
               species = missing, n_species_landings, n_species_sampling)

      if (nrow(invalid_cross_species_in_landings) > 0) {
        check_log <- dplyr::bind_rows(check_log, log_check(invalid_cross_species_in_landings, "4.7", "WARNING", "Species in landings not in sampling (trip-level)"))
        all_warnings <- dplyr::bind_rows(all_warnings, invalid_cross_species_in_landings %>%
                                    dplyr::mutate(check_id = "4.7", severity = "WARNING") %>%
                                    dplyr::select(-dplyr::any_of(c(".row_id", "quarter_declared", "expected_quarter"))))
      }

    } else {
      cat("  [OK] Check 4.7: trip-level species completeness skipped\n")
    }
  }
  cat("\n")

  # 4.8 Quarter alignment diagnostic (WARNING)  ----
  cat("4.8 Quarter alignment diagnostic (landings vs sampling Lan, by stratum)\n")

  landings_quarters <- landings %>%
    dplyr::group_by(flag_country, year, area, fishing_activity_category_eu_l6, species) %>%
    dplyr::summarise(.q_landings = paste(sort(unique(quarter)), collapse = "|"), .groups = "drop")

  sampling_quarters <- sampling_landings_for_cross %>%
    dplyr::group_by(flag_country, year, area, fishing_activity_category_eu_l6, species) %>%
    dplyr::summarise(.q_sampling = paste(sort(unique(quarter_cc)), collapse = "|"), .groups = "drop")

  invalid_cross_quarter_set <- landings_quarters %>%
    dplyr::full_join(sampling_quarters, by = c("flag_country", "year", "area", "fishing_activity_category_eu_l6", "species")) %>%
    dplyr::mutate(
      .q_landings = tidyr::replace_na(.q_landings, ""),
      .q_sampling = tidyr::replace_na(.q_sampling, "")
    ) %>%
    dplyr::filter(.q_landings != .q_sampling)

  if (nrow(invalid_cross_quarter_set) > 0) {
    cat("  [WARN] Check 4.8:", nrow(invalid_cross_quarter_set),
        "strata with different quarter sets between landings and sampling\n")
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_cross_quarter_set, "4.8", "WARNING",
                                                "Quarter set mismatch between landings and sampling (landings corrected from month, sampling derived from date)"))
    all_warnings <- dplyr::bind_rows(all_warnings, invalid_cross_quarter_set %>%
                                dplyr::mutate(check_id = "4.8", severity = "WARNING") %>%
                                dplyr::select(-dplyr::any_of(c(".row_id", "quarter_declared", "expected_quarter"))))
  } else {
    cat("  [OK] No issues detected\n")
  }
  cat("\n")

  # 4.9 Harbour mismatch (WARNING)  ----
  cat("4.9 Harbour mismatch (landings vs sampling Lan)\n")

  if (!("harbour" %in% names(landings)) || !("harbour" %in% names(sampling_landings))) {
    cat("  [OK] Check 4.9: harbour mismatch skipped\n")
  } else {
    harbour_landings <- landings %>%
      dplyr::mutate(harbour = as.character(harbour)) %>%
      dplyr::group_by(flag_country, year, quarter, area, fishing_activity_category_eu_l6, species) %>%
      dplyr::summarise(landings_harbours = paste(sort(unique(na.omit(harbour))), collapse = "|"), .groups = "drop")

    harbour_sampling <- sampling_landings %>%
      dplyr::mutate(quarter = quarter_from_date, harbour = as.character(harbour)) %>%
      dplyr::group_by(flag_country, year, quarter, area, fishing_activity_category_eu_l6, species) %>%
      dplyr::summarise(sampling_harbours = paste(sort(unique(na.omit(harbour))), collapse = "|"), .groups = "drop")

    invalid_harbour_mismatch <- harbour_landings %>%
      dplyr::inner_join(harbour_sampling,
                 by = c("flag_country","year","quarter","area","fishing_activity_category_eu_l6","species")) %>%
      dplyr::filter(landings_harbours != sampling_harbours)

    if (nrow(invalid_harbour_mismatch) > 0) {
      cat("  [WARN] Check 4.9:", nrow(invalid_harbour_mismatch),
          "strata with harbour mismatch between landings and sampling\n")
      check_log <- dplyr::bind_rows(check_log, log_check(invalid_harbour_mismatch, "4.9", "WARNING", "Harbour mismatch (stratum-level)"))
      all_warnings <- dplyr::bind_rows(all_warnings, invalid_harbour_mismatch %>%
                                  dplyr::mutate(check_id = "4.9", severity = "WARNING") %>%
                                  dplyr::select(-dplyr::any_of(c(".row_id", "quarter_declared", "expected_quarter"))))
    } else {
      cat("  [OK] No issues detected\n")
    }
  }
  cat("\n")

  # 4.10 Pre-raising stratum sufficiency (WARNING) ----
  # Flags strata that passed all QC checks but have too little sampling to
  # produce reliable LFD raising results:
  #   < 3 distinct length classes  -> LFD has too few support points
  #   coverage < 1%                -> sample weight negligible vs landings
  # Records are NOT removed; they are kept with a WARNING so the analyst
  # can decide before running raise_lfd().
  cat("4.10 Pre-raising stratum sufficiency check\n")

  sampling_sufficiency <- sampling_landings_for_cross %>%
    dplyr::mutate(quarter = quarter_cc) %>%
    dplyr::group_by(flag_country, year, quarter, area,
                    fishing_activity_category_eu_l6, species) %>%
    dplyr::summarise(
      n_length_classes = dplyr::n_distinct(
        length_class[!is.na(length_class) & !is.na(number_at_length) &
                     suppressWarnings(as.numeric(number_at_length)) > 0]),
      .groups = "drop"
    )

  insuff_strata <- cross_strata %>%
    dplyr::inner_join(sampling_sufficiency,
                      by = c("flag_country", "year", "quarter", "area",
                             "fishing_activity_category_eu_l6", "species")) %>%
    dplyr::filter(has_LFD & total_landings_weight > 0) %>%
    dplyr::mutate(
      total_sample_weight_kg = total_sample_weight / 1000,
      coverage_frac = total_sample_weight_kg / total_landings_weight
    ) %>%
    dplyr::filter(n_length_classes < 3 | coverage_frac < 0.01)

  if (nrow(insuff_strata) > 0) {
    n_few_lc  <- sum(insuff_strata$n_length_classes < 3)
    n_low_cov <- sum(insuff_strata$coverage_frac    < 0.01, na.rm = TRUE)
    cat("  [WARN] Check 4.10:", nrow(insuff_strata),
        "strata with insufficient sampling for reliable raising\n")
    check_log <- dplyr::bind_rows(check_log,
      log_check(insuff_strata %>%
                  dplyr::select(-total_sample_weight_kg, -coverage_frac),
                "4.10", "WARNING",
                "Pre-raising stratum insufficiency (< 3 length classes or coverage < 1%)"))
    all_warnings <- dplyr::bind_rows(all_warnings,
      insuff_strata %>%
        dplyr::select(-total_sample_weight_kg, -coverage_frac) %>%
        dplyr::mutate(check_id = "4.10", severity = "WARNING"))
  } else {
    cat("  [OK] Check 4.10: all strata have sufficient sampling coverage\n")
  }
  cat("\n")
  
  # SECTION 5: WRITE FINAL OUTPUTS ----
  
  cat("Writing output files...\n")
  
  # Clean datasets ----
  landings_out <- landings %>% dplyr::select(-dplyr::any_of(".row_id"))
  readr::write_csv(landings_out, file.path(out_dir, paste0("landings_clean", file_suffix, ".csv")))
  
  sampling_clean <- dplyr::bind_rows(
    sampling_landings %>% dplyr::select(-dplyr::any_of(c(".row_id", "date_parsed", "quarter_from_date", "month_from_date"))),
    sampling_discards %>% dplyr::select(-dplyr::any_of(c(".row_id", "date_parsed", "quarter_from_date", "month_from_date")))
  )
  readr::write_csv(sampling_clean, file.path(out_dir, paste0("sampling_clean", file_suffix, ".csv")))
  strata_diagnostics <- cross_strata %>%
    dplyr::left_join(
      sampling_sufficiency,
      by = c("flag_country", "year", "quarter", "area",
             "fishing_activity_category_eu_l6", "species")
    ) %>%
    dplyr::mutate(
      n_length_classes = tidyr::replace_na(n_length_classes, 0L),
      total_sample_weight_kg = total_sample_weight / 1000,
      rf = dplyr::if_else(
        total_sample_weight_kg > 0 & total_landings_weight > 0,
        total_landings_weight / total_sample_weight_kg,
        NA_real_
      ),
      coverage_frac = dplyr::if_else(
        total_landings_weight > 0,
        total_sample_weight_kg / total_landings_weight,
        NA_real_
      ),
      coverage_pct = dplyr::if_else(
        !is.na(coverage_frac),
        100 * coverage_frac,
        NA_real_
      ),
      overlap_status = dplyr::case_when(
        total_landings_weight > 0 & has_LFD  ~ "common",
        total_landings_weight == 0 & has_LFD ~ "sampling_only",
        total_landings_weight > 0 & !has_LFD ~ "landings_only",
        TRUE                                 ~ "none"
      ),
      rf_flag = !is.na(rf) & (rf > 50),
      sufficiency_flag = has_LFD & total_landings_weight > 0 &
        (n_length_classes < 3 | (!is.na(coverage_frac) & coverage_frac < 0.01))
    ) %>%
    dplyr::arrange(flag_country, year, quarter, area,
                   fishing_activity_category_eu_l6, species)
  readr::write_csv(strata_diagnostics, strata_diag_file)

  all_errors <- format_qc_issue_output(all_errors)
  all_warnings <- format_qc_issue_output(all_warnings)
  
  # Consolidated errors ----
  if (nrow(all_errors) > 0) {
    readr::write_csv(all_errors, file.path(out_dir, paste0("crosscheck_errors", file_suffix, ".csv")))
  } else {
    readr::write_csv(all_errors, file.path(out_dir, paste0("crosscheck_errors", file_suffix, ".csv")))
  }
  
  # Consolidated warnings ----
  if (nrow(all_warnings) > 0) {
    readr::write_csv(all_warnings, file.path(out_dir, paste0("crosscheck_warnings", file_suffix, ".csv")))
  } else {
    readr::write_csv(all_warnings, file.path(out_dir, paste0("crosscheck_warnings", file_suffix, ".csv")))
  }
  
  # Coverage data (species x year x area) ----
  coverage_levels <- c("common", "sampling only", "landings only", "none")
  coverage_palette <- c(
    "common" = "#2ca25f",
    "sampling only" = "#fc8d59",
    "landings only" = "#d73027",
    "none" = "#d9d9d9"
  )

  heatmap_data <- cross_strata %>%
    dplyr::group_by(flag_country, year, area, species) %>%
    dplyr::summarise(
      has_landings = any(total_landings_weight > 0, na.rm = TRUE),
      has_sampling = any(has_LFD, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      coverage = dplyr::case_when(
        has_landings & has_sampling  ~ "common",
        has_sampling & !has_landings ~ "sampling only",
        has_landings & !has_sampling ~ "landings only",
        TRUE                          ~ "none"
      ),
      coverage = factor(coverage, levels = coverage_levels),
      coverage_fill = unname(coverage_palette[as.character(coverage)])
    )

  # Coverage heatmap (species x year x area) ----
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    heatmap_plot <- ggplot2::ggplot(
      heatmap_data,
      ggplot2::aes(x = as.factor(year), y = species, fill = coverage_fill)
    ) +
      ggplot2::geom_tile(colour = "white", linewidth = 0.3) +
      ggplot2::geom_tile(
        data = tibble::tibble(
          year = rep(heatmap_data$year[1], length(coverage_levels)),
          species = rep(heatmap_data$species[1], length(coverage_levels)),
          coverage_fill = unname(coverage_palette)
        ),
        ggplot2::aes(x = as.factor(year), y = species, fill = coverage_fill),
        inherit.aes = FALSE,
        alpha = 0,
        show.legend = TRUE
      ) +
      ggplot2::scale_fill_identity(
        name = "Coverage",
        breaks = unname(coverage_palette[coverage_levels]),
        labels = coverage_levels,
        guide = ggplot2::guide_legend(
          title.position = "top",
          override.aes = list(colour = "grey60", linewidth = 0.3, alpha = 1)
        )
      ) +
      ggplot2::facet_wrap(~ area, ncol = 2, scales = "free_y") +
      ggplot2::labs(
        title    = "Coverage heatmap: species x year x area",
        subtitle = "Green = both | orange = sampling only | red = landings only | grey = none",
        x        = "Year",
        y        = "Species"
      ) +
      ggplot2::theme_bw(base_size = 9) +
      ggplot2::theme(
        axis.text.x  = ggplot2::element_text(angle = 45, hjust = 1),
        axis.text.y  = ggplot2::element_text(size = 7),
        strip.text   = ggplot2::element_text(size = 8, face = "bold"),
        panel.spacing = ggplot2::unit(0.4, "lines"),
        legend.position = "right",
        legend.title = ggplot2::element_text(face = "bold"),
        legend.key = ggplot2::element_rect(fill = "white", colour = "grey70"),
        legend.key.height = ggplot2::unit(0.7, "cm"),
        legend.box.margin = ggplot2::margin(0, 0, 0, 8),
        plot.margin = ggplot2::margin(5.5, 16, 5.5, 5.5)
      )

    heatmap_file <- file.path(raising_diag_dir, paste0("coverage_heatmap", file_suffix, ".png"))
    n_areas   <- dplyr::n_distinct(heatmap_data$area)
    n_species <- dplyr::n_distinct(heatmap_data$species)
    plot_h <- max(14, ceiling(n_species / max(1, n_areas)) * 0.5 + 6)
    ggplot2::ggsave(heatmap_file, plot = heatmap_plot,
                    width = 24, height = plot_h, units = "cm", dpi = 300, bg = "white")
    cat("  [OK] Coverage heatmap written\n")
  } else {
    cat("  [OK] Coverage heatmap skipped\n")
  }

  # Coverage gaps for data providers ----
  gaps_data <- heatmap_data %>%
    dplyr::filter(coverage == "landings only") %>%
    dplyr::left_join(
      cross_strata %>%
        dplyr::group_by(flag_country, year, area, species) %>%
        dplyr::summarise(total_landings_weight_kg = sum(total_landings_weight, na.rm = TRUE),
                         .groups = "drop"),
      by = c("flag_country", "year", "area", "species")
    ) %>%
    dplyr::select(flag_country, year, area, species,
                  coverage, total_landings_weight_kg) %>%
    dplyr::arrange(dplyr::desc(total_landings_weight_kg), flag_country, year, area, species)

  gaps_file <- file.path(raising_diag_dir, paste0("coverage_gaps_landings_only", file_suffix, ".csv"))
  readr::write_csv(gaps_data, gaps_file)
  if (nrow(gaps_data) > 0) {
    cat("  [WARN] Coverage gaps table written:", nrow(gaps_data),
        "strata without sampling coverage\n")
  } else {
    cat("  [OK] Coverage gaps table written\n")
  }

  # RF histogram ----
  if (requireNamespace("ggplot2", quietly = TRUE) && nrow(rf_strata) > 0) {
    rf_plot <- ggplot2::ggplot(rf_strata, ggplot2::aes(x = log10(rf))) +
      ggplot2::geom_histogram(bins = 40, fill = "#4575b4", colour = "white", alpha = 0.85) +
      ggplot2::geom_vline(xintercept = log10(50),   colour = "#d73027", linetype = "dashed") +
      ggplot2::scale_x_continuous(
        labels = function(x) format(round(x, 1), nsmall = 1, trim = TRUE)
      ) +
      ggplot2::labs(
        title    = "Raising Factor distribution (log10 scale)",
        subtitle = paste0("RF = landings / sample. Higher RF means lower sample coverage. ",
                          "Red dashed line marks RF = 50 (coverage = 2%). ",
                          nrow(rf_strata), " strata computed."),
        caption  = "Reference: RF = 50 corresponds to 2% sample coverage.",
        x = "log10(RF = landings / sample)",
        y = "Number of strata"
      ) +
      ggplot2::theme_bw(base_size = 10) +
      ggplot2::theme(
        plot.title.position = "plot",
        plot.title = ggplot2::element_text(face = "bold", margin = ggplot2::margin(b = 4)),
        plot.subtitle = ggplot2::element_text(lineheight = 1.05, margin = ggplot2::margin(b = 8)),
        plot.caption = ggplot2::element_text(hjust = 0, colour = "grey30",
                                             margin = ggplot2::margin(t = 8)),
        axis.text.x = ggplot2::element_text(size = 8.5, margin = ggplot2::margin(t = 4)),
        axis.text.y = ggplot2::element_text(size = 8.5),
        panel.grid.minor = ggplot2::element_blank(),
        plot.margin = ggplot2::margin(10, 14, 10, 12)
      )

    rf_hist_file <- file.path(raising_diag_dir, paste0("RF_histogram", file_suffix, ".png"))
    ggplot2::ggsave(rf_hist_file, plot = rf_plot,
                    width = 22, height = 14, units = "cm", dpi = 300, bg = "white")
    cat("  [OK] RF histogram written\n")
  } else {
    cat("  [OK] RF histogram skipped\n")
  }

  # Summary (one row per check) ----
  check_log <- apply_crosscheck_scope(check_log)
  readr::write_csv(check_log, file.path(out_dir, paste0("crosscheck_summary", file_suffix, ".csv")))
  cat("  [OK] Output tables written\n")

  # QC summary bar chart ----
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    chart_file <- file.path(out_dir, paste0("qc_summary_chart", file_suffix, ".png"))
    if (nrow(check_log) > 0) {
      chart_data <- check_log %>%
        dplyr::mutate(
          severity    = factor(severity, levels = c("ERROR", "WARNING")),
          scope_label = dplyr::case_when(
            is.na(scope) | scope == "" ~ "record",
            TRUE ~ scope
          ),
          scope_prefix = dplyr::if_else(
            scope_label == "record",
            "",
            paste0("[", scope_label, "] ")
          ),
          check_label_text = paste0(
            scope_prefix,
            check_id, "\n",
            stringr::str_wrap(stringr::str_trunc(message, 64), width = 28)
          ),
          check_label = factor(
            check_label_text,
            levels = rev(unique(check_label_text))
          ),
          label_lines = stringr::str_count(check_label_text, "\n") + 1L,
          label_n = format(n_records, big.mark = ",", scientific = FALSE, trim = TRUE),
          severity_fill = dplyr::case_when(
            severity == "ERROR" ~ "#d73027",
            TRUE ~ "#fc8d59"
          )
        )
      qc_chart <- ggplot2::ggplot(
        chart_data,
        ggplot2::aes(x = check_label, y = n_records, fill = severity_fill)
      ) +
        ggplot2::geom_col(width = 0.75) +
        ggplot2::geom_col(
          data = dplyr::tibble(
            check_label = factor(rep(levels(chart_data$check_label)[1], 2),
                                 levels = levels(chart_data$check_label)),
            n_records = c(0, 0),
            severity_fill = c("#d73027", "#fc8d59")
          ),
          ggplot2::aes(x = check_label, y = n_records, fill = severity_fill),
          inherit.aes = FALSE,
          width = 0.75,
          alpha = 0,
          show.legend = TRUE
        ) +
        ggplot2::geom_text(
          ggplot2::aes(label = label_n),
          hjust = -0.15,
          size = 3
        ) +
        ggplot2::scale_fill_identity(
          name   = "Severity",
          breaks = c("#d73027", "#fc8d59"),
          labels = c("ERROR", "WARNING"),
          guide  = ggplot2::guide_legend(
            title.position = "top",
            override.aes = list(alpha = 1)
          )
        ) +
        ggplot2::scale_y_continuous(
          expand = ggplot2::expansion(mult = c(0, 0.18))
        ) +
        ggplot2::coord_flip(clip = "off") +
        ggplot2::labs(
          title = "Cross-checks: flagged records per check",
          subtitle = "Overview of landings-sampling consistency findings. Labels show the check scope.",
          x = NULL, y = "Records or strata flagged"
        ) +
        ggplot2::theme_bw(base_size = 10) +
        ggplot2::theme(
          legend.position = "top",
          legend.justification = "left",
          legend.box.margin = ggplot2::margin(0, 0, 6, 0),
          legend.title = ggplot2::element_text(face = "bold"),
          axis.text.y = ggplot2::element_text(size = 8, lineheight = 0.95,
                                              margin = ggplot2::margin(r = 8)),
          axis.text.x = ggplot2::element_text(size = 8,
                                              margin = ggplot2::margin(t = 4)),
          panel.grid.major.y = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          plot.title.position = "plot",
          plot.title = ggplot2::element_text(face = "bold", margin = ggplot2::margin(b = 4)),
          plot.subtitle = ggplot2::element_text(lineheight = 1.05, margin = ggplot2::margin(b = 10)),
          plot.margin = ggplot2::margin(10, 55, 10, 14)
        )
      ggplot2::ggsave(chart_file, plot = qc_chart,
                      width = 28,
                      height = max(12, sum(chart_data$label_lines) * 0.45 + 4),
                      units = "cm", dpi = 300, bg = "white")
      cat("  [OK] QC summary chart written\n")
    } else {
      qc_chart <- ggplot2::ggplot() +
        ggplot2::annotate(
          "text",
          x = 1, y = 1,
          label = "No QC findings\nAll cross-checks passed",
          size = 5,
          fontface = "bold"
        ) +
        ggplot2::xlim(0.5, 1.5) +
        ggplot2::ylim(0.5, 1.5) +
        ggplot2::labs(
          title = "Cross-checks: flagged records per check",
          subtitle = "No checks triggered for the current filters"
        ) +
        ggplot2::theme_void(base_size = 10) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold"),
          plot.subtitle = ggplot2::element_text(colour = "grey30")
        )
      ggplot2::ggsave(chart_file, plot = qc_chart,
                      width = 18, height = 10, units = "cm", dpi = 300, bg = "white")
      cat("  [OK] QC summary chart written\n")
    }
  } else {
    cat("  [OK] QC summary chart skipped\n")
  }

  # Final console summary ----
  n_err  <- sum(check_log$severity == "ERROR",   na.rm = TRUE)
  n_warn <- sum(check_log$severity == "WARNING",  na.rm = TRUE)
  n_err_rec  <- if (nrow(all_errors)   > 0) nrow(all_errors)   else 0L
  n_warn_rec <- if (nrow(all_warnings) > 0) nrow(all_warnings) else 0L
  
  cat("\n========================================\n")
  cat("crosschecks Completed\n")
  cat("========================================\n")

  if (n_err > 0) {
    cat("[ERROR] Errors found:", n_err_rec, "record(s)\n")
  } else {
    cat("[OK] Errors found: 0\n")
  }

  if (n_warn > 0) {
    cat("[WARN] Warnings found:", n_warn_rec, "record(s)\n")
  } else {
    cat("[OK] Warnings found: 0\n")
  }

  if (nrow(gaps_data) > 0) {
    cat("[WARN] Landings-only strata without sampling coverage:", nrow(gaps_data), "\n")
  }
  cat("[OK] Clean landings records:", nrow(landings_out), "\n")
  cat("[OK] Clean sampling records:", nrow(sampling_clean), "\n")
  
  
  drop_cols <- c(".row_id", "date_parsed", "quarter_from_date", "month_from_date")
  out <- list(
    landings_clean  = landings_out,
    sampling_clean  = dplyr::bind_rows(
      sampling_landings %>% dplyr::select(-dplyr::any_of(drop_cols)),
      sampling_discards %>% dplyr::select(-dplyr::any_of(drop_cols))
    ),
    errors        = all_errors,
    warnings      = all_warnings,
    summary       = check_log,
    output_dir    = out_dir,
    output_suffix = file_suffix
  )
  return(out)
}

