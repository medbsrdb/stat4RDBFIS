#' Landings Quality Control for LFD Raising - stat4RDBFIS
#'
#' Validate RCG landings data before downstream sampling QC and cross-checks.
#' This function is designed as the first QC step in the stat4RDBFIS workflow.
#'
#' @param landings_df data.frame. Landings template already loaded in memory.
#' @param out_dir character. Output directory where clean data, logs, and
#'   charts are written.
#' @param validation_lists_path character or \code{NULL}. Path to
#'   \code{validation_lists.json}. When \code{NULL}, the bundled package copy
#'   is used.
#' @param MS character vector or NULL. Optional subset of \code{flag_country}
#'   ISO3 codes.
#' @param GSA character vector or NULL. Optional subset of GSA codes.
#' @param YEAR integer vector or NULL. Optional subset of years.
#'
#' @return A named list with elements \code{landings_df}, \code{errors},
#'   \code{warnings}, \code{summary}, \code{output_dir}, and
#'   \code{file_suffix}.
#'
#' @details
#' Records flagged as \code{ERROR} are removed from the clean dataset.
#' Records flagged as \code{WARNING} are retained and reported for manual review.
#'
#' @seealso \code{\link{samplings_qc}}, \code{\link{crosschecks}}
#' @family qc functions
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' qc_land <- landings_qc(
#'   landings_df = landings_df,
#'   validation_lists_path = system.file(
#'     "extdata", "validation_lists.json", package = "stat4RDBFIS"
#'   )
#' )
#' }
#'
#' @author stat4RDBFIS Core Team
#' @export

landings_qc <- function(
  landings_df,
  out_dir = file.path("Consistency_Checks", "qc_outputs_landings"),
  validation_lists_path = NULL,
  MS = NULL,
  GSA = NULL,
  YEAR = NULL
) {
  # landings_qc: Landings Quality Control for LFD Raising (stat4RDBFIS)
  # Validates RCG_Landings_template.csv data against RCG spec constraints.
  # ERROR   -> record excluded from clean dataset and from LFD raising
  # WARNING -> record kept, flagged for manual review

  # DEPENDENCIES (using :: notation; no global library() side effects) ----
  for (pkg in c("dplyr", "tidyr", "readr", "stringr", "jsonlite")) {
    if (!requireNamespace(pkg, quietly = TRUE))
      stop("Required package not installed: ", pkg,
           "\n  Install with: install.packages(\"", pkg, "\")")
  }
  
  # Source-safe pipe bridge: keeps the tutorial workflow working when files are sourced.
  `%>%` <- dplyr::`%>%`
  ok_tag <- "[OK]"
  warn_tag <- "[WARN]"
  err_tag <- "[ERROR]"
  arrow_tag <- "->"
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
      grepl("^(=+|landings_qc:|Loading validation lists\\.\\.\\.|Writing output files\\.\\.\\.|Step\\b|landings_qc Completed)", msg_trim) ||
      grepl("^\\[OK\\]|^\\[WARN\\]|^\\[ERROR\\]", msg_trim) ||
      grepl("^\\s+\\[OK\\]|^\\s+\\[WARN\\]|^\\s+\\[ERROR\\]", msg_clean)
    drop <- grepl("\\.(csv|png)\\b", msg_trim, ignore.case = TRUE) ||
      grepl("Records affected:|Outputs saved to|Next step|Filter applied|Output suffix|Species affected:|Price/kg <|Price/kg >|Possible causes:|Strata affected:|Duplicates:|Missing:", msg_trim)
    if (keep && !drop)
      raw_cat(msg_clean, file = file, sep = "", fill = fill, labels = labels, append = append)
  }
  
  # LOAD VALIDATION LISTS ----
  cat("========================================\n")
  cat("landings_qc: Landings Quality Control\n")
  cat("========================================\n\n")
  
  validation_lists_path <- stat4rdbfis_extdata_path(
    "validation_lists.json",
    validation_lists_path
  )
  cat("Loading validation lists...\n")
  VLISTS <- load_validation_lists(validation_lists_path)
  
  # Extract validation codes
  VALID_COUNTRIES <- VLISTS$country$code
  VALID_GSA <- VLISTS$gsa$code
  VALID_GEARS_L6 <- VLISTS$gears_l6$code
  VALID_SPECIES <- VLISTS$species$code
  
  # Harbour lookup: named list -> VALID_HARBOURS[["GRC"]] = vector of valid codes
  VALID_HARBOURS <- if (!is.null(VLISTS$harbours)) {
    h <- VLISTS$harbours
    setNames(lapply(seq_len(nrow(h)), function(i) h$harbours[[i]]$code), h$country)
  } else NULL

    cat("  [OK] Loaded", length(VALID_COUNTRIES), "countries\n")
  cat("  [OK] Loaded", length(VALID_GSA), "GSA codes\n")
  cat("  [OK] Loaded", length(VALID_GEARS_L6), "EU L6 gears\n")
  cat("  [OK] Loaded", length(VALID_SPECIES), "species codes\n")
  if (!is.null(VALID_HARBOURS))
    cat("  [OK] Loaded harbour codes for", length(VALID_HARBOURS), "countries\n")
  cat("\n")
  
  # Year range computed after normalization; see Step 3.1

  # MANDATORY COLUMNS ----
  MANDATORY_COLS <- c(
    "flag_country", "year", "quarter", "area", "species",
    "fishing_activity_category_eu_l6", 
    "official_landings_weight", "official_landings_value"
  )
  
  # MAIN SCRIPT ----
  
  # Create output directory
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # Build file suffix early; available throughout the entire function
  suffix_parts <- c()
  if (!is.null(MS))   suffix_parts <- c(suffix_parts, paste0("MS-",   paste(MS,   collapse = "-")))
  if (!is.null(GSA))  suffix_parts <- c(suffix_parts, paste0("GSA-",  paste(GSA,  collapse = "-")))
  if (!is.null(YEAR)) suffix_parts <- c(suffix_parts, paste0("YEAR-", paste(YEAR, collapse = "-")))
  file_suffix <- if (length(suffix_parts) > 0) paste0("_", paste(suffix_parts, collapse = "_")) else ""

  
  # Initialize logs
  check_log <- dplyr::tibble()
  all_errors <- dplyr::tibble()
  all_warnings <- dplyr::tibble()
  
  # 3.1 LOAD AND NORMALIZE ----
  cat("Step 3.1: Loading and normalizing data...\n")
  
  if (missing(landings_df) || is.null(landings_df)) {
    stop("ERROR: landings_df is missing or NULL")
  }
  
  # Load as character first (mimic read_delim behaviour)
  landings_raw <- landings_df %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  cat("  [OK] Loaded:", nrow(landings_raw), "records\n")

  names(landings_raw) <- trimws(names(landings_raw))

  # 3.2 STRUCTURAL TEMPLATE CHECKS (run BEFORE numeric normalization) ----
  # Must precede toupper() and as.numeric() calls: if mandatory columns are
  # absent those operations fail with cryptic "object not found" errors.
  cat("Step 3.2: Structural template checks (pre-normalization)...\n")

  missing_cols <- setdiff(MANDATORY_COLS, names(landings_raw))
  if (length(missing_cols) > 0) {
    cat("  [ERROR] FATAL ERROR: Missing mandatory columns:", paste(missing_cols, collapse = ", "), "\n")
    writeLines(
      paste("FATAL ERROR: Missing mandatory columns:", paste(missing_cols, collapse = ", ")),
      file.path(out_dir, "FATAL_ERROR.txt")
    )
    stop("Missing mandatory columns. Script stopped.")
  }

  if (any(duplicated(names(landings_raw)))) {
    dup_cols <- names(landings_raw)[duplicated(names(landings_raw))]
    cat("  [ERROR] FATAL ERROR: Duplicated column names:", paste(dup_cols, collapse = ", "), "\n")
    writeLines(
      paste("FATAL ERROR: Duplicated column names:", paste(dup_cols, collapse = ", ")),
      file.path(out_dir, "FATAL_ERROR.txt")
    )
    stop("Duplicated column names. Script stopped.")
  }

  cat("  [OK] All mandatory columns present\n")
  cat("  [OK] No duplicate column names\n\n")

  # Trim spaces
  landings <- landings_raw %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.character), trimws)) %>%
    dplyr::mutate(.row_id = dplyr::row_number())

  # Apply UPPERCASE only to code fields that require it
  landings <- landings %>%
    dplyr::mutate(
      flag_country = toupper(flag_country),
      area = toupper(area),
      species = toupper(species),
      fishing_activity_category_eu_l6 = toupper(fishing_activity_category_eu_l6)
    )
  
  # Store originals for error reporting
  landings <- landings %>%
    dplyr::mutate(
      year_orig = year,
      quarter_orig = quarter,
      month_orig = month,
      weight_orig = official_landings_weight,
      value_orig = official_landings_value
    )
  
  # Convert numeric fields
  landings <- landings %>%
    dplyr::mutate(
      year = suppressWarnings(as.numeric(year)),
      quarter = suppressWarnings(as.numeric(quarter)),
      month = suppressWarnings(as.numeric(month)),
      official_landings_weight = suppressWarnings(as.numeric(official_landings_weight)),
      official_landings_value = suppressWarnings(as.numeric(official_landings_value))
    )
  
  # Check conversion failures on critical raising fields
  conversion_failures <- landings %>%
    dplyr::filter(
      is.na(year) | is.na(quarter) |
        is.na(official_landings_weight) |
        (!is.na(month_orig) & month_orig != "" & is.na(month))
    )
  
  if(nrow(conversion_failures) > 0) {
    print_check_report(conversion_failures, "3.1.4", "ERROR",
                       "Numeric conversion failure in year, quarter, weight, or month (when provided)",
                       NULL, NULL,
                       error_file = paste0("landings_qc_errors", file_suffix, ".csv"))
  
    check_log <- dplyr::bind_rows(check_log,
                           log_check(conversion_failures, "3.1.4", "ERROR", "Numeric conversion failure on critical fields")
    )
    all_errors <- dplyr::bind_rows(all_errors,
                            conversion_failures %>% dplyr::mutate(check_id = "3.1.4", check_message = "Numeric conversion failure on critical fields")
    )
    landings <- landings %>% dplyr::filter(!(.row_id %in% conversion_failures$.row_id))
  }
  
  # Optional field: official_landings_value
  value_conversion_failures <- landings %>%
    dplyr::filter(!is.na(value_orig) & value_orig != "" & is.na(official_landings_value))
  
  if(nrow(value_conversion_failures) > 0) {
    cat("  [WARN] WARNING 3.1.5: Numeric conversion failure in official_landings_value (optional for raising)\n")
    check_log <- dplyr::bind_rows(check_log,
                           log_check(value_conversion_failures, "3.1.5", "WARNING",
                                     "Failed to convert official_landings_value to numeric (optional for raising)"))
    all_warnings <- dplyr::bind_rows(all_warnings,
                              value_conversion_failures %>% dplyr::mutate(check_id = "3.1.5", check_message = "Numeric conversion failure in official_landings_value"))
  }
  
  cat("  [OK] After normalization:", nrow(landings), "records\n\n")
  # Compute year range from normalized data
  # APPLY USER FILTERS ----
  filters_applied <- FALSE
  
  if (!is.null(MS)) {
    before <- nrow(landings)
    landings <- landings %>% dplyr::filter(flag_country %in% MS)
    cat("  [OK] Country filter:", before, "->", nrow(landings), "records\n")
    filters_applied <- TRUE
  }
  if (!is.null(GSA)) {
    before <- nrow(landings)
    landings <- landings %>% dplyr::filter(area %in% GSA)
    cat("  [OK] Area filter:", before, "->", nrow(landings), "records\n")
    filters_applied <- TRUE
  }
  if (!is.null(YEAR)) {
    before <- nrow(landings)
    landings <- landings %>% dplyr::filter(year %in% YEAR)
    cat("  [OK] Year filter:", before, "->", nrow(landings), "records\n")
    filters_applied <- TRUE
  }
  
  # 3.3 CODE LIST VALIDATION ----
  cat("Step 3.3: Code list validation...\n")
  
  # 3.3.1 flag_country ----
  invalid_country <- landings %>% dplyr::filter(!(flag_country %in% VALID_COUNTRIES))
  if(nrow(invalid_country) > 0) {
    print_check_report(invalid_country, "3.3.1", "ERROR", "Invalid country code",
                       "flag_country", VALID_COUNTRIES,
                       error_file = paste0("landings_qc_errors", file_suffix, ".csv"))
    check_log <- dplyr::bind_rows(check_log,
                           log_check(invalid_country, "3.3.1", "ERROR", "Invalid country code",
                                     invalid_country$flag_country, VALID_COUNTRIES)
    )
    all_errors <- dplyr::bind_rows(all_errors,
                            invalid_country %>% dplyr::mutate(check_id = "3.3.1", check_message = "Invalid flag_country")
    )
    landings <- landings %>% dplyr::filter(!(.row_id %in% invalid_country$.row_id))
  } else {
    cat("  [OK] Check 3.3.1: flag_country - all values valid\n")
  }
  
  # 3.3.2 area (GSA) ----
  invalid_gsa <- landings %>% dplyr::filter(!(area %in% VALID_GSA))
  if(nrow(invalid_gsa) > 0) {
    print_check_report(invalid_gsa, "3.3.2", "ERROR", "Invalid GSA code",
                       "area", VALID_GSA,
                       error_file = paste0("landings_qc_errors", file_suffix, ".csv"))
    check_log <- dplyr::bind_rows(check_log,
                           log_check(invalid_gsa, "3.3.2", "ERROR", "Invalid GSA code",
                                     invalid_gsa$area, VALID_GSA)
    )
    all_errors <- dplyr::bind_rows(all_errors,
                            invalid_gsa %>% dplyr::mutate(check_id = "3.3.2", check_message = "Invalid GSA")
    )
    landings <- landings %>% dplyr::filter(!(.row_id %in% invalid_gsa$.row_id))
  } else {
    cat("  [OK] Check 3.3.2: area (GSA) - all values valid\n")
  }
  
  # 3.3.3 species (ASFIS) ----
  invalid_species <- landings %>% dplyr::filter(!(species %in% VALID_SPECIES))
  if(nrow(invalid_species) > 0) {
    print_check_report(invalid_species, "3.3.3", "ERROR", "Invalid ASFIS species code",
                       "species", head(VALID_SPECIES, 20),
                       error_file = paste0("landings_qc_errors", file_suffix, ".csv"))
    check_log <- dplyr::bind_rows(check_log,
                           log_check(invalid_species, "3.3.3", "ERROR", "Invalid ASFIS species code",
                                     invalid_species$species, head(VALID_SPECIES, 20))
    )
    all_errors <- dplyr::bind_rows(all_errors,
                            invalid_species %>% dplyr::mutate(check_id = "3.3.3", check_message = "Invalid species")
    )
    landings <- landings %>% dplyr::filter(!(.row_id %in% invalid_species$.row_id))
  } else {
    cat("  [OK] Check 3.3.3: species (ASFIS) - all values valid\n")
  }
  
  # 3.3.4 fishing_activity_category_eu_l6 ----
  invalid_gear <- landings %>% dplyr::filter(!(fishing_activity_category_eu_l6 %in% VALID_GEARS_L6))
  if(nrow(invalid_gear) > 0) {
    print_check_report(invalid_gear, "3.3.4", "ERROR", "Invalid EU L6 gear code",
                       "fishing_activity_category_eu_l6", VALID_GEARS_L6,
                       error_file = paste0("landings_qc_errors", file_suffix, ".csv"))
    check_log <- dplyr::bind_rows(check_log,
                           log_check(invalid_gear, "3.3.4", "ERROR", "Invalid EU L6 gear code",
                                     invalid_gear$fishing_activity_category_eu_l6, VALID_GEARS_L6)
    )
    all_errors <- dplyr::bind_rows(all_errors,
                            invalid_gear %>% dplyr::mutate(check_id = "3.3.4", check_message = "Invalid gear")
    )
    landings <- landings %>% dplyr::filter(!(.row_id %in% invalid_gear$.row_id))
  } else {
    cat("  [OK] Check 3.3.4: fishing_activity_category_eu_l6 - all values valid\n")
  }
  
  # 3.3.5 fishing_activity_category_national - max 14 characters (WARNING) ----
  if ("fishing_activity_category_national" %in% names(landings)) {
    invalid_natgear_len <- landings %>%
      dplyr::filter(!is.na(fishing_activity_category_national) &
               nchar(as.character(fishing_activity_category_national)) > 14)
    if (nrow(invalid_natgear_len) > 0) {
      cat("  [WARN] WARNING 3.3.5:", nrow(invalid_natgear_len),
          "records where fishing_activity_category_national exceeds 14 characters\n")
      check_log <- dplyr::bind_rows(check_log,
                             log_check(invalid_natgear_len, "3.3.5", "WARNING",
                                       "fishing_activity_category_national exceeds 14 characters (RCG spec: length <= 14)"))
      all_warnings <- dplyr::bind_rows(all_warnings,
                                invalid_natgear_len %>%
                                  dplyr::mutate(check_id = "3.3.5",
                                         check_message = "fishing_activity_category_national > 14 chars"))
    } else {
      cat("  [OK] Check 3.3.5: fishing_activity_category_national length - OK\n")
    }
  }
  
  # 3.3.6 harbour - validate against country-specific list (WARNING) ----
  if (!is.null(VALID_HARBOURS) &&
      all(c("harbour", "flag_country") %in% names(landings))) {
    invalid_harbour <- landings %>%
      dplyr::filter(!is.na(harbour) & trimws(as.character(harbour)) != "" &
               !is.na(flag_country) & flag_country %in% names(VALID_HARBOURS)) %>%
      dplyr::rowwise() %>%
      dplyr::filter({
        valid_h <- tryCatch(VALID_HARBOURS[[flag_country]], error = function(e) NULL)
        !is.null(valid_h) && !(trimws(as.character(harbour)) %in% valid_h)
      }) %>%
      dplyr::ungroup()
    if (nrow(invalid_harbour) > 0) {
      cat("  [WARN] WARNING 3.3.6:", nrow(invalid_harbour),
          "records with harbour code not valid for declared flag_country\n")
      check_log <- dplyr::bind_rows(check_log,
                             log_check(invalid_harbour, "3.3.6", "WARNING",
                                       "Harbour code not in country-specific allowed list (Harbours_2025)"))
      all_warnings <- dplyr::bind_rows(all_warnings,
                                invalid_harbour %>%
                                  dplyr::mutate(check_id = "3.3.6",
                                         check_message = paste0("Invalid harbour [", harbour,
                                                                "] for country [", flag_country, "]")))
    } else {
      cat("  [OK] Check 3.3.6: harbour codes - all valid for declared country\n")
    }
  } else if (is.null(VALID_HARBOURS)) {
    cat("  [OK] Check 3.3.6: harbour validation skipped\n")
  }
  
  cat("  [OK] After code list validation:", nrow(landings), "records\n\n")
  
  # 3.4 NUMERIC AND TEMPORAL VALIDATION ----
  cat("Step 3.4: Numeric and temporal validation...\n")
  
  # 3.4.2 quarter (1-4) ----
  invalid_quarter <- landings %>% dplyr::filter(!(quarter %in% 1:4))
  if(nrow(invalid_quarter) > 0) {
    cat("  [ERROR] ERROR 3.4.2: Invalid quarter (must be 1, 2, 3, or 4)\n")
    check_log <- dplyr::bind_rows(check_log,
                           log_check(invalid_quarter, "3.4.2", "ERROR", "Quarter must be 1-4")
    )
    all_errors <- dplyr::bind_rows(all_errors,
                            invalid_quarter %>% dplyr::mutate(check_id = "3.4.2", check_message = "Invalid quarter")
    )
    landings <- landings %>% dplyr::filter(!(.row_id %in% invalid_quarter$.row_id))
  } else {
    cat("  [OK] Check 3.4.2: quarter (1-4) - all values valid\n")
  }
  
  # 3.4.3 month (1-12 when present) ----
  invalid_month <- landings %>%
    dplyr::filter(!is.na(month) & (month < 1 | month > 12))
  if(nrow(invalid_month) > 0) {
    cat("  [WARN] WARNING 3.4.3: Invalid month (must be 1-12)\n")
    check_log <- dplyr::bind_rows(check_log,
                           log_check(invalid_month, "3.4.3", "WARNING", "Month must be 1-12")
    )
    all_warnings <- dplyr::bind_rows(all_warnings,
                            invalid_month %>% dplyr::mutate(check_id = "3.4.3", check_message = "Invalid month")
    )
  } else {
    cat("  [OK] Check 3.4.3: month (1-12) - all values valid\n")
  }
  
  # 3.4.4 official_landings_weight > 0 ----
  invalid_weight <- landings %>% dplyr::filter(official_landings_weight <= 0)
  if(nrow(invalid_weight) > 0) {
    cat("  [ERROR] ERROR 3.4.4: Landings weight must be > 0\n")
    check_log <- dplyr::bind_rows(check_log,
                           log_check(invalid_weight, "3.4.4", "ERROR", "Landings weight must be > 0")
    )
    all_errors <- dplyr::bind_rows(all_errors,
                            invalid_weight %>% dplyr::mutate(check_id = "3.4.4", check_message = "Weight <= 0")
    )
    landings <- landings %>% dplyr::filter(!(.row_id %in% invalid_weight$.row_id))
  } else {
    cat("  [OK] Check 3.4.4: official_landings_weight > 0 - all values valid\n")
  }
  
  # 3.4.5 official_landings_value: WARNING when <= 0 ----
  # Per Word: Integer > 0. Zero is not meaningful (0 euro revenue),
  # but record is kept; user must review.
  # NOTE: Records with value <= 0 ARE INCLUDED in raising but flagged for review.
  invalid_value <- landings %>% dplyr::filter(official_landings_value <= 0)
  if(nrow(invalid_value) > 0) {
    cat("  [WARN] WARNING 3.4.5: Landings value must be > 0 (found <= 0)\n")
    check_log <- dplyr::bind_rows(check_log,
                           log_check(invalid_value, "3.4.5", "WARNING", "Landings value <= 0 (must be > 0 per specification)")
    )
    all_warnings <- dplyr::bind_rows(all_warnings,
                            invalid_value %>% dplyr::mutate(check_id = "3.4.5", check_message = "Value <= 0")
    )
  } else {
    cat("  [OK] Check 3.4.5: official_landings_value > 0 - all values valid\n")
  }
  
  cat("  [OK] After numeric/temporal checks:", nrow(landings), "records\n\n")
  
  # 3.5 QUARTER-MONTH CONSISTENCY ----
  cat("Step 3.5: Quarter-month consistency...\n")
  
  quarter_month_inconsistent <- landings %>%
    dplyr::filter(!is.na(month)) %>%
    dplyr::mutate(
      quarter_from_month = dplyr::case_when(
        month %in% 1:3 ~ 1L,
        month %in% 4:6 ~ 2L,
        month %in% 7:9 ~ 3L,
        month %in% 10:12 ~ 4L,
        TRUE ~ NA_integer_
      )
    ) %>%
    dplyr::filter(quarter != quarter_from_month)
  
  if(nrow(quarter_month_inconsistent) > 0) {
    cat("  [WARN] WARNING 3.5: Quarter-month inconsistency\n")
    check_log <- dplyr::bind_rows(check_log,
                           log_check(quarter_month_inconsistent, "3.5", "WARNING", "Quarter-month mismatch")
    )
    all_warnings <- dplyr::bind_rows(all_warnings,
                            quarter_month_inconsistent %>% dplyr::mutate(check_id = "3.5", check_message = "Quarter-month mismatch")
    )
  } else {
    cat("  [OK] Check 3.5: quarter-month consistency - all values consistent\n")
  }
  
  cat("  [OK] After consistency checks:", nrow(landings), "records\n\n")
  
  # 3.6 DUPLICATE STRATA DETECTION ----
  cat("Step 3.6: Duplicate strata detection...\n")
  
  # Work on a keyed copy; landings itself does NOT get stratum_key
  landings_with_key <- landings %>%
    dplyr::mutate(
      stratum_key = paste(
        flag_country, year, quarter,
        ifelse(is.na(month), "NA", month),
        area, species, fishing_activity_category_eu_l6,
        sep = "_"
      )
    )
  
  duplicate_strata <- landings_with_key %>%
    dplyr::group_by(stratum_key) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::ungroup()
  
  if(nrow(duplicate_strata) > 0) {
  
    # Exact duplicates: same key AND same weight AND same value -> auto-remove all but first
    true_duplicates <- duplicate_strata %>%
      dplyr::group_by(stratum_key) %>%
      dplyr::filter(dplyr::n_distinct(official_landings_weight) == 1 &
               dplyr::n_distinct(official_landings_value) == 1) %>%
      dplyr::arrange(stratum_key) %>%
      dplyr::slice(-1) %>%
      dplyr::ungroup()
  
    if(nrow(true_duplicates) > 0) {
      cat("  [WARN] WARNING 3.6: Auto-removed", nrow(true_duplicates), "exact duplicate strata\n")
      check_log <- dplyr::bind_rows(check_log,
        log_check(true_duplicates, "3.6", "WARNING", "Exact duplicate strata (auto-removed)"))
      all_warnings <- dplyr::bind_rows(all_warnings,
        true_duplicates %>% dplyr::mutate(check_id = "3.6", check_message = "Exact duplicate"))
      landings_with_key <- landings_with_key %>%
        dplyr::filter(!(.row_id %in% true_duplicates$.row_id))
    }
  
    # Conflicting duplicates: same key, different weight or value -> manual review required
    conflicting_duplicates <- landings_with_key %>%
      dplyr::group_by(stratum_key) %>%
      dplyr::filter(dplyr::n() > 1) %>%
      dplyr::filter(dplyr::n_distinct(official_landings_weight) > 1 |
               dplyr::n_distinct(official_landings_value) > 1) %>%
      dplyr::ungroup()
  
    if(nrow(conflicting_duplicates) > 0) {
      cat("  [WARN] WARNING 3.6:", nrow(conflicting_duplicates),
          "conflicting duplicate strata (same key, different weight/value)\n")
      check_log <- dplyr::bind_rows(check_log,
        log_check(conflicting_duplicates, "3.6", "WARNING", "Conflicting duplicate strata - manual review required"))
      all_warnings <- dplyr::bind_rows(all_warnings,
        conflicting_duplicates %>% dplyr::mutate(check_id = "3.6", check_message = "Conflicting duplicate"))
    }
  } else {
    cat("  [OK] Check 3.6: duplicate strata - no duplicates found\n")
  }
  
  # Sync landings back from keyed copy (drop helper column)
  landings <- landings_with_key %>% dplyr::select(-stratum_key)
  cat("  [OK] After duplicate handling:", nrow(landings), "records\n\n")

  # 3.7 WEIGHT OUTLIER DETECTION - IQR per stratum (WARNING) ----
  # For each stratum (flag_country - year - quarter - area -
  # fishing_activity_category_eu_l6 - species), flags records whose
  # official_landings_weight exceeds Q3 + 3 * IQR (Tukey's extreme outlier rule).
  # The 3 * IQR threshold (vs the standard 1.5 * IQR) avoids false positives on the
  # typically right-skewed distribution of landings weights.
  # Records are KEPT (WARNING, not ERROR) because an outlier weight could be
  # legitimate; the flag is meant for human review before raising.
  cat("Step 3.7: Weight outlier detection (IQR per stratum)...\n")

  wt_outlier_strata <- landings %>%
    dplyr::group_by(flag_country, year, quarter, area,
                    fishing_activity_category_eu_l6, species) %>%
    dplyr::filter(dplyr::n() >= 4L) %>%          # need >=4 obs to compute IQR
    dplyr::mutate(
      .q1  = stats::quantile(official_landings_weight, 0.25, na.rm = TRUE),
      .q3  = stats::quantile(official_landings_weight, 0.75, na.rm = TRUE),
      .iqr = .q3 - .q1,
      .upper_fence = .q3 + 3 * .iqr
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(official_landings_weight > .upper_fence & .iqr > 0)

  if (nrow(wt_outlier_strata) > 0) {
    cat("  [WARN] WARNING 3.7:", nrow(wt_outlier_strata),
        "records with extreme landings weight (> Q3 + 3 x IQR within stratum)\n")
    check_log <- dplyr::bind_rows(check_log,
      log_check(wt_outlier_strata %>%
                  dplyr::select(-dplyr::starts_with(".")),
                "3.7", "WARNING",
                "official_landings_weight extreme outlier (> Q3 + 3xIQR within stratum)"))
    all_warnings <- dplyr::bind_rows(all_warnings,
      wt_outlier_strata %>%
        dplyr::select(-dplyr::starts_with(".")) %>%
        dplyr::mutate(check_id = "3.7",
                      check_message = "Extreme weight outlier within stratum"))
  } else {
    cat("  [OK] Check 3.7: no extreme weight outliers detected\n")
  }
  cat("  [OK] After weight outlier check:", nrow(landings), "records\n\n")

  # 3.8 PRICE PER KG PLAUSIBILITY (WARNING) ----
  # Computes official_landings_value / official_landings_weight for each record
  # and flags strata where the ratio is outside a plausible range.
  # Both fields are optional for raising, so this is WARNING-only.
  #
  # Plausibility bounds (EUR/kg):
  #   < 0.01  -> likely weight entered in grams (should be kg) or value near zero
  #   > 5000  -> likely weight entered in tonnes (should be kg), or value in
  #              wrong currency units / inflated by factor of 1000
  #
  # The bounds are wide enough to accommodate all Mediterranean species
  # (anchovy ~1 EUR/kg up to shellfish / bluefin tuna >200 EUR/kg) while
  # catching clear unit-of-measure errors.
  cat("Step 3.8: Price-per-kg plausibility check...\n")

  price_lo <- 0.01    # EUR/kg lower bound
  price_hi <- 5000    # EUR/kg upper bound

  has_value_col <- "official_landings_value" %in% names(landings)

  if (!has_value_col) {
    cat("  [OK] Check 3.8: price/kg skipped\n\n")
  } else {
    price_check <- landings %>%
      dplyr::filter(
        !is.na(official_landings_value) & official_landings_value > 0 &
        !is.na(official_landings_weight) & official_landings_weight > 0
      ) %>%
      dplyr::mutate(.price_per_kg = official_landings_value / official_landings_weight) %>%
      dplyr::filter(.price_per_kg < price_lo | .price_per_kg > price_hi)

    if (nrow(price_check) > 0) {
      cat("  [WARN] WARNING 3.8:", nrow(price_check),
          "record(s) with implausible price/kg\n")
      check_log <- dplyr::bind_rows(check_log,
        log_check(price_check %>% dplyr::select(-dplyr::starts_with(".")),
                  "3.8", "WARNING",
                  paste0("Implausible price/kg (outside [", price_lo, ", ", price_hi,
                         "] EUR/kg) - check weight or value units")))
      all_warnings <- dplyr::bind_rows(all_warnings,
        price_check %>%
          dplyr::select(-dplyr::starts_with(".")) %>%
          dplyr::mutate(check_id = "3.8",
                        check_message = "Implausible official_landings_value/weight ratio"))
    } else {
      cat("  [OK] Check 3.8: price/kg - all values within plausible range\n")
    }
    cat("\n")
  }

  # WRITE OUTPUTS ----
  cat("\nWriting output files...\n")
  
  # Drop QC helper columns before writing
  landings_clean <- landings %>%
    dplyr::select(-dplyr::ends_with("_orig"), -dplyr::any_of(c("stratum_key", ".row_id")))
  
  readr::write_csv(landings_clean,
            file.path(out_dir, paste0("landings_clean_for_qc", file_suffix, ".csv")))
  
  if (nrow(all_errors) > 0) {
    readr::write_csv(all_errors, file.path(out_dir, paste0("landings_qc_errors", file_suffix, ".csv")))
  } else {
    readr::write_csv(all_errors, file.path(out_dir, paste0("landings_qc_errors", file_suffix, ".csv")))
  }
  
  if (nrow(all_warnings) > 0) {
    readr::write_csv(all_warnings, file.path(out_dir, paste0("landings_qc_warnings", file_suffix, ".csv")))
  } else {
    readr::write_csv(all_warnings, file.path(out_dir, paste0("landings_qc_warnings", file_suffix, ".csv")))
  }
  
  if (nrow(check_log) > 0) {
    check_log <- check_log %>%
      dplyr::mutate(
        scope = dplyr::coalesce(
          scope,
          dplyr::case_when(
            check_id %in% c("3.6") ~ "stratum",
            TRUE                     ~ "record"
          )
        )
      )
  } else {
    check_log$scope <- character()
  }

  readr::write_csv(check_log, file.path(out_dir, paste0("landings_qc_summary", file_suffix, ".csv")))
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
          title = "Landings QC: flagged records per check",
          subtitle = "Overview of QC findings before sampling checks. Labels show the check scope.",
          x = NULL, y = "Records flagged"
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
          label = "No QC findings\nAll landings checks passed",
          size = 5,
          fontface = "bold"
        ) +
        ggplot2::xlim(0.5, 1.5) +
        ggplot2::ylim(0.5, 1.5) +
        ggplot2::labs(
          title = "Landings QC: flagged records per check",
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

  # FINAL SUMMARY ----
  cat("\n========================================\n")
  cat("landings_qc Completed\n")
  cat("========================================\n")
  
  if (nrow(all_errors) > 0) {
    cat("[ERROR] Errors found:", nrow(all_errors), "record(s)\n")
  } else {
    cat("[OK] Errors found: 0\n")
  }
  
  if (nrow(all_warnings) > 0) {
    cat("[WARN] Warnings found:", nrow(all_warnings), "record(s)\n")
  } else {
    cat("[OK] Warnings found: 0\n")
  }
  cat("[OK] Clean records:", nrow(landings_clean), "\n")

  # Return structured output for downstream steps
  out <- list(
    landings_df = landings_clean,
    errors = all_errors,
    warnings = all_warnings,
    summary = check_log,
    output_dir = out_dir,
    file_suffix = file_suffix
  )
  return(out)
}

