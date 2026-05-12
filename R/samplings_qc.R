#' Sampling Quality Control for LFD Raising - stat4RDBFIS
#'
#' Validate RCG sampling data before cross-checking with landings.
#' This function is designed as the second QC step in the stat4RDBFIS workflow.
#'
#' @param sampling_df data.frame. Sampling template already loaded in memory.
#' @param out_dir character. Output directory where clean data, logs, and
#'   charts are written.
#' @param validation_lists_path character or \code{NULL}. Path to
#'   \code{validation_lists.json}. When \code{NULL}, the bundled package copy
#'   is used.
#' @param MS character vector or NULL. Optional subset of \code{flag_country}
#'   ISO3 codes.
#' @param GSA character vector or NULL. Optional subset of GSA codes.
#' @param YEAR integer vector or NULL. Optional subset of years.
#' @param species_length_path character or \code{NULL}. Optional path to
#'   \code{species_length.json} used for species-specific length plausibility
#'   checks. When \code{NULL}, the bundled package copy is used if available.
#'
#' @return A named list with elements \code{sampling_df}, \code{errors},
#'   \code{warnings}, \code{summary}, \code{output_dir}, and
#'   \code{file_suffix}.
#'
#' @details
#' Records flagged as \code{ERROR} are removed from the clean dataset.
#' Records flagged as \code{WARNING} are retained and reported for manual review.
#'
#' @seealso \code{\link{landings_qc}}, \code{\link{crosschecks}}
#' @family qc functions
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' qc_samp <- samplings_qc(
#'   sampling_df = sampling_df,
#'   validation_lists_path = system.file(
#'     "extdata", "validation_lists.json", package = "stat4RDBFIS"
#'   )
#' )
#' }
#'
#' @author stat4RDBFIS Core Team
#' @export

samplings_qc <- function(
  sampling_df,
  out_dir = file.path("Consistency_Checks", "qc_outputs_sampling"),
  validation_lists_path = NULL,
  MS = NULL,
  GSA = NULL,
  YEAR = NULL,
  species_length_path = NULL
) {
  # samplings_qc: Sampling Quality Control for LFD Raising (stat4RDBFIS)
  # Validates RCG_Sampling_template.csv data against RCG spec constraints.
  # ERROR   -> record excluded from clean dataset and from LFD raising
  # WARNING -> record kept, flagged for manual review

  # DEPENDENCIES ----
  for (pkg in c("dplyr", "tidyr", "readr", "stringr", "jsonlite", "lubridate")) {
    if (!requireNamespace(pkg, quietly = TRUE))
      stop("Required package not installed: ", pkg,
           "\n  Install with: install.packages(\"", pkg, "\"")
           
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
      grepl("^(=+|samplings_qc:|stat4RDBFIS - Activity 10 RDBFIS III|Pre-flight checks complete|Loading validation lists\\.\\.\\.|Writing output files\\.\\.\\.|Step\\b|samplings_qc Completed)", msg_trim) ||
      grepl("^\\[OK\\]|^\\[WARN\\]|^\\[ERROR\\]", msg_trim) ||
      grepl("^\\s+\\[OK\\]|^\\s+\\[WARN\\]|^\\s+\\[ERROR\\]", msg_clean)
    drop <- grepl("\\.(csv|png)\\b", msg_trim, ignore.case = TRUE) ||
      grepl("Records affected:|Output saved to:|Outlier records saved to:|Outputs saved to|Next step|Filter applied|Output suffix|Species affected:|Possible causes:|Valid ranges per spec:|Threshold:|Method:|Strata without aged fish:|Duplicate fish_id summary|Records plotted:|Column names normalized|Analytical note:|Maximum allowed length:|Requirements:|Strata skipped", msg_trim)
    if (keep && !drop)
      raw_cat(msg_clean, file = file, sep = "", fill = fill, labels = labels, append = append)
  }
  
  # QC bounds (spec-defined, not user-configurable)
  # Coordinate ranges from RCG/RDBFIS data request spec:
  lat_min  <- 34.00000;  lat_max  <- 45.79999   # Mediterranean & Black Sea
  lon_min  <- -9.99999;  lon_max  <- 35.00000
  age_max  <- 30    # RCG spec: max age = 30 years
  max_length_station_code  <- 50
  max_length_national_gear <- 50
  
  
  # Year range computed after normalization; see Step 3.1

  # MANDATORY COLUMNS ----
  MANDATORY_COLS <- c(
    "sampling_type", "flag_country", "year", "trip_code", "aggregation_level",
    "station_code", "date", "area", "catch_registration", "species_registration",
    "fishing_activity_category_eu_l6", "species", "catch_category",
    "weight", "subsample_weight", "sex", "maturity_method", "maturity_scale",
    "maturity_stage", "ageing_method", "length_code", "length_class",
    "number_at_length", "commercial_size_category_scale",
    "commercial_size_category", "fish_id", "individual_weight", "age",
    "n_sets_hauls", "days_at_sea", "initial_latitude", "initial_longitude",
    "final_latitude", "final_longitude", "duration_fishing_operation",
    "depth_fishing_operation", "water_depth", "fishing_activity_category_national"
  )
  
  
  # NOTE: check_maturity_stage_valid() is defined in qc_utils.R (sourced before this file).

  # PRE-FLIGHT CHECKS ----
  cat("========================================\n")
  cat("samplings_qc: Sampling Quality Control\n")
  cat("stat4RDBFIS - Activity 10 RDBFIS III\n")
  cat("========================================\n\n")
  
  if (!inherits(sampling_df, "data.frame")) {
    stop(
      "FATAL ERROR: sampling_df must be a data.frame already loaded in memory.\n",
      "Please load the sampling data in Tutorial step 0, then pass the object to sampling_qc()."
    )
  }

  validation_lists_path <- stat4rdbfis_extdata_path(
    "validation_lists.json",
    validation_lists_path
  )
  species_length_path <- stat4rdbfis_extdata_path(
    "species_length.json",
    species_length_path,
    must_work = FALSE
  )
  if (!nzchar(species_length_path)) {
    species_length_path <- NULL
  }
  
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # Build file suffix early; available throughout the entire function
  suffix_parts <- c()
  if (!is.null(MS))   suffix_parts <- c(suffix_parts, paste0("MS-",   paste(MS,   collapse = "-")))
  if (!is.null(GSA))  suffix_parts <- c(suffix_parts, paste0("GSA-",  paste(GSA,  collapse = "-")))
  if (!is.null(YEAR)) suffix_parts <- c(suffix_parts, paste0("YEAR-", paste(YEAR, collapse = "-")))
  file_suffix <- if (length(suffix_parts) > 0) paste0("_", paste(suffix_parts, collapse = "_")) else ""

  
  cat("Pre-flight checks complete\n")
  cat("  [OK] Input data present in memory\n")
  cat("  [OK] Validation lists found\n")
  cat("  [OK] Output directory ready\n\n")
  
  # LOAD VALIDATION LISTS ----
  cat("Loading validation lists...\n")
  
  VLISTS <- load_validation_lists(validation_lists_path)
  
  VALID_COUNTRIES <- VLISTS$country$code
  VALID_GSA <- VLISTS$gsa$code
  VALID_GEARS_L6 <- VLISTS$gears_l6$code
  VALID_SPECIES <- VLISTS$species$code
  VALID_SAMPLING_TYPE <- VLISTS$sampling_type$code
  VALID_AGGREGATION <- VLISTS$aggregation_level$code
  VALID_CATCH_REG <- VLISTS$catch_registration$code
  VALID_SPECIES_REG <- VLISTS$species_registration$code
  VALID_CATCH_CAT <- VLISTS$catch_category$code
  VALID_SEX <- VLISTS$sex$code
  VALID_MATURITY_METHOD <- VLISTS$maturity_method$code
  VALID_MATURITY_SCALE <- VLISTS$maturity_scale$code
  VALID_AGEING_METHOD <- VLISTS$ageing_method$code
  VALID_LENGTH_CODE <- VLISTS$length_code$code
  VALID_COMMERCIAL_SIZE_CAT <- VLISTS$commercial_size_category$code
  
  # Harbour lookup: named list -> VALID_HARBOURS[["GRC"]] = vector of valid codes
  VALID_HARBOURS <- if (!is.null(VLISTS$harbours)) {
    h <- VLISTS$harbours
    setNames(lapply(seq_len(nrow(h)), function(i) h$harbours[[i]]$code), h$country)
  } else NULL
  
  cat("  [OK] Loaded", length(VALID_COUNTRIES), "countries\n")
  cat("  [OK] Loaded", length(VALID_GSA), "GSA codes\n")
  cat("  [OK] Loaded", length(VALID_GEARS_L6), "EU L6 gears\n")
  cat("  [OK] Loaded", length(VALID_SPECIES), "ASFIS species codes\n")
  cat("  [OK] Loaded", length(VALID_SAMPLING_TYPE), "sampling types\n")
  cat("  [OK] Loaded", length(VALID_MATURITY_SCALE), "maturity scales\n")
  if (!is.null(VALID_HARBOURS))
    cat("  [OK] Loaded harbour codes for", length(VALID_HARBOURS), "countries\n")
  cat("\n")
  
  # INITIALIZE ERROR TRACKING ----
  check_log <- dplyr::tibble()
  all_errors <- dplyr::tibble()
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
  
  # 3.1 LOAD AND NORMALIZE DATA ----
  cat("Step 3.1: Loading and normalizing data...\n")
  # NOTE: sampling_df presence is already guaranteed by the inherits() check above.

  sampling_raw <- sampling_df %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

  # Robust column name cleaning:
  # 1. tolower()  - handles columns submitted in mixed/upper case
  # 2. trimws()   - removes leading/trailing ASCII whitespace
  # 3. gsub non-breaking space (U+00A0) and BOM (U+FEFF) - both invisible in
  #    console but break dplyr column lookup; typical artifact of Excel CSV export
  # 4. gsub carriage returns and internal whitespace
  clean_colnames <- function(nms) {
    nms <- tolower(nms)
    nms <- trimws(nms)
    nms <- gsub("\u00a0", "", nms, fixed = TRUE)   # non-breaking space
    nms <- gsub("\ufeff", "", nms, fixed = TRUE)   # UTF-8 BOM
    nms <- gsub("\r",     "", nms, fixed = TRUE)   # carriage return
    nms <- gsub("\\s+",  "_", nms)                # internal whitespace -> underscore
    nms
  }

  original_names      <- names(sampling_raw)
  names(sampling_raw) <- clean_colnames(original_names)

  # Report any names that were actually changed so the user is aware
  changed <- original_names != names(sampling_raw)
  if (any(changed)) {
    cat("  [WARN] Column names normalized (whitespace / case / BOM):\n")
    for (i in which(changed))
      cat("    '", original_names[i], "' -> '", names(sampling_raw)[i], "'\n", sep = "")
  }

  cat("  [OK] Loaded:", nrow(sampling_raw), "records from input file\n")

  # 3.2 STRUCTURAL TEMPLATE CHECKS (run BEFORE numeric normalization) ----
  # Must precede toupper() and as.numeric() calls: if mandatory columns are
  # absent those operations fail with cryptic "object not found" errors.
  cat("Step 3.2: Structural template checks (pre-normalization)...\n")

  missing_cols_early <- setdiff(MANDATORY_COLS, names(sampling_raw))
  if (length(missing_cols_early) > 0) {
    cat("  [ERROR] FATAL ERROR: Missing mandatory columns\n")
    cat("    Missing:", paste(missing_cols_early, collapse = ", "), "\n\n")
    fatal_message <- paste0(
      "FATAL ERROR - Script Stopped\n",
      "================================\n",
      "The input file is missing mandatory columns required by the MedBS RCG Sampling template.\n\n",
      "Missing columns:\n",
      paste("  -", missing_cols_early, collapse = "\n"), "\n\n",
      "Action required:\n",
      "1. Verify your input file matches the official RCG Sampling template\n",
      "2. Add the missing columns\n",
      "3. Re-run this script\n\n",
      "Input file: <in-memory sampling_df>\n",
      "Timestamp: ", as.character(Sys.time()), "\n"
    )
    writeLines(fatal_message, file.path(out_dir, "FATAL_ERROR.txt"))
    stop(fatal_message)
  }

  if (any(duplicated(names(sampling_raw)))) {
    dup_cols_early <- names(sampling_raw)[duplicated(names(sampling_raw))]
    cat("  [ERROR] FATAL ERROR: Duplicated column names\n")
    cat("    Duplicates:", paste(dup_cols_early, collapse = ", "), "\n\n")
    fatal_message <- paste0(
      "FATAL ERROR - Script Stopped\n",
      "================================\n",
      "The input file contains duplicated column names.\n\n",
      "Duplicated columns:\n",
      paste("  -", dup_cols_early, collapse = "\n"), "\n\n",
      "Action required:\n",
      "1. Open the input file\n",
      "2. Remove or rename duplicated columns\n",
      "3. Save and re-run this script\n\n",
      "Input file: <in-memory sampling_df>\n",
      "Timestamp: ", as.character(Sys.time()), "\n"
    )
    writeLines(fatal_message, file.path(out_dir, "FATAL_ERROR.txt"))
    stop(fatal_message)
  }

  cat("  [OK] All mandatory columns present (", length(MANDATORY_COLS), "columns)\n")
  cat("  [OK] No duplicate column names\n\n")
  
  sampling <- sampling_raw %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.character), stringr::str_trim)) %>%
    dplyr::mutate(.row_id = dplyr::row_number())
  
  sampling <- sampling %>%
    dplyr::mutate(
      flag_country = toupper(flag_country),
      area = toupper(area),
      species = toupper(species),
      fishing_activity_category_eu_l6 = toupper(fishing_activity_category_eu_l6),
      sex = toupper(sex),
      sampling_type = toupper(sampling_type),
      aggregation_level = toupper(aggregation_level)
    )
  
  sampling <- sampling %>%
    dplyr::mutate(
      year_orig = year,
      weight_orig = weight,
      subsample_weight_orig = subsample_weight,
      length_class_orig = length_class,
      number_at_length_orig = number_at_length
    )
  
  sampling <- sampling %>%
    dplyr::mutate(
      year = suppressWarnings(as.numeric(year)),
      n_sets_hauls = suppressWarnings(as.numeric(n_sets_hauls)),
      days_at_sea = suppressWarnings(as.numeric(days_at_sea)),
      duration_fishing_operation = suppressWarnings(as.numeric(duration_fishing_operation)),
      depth_fishing_operation = suppressWarnings(as.numeric(depth_fishing_operation)),
      water_depth = suppressWarnings(as.numeric(water_depth)),
      weight = suppressWarnings(as.numeric(weight)),
      subsample_weight = suppressWarnings(as.numeric(subsample_weight)),
      length_class = suppressWarnings(as.numeric(length_class)),
      number_at_length = suppressWarnings(as.numeric(number_at_length)),
      age = suppressWarnings(as.numeric(age)),
      individual_weight = suppressWarnings(as.numeric(individual_weight)),
      fish_id = suppressWarnings(as.numeric(fish_id)),
      initial_latitude = suppressWarnings(as.numeric(initial_latitude)),
      initial_longitude = suppressWarnings(as.numeric(initial_longitude)),
      final_latitude = suppressWarnings(as.numeric(final_latitude)),
      final_longitude = suppressWarnings(as.numeric(final_longitude))
    )
  
  conversion_failures <- sampling %>%
    dplyr::filter(
      (!is.na(year_orig) & year_orig != "" & is.na(year)) |
        (!is.na(weight_orig) & weight_orig != "" & is.na(weight)) |
        (!is.na(subsample_weight_orig) & subsample_weight_orig != "" & is.na(subsample_weight)) |
        (!is.na(length_class_orig) & length_class_orig != "" & is.na(length_class)) |
        (!is.na(number_at_length_orig) & number_at_length_orig != "" & is.na(number_at_length))
    )
  
  if (nrow(conversion_failures) > 0) {
    cat("  [ERROR] ERROR 3.1.4: Numeric conversion failure on critical fields\n")
    cat("    Records affected:", nrow(conversion_failures), "\n")
    check_log <- dplyr::bind_rows(
      check_log,
      log_check(conversion_failures, "3.1.4", "ERROR",
                "Failed to convert text to numbers (year, weight, subsample_weight, length_class, number_at_length)")
    )
    all_errors <- dplyr::bind_rows(
      all_errors,
      conversion_failures %>% dplyr::mutate(
        check_id = "3.1.4",
        check_message = "Numeric conversion failure - non-numeric values in numeric fields"
      )
    )
    sampling <- sampling %>% dplyr::filter(!(.row_id %in% conversion_failures$.row_id))
  }
  
  cat("  [OK] After normalization:", nrow(sampling), "records\n\n")

  # APPLY USER FILTERS ----
  filters_applied <- FALSE
  
  if (!is.null(MS)) {
    before <- nrow(sampling)
    sampling <- sampling %>% dplyr::filter(flag_country %in% MS)
    cat("  [OK] Country filter:", before, "->", nrow(sampling), "records\n")
    filters_applied <- TRUE
  }
  if (!is.null(GSA)) {
    before <- nrow(sampling)
    sampling <- sampling %>% dplyr::filter(area %in% GSA)
    cat("  [OK] Area filter:", before, "->", nrow(sampling), "records\n")
    filters_applied <- TRUE
  }
  if (!is.null(YEAR)) {
    before <- nrow(sampling)
    sampling <- sampling %>% dplyr::filter(year %in% YEAR)
    cat("  [OK] Year filter:", before, "->", nrow(sampling), "records\n")
    filters_applied <- TRUE
  }

  
  # 3.3 CODE LIST VALIDATION ----
  cat("Step 3.3: Code list validation (case-sensitive where applicable)...\n")
  
  invalid_sampling_type <- sampling %>% dplyr::filter(!(sampling_type %in% VALID_SAMPLING_TYPE))
  if (nrow(invalid_sampling_type) > 0) {
    print_check_report(invalid_sampling_type, "3.3.1", "ERROR", "Invalid sampling_type", "sampling_type", VALID_SAMPLING_TYPE,
                       error_file = paste0("sampling_qc_errors", file_suffix, ".csv"))
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_sampling_type, "3.3.1", "ERROR", "Invalid sampling_type",
                                                       invalid_sampling_type$sampling_type, VALID_SAMPLING_TYPE))
    all_errors <- dplyr::bind_rows(all_errors, invalid_sampling_type %>% dplyr::mutate(check_id = "3.3.1", check_message = "Invalid sampling_type"))
    sampling <- sampling %>% dplyr::filter(!(.row_id %in% invalid_sampling_type$.row_id))
  } else {
    cat("  [OK] Check 3.3.1: sampling_type - all values valid\n")
  }
  
  invalid_country <- sampling %>% dplyr::filter(!(flag_country %in% VALID_COUNTRIES))
  if (nrow(invalid_country) > 0) {
    print_check_report(invalid_country, "3.3.2", "ERROR", "Invalid flag_country (ISO3 code)", "flag_country", VALID_COUNTRIES,
                       error_file = paste0("sampling_qc_errors", file_suffix, ".csv"))
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_country, "3.3.2", "ERROR", "Invalid flag_country",
                                                       invalid_country$flag_country, VALID_COUNTRIES))
    all_errors <- dplyr::bind_rows(all_errors, invalid_country %>% dplyr::mutate(check_id = "3.3.2", check_message = "Invalid flag_country"))
    sampling <- sampling %>% dplyr::filter(!(.row_id %in% invalid_country$.row_id))
  } else {
    cat("  [OK] Check 3.3.2: flag_country - all values valid\n")
  }
  
  invalid_gsa <- sampling %>% dplyr::filter(!(area %in% VALID_GSA))
  if (nrow(invalid_gsa) > 0) {
    print_check_report(invalid_gsa, "3.3.3", "ERROR", "Invalid GSA code", "area", VALID_GSA,
                       error_file = paste0("sampling_qc_errors", file_suffix, ".csv"))
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_gsa, "3.3.3", "ERROR", "Invalid GSA code",
                                                       invalid_gsa$area, VALID_GSA))
    all_errors <- dplyr::bind_rows(all_errors, invalid_gsa %>% dplyr::mutate(check_id = "3.3.3", check_message = "Invalid GSA"))
    sampling <- sampling %>% dplyr::filter(!(.row_id %in% invalid_gsa$.row_id))
  } else {
    cat("  [OK] Check 3.3.3: area (GSA) - all values valid\n")
  }
  
  invalid_gear <- sampling %>% dplyr::filter(!(fishing_activity_category_eu_l6 %in% VALID_GEARS_L6))
  if (nrow(invalid_gear) > 0) {
    print_check_report(invalid_gear, "3.3.4", "ERROR", "Invalid EU L6 gear", "fishing_activity_category_eu_l6", VALID_GEARS_L6,
                       error_file = paste0("sampling_qc_errors", file_suffix, ".csv"))
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_gear, "3.3.4", "ERROR", "Invalid EU L6 gear",
                                                       invalid_gear$fishing_activity_category_eu_l6, VALID_GEARS_L6))
    all_errors <- dplyr::bind_rows(all_errors, invalid_gear %>% dplyr::mutate(check_id = "3.3.4", check_message = "Invalid gear"))
    sampling <- sampling %>% dplyr::filter(!(.row_id %in% invalid_gear$.row_id))
  } else {
    cat("  [OK] Check 3.3.4: fishing_activity_category_eu_l6 - all values valid\n")
  }
  
  invalid_national_gear <- sampling %>%
    dplyr::filter(!is.na(fishing_activity_category_national) &
                    fishing_activity_category_national != "" &
                    nchar(fishing_activity_category_national) > max_length_national_gear)
  
  if (nrow(invalid_national_gear) > 0) {
    cat("  [WARN] WARNING 3.3.5: fishing_activity_category_national exceeds maximum length\n")
    cat("    Records affected:", nrow(invalid_national_gear), "\n")
    cat("    Maximum allowed length:", max_length_national_gear, "characters\n")
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_national_gear, "3.3.5", "WARNING",
                                                       "fishing_activity_category_national exceeds maximum length"))
    all_warnings <- dplyr::bind_rows(all_warnings, invalid_national_gear %>% dplyr::mutate(check_id = "3.3.5",
                                                                                           check_message = "National gear name too long"))
  } else {
    cat("  [OK] Check 3.3.5: fishing_activity_category_national - length valid\n")
  }
  
  invalid_aggregation <- sampling %>% dplyr::filter(!(aggregation_level %in% VALID_AGGREGATION))
  if (nrow(invalid_aggregation) > 0) {
    print_check_report(invalid_aggregation, "3.3.6", "ERROR", "Invalid aggregation_level", "aggregation_level", VALID_AGGREGATION,
                       error_file = paste0("sampling_qc_errors", file_suffix, ".csv"))
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_aggregation, "3.3.6", "ERROR", "Invalid aggregation_level",
                                                       invalid_aggregation$aggregation_level, VALID_AGGREGATION))
    all_errors <- dplyr::bind_rows(all_errors, invalid_aggregation %>% dplyr::mutate(check_id = "3.3.6", check_message = "Invalid aggregation_level"))
    sampling <- sampling %>% dplyr::filter(!(.row_id %in% invalid_aggregation$.row_id))
  } else {
    cat("  [OK] Check 3.3.6: aggregation_level - all values valid\n")
  }
  
  invalid_catch_reg <- sampling %>% dplyr::filter(!(catch_registration %in% VALID_CATCH_REG))
  if (nrow(invalid_catch_reg) > 0) {
    print_check_report(invalid_catch_reg, "3.3.7", "ERROR", "Invalid catch_registration (case-sensitive!)",
                       "catch_registration", VALID_CATCH_REG,
                       error_file = paste0("sampling_qc_errors", file_suffix, ".csv"))
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_catch_reg, "3.3.7", "ERROR", "Invalid catch_registration",
                                                       invalid_catch_reg$catch_registration, VALID_CATCH_REG))
    all_errors <- dplyr::bind_rows(all_errors, invalid_catch_reg %>% dplyr::mutate(check_id = "3.3.7", check_message = "Invalid catch_registration"))
    sampling <- sampling %>% dplyr::filter(!(.row_id %in% invalid_catch_reg$.row_id))
  } else {
    cat("  [OK] Check 3.3.7: catch_registration - all values valid (case-sensitive)\n")
  }
  
  invalid_species_reg <- sampling %>% dplyr::filter(!(species_registration %in% VALID_SPECIES_REG))
  if (nrow(invalid_species_reg) > 0) {
    print_check_report(invalid_species_reg, "3.3.8", "ERROR", "Invalid species_registration (case-sensitive!)",
                       "species_registration", VALID_SPECIES_REG,
                       error_file = paste0("sampling_qc_errors", file_suffix, ".csv"))
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_species_reg, "3.3.8", "ERROR", "Invalid species_registration",
                                                       invalid_species_reg$species_registration, VALID_SPECIES_REG))
    all_errors <- dplyr::bind_rows(all_errors, invalid_species_reg %>% dplyr::mutate(check_id = "3.3.8", check_message = "Invalid species_registration"))
    sampling <- sampling %>% dplyr::filter(!(.row_id %in% invalid_species_reg$.row_id))
  } else {
    cat("  [OK] Check 3.3.8: species_registration - all values valid (case-sensitive)\n")
  }
  
  invalid_catch_category <- sampling %>% dplyr::filter(!(catch_category %in% VALID_CATCH_CAT))
  if (nrow(invalid_catch_category) > 0) {
    print_check_report(invalid_catch_category, "3.3.9", "ERROR", "Invalid catch_category (case-sensitive!)",
                       "catch_category", VALID_CATCH_CAT,
                       error_file = paste0("sampling_qc_errors", file_suffix, ".csv"))
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_catch_category, "3.3.9", "ERROR", "Invalid catch_category",
                                                       invalid_catch_category$catch_category, VALID_CATCH_CAT))
    all_errors <- dplyr::bind_rows(all_errors, invalid_catch_category %>% dplyr::mutate(check_id = "3.3.9", check_message = "Invalid catch_category"))
    sampling <- sampling %>% dplyr::filter(!(.row_id %in% invalid_catch_category$.row_id))
  } else {
    cat("  [OK] Check 3.3.9: catch_category - all values valid (case-sensitive)\n")
  }
  
  invalid_species <- sampling %>% dplyr::filter(!(species %in% VALID_SPECIES))
  if (nrow(invalid_species) > 0) {
    print_check_report(invalid_species, "3.3.10", "ERROR", "Invalid ASFIS species code", "species", head(VALID_SPECIES, 20),
                       error_file = paste0("sampling_qc_errors", file_suffix, ".csv"))
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_species, "3.3.10", "ERROR", "Invalid ASFIS species",
                                                       invalid_species$species, head(VALID_SPECIES, 20)))
    all_errors <- dplyr::bind_rows(all_errors, invalid_species %>% dplyr::mutate(check_id = "3.3.10", check_message = "Invalid species"))
    sampling <- sampling %>% dplyr::filter(!(.row_id %in% invalid_species$.row_id))
  } else {
    cat("  [OK] Check 3.3.10: species (ASFIS) - all values valid\n")
  }
  
  invalid_sex <- sampling %>% dplyr::filter(!is.na(sex) & sex != "" & !(sex %in% VALID_SEX))
  if (nrow(invalid_sex) > 0) {
    cat("  [WARN] WARNING 3.3.11: Invalid sex code\n")
    cat("    Records affected:", nrow(invalid_sex), "\n")
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_sex, "3.3.11", "WARNING", "Invalid sex code",
                                                       invalid_sex$sex, VALID_SEX))
    all_warnings <- dplyr::bind_rows(all_warnings, invalid_sex %>% dplyr::mutate(check_id = "3.3.11", check_message = "Invalid sex"))
  } else {
    cat("  [OK] Check 3.3.11: sex - all values valid\n")
  }
  
  invalid_maturity_method <- sampling %>%
    dplyr::filter(!is.na(maturity_method) & maturity_method != "" &
                    !(maturity_method %in% VALID_MATURITY_METHOD))
  if (nrow(invalid_maturity_method) > 0) {
    cat("  [WARN] WARNING 3.3.12: Invalid maturity_method (case-sensitive)\n")
    cat("    Records affected:", nrow(invalid_maturity_method), "\n")
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_maturity_method, "3.3.12", "WARNING", "Invalid maturity_method",
                                                       invalid_maturity_method$maturity_method, VALID_MATURITY_METHOD))
    all_warnings <- dplyr::bind_rows(all_warnings, invalid_maturity_method %>% dplyr::mutate(check_id = "3.3.12", check_message = "Invalid maturity_method"))
  } else {
    cat("  [OK] Check 3.3.12: maturity_method - all values valid (case-sensitive)\n")
  }
  
  invalid_maturity_scale <- sampling %>%
    dplyr::filter(!is.na(maturity_scale) & maturity_scale != "" &
                    !(maturity_scale %in% VALID_MATURITY_SCALE))
  if (nrow(invalid_maturity_scale) > 0) {
    cat("  [WARN] WARNING 3.3.13: Invalid maturity_scale (case-sensitive)\n")
    cat("    Records affected:", nrow(invalid_maturity_scale), "\n")
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_maturity_scale, "3.3.13", "WARNING", "Invalid maturity_scale",
                                                       invalid_maturity_scale$maturity_scale, VALID_MATURITY_SCALE))
    all_warnings <- dplyr::bind_rows(all_warnings, invalid_maturity_scale %>% dplyr::mutate(check_id = "3.3.13", check_message = "Invalid maturity_scale"))
  } else {
    cat("  [OK] Check 3.3.13: maturity_scale - all values valid (case-sensitive)\n")
  }
  
  is_missing_token_local <- if (exists("is_missing_token", mode = "function")) {
    is_missing_token
  } else {
    function(x) is.na(x) | trimws(as.character(x)) %in% c("", "NA")
  }

  sampling <- sampling %>%
    dplyr::mutate(
      maturity_stage_valid = mapply(
        check_maturity_stage_valid,
        maturity_stage,
        maturity_scale,
        MoreArgs = list(stages_list = VLISTS$maturity_stages_by_scale),
        SIMPLIFY = TRUE
      )
    )
  
  invalid_maturity_stage <- sampling %>%
    dplyr::filter(!is.na(maturity_stage) & maturity_stage != "" &
                    !is.na(maturity_scale) & maturity_scale != "" &
                    !maturity_stage_valid)
  
  if (nrow(invalid_maturity_stage) > 0) {
    cat("  [WARN] WARNING 3.3.14: Invalid maturity_stage for declared maturity_scale\n")
    cat("    Records affected:", nrow(invalid_maturity_stage), "\n")
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_maturity_stage, "3.3.14", "WARNING",
                                                       "maturity_stage incompatible with declared maturity_scale"))
    all_warnings <- dplyr::bind_rows(all_warnings, invalid_maturity_stage %>% dplyr::mutate(check_id = "3.3.14", check_message = "Invalid maturity_stage for scale"))
  } else {
    cat("  [OK] Check 3.3.14: maturity_stage - compatible with scales\n")
  }
  invalid_maturity_payload_without_method <- sampling %>%
    dplyr::filter(
      is_missing_token_local(maturity_method) &
        (!is_missing_token_local(maturity_scale) | !is_missing_token_local(maturity_stage))
    )

  if (nrow(invalid_maturity_payload_without_method) > 0) {
    cat("  [WARN] WARNING 3.3.15: maturity payload present while maturity_method is missing\n")
    cat("    Records affected:", nrow(invalid_maturity_payload_without_method), "\n")
    check_log <- dplyr::bind_rows(
      check_log,
      log_check(invalid_maturity_payload_without_method, "3.3.15", "WARNING",
                "maturity_scale or maturity_stage present but maturity_method is missing")
    )
    all_warnings <- dplyr::bind_rows(
      all_warnings,
      invalid_maturity_payload_without_method %>%
        dplyr::mutate(check_id = "3.3.15",
                      check_message = "Maturity payload present without maturity_method")
    )
  } else {
    cat("  [OK] Check 3.3.15: maturity payload requires maturity_method - OK\n")
  }

  invalid_maturity_method_incomplete <- sampling %>%
    dplyr::filter(
      !is_missing_token_local(maturity_method) &
        (is_missing_token_local(maturity_scale) | is_missing_token_local(maturity_stage))
    )

  if (nrow(invalid_maturity_method_incomplete) > 0) {
    cat("  [WARN] WARNING 3.3.16: maturity_method present but maturity_scale or maturity_stage is missing\n")
    cat("    Records affected:", nrow(invalid_maturity_method_incomplete), "\n")
    check_log <- dplyr::bind_rows(
      check_log,
      log_check(invalid_maturity_method_incomplete, "3.3.16", "WARNING",
                "maturity_method declared but maturity_scale or maturity_stage is missing")
    )
    all_warnings <- dplyr::bind_rows(
      all_warnings,
      invalid_maturity_method_incomplete %>%
        dplyr::mutate(check_id = "3.3.16",
                      check_message = "maturity_method present with incomplete maturity fields")
    )
  } else {
    cat("  [OK] Check 3.3.16: maturity_method completeness - OK\n")
  }

  sampling <- sampling %>% dplyr::select(-maturity_stage_valid)
  
  invalid_ageing_method <- sampling %>%
    dplyr::filter(!is.na(ageing_method) & ageing_method != "" &
                    !(ageing_method %in% VALID_AGEING_METHOD))
  if (nrow(invalid_ageing_method) > 0) {
    cat("  [WARN] WARNING 3.3.17: Invalid ageing_method (case-sensitive)\n")
    cat("    Records affected:", nrow(invalid_ageing_method), "\n")
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_ageing_method, "3.3.17", "WARNING", "Invalid ageing_method",
                                                       invalid_ageing_method$ageing_method, VALID_AGEING_METHOD))
    all_warnings <- dplyr::bind_rows(all_warnings, invalid_ageing_method %>% dplyr::mutate(check_id = "3.3.17", check_message = "Invalid ageing_method"))
  } else {
    cat("  [OK] Check 3.3.17: ageing_method - all values valid (case-sensitive)\n")
  }
  
  invalid_length_code <- sampling %>%
    dplyr::filter(!is.na(length_code) & length_code != "" &
                    !(length_code %in% VALID_LENGTH_CODE))
  if (nrow(invalid_length_code) > 0) {
    cat("  [WARN] WARNING 3.3.18: Invalid length_code (case-sensitive)\n")
    cat("    Records affected:", nrow(invalid_length_code), "\n")
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_length_code, "3.3.18", "WARNING", "Invalid length_code",
                                                       invalid_length_code$length_code, VALID_LENGTH_CODE))
    all_warnings <- dplyr::bind_rows(all_warnings, invalid_length_code %>% dplyr::mutate(check_id = "3.3.18", check_message = "Invalid length_code"))
  } else {
    cat("  [OK] Check 3.3.18: length_code - all values valid (case-sensitive)\n")
  }

  invalid_commercial_size_non_lan <- sampling %>%
    dplyr::filter(
      catch_category != "Lan" &
        (
          (!is.na(commercial_size_category_scale) & commercial_size_category_scale != "") |
            (!is.na(commercial_size_category) & commercial_size_category != "")
        )
    )
  
  if (nrow(invalid_commercial_size_non_lan) > 0) {
    cat("  [WARN] WARNING 3.3.19: commercial_size fields must be empty when catch_category is not 'Lan'\n")
    cat("    Records affected:", nrow(invalid_commercial_size_non_lan), "\n")
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_commercial_size_non_lan, "3.3.19", "WARNING",
                                                       "commercial_size_category fields should only be used for landings (Lan)"))
    all_warnings <- dplyr::bind_rows(all_warnings, invalid_commercial_size_non_lan %>% dplyr::mutate(check_id = "3.3.19", check_message = "Commercial size present for non-Lan"))
  } else {
    cat("  [OK] Check 3.3.19-3.3.20: commercial_size_category - correct usage\n")
  }
  
  invalid_commercial_size_lan <- sampling %>%
    dplyr::filter(
      catch_category == "Lan" &
        !is.na(commercial_size_category) & commercial_size_category != "" &
        !(commercial_size_category %in% VALID_COMMERCIAL_SIZE_CAT)
    )
  
  if (nrow(invalid_commercial_size_lan) > 0) {
    print_check_report(invalid_commercial_size_lan, "3.3.20", "ERROR",
                       "Invalid commercial_size_category for catch_category = 'Lan'",
                       "commercial_size_category", VALID_COMMERCIAL_SIZE_CAT,
                       error_file = paste0("sampling_qc_errors", file_suffix, ".csv"))
    
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_commercial_size_lan, "3.3.20", "ERROR",
                                                       "Invalid commercial_size_category for landings",
                                                       invalid_commercial_size_lan$commercial_size_category, VALID_COMMERCIAL_SIZE_CAT))
    all_errors <- dplyr::bind_rows(all_errors, invalid_commercial_size_lan %>% dplyr::mutate(check_id = "3.3.20", check_message = "Invalid commercial_size_category (Lan)"))
    sampling <- sampling %>% dplyr::filter(!(.row_id %in% invalid_commercial_size_lan$.row_id))
  } else {
    cat("  [OK] Check 3.3.20: commercial_size_category - values valid for Lan\n")
  }
  
  cat("  [OK] After code list validation:", nrow(sampling), "records\n\n")
  
  # 3.4 NUMERIC AND TEMPORAL CHECKS ----
  cat("Step 3.4: Numeric and temporal validation...\n")
  
  invalid_n_sets_hauls <- sampling %>%
    dplyr::filter(sampling_type == "S" & !is.na(n_sets_hauls) & n_sets_hauls <= 0)
  if (nrow(invalid_n_sets_hauls) > 0) {
    cat("  [ERROR] ERROR 3.4.2: n_sets_hauls must be > 0 for sea sampling\n")
    cat("    Records affected:", nrow(invalid_n_sets_hauls), "\n")
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_n_sets_hauls, "3.4.2", "ERROR", "n_sets_hauls must be positive for sea sampling"))
    all_errors <- dplyr::bind_rows(all_errors, invalid_n_sets_hauls %>% dplyr::mutate(check_id = "3.4.2", check_message = "Invalid n_sets_hauls for sea sampling"))
    sampling <- sampling %>% dplyr::filter(!(.row_id %in% invalid_n_sets_hauls$.row_id))
  } else {
    cat("  [OK] Check 3.4.2: n_sets_hauls - valid for sea sampling\n")
  }
  
  # 3.4.3 days_at_sea - range 1-1000 for sea sampling (WARNING) ----
  # Spec: positiveInteger 1-1000; empty allowed for non-sea records
  invalid_days_at_sea <- sampling %>%
    dplyr::filter(!is.na(days_at_sea) & (days_at_sea < 1 | days_at_sea > 1000))
  if (nrow(invalid_days_at_sea) > 0) {
    cat("  [WARN] WARNING 3.4.3:", nrow(invalid_days_at_sea),
        "records with days_at_sea outside valid range (1-1000)
")
    check_log <- dplyr::bind_rows(check_log,
                                  log_check(invalid_days_at_sea, "3.4.3", "WARNING",
                                            "days_at_sea outside valid range (1-1000)"))
    all_warnings <- dplyr::bind_rows(all_warnings,
                                     invalid_days_at_sea %>%
                                       dplyr::mutate(check_id = "3.4.3",
                                                     check_message = paste0("days_at_sea out of range [", days_at_sea, "]")))
  } else {
    cat("  [OK] Check 3.4.3: days_at_sea range - OK\n")
  }
  
  # 3.4.4 duration_fishing_operation - range 1-43200 minutes (WARNING) ----
  # Spec: positiveInteger 1-43200 (= 720 hours); empty allowed
  invalid_duration <- sampling %>%
    dplyr::filter(!is.na(duration_fishing_operation) &
                    (duration_fishing_operation < 1 | duration_fishing_operation > 43200))
  if (nrow(invalid_duration) > 0) {
    cat("  [WARN] WARNING 3.4.4:", nrow(invalid_duration),
        "records with duration_fishing_operation outside valid range (1-43200 min)
")
    check_log <- dplyr::bind_rows(check_log,
                                  log_check(invalid_duration, "3.4.4", "WARNING",
                                            "duration_fishing_operation outside valid range (1-43200 minutes)"))
    all_warnings <- dplyr::bind_rows(all_warnings,
                                     invalid_duration %>%
                                       dplyr::mutate(check_id = "3.4.4",
                                                     check_message = paste0("duration_fishing_operation out of range [",
                                                                            duration_fishing_operation, " min]")))
  } else {
    cat("  [OK] Check 3.4.4: duration_fishing_operation range - OK\n")
  }
  
  sampling <- sampling %>%
    dplyr::mutate(
      date_parsed = lubridate::parse_date_time(date, orders = c("dmy", "ymd", "mdy"), quiet = TRUE),
      date_year = lubridate::year(date_parsed),
      year_num  = suppressWarnings(as.numeric(year))
    )
  
  invalid_date <- sampling %>% dplyr::filter(!is.na(date) & date != "" & (is.na(date_parsed) | (!is.na(year_num) & date_year != year_num)))
  if (nrow(invalid_date) > 0) {
    cat("  [ERROR] ERROR 3.4.5: Invalid date format or date-year mismatch\n")
    cat("    Records affected:", nrow(invalid_date), "\n")
    cat("    -> Date-year mismatch prevents correct quarter assignment for LFD raising stratum\n")
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_date, "3.4.5", "ERROR",
                                                       "Invalid date or year in date doesn't match year column - prevents correct stratum assignment"))
    all_errors <- dplyr::bind_rows(all_errors, invalid_date %>% dplyr::mutate(check_id = "3.4.5", check_message = "Invalid date or date-year mismatch"))
    sampling <- sampling %>% dplyr::filter(!(.row_id %in% invalid_date$.row_id))
  } else {
    cat("  [OK] Check 3.4.5: date - valid format and consistent with year\n")
  }

  invalid_length_class <- sampling %>% dplyr::filter(!is.na(length_class) & length_class < 1)
  if (nrow(invalid_length_class) > 0) {
    cat("  [ERROR] ERROR 3.4.6: length_class must be >= 1\n")
    cat("    Records affected:", nrow(invalid_length_class), "\n")
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_length_class, "3.4.6", "ERROR", "length_class must be >= 1"))
    all_errors <- dplyr::bind_rows(all_errors, invalid_length_class %>% dplyr::mutate(check_id = "3.4.6", check_message = "Invalid length_class"))
    sampling <- sampling %>% dplyr::filter(!(.row_id %in% invalid_length_class$.row_id))
  } else {
    cat("  [OK] Check 3.4.6: length_class - all values >= 1\n")
  }

  # 3.4.6 Length class plausibility check (possible wrong unit) ----
  # RCG spec: length_class is ALWAYS in mm. length_code is measurement
  # precision, NOT the unit.  When a MS enters values in cm instead of mm
  # the max observed length_class will be far below the known Lmax for
  # that species.
  #
  # Threshold: max observed length_class per species < 20% of Lmax_mm.
  # Example - HKE: Lmax = 1400 mm, threshold = 280 mm.
  # If the largest observed HKE is 75 mm the data are almost certainly in cm.
  #
  # Requires species_length_path pointing to species_length.json
  # (shipped with the package).  If not provided the check is skipped.

  if (!is.null(species_length_path) && file.exists(species_length_path)) {

    sp_ref <- jsonlite::fromJSON(species_length_path)
    sp_with_ref <- intersect(unique(sampling$species), names(sp_ref))

    # Keep only species that have a non-null Lmax_mm
    sp_with_ref <- sp_with_ref[
      vapply(sp_with_ref, function(s) !is.null(sp_ref[[s]]$Lmax_mm) && !is.na(sp_ref[[s]]$Lmax_mm), logical(1))
    ]

    if (length(sp_with_ref) == 0) {
      cat("  [OK] Check 3.4.6: species-specific length plausibility skipped\n")
    } else {

      ref_lookup <- dplyr::tibble(
        species  = sp_with_ref,
        Lmax_mm  = vapply(sp_with_ref, function(s) as.numeric(sp_ref[[s]]$Lmax_mm), numeric(1))
      )

      sp_max_lc <- sampling %>%
        dplyr::filter(!is.na(length_class), species %in% sp_with_ref) %>%
        dplyr::group_by(species) %>%
        dplyr::summarise(.lc_max = max(length_class, na.rm = TRUE), .groups = "drop") %>%
        dplyr::left_join(ref_lookup, by = "species") %>%
        dplyr::mutate(.flag = .lc_max < Lmax_mm * 0.20)

      sp_flagged <- sp_max_lc %>% dplyr::filter(.flag) %>% dplyr::pull(species)

      if (length(sp_flagged) > 0) {
        invalid_lclass <- sampling %>%
          dplyr::filter(species %in% sp_flagged, !is.na(length_class)) %>%
          dplyr::left_join(ref_lookup, by = "species") %>%
          dplyr::left_join(sp_max_lc %>% dplyr::select(species, .lc_max), by = "species")

        cat("  [WARN] WARNING 3.4.6: length_class likely NOT in mm (possible wrong unit)\n")
        cat("    Species affected :", paste(sort(sp_flagged), collapse = ", "), "\n")
        cat("    Records affected :", nrow(invalid_lclass), "\n")
        cat("    Note: RCG spec requires length_class in mm.\n")
        cat("    Verify source data before LFD raising.\n")

        for (sp in sort(sp_flagged)) {
          row <- sp_max_lc %>% dplyr::filter(species == sp)
          cat(sprintf("      %s: max observed = %.0f, Lmax = %.0f mm (threshold 20%% = %.0f mm)\n",
                      sp, row$.lc_max, row$Lmax_mm, row$Lmax_mm * 0.20))
        }

        check_log <- dplyr::bind_rows(check_log,
          log_check(
            invalid_lclass %>% dplyr::select(-Lmax_mm, -.lc_max),
            "3.4.7", "WARNING",
            paste0("length_class likely not in mm \u2014 species: ",
                   paste(sort(sp_flagged), collapse = ", "))
          ))

        all_warnings <- dplyr::bind_rows(all_warnings,
          invalid_lclass %>%
            dplyr::mutate(
              check_id      = "3.4.7",
              check_message = paste0(
                "length_class likely not in mm (max observed = ", .lc_max,
                "; Lmax = ", Lmax_mm, " mm; threshold = ",
                round(Lmax_mm * 0.20), " mm)")
            ) %>%
            dplyr::select(-Lmax_mm, -.lc_max))

      } else {
        cat("  [OK] Check 3.4.6: length_class plausible in mm for all species\n")
      }
    }
  } else {
    if (is.null(species_length_path)) {
      cat("  [OK] Check 3.4.6: species-specific length plausibility skipped\n")
    } else {
      warning("Check 3.4.6 skipped: species_length.json not found at ", species_length_path)
    }
  }

  invalid_number_at_length <- sampling %>% dplyr::filter(!is.na(number_at_length) & number_at_length < 1)
  if (nrow(invalid_number_at_length) > 0) {
    cat("  [ERROR] ERROR 3.4.8: number_at_length must be >= 1\n")
    cat("    Records affected:", nrow(invalid_number_at_length), "\n")
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_number_at_length, "3.4.8", "ERROR", "number_at_length must be >= 1"))
    all_errors <- dplyr::bind_rows(all_errors, invalid_number_at_length %>% dplyr::mutate(check_id = "3.4.8", check_message = "Invalid number_at_length"))
    sampling <- sampling %>% dplyr::filter(!(.row_id %in% invalid_number_at_length$.row_id))
  } else {
    cat("  [OK] Check 3.4.8: number_at_length - all values >= 1\n")
  }
  
  invalid_weight_subsample <- sampling %>%
    dplyr::filter(sampling_type %in% c("S", "D", "M")) %>%
    dplyr::filter(is.na(weight) | weight == 0 | is.na(subsample_weight) | subsample_weight == 0)
  
  if (nrow(invalid_weight_subsample) > 0) {
    cat("  [ERROR] ERROR 3.4.9: weight and subsample_weight MANDATORY for sampling_type S/D/M\n")
    cat("    Records affected:", nrow(invalid_weight_subsample), "\n")
    cat("    -> These fields are CRITICAL for LFD raising procedures in Script 3!\n")
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_weight_subsample, "3.4.9", "ERROR", "weight and subsample_weight mandatory for S/D/M - required for raising"))
    all_errors <- dplyr::bind_rows(all_errors, invalid_weight_subsample %>% dplyr::mutate(check_id = "3.4.9", check_message = "Missing weight/subsample_weight for S/D/M"))
    sampling <- sampling %>% dplyr::filter(!(.row_id %in% invalid_weight_subsample$.row_id))
  } else {
    cat("  [OK] Check 3.4.9: weight/subsample_weight - present for S/D/M sampling\n")
  }
  
  invalid_value_positive <- sampling %>%
    dplyr::filter((!is.na(weight) & weight <= 0) | (!is.na(subsample_weight) & subsample_weight <= 0))
  
  if (nrow(invalid_value_positive) > 0) {
    cat("  [ERROR] ERROR 3.4.10: weight/subsample_weight must be > 0 when present\n")
    cat("    Records affected:", nrow(invalid_value_positive), "\n")
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_value_positive, "3.4.10", "ERROR", "weight/subsample_weight must be positive"))
    all_errors <- dplyr::bind_rows(all_errors, invalid_value_positive %>% dplyr::mutate(check_id = "3.4.10", check_message = "Weight/subsample_weight <= 0"))
    sampling <- sampling %>% dplyr::filter(!(.row_id %in% invalid_value_positive$.row_id))
  } else {
    cat("  [OK] Check 3.4.10: weight/subsample_weight - all positive values\n")
  }
  
  invalid_individual_weight <- sampling %>% dplyr::filter(!is.na(individual_weight) & individual_weight <= 0)
  if (nrow(invalid_individual_weight) > 0) {
    cat("  [WARN] WARNING 3.4.11: individual_weight must be > 0\n")
    cat("    Records affected:", nrow(invalid_individual_weight), "\n")
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_individual_weight, "3.4.11", "WARNING", "individual_weight must be positive"))
    all_warnings <- dplyr::bind_rows(all_warnings, invalid_individual_weight %>% dplyr::mutate(check_id = "3.4.11", check_message = "Invalid individual_weight"))
  } else {
    cat("  [OK] Check 3.4.11: individual_weight - all positive values\n")
  }

  invalid_age <- sampling %>% dplyr::filter(!is.na(age) & (age < 0 | age > age_max))
  if (nrow(invalid_age) > 0) {
    cat("  [WARN] WARNING 3.4.12: Age outside reasonable range (0-", age_max, ")\n")
    cat("    Records affected:", nrow(invalid_age), "\n")
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_age, "3.4.12", "WARNING", paste0("Age outside reasonable range (0-", age_max, ")")))
    all_warnings <- dplyr::bind_rows(all_warnings, invalid_age %>% dplyr::mutate(check_id = "3.4.12", check_message = "Age out of range"))
  } else {
    cat("  [OK] Check 3.4.12: age - all within biological range\n")
  }

  invalid_fish_id <- sampling %>% dplyr::filter(!is.na(fish_id) & fish_id < 1)
  if (nrow(invalid_fish_id) > 0) {
    cat("  [WARN] WARNING 3.4.13: fish_id must be >= 1\n")
    cat("    Records affected:", nrow(invalid_fish_id), "\n")
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_fish_id, "3.4.13", "WARNING", "fish_id must be >= 1"))
    all_warnings <- dplyr::bind_rows(all_warnings, invalid_fish_id %>% dplyr::mutate(check_id = "3.4.13", check_message = "Invalid fish_id"))
  } else {
    cat("  [OK] Check 3.4.13: fish_id - all valid values\n")
  }

  # ---------------------------------------------------------------------------
  # 3.4.14 - 3.4.19  ALK INTERNAL CONSISTENCY CHECKS
  # These checks apply only to records that carry individual biological data
  # (i.e. rows where length_class is present, which are the rows relevant to
  # age-length key construction for the raising procedure).
  # ---------------------------------------------------------------------------
  cat("  --- ALK consistency checks (3.4.14-3.4.19) ---\n")

  # Helper: treat both NA and the string "NA" as "not available"
  is_na_or_str <- is_missing_token_local

  # 3.4.14  ageing_method present => age must not be missing (ERROR) ----
  # Rule: if a valid ageing method is declared (not NA / not "NA"), the age
  # value must be populated.  Missing age makes the ALK entry useless and
  # blocks age-based stock assessment downstream.
  invalid_alk_age_missing <- sampling %>%
    dplyr::filter(
      !is_na_or_str(ageing_method) &
        ageing_method %in% setdiff(VALID_AGEING_METHOD, "NA") &
        is_na_or_str(age)
    )

  if (nrow(invalid_alk_age_missing) > 0) {
    cat("  [ERROR] ERROR 3.4.14:", nrow(invalid_alk_age_missing),
        "records have ageing_method != NA but age is missing\n")
    check_log <- dplyr::bind_rows(
      check_log,
      log_check(invalid_alk_age_missing, "3.4.14", "ERROR",
                "ageing_method declared but age is missing - ALK entry unusable")
    )
    all_errors <- dplyr::bind_rows(
      all_errors,
      invalid_alk_age_missing %>%
        dplyr::mutate(check_id = "3.4.14",
                      check_message = "ageing_method present but age missing")
    )
  } else {
    cat("  [OK] Check 3.4.14: ageing_method/age coherence - OK\n")
  }

  # 3.4.15  age present => ageing_method must not be "NA" (WARNING) ----
  # Rule: if an age value is recorded, there must be a declared method that
  # produced it.  Reciprocal of 3.4.14; treated as WARNING because it does
  # not necessarily break the raising, but is a data quality flag.
  invalid_alk_method_missing <- sampling %>%
    dplyr::filter(
      !is.na(age) &
        is_na_or_str(ageing_method)
    )

  if (nrow(invalid_alk_method_missing) > 0) {
    cat("  [WARN] WARNING 3.4.15:", nrow(invalid_alk_method_missing),
        "records have age populated but ageing_method is NA\n")
    check_log <- dplyr::bind_rows(
      check_log,
      log_check(invalid_alk_method_missing, "3.4.15", "WARNING",
                "age present but ageing_method is NA")
    )
    all_warnings <- dplyr::bind_rows(
      all_warnings,
      invalid_alk_method_missing %>%
        dplyr::mutate(check_id = "3.4.15",
                      check_message = "age present but ageing_method is NA")
    )
  } else {
    cat("  [OK] Check 3.4.15: age/ageing_method reciprocal coherence - OK\n")
  }

  # 3.4.16  Age decimal precision: only .0 or .5 allowed (WARNING) ----
  # RCG spec: "Dec(1) for age available with precision of half year."
  # Decimal values other than 0 or 0.5 indicate a data entry error.
  invalid_alk_age_decimal <- sampling %>%
    dplyr::filter(
      !is.na(age) &
        !is_na_or_str(ageing_method) &
        !((age %% 1) %in% c(0, 0.5))
    )

  if (nrow(invalid_alk_age_decimal) > 0) {
    cat("  [WARN] WARNING 3.4.16:", nrow(invalid_alk_age_decimal),
        "records have age with invalid decimal part (only .0 or .5 allowed)\n")
    check_log <- dplyr::bind_rows(
      check_log,
      log_check(invalid_alk_age_decimal, "3.4.16", "WARNING",
                "age decimal precision invalid - only .0 or .5 allowed per RCG spec")
    )
    all_warnings <- dplyr::bind_rows(
      all_warnings,
      invalid_alk_age_decimal %>%
        dplyr::mutate(check_id = "3.4.16",
                      check_message = "age decimal not .0 or .5")
    )
  } else {
    cat("  [OK] Check 3.4.16: age decimal precision - all .0 or .5\n")
  }

  half_year_ages <- sampling %>%
    dplyr::filter(
      !is.na(age) &
        !is_na_or_str(ageing_method) &
        abs((age %% 1) - 0.5) < 1e-9
    )

  if (nrow(half_year_ages) > 0) {
    cat("  [WARN] WARNING 3.4.17:", nrow(half_year_ages),
        "records have half-year ages (.5)\n")
    cat("    Analytical note: verify downstream age-based modules preserve half-year classes\n")
    check_log <- dplyr::bind_rows(
      check_log,
      log_check(half_year_ages, "3.4.17", "WARNING",
                "Half-year ages (.5) present - confirm downstream age modules preserve them")
    )
    all_warnings <- dplyr::bind_rows(
      all_warnings,
      half_year_ages %>%
        dplyr::mutate(check_id = "3.4.17",
                      check_message = "Half-year ages present")
    )
  } else {
    cat("  [OK] Check 3.4.17: no half-year ages present\n")
  }

  # 3.4.18 fish_id age uniqueness within trip/species (ERROR) ----
  # Rule: within a given trip (trip_code), the same fish_id for the same
  # species cannot carry two different age values.  This would create
  # duplicate, contradictory ALK entries for the same individual.
  # Only applies to records where both fish_id and age are populated.
  alk_dup_check <- sampling %>%
    dplyr::filter(!is.na(fish_id) & !is.na(age)) %>%
    dplyr::group_by(trip_code, species, fish_id) %>%
    dplyr::summarise(
      n_distinct_ages = dplyr::n_distinct(age),
      ages_found      = paste(sort(unique(age)), collapse = " / "),
      n_records       = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::filter(n_distinct_ages > 1)

  if (nrow(alk_dup_check) > 0) {
    # Re-join to get original row ids for the error table
    invalid_alk_fish_dup <- sampling %>%
      dplyr::filter(!is.na(fish_id) & !is.na(age)) %>%
      dplyr::semi_join(alk_dup_check, by = c("trip_code", "species", "fish_id"))

    cat("  [ERROR] ERROR 3.4.18:", nrow(alk_dup_check),
        "fish_id/trip/species combinations carry contradictory age values\n")
    cat("    Total records affected:", nrow(invalid_alk_fish_dup), "\n")
    check_log <- dplyr::bind_rows(
      check_log,
      log_check(invalid_alk_fish_dup, "3.4.18", "ERROR",
                "Same fish_id carries multiple different ages within trip/species")
    )
    all_errors <- dplyr::bind_rows(
      all_errors,
      invalid_alk_fish_dup %>%
        dplyr::mutate(check_id = "3.4.18",
                      check_message = "fish_id with contradictory age values")
    )

    # Print a compact summary table to console for easy diagnosis
    cat("\n    Duplicate fish_id summary (trip / species / fish_id / ages found):\n")
    alk_dup_summary <- alk_dup_check %>%
      dplyr::mutate(label = paste0("    trip=", trip_code,
                                   "  sp=", species,
                                   "  fish_id=", fish_id,
                                   "  ages=", ages_found))
    cat(paste(alk_dup_summary$label, collapse = "\n"), "\n\n")
  } else {
    cat("  [OK] Check 3.4.18: fish_id age uniqueness within trip/species - OK\n")
  }

  # 3.4.19 ALK stratum coverage diagnostic (INFO) ----
  # Non-blocking diagnostic: reports which strata (year/quarter/area/species)
  # have aged fish and which do not.  A stratum without any aged fish cannot
  # contribute to age-structured stock assessment and the user may want to
  # investigate whether ageing data exist but were not submitted.
  # Quarter is derived from the date column (already validated in 3.4.4).
  # Use date_parsed (already a POSIXct, created and validated in 3.4.4) rather
  # than the raw character date column, which would cause format() to fail.
  if ("date_parsed" %in% names(sampling) && !all(is.na(sampling$date_parsed))) {
    alk_coverage <- sampling %>%
      dplyr::filter(!is.na(length_class)) %>%        # biological measurement rows only
      dplyr::mutate(
        date_parsed_dt = lubridate::as_datetime(date_parsed),
        quarter = dplyr::case_when(
          !is.na(date_parsed_dt) ~ as.integer(ceiling(lubridate::month(date_parsed_dt) / 3)),
          TRUE                   ~ NA_integer_
        )
      ) %>%
      dplyr::group_by(year, quarter, area, species) %>%
      dplyr::summarise(
        n_fish_measured = dplyr::n(),
        n_fish_aged     = sum(!is.na(age) & !is_na_or_str(ageing_method)),
        pct_aged        = round(100 * n_fish_aged / dplyr::n(), 1),
        .groups         = "drop"
      ) %>%
      dplyr::arrange(year, quarter, area, species)

    strata_no_age <- alk_coverage %>% dplyr::filter(n_fish_aged == 0)

    if (nrow(strata_no_age) > 0) {
      cat("  [WARN] WARNING 3.4.19:", nrow(strata_no_age),
          "strata have measured fish but NO aged individuals\n")
      cat("    Strata without aged fish:\n")
      strata_str <- strata_no_age %>%
        dplyr::mutate(label = paste0("    yr=", year, " q=", quarter,
                                     " area=", area, " sp=", species,
                                     "  n_measured=", n_fish_measured))
      cat(paste(strata_str$label, collapse = "\n"), "\n")

      check_log <- dplyr::bind_rows(
        check_log,
        log_check(
          strata_no_age,
          "3.4.19", "WARNING",
          paste0("ALK coverage: ", nrow(strata_no_age),
                 " strata have measured fish but no aged individuals")
        )
      )
      strata_no_age_records <- sampling %>%
        dplyr::filter(!is.na(length_class)) %>%
        dplyr::mutate(
          date_parsed_dt = lubridate::as_datetime(date_parsed),
          quarter = dplyr::case_when(
            !is.na(date_parsed_dt) ~ as.integer(ceiling(lubridate::month(date_parsed_dt) / 3)),
            TRUE                   ~ NA_integer_
          )
        ) %>%
        dplyr::semi_join(
          strata_no_age %>% dplyr::select(year, quarter, area, species),
          by = c("year", "quarter", "area", "species")
        ) %>%
        dplyr::select(-dplyr::any_of("date_parsed_dt"))

      all_warnings <- dplyr::bind_rows(
        all_warnings,
        strata_no_age_records %>%
          dplyr::mutate(check_id      = "3.4.19",
                        check_message = "Stratum with measured fish but no aged individuals")
      )
    } else {
      cat("  [OK] Check 3.4.19: ALK coverage - all strata with measured fish have aged individuals\n")
    }

    # Save the full coverage table to the output directory for user inspection
    alk_dir <- file.path(out_dir, "ALK_outlier")
    if (!dir.exists(alk_dir)) dir.create(alk_dir, recursive = TRUE)
    alk_coverage_path <- file.path(alk_dir, paste0("ALK_coverage_by_stratum", file_suffix, ".csv"))
    utils::write.csv(alk_coverage, alk_coverage_path, row.names = FALSE)
    cat("  ALK stratum coverage table saved to:", alk_coverage_path, "\n\n")
  } else {
    cat("  [OK] Check 3.4.19: ALK coverage skipped\n\n")
  }


  # ---------------------------------------------------------------------------
  # 3.4.20 ALK outlier detection - Landings (catch_category == "Lan")
  # 3.4.21 ALK outlier detection - Discards (catch_category == "Dis")
  # For each stratum (year / quarter / area / species) with >= 5 valid
  # age-length pairs, fit lm(length_class ~ age) and flag records with
  # |rstandard()| > 2.5.  Separate check and output for Lan and Dis.
  # Severity: WARNING (non-blocking).
  # ---------------------------------------------------------------------------

  has_ggplot2 <- requireNamespace("ggplot2", quietly = TRUE)
  if (!has_ggplot2)
    cat("  [OK] Outlier plots skipped\n")

  run_alk_outlier_check <- function(cc_label, check_id_str) {

    cat("  --- ALK outlier detection", check_id_str, "(catch_category =", cc_label, ") ---\n")

    alk_min_n <- 5L

    alk_sub <- sampling %>%
      dplyr::filter(
        catch_category == cc_label &
          !is.na(age) & !is.na(length_class) &
          !is_na_or_str(ageing_method)
      ) %>%
      dplyr::mutate(
        quarter = dplyr::case_when(
          !is.na(date_parsed) ~ as.integer(ceiling(as.integer(format(date_parsed, "%m")) / 3)),
          TRUE                ~ NA_integer_
        )
      )

    if (nrow(alk_sub) == 0) {
      cat("  [OK] Check", check_id_str, ": ALK outlier detection skipped [", cc_label, "]\n\n")
      return(invisible(NULL))
    }

    strata_list <- alk_sub %>%
      dplyr::group_by(year, quarter, area, species) %>%
      dplyr::summarise(n_pairs = dplyr::n(), .groups = "drop") %>%
      dplyr::filter(n_pairs >= alk_min_n)

    skipped <- alk_sub %>%
      dplyr::group_by(year, quarter, area, species) %>%
      dplyr::summarise(n_pairs = dplyr::n(), .groups = "drop") %>%
      dplyr::filter(n_pairs < alk_min_n)

    if (nrow(skipped) > 0)
      cat("  [OK] Check", check_id_str, ": ALK outlier detection applied to eligible strata only [", cc_label, "]\n")

    all_outliers <- dplyr::tibble()
    plot_dir     <- file.path(out_dir, "ALK_outlier")

    for (i in seq_len(nrow(strata_list))) {
      sp <- strata_list$species[i]
      yr <- strata_list$year[i]
      qt <- strata_list$quarter[i]
      ar <- strata_list$area[i]

      sd <- alk_sub %>%
        dplyr::filter(species == sp, year == yr, quarter == qt, area == ar)

      fit <- tryCatch(lm(length_class ~ age, data = sd), error = function(e) NULL)
      if (is.null(fit)) next

      sd <- sd %>%
        dplyr::mutate(
          .fitted   = fitted(fit),
          .resid_st = rstandard(fit),
          .outlier  = !is.na(.resid_st) & abs(.resid_st) > 2.5
        )

      outliers <- sd %>% dplyr::filter(.outlier)
      if (nrow(outliers) == 0) next

      all_outliers <- dplyr::bind_rows(
        all_outliers,
        outliers %>%
          dplyr::select(
            dplyr::any_of(c(
              "year", "quarter", "area", "species", "catch_category",
              "trip_code", "fish_id", "age", "length_class", "sex",
              "ageing_method", ".row_id"
            )),
            resid_standardised = .resid_st
          ) %>%
          dplyr::mutate(
            check_id      = check_id_str,
            check_message = paste0("ALK outlier: |rstandard| = ",
                                   round(abs(resid_standardised), 2))
          )
      )

      if (has_ggplot2) {
        if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

        age_range <- range(sd$age, na.rm = TRUE)
        age_seq   <- seq(age_range[1], age_range[2], length.out = 100)
        sigma_fit <- summary(fit)$sigma
        pred_df   <- data.frame(age = age_seq)
        pred_df$length_class <- predict(fit, newdata = pred_df)
        pred_df$upper <- pred_df$length_class + 2.5 * sigma_fit
        pred_df$lower <- pred_df$length_class - 2.5 * sigma_fit

        sd <- sd %>%
          dplyr::mutate(
            .label = dplyr::case_when(
              .outlier & !is.na(fish_id) ~ as.character(fish_id),
              .outlier                   ~ as.character(.row_id),
              TRUE                       ~ NA_character_
            )
          )

        p <- ggplot2::ggplot(sd, ggplot2::aes(x = age, y = length_class)) +
          ggplot2::geom_ribbon(
            data = pred_df,
            ggplot2::aes(x = age, ymin = lower, ymax = upper),
            inherit.aes = FALSE,
            fill = "#AECDE8", alpha = 0.4
          ) +
          ggplot2::geom_line(
            data = pred_df,
            ggplot2::aes(x = age, y = length_class),
            inherit.aes = FALSE,
            colour = "#2171B5", linewidth = 0.8
          ) +
          ggplot2::geom_point(
            data = sd %>% dplyr::filter(!.outlier),
            colour = "grey50", size = 2, alpha = 0.7
          ) +
          ggplot2::geom_point(data = outliers, colour = "#CB181D", size = 3) +
          ggplot2::geom_text(
            ggplot2::aes(label = .label),
            colour = "#CB181D", size = 2.8, vjust = -0.8, na.rm = TRUE
          ) +
          ggplot2::labs(
            title    = paste0("ALK outliers (", cc_label, "): ", sp,
                              "  |  ", ar, "  yr=", yr, "  q=", qt),
            subtitle = paste0("lm(length_class ~ age)  |  n=", nrow(sd),
                              "  outliers=", nrow(outliers),
                              "  (|rstandard| > 2.5)"),
            x       = "Age (years)",
            y       = "Length class (mm)",
            caption = "Red points: |standardised residual| > 2.5.  Labels: fish_id or row number."
          ) +
          ggplot2::theme_bw(base_size = 11) +
          ggplot2::theme(
            plot.title    = ggplot2::element_text(face = "bold", size = 12),
            plot.subtitle = ggplot2::element_text(size = 9, colour = "grey40"),
            plot.caption  = ggplot2::element_text(size = 8, colour = "grey50")
          )

        plot_fname <- file.path(
          plot_dir,
          paste0("ALK_outliers_", check_id_str, "_", cc_label, "_", sp, "_", ar,
                 "_yr", yr, "_q", qt, ".png")
        )
        ggplot2::ggsave(plot_fname, plot = p, width = 7, height = 5, dpi = 150)
      }
    }

    if (nrow(all_outliers) > 0) {
      n_strata <- all_outliers %>%
        dplyr::distinct(year, quarter, area, species) %>% nrow()

      cat("  [WARN] WARNING", check_id_str, ":", nrow(all_outliers),
          "ALK outlier records across", n_strata, "strata [", cc_label, "]\n")
      cat("    Threshold: |standardised residual| > 2.5\n")

      check_log <<- dplyr::bind_rows(
        check_log,
        log_check(all_outliers, check_id_str, "WARNING",
                  paste0("ALK age-length outliers (|rstandard| > 2.5) [", cc_label, "]: ",
                         nrow(all_outliers), " records in ", n_strata, " strata"))
      )
      all_warnings <<- dplyr::bind_rows(
        all_warnings,
        all_outliers %>%
          dplyr::select(-dplyr::any_of(c("check_id", "check_message"))) %>%
          dplyr::mutate(
            check_id      = check_id_str,
            check_message = paste0("ALK outlier [", cc_label, "] |rstandard|=",
                                   round(abs(resid_standardised), 2))
          )
      )

      if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
      csv_path <- file.path(plot_dir,
                            paste0("ALK_outliers_", check_id_str, "_",
                                   cc_label, file_suffix, ".csv"))
      utils::write.csv(format_qc_issue_output(all_outliers), csv_path, row.names = FALSE)
      cat("    Outlier records saved to:", csv_path, "\n")
      if (has_ggplot2)
        cat("    Output saved to:", plot_dir, "\n\n")

    } else {
      cat("  [OK] Check", check_id_str, ": no ALK outliers detected (|rstandard| > 2.5) [",
          cc_label, "]\n\n")
    }
  }

  run_alk_outlier_check("Lan", "3.4.20")
  run_alk_outlier_check("Dis", "3.4.21")




  # ---------------------------------------------------------------------------
  # 3.4.22 Length class outlier detection - Landings (catch_category == "Lan")
  # 3.4.23 Length class outlier detection - Discards (catch_category == "Dis")
  # For each stratum (year / quarter / area / species) with >= 5 length
  # measurements, flag records outside [Q1 - 1.5*IQR, Q3 + 1.5*IQR].
  # Upper bound also capped at Lmax (from species_length.json) if available.
  # Separate check and output for Lan and Dis.
  # Severity: WARNING (non-blocking).
  # ---------------------------------------------------------------------------

  # Load species Lmax lookup once (reuse species_length_path from 3.4.6)
  lmax_lookup <- NULL
  if (!is.null(species_length_path) && file.exists(species_length_path)) {
    lmax_raw <- tryCatch(jsonlite::fromJSON(species_length_path), error = function(e) NULL)
    if (!is.null(lmax_raw)) {
      sp_names <- names(lmax_raw)
      # JSON structure: each entry is a list with $Lmax_mm (same as used in 3.4.6)
      lmax_vals <- vapply(
        sp_names,
        function(s) {
          v <- lmax_raw[[s]]$Lmax_mm
          if (!is.null(v) && !is.na(v)) as.numeric(v) else NA_real_
        },
        numeric(1)
      )
      lmax_lookup <- dplyr::tibble(
        species = toupper(sp_names),
        lmax_mm = lmax_vals
      ) %>% dplyr::filter(!is.na(lmax_mm))
    }
  }

  run_len_outlier_check <- function(cc_label, check_id_str) {

    cat("  --- Length outlier detection", check_id_str,
        "(catch_category =", cc_label, ") ---\n")

    len_min_n <- 5L

    len_sub <- sampling %>%
      dplyr::filter(
        catch_category == cc_label &
          !is.na(length_class) & length_class >= 1
      ) %>%
      dplyr::mutate(
        quarter = dplyr::case_when(
          !is.na(date_parsed) ~ as.integer(ceiling(as.integer(format(date_parsed, "%m")) / 3)),
          TRUE                ~ NA_integer_
        )
      )

    if (nrow(len_sub) == 0) {
      cat("  [OK] Check", check_id_str, ": length outlier detection skipped [", cc_label, "]\n\n")
      return(invisible(NULL))
    }

    # Compute IQR fences per stratum
    strata_stats <- len_sub %>%
      dplyr::group_by(year, quarter, area, species) %>%
      dplyr::summarise(
        n_len       = dplyr::n(),
        q1          = stats::quantile(length_class, 0.25, na.rm = TRUE),
        q3          = stats::quantile(length_class, 0.75, na.rm = TRUE),
        iqr         = stats::IQR(length_class, na.rm = TRUE),
        .groups     = "drop"
      ) %>%
      dplyr::filter(n_len >= len_min_n) %>%
      dplyr::mutate(
        lower_fence = q1 - 1.5 * iqr,
        upper_fence = q3 + 1.5 * iqr
      )

    # Add Lmax upper cap if available
    if (!is.null(lmax_lookup)) {
      strata_stats <- strata_stats %>%
        dplyr::left_join(lmax_lookup, by = "species") %>%
        dplyr::mutate(
          upper_fence = dplyr::case_when(
            !is.na(lmax_mm) ~ pmin(upper_fence, lmax_mm),
            TRUE            ~ upper_fence
          )
        )
    }

    skipped <- len_sub %>%
      dplyr::group_by(year, quarter, area, species) %>%
      dplyr::summarise(n_len = dplyr::n(), .groups = "drop") %>%
      dplyr::filter(n_len < len_min_n)

    if (nrow(skipped) > 0)
      cat("  [OK] Check", check_id_str, ": length outlier detection applied to eligible strata only [", cc_label, "]\n")

    len_flagged <- len_sub %>%
      dplyr::inner_join(
        strata_stats %>%
          dplyr::select(year, quarter, area, species,
                        lower_fence, upper_fence, q1, q3, iqr),
        by = c("year", "quarter", "area", "species")
      ) %>%
      dplyr::mutate(
        .outlier = length_class < lower_fence | length_class > upper_fence
      )

    all_outliers <- len_flagged %>% dplyr::filter(.outlier)
    plot_dir     <- file.path(out_dir, "LEN_outlier")

    # Per-stratum plots
    if (has_ggplot2 && nrow(all_outliers) > 0) {
      if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

      strata_with_out <- all_outliers %>%
        dplyr::distinct(year, quarter, area, species)

      for (i in seq_len(nrow(strata_with_out))) {
        sp <- strata_with_out$species[i]
        yr <- strata_with_out$year[i]
        qt <- strata_with_out$quarter[i]
        ar <- strata_with_out$area[i]

        sd <- len_flagged %>%
          dplyr::filter(species == sp, year == yr, quarter == qt, area == ar)

        fence_lo  <- unique(sd$lower_fence)
        fence_hi  <- unique(sd$upper_fence)
        out_s     <- sd %>% dplyr::filter(.outlier)

        # Add Lmax annotation if available
        lmax_val <- if (!is.null(lmax_lookup)) {
          lmax_lookup$lmax_mm[lmax_lookup$species == sp]
        } else numeric(0)

        p <- ggplot2::ggplot(sd, ggplot2::aes(x = length_class)) +
          ggplot2::geom_histogram(
            ggplot2::aes(fill = .outlier),
            binwidth = 10, colour = "white", alpha = 0.85
          ) +
          ggplot2::scale_fill_manual(
            values = c("FALSE" = "grey60", "TRUE" = "#CB181D"),
            labels = c("FALSE" = "Normal", "TRUE" = "Outlier"),
            name   = NULL
          ) +
          ggplot2::geom_vline(xintercept = fence_lo, linetype = "dashed",
                              colour = "#2171B5", linewidth = 0.8) +
          ggplot2::geom_vline(xintercept = fence_hi, linetype = "dashed",
                              colour = "#2171B5", linewidth = 0.8) +
          ggplot2::annotate("text", x = fence_lo, y = Inf,
                            label = paste0("Q1-1.5IQR\n", round(fence_lo)),
                            vjust = 1.4, hjust = 1.1, size = 2.8,
                            colour = "#2171B5") +
          ggplot2::annotate("text", x = fence_hi, y = Inf,
                            label = paste0("Q3+1.5IQR\n", round(fence_hi)),
                            vjust = 1.4, hjust = -0.1, size = 2.8,
                            colour = "#2171B5") +
          {if (length(lmax_val) == 1 && !is.na(lmax_val))
            ggplot2::geom_vline(xintercept = lmax_val, linetype = "dotted",
                                colour = "#D95F02", linewidth = 0.8)
          } +
          {if (length(lmax_val) == 1 && !is.na(lmax_val))
            ggplot2::annotate("text", x = lmax_val, y = Inf,
                              label = paste0("Lmax\n", lmax_val),
                              vjust = 1.4, hjust = -0.1, size = 2.8,
                              colour = "#D95F02")
          } +
          ggplot2::labs(
            title    = paste0("Length outliers (", cc_label, "): ", sp,
                              "  |  ", ar, "  yr=", yr, "  q=", qt),
            subtitle = paste0("n=", nrow(sd), "  outliers=", nrow(out_s),
                              "  (IQR rule", if (!is.null(lmax_lookup)) " + Lmax cap" else "", ")"),
            x       = "Length class (mm)",
            y       = "Count",
            caption = paste0("Red: outside IQR fences (blue dashed).",
                             if (length(lmax_val)==1 && !is.na(lmax_val))
                               "  Orange dotted: Lmax." else "")
          ) +
          ggplot2::theme_bw(base_size = 11) +
          ggplot2::theme(
            plot.title    = ggplot2::element_text(face = "bold", size = 12),
            plot.subtitle = ggplot2::element_text(size = 9, colour = "grey40"),
            plot.caption  = ggplot2::element_text(size = 8, colour = "grey50"),
            legend.position = "top"
          )

        plot_fname <- file.path(
          plot_dir,
          paste0("LEN_outliers_", cc_label, "_", sp, "_", ar,
                 "_yr", yr, "_q", qt, ".png")
        )
        ggplot2::ggsave(plot_fname, plot = p, width = 7, height = 5, dpi = 150)
      }
    }

    if (nrow(all_outliers) > 0) {
      n_strata <- all_outliers %>%
        dplyr::distinct(year, quarter, area, species) %>% nrow()

      cat("  [WARN] WARNING", check_id_str, ":", nrow(all_outliers),
          "length outlier records across", n_strata, "strata [", cc_label, "]\n")
      cat("    Method: IQR rule [Q1-1.5*IQR, Q3+1.5*IQR]",
          if (!is.null(lmax_lookup)) "+ Lmax upper cap" else "", "\n")

      check_log <<- dplyr::bind_rows(
        check_log,
        log_check(all_outliers, check_id_str, "WARNING",
                  paste0("Length outliers (IQR", if (!is.null(lmax_lookup)) "+Lmax" else "",
                         ") [", cc_label, "]: ",
                         nrow(all_outliers), " records in ", n_strata, " strata"))
      )
      all_warnings <<- dplyr::bind_rows(
        all_warnings,
        all_outliers %>%
          dplyr::select(
            dplyr::any_of(c(
              "year", "quarter", "area", "species", "catch_category",
              "trip_code", "fish_id", "length_class", "sex", ".row_id",
              "lower_fence", "upper_fence"
            ))
          ) %>%
          dplyr::mutate(
            check_id      = check_id_str,
            check_message = paste0("Length outlier [", cc_label, "]: ",
                                   length_class, " outside [",
                                   round(lower_fence), ", ",
                                   round(upper_fence), "]")
          )
      )

      if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
      csv_path <- file.path(plot_dir,
                            paste0("LEN_outliers_", check_id_str, "_",
                                   cc_label, file_suffix, ".csv"))
      utils::write.csv(
        format_qc_issue_output(all_outliers %>%
          dplyr::select(dplyr::any_of(c(
            "year", "quarter", "area", "species", "catch_category",
            "trip_code", "fish_id", "length_class", "sex", ".row_id",
            "lower_fence", "upper_fence", "q1", "q3", "iqr"
          )))),
        csv_path, row.names = FALSE
      )
      cat("    Outlier records saved to:", csv_path, "\n")
      if (has_ggplot2)
        cat("    Output saved to:", plot_dir, "\n\n")

    } else {
      cat("  [OK] Check", check_id_str,
          ": no length outliers detected (IQR rule) [", cc_label, "]\n\n")
    }
  }

  run_len_outlier_check("Lan", "3.4.22")
  run_len_outlier_check("Dis", "3.4.23")

  # 3.4.24 Individual weight vs length class plausibility (WARNING) ----
  # Rule: W(g) / L(mm)^3 must be in [1e-9, 5e-4] - a species-agnostic envelope
  # that covers all known Mediterranean fish species.
  # Catches the two most frequent unit entry errors:
  #   * Weight in kg instead of g  -> ratio ~1000x too small  -> below 1e-9
  #   * Length in cm instead of mm -> length 10x too small    -> ratio ~1000x too large -> above 5e-4
  # Applies only to records where BOTH individual_weight > 0 AND length_class > 0
  # (i.e., individual biological measurements, not pooled LFD records).
  wl_ratio_min <- 1e-9
  wl_ratio_max <- 5e-4

  invalid_wl_ratio <- sampling %>%
    dplyr::filter(
      !is.na(individual_weight) & individual_weight > 0 &
      !is.na(length_class)      & length_class      > 0
    ) %>%
    dplyr::mutate(
      .wl_ratio = individual_weight / (as.numeric(length_class)^3)
    ) %>%
    dplyr::filter(.wl_ratio < wl_ratio_min | .wl_ratio > wl_ratio_max)

  if (nrow(invalid_wl_ratio) > 0) {
    sp_flagged_wl <- sort(unique(invalid_wl_ratio$species))
    cat("  [WARN] WARNING 3.4.24:", nrow(invalid_wl_ratio),
        "records with implausible individual_weight vs length_class ratio\n")
    cat("    Possible unit error (kg instead of g, or cm instead of mm)\n")
    cat("    Species affected:", paste(sp_flagged_wl, collapse = ", "), "\n")
    cat("    Valid W(g)/L(mm)^3 range: [", wl_ratio_min, ",", wl_ratio_max, "]\n")
    check_log <- dplyr::bind_rows(check_log,
      log_check(invalid_wl_ratio %>% dplyr::select(-.wl_ratio), "3.4.24", "WARNING",
                "Implausible individual_weight/length_class ratio - possible unit error (kg vs g, or cm vs mm)"))
    all_warnings <- dplyr::bind_rows(all_warnings,
      invalid_wl_ratio %>%
        dplyr::select(-.wl_ratio) %>%
        dplyr::mutate(check_id = "3.4.24",
                      check_message = "Individual weight/length ratio outside biological envelope"))
  } else {
    cat("  [OK] Check 3.4.24: individual_weight/length_class ratio - all within biological range\n")
  }


  # --- end ALK consistency checks ---

  cat("  [OK] After numeric/temporal checks:", nrow(sampling), "records\n\n")
  
  # 3.5 TEMPLATE CONDITIONAL RULES ----
  cat("Step 3.5: Template conditional rules...\n")
  
  invalid_aggregation_T <- sampling %>% dplyr::filter(aggregation_level == "T" & station_code != "999")
  if (nrow(invalid_aggregation_T) > 0) {
    cat("  [ERROR] ERROR 3.5.1: aggregation_level='T' requires station_code='999'\n")
    cat("    Records affected:", nrow(invalid_aggregation_T), "\n")
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_aggregation_T, "3.5.1", "ERROR", "Trip-level aggregation (T) requires station_code='999'"))
    all_errors <- dplyr::bind_rows(all_errors, invalid_aggregation_T %>% dplyr::mutate(check_id = "3.5.1", check_message = "Trip-level without station_code=999"))
    sampling <- sampling %>% dplyr::filter(!(.row_id %in% invalid_aggregation_T$.row_id))
  } else {
    cat("  [OK] Check 3.5.1: aggregation_level 'T' - all have station_code='999'\n")
  }
  
  invalid_aggregation_H <- sampling %>%
    dplyr::filter(aggregation_level == "H" &
                    (is.na(station_code) | station_code == "" | nchar(station_code) > max_length_station_code))
  if (nrow(invalid_aggregation_H) > 0) {
    cat("  [ERROR] ERROR 3.5.2: aggregation_level='H' requires valid station_code\n")
    cat("    Records affected:", nrow(invalid_aggregation_H), "\n")
    cat("    Requirements: non-empty, max length", max_length_station_code, "chars\n")
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_aggregation_H, "3.5.2", "ERROR", "Haul-level aggregation (H) requires valid station_code"))
    all_errors <- dplyr::bind_rows(all_errors, invalid_aggregation_H %>% dplyr::mutate(check_id = "3.5.2", check_message = "Haul-level without valid station_code"))
    sampling <- sampling %>% dplyr::filter(!(.row_id %in% invalid_aggregation_H$.row_id))
  } else {
    cat("  [OK] Check 3.5.2: aggregation_level 'H' - all have valid station_code\n")
  }
  
  invalid_catch_species_reg <- sampling %>% dplyr::filter(catch_registration == "Non" & species_registration != "Non")
  if (nrow(invalid_catch_species_reg) > 0) {
    cat("  [ERROR] ERROR 3.5.3: catch_registration='Non' requires species_registration='Non'\n")
    cat("    Records affected:", nrow(invalid_catch_species_reg), "\n")
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_catch_species_reg, "3.5.3", "ERROR", "Catch registration 'Non' requires species registration 'Non'"))
    all_errors <- dplyr::bind_rows(all_errors, invalid_catch_species_reg %>% dplyr::mutate(check_id = "3.5.3", check_message = "Registration flag inconsistency"))
    sampling <- sampling %>% dplyr::filter(!(.row_id %in% invalid_catch_species_reg$.row_id))
  } else {
    cat("  [OK] Check 3.5.3: catch/species registration - consistent\n")
  }
  
  invalid_length_number_empty <- sampling %>%
    dplyr::filter((is.na(length_class) | length_class == "") &
                    !is.na(number_at_length) & number_at_length != "")
  if (nrow(invalid_length_number_empty) > 0) {
    cat("  [ERROR] ERROR 3.5.4: If length_class is empty, number_at_length must be empty\n")
    cat("    Records affected:", nrow(invalid_length_number_empty), "\n")
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_length_number_empty, "3.5.4", "ERROR", "number_at_length present without length_class"))
    all_errors <- dplyr::bind_rows(all_errors, invalid_length_number_empty %>% dplyr::mutate(check_id = "3.5.4", check_message = "number_at_length without length_class"))
    sampling <- sampling %>% dplyr::filter(!(.row_id %in% invalid_length_number_empty$.row_id))
  } else {
    cat("  [OK] Check 3.5.4: length_class/number_at_length - consistent (empty)\n")
  }
  
  invalid_length_number_present <- sampling %>%
    dplyr::filter(!is.na(number_at_length) & number_at_length != "" &
                    (is.na(length_class) | length_class == ""))
  if (nrow(invalid_length_number_present) > 0) {
    cat("  [ERROR] ERROR 3.5.5: If number_at_length is present, length_class must be present\n")
    cat("    Records affected:", nrow(invalid_length_number_present), "\n")
    check_log <- dplyr::bind_rows(check_log, log_check(invalid_length_number_present, "3.5.5", "ERROR", "length_class missing when number_at_length present"))
    all_errors <- dplyr::bind_rows(all_errors, invalid_length_number_present %>% dplyr::mutate(check_id = "3.5.5", check_message = "length_class missing with number_at_length"))
    sampling <- sampling %>% dplyr::filter(!(.row_id %in% invalid_length_number_present$.row_id))
  } else {
    cat("  [OK] Check 3.5.5: length_class/number_at_length - consistent (present)\n")
  }
  
  cat("  [OK] After conditional rules:", nrow(sampling), "records\n\n")

  # 3.5.6 Implausible number_at_length (WARNING) ----
  # number_at_length > 1000 on a single record strongly suggests the value was
  # entered as a total catch count rather than a per-length-class count, or that
  # pooled records from multiple hauls were not properly disaggregated.
  # Per-length-class counts > 1000 are biologically possible in large pelagic
  # hauls (e.g. anchovy, sardine) but unusual for demersal species; the check
  # flags all cases for human review without removing records.
  cat("Step 3.5.6: Implausible number_at_length check...\n")

  nal_threshold <- 1000L

  invalid_nal <- sampling %>%
    dplyr::filter(!is.na(number_at_length) & number_at_length > nal_threshold)

  if (nrow(invalid_nal) > 0) {
    sp_flagged <- paste(sort(unique(invalid_nal$species)), collapse = ", ")
    cat("  [WARN] WARNING 3.5.6:", nrow(invalid_nal),
        "record(s) with number_at_length >", nal_threshold, "\n")
    cat("    Possible causes: total catch count instead of per-length-class count,\n")
    cat("                     or aggregation across hauls not disaggregated\n")
    cat("    Species affected:", sp_flagged, "\n")
    check_log <- dplyr::bind_rows(check_log,
      log_check(invalid_nal, "3.5.6", "WARNING",
                paste0("number_at_length > ", nal_threshold,
                       " - possible aggregation or unit error")))
    all_warnings <- dplyr::bind_rows(all_warnings,
      invalid_nal %>%
        dplyr::mutate(check_id = "3.5.6",
                      check_message = paste0("number_at_length > ", nal_threshold)))
  } else {
    cat("  [OK] Check 3.5.6: number_at_length <=", nal_threshold, "- all values plausible\n")
  }
  cat("\n")

  invalid_individual_weight_nal <- sampling %>%
    dplyr::filter(
      !is.na(individual_weight) & individual_weight > 0 &
        (is.na(number_at_length) | number_at_length != 1)
    )

  if (nrow(invalid_individual_weight_nal) > 0) {
    cat("  [WARN] WARNING 3.5.7:", nrow(invalid_individual_weight_nal),
        "record(s) have individual_weight present but number_at_length is not 1\n")
    check_log <- dplyr::bind_rows(
      check_log,
      log_check(invalid_individual_weight_nal, "3.5.7", "WARNING",
                "individual_weight present but number_at_length is not 1")
    )
    all_warnings <- dplyr::bind_rows(
      all_warnings,
      invalid_individual_weight_nal %>%
        dplyr::mutate(check_id = "3.5.7",
                      check_message = "individual_weight present with number_at_length != 1")
    )
  } else {
    cat("  [OK] Check 3.5.7: individual_weight implies number_at_length = 1\n")
  }
  cat("\n")

  # 3.6 COORDINATE VALIDATION ----
  cat("Step 3.6: Coordinate syntax checks...\n")
  
  invalid_coordinates <- sampling %>%
    dplyr::filter(
      (!is.na(initial_latitude) & (initial_latitude < lat_min | initial_latitude > lat_max)) |
        (!is.na(initial_longitude) & (initial_longitude < lon_min | initial_longitude > lon_max)) |
        (!is.na(final_latitude) & (final_latitude < lat_min | final_latitude > lat_max)) |
        (!is.na(final_longitude) & (final_longitude < lon_min | final_longitude > lon_max))
    )
  
  if (nrow(invalid_coordinates) > 0) {
    cat("  [ERROR] ERROR 3.6:", nrow(invalid_coordinates),
        "records with coordinates outside RCG spec range\n")
    cat("    Valid ranges per spec: Lat [", lat_min, ",", lat_max,
        "] | Lon [", lon_min, ",", lon_max, "]\n")
    check_log <- dplyr::bind_rows(check_log,
      log_check(invalid_coordinates, "3.6", "ERROR",
                paste0("Coordinates outside RCG spec range",
                       " (Lat: ", lat_min, "-", lat_max,
                       ", Lon: ", lon_min, "-", lon_max, ")")))
    all_errors <- dplyr::bind_rows(all_errors,
      invalid_coordinates %>%
        dplyr::mutate(check_id = "3.6",
                      check_message = "Coordinates outside RCG geographic range"))
    sampling <- sampling %>% dplyr::filter(!(.row_id %in% invalid_coordinates$.row_id))
  } else {
    cat("  [OK] Check 3.6: coordinates - all within RCG spec range\n")
  }
  
  cat("  [OK] After coordinate checks:", nrow(sampling), "records\n\n")

  # 3.7 SEX RATIO PLAUSIBILITY (WARNING) ----
  # For each stratum (species - area - year), flag if the observed sex ratio
  # F/(F+M) is extreme (< 0.02 or > 0.98) on samples with >= 20 sexed fish.
  # Extreme ratios on large samples are highly unlikely biologically and often
  # indicate systematic coding errors (e.g. all records coded as "F").
  cat("Step 3.7: Sex ratio plausibility check...\n")

  sex_ratio_min_n <- 20L   # minimum sexed fish per stratum to apply the test
  sex_ratio_lo    <- 0.02  # flag if F/(F+M) < 2%
  sex_ratio_hi    <- 0.98  # flag if F/(F+M) > 98%

  has_sex_col <- "sex" %in% names(sampling)

  if (!has_sex_col) {
    cat("  [OK] Check 3.7: sex ratio skipped\n\n")
  } else {
    sex_strata <- sampling %>%
      dplyr::filter(!is.na(sex) & sex %in% c("F", "M")) %>%
      dplyr::group_by(flag_country, year, area, species) %>%
      dplyr::summarise(
        n_sexed  = dplyr::n(),
        n_female = sum(sex == "F", na.rm = TRUE),
        n_male   = sum(sex == "M", na.rm = TRUE),
        sex_ratio_F = n_female / (n_female + n_male),
        .groups = "drop"
      ) %>%
      dplyr::filter(n_sexed >= sex_ratio_min_n &
                      (sex_ratio_F < sex_ratio_lo | sex_ratio_F > sex_ratio_hi))

    if (nrow(sex_strata) > 0) {
      cat("  [WARN] WARNING 3.7:", nrow(sex_strata),
          "strata with extreme sex ratio (F/(F+M) outside [", sex_ratio_lo, ",", sex_ratio_hi,
          "] on >= ", sex_ratio_min_n, " fish)\n")
      for (i in seq_len(nrow(sex_strata))) {
        r <- sex_strata[i, ]
        cat(sprintf("    %s | %s | %s | %s  ->  F/(F+M) = %.3f  (n=%d)\n",
                    r$flag_country, r$year, r$area, r$species,
                    r$sex_ratio_F, r$n_sexed))
      }
      check_log <- dplyr::bind_rows(check_log,
        log_check(sex_strata, "3.7", "WARNING",
                  paste0("Extreme sex ratio F/(F+M) outside [", sex_ratio_lo, ",", sex_ratio_hi,
                         "] on >= ", sex_ratio_min_n, " sexed fish")))
      all_warnings <- dplyr::bind_rows(all_warnings,
        sex_strata %>%
          dplyr::mutate(check_id = "3.7",
                        check_message = paste0("Extreme sex ratio: F/(F+M) = ",
                                               round(sex_ratio_F, 3))))
    } else {
      cat("  [OK] Check 3.7: sex ratio - no extreme ratios detected\n")
    }
  }
  cat("\n")

  # 3.8 STATION MAP (diagnostic plot, if coordinates available) ----
  # Plots initial_latitude / initial_longitude for all records that have
  # coordinates, coloured by area (GSA). Saved as PNG to out_dir.
  # The plot is informational; it does not modify or filter any records.
  cat("Step 3.8: Station map (diagnostic plot)...\n")

  coords_present <- sampling %>%
    dplyr::filter(!is.na(initial_latitude) & !is.na(initial_longitude))

  if (nrow(coords_present) == 0) {
    cat("  [OK] Check 3.8: station map skipped\n\n")
  } else if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cat("  [OK] Check 3.8: station map skipped\n\n")
  } else {
    map_plot <- ggplot2::ggplot(
      coords_present,
      ggplot2::aes(x = initial_longitude, y = initial_latitude,
                   colour = area)
    ) +
      ggplot2::geom_point(alpha = 0.6, size = 1.5) +
      ggplot2::coord_fixed(ratio = 1.3,
                           xlim = c(lon_min, lon_max),
                           ylim = c(lat_min, lat_max)) +
      ggplot2::labs(
        title  = "Sampling stations (initial position)",
        x      = "Longitude", y = "Latitude", colour = "Area (GSA)"
      ) +
      ggplot2::theme_bw()

    map_file <- file.path(out_dir,
                          paste0("sampling_stations_map", file_suffix, ".png"))
    ggplot2::ggsave(map_file, plot = map_plot,
                    width = 22, height = 16, units = "cm", dpi = 300)
    cat("  [OK] Check 3.8: station map saved to", map_file, "\n")
    cat("    Records plotted:", nrow(coords_present), "|\n\n")
  }

  # WRITE OUTPUT FILES ----
  cat("Writing output files...\n")
  
  sampling_clean <- sampling %>%
    dplyr::select(-dplyr::ends_with("_orig"), -dplyr::any_of(c("date_parsed", "date_parsed_dt", "date_year", "year_num", ".row_id")))

  all_errors <- format_qc_issue_output(all_errors)
  all_warnings <- format_qc_issue_output(all_warnings)
  
  readr::write_csv(sampling_clean, file.path(out_dir, paste0("sampling_clean_for_qc", file_suffix, ".csv")))
  
  if (nrow(all_errors) > 0) {
    readr::write_csv(all_errors, file.path(out_dir, paste0("sampling_qc_errors", file_suffix, ".csv")))
  } else {
    readr::write_csv(all_errors, file.path(out_dir, paste0("sampling_qc_errors", file_suffix, ".csv")))
  }
  
  if (nrow(all_warnings) > 0) {
    readr::write_csv(all_warnings, file.path(out_dir, paste0("sampling_qc_warnings", file_suffix, ".csv")))
  } else {
    readr::write_csv(all_warnings, file.path(out_dir, paste0("sampling_qc_warnings", file_suffix, ".csv")))
  }
  
  if (nrow(check_log) > 0) {
    check_log <- check_log %>%
      dplyr::mutate(
        scope = dplyr::coalesce(
          scope,
          dplyr::case_when(
            check_id %in% c("3.4.19") ~ "stratum",
            TRUE                      ~ "record"
          )
        )
      )
  } else {
    check_log$scope <- character()
  }

  if (nrow(check_log) > 0) {
    readr::write_csv(check_log, file.path(out_dir, paste0("sampling_qc_summary", file_suffix, ".csv")))
  } else {
    # Write empty summary with column headers so downstream scripts always find the file
    readr::write_csv(check_log, file.path(out_dir, paste0("sampling_qc_summary", file_suffix, ".csv")))
  }
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
          title = "Sampling QC: flagged records per check",
          subtitle = "Overview of QC findings before cross-checks. Labels show the check scope.",
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
          label = "No QC findings\nAll sampling checks passed",
          size = 5,
          fontface = "bold"
        ) +
        ggplot2::xlim(0.5, 1.5) +
        ggplot2::ylim(0.5, 1.5) +
        ggplot2::labs(
          title = "Sampling QC: flagged records per check",
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
  cat("samplings_qc Completed\n")
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
  cat("[OK] Clean records:", nrow(sampling_clean), "\n")

  out <- list(
    sampling_df = sampling_clean,
    errors = all_errors,
    warnings = all_warnings,
    summary = check_log,
    output_dir = out_dir,
    file_suffix = file_suffix
  )
  return(out)
}

