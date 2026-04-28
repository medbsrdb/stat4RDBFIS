# qc_utils.R
# Shared utilities for landings_qc(), samplings_qc(), and crosschecks().
# In tutorial mode this file is sourced before the three QC scripts; in package
# form these helpers live in the same namespace.
# All functions use explicit pkg:: namespacing - no library() side effects.
#
# Functions:
#   load_validation_lists(path)          -- reads and validates validation_lists.json
#   is_missing_token(x)                  -- treats NA / "NA" / "" as missing
#   collapse_repeated_weight(x)          -- collapses repeated PSU/haul total weights
#   log_check(...)                       -- builds one row for the QC check log
#   print_check_report(...)              -- prints a formatted console block for one check
#   check_maturity_stage_valid(...)      -- validates maturity stage against its scale
#
# Version: 0.0.4 - March 2026
# Author: stat4RDBFIS Core Team

# ============================================================
# load_validation_lists()
# ============================================================
#' Load and validate QC validation lists from a JSON file.
#'
#' @param path character. Full path to \code{validation_lists.json}.
#' @return Named list of validation tables and code lists used by the QC functions.
#' @author stat4RDBFIS Core Team
#' @export load_validation_lists
load_validation_lists <- function(path) {
  if (!requireNamespace("jsonlite", quietly = TRUE))
    stop("Package 'jsonlite' is required but not installed.\n",
         "  Install with: install.packages(\"jsonlite\")")
  if (!file.exists(path))
    stop("Validation lists file not found at:\n  ", path,
         "\n  Check that QC_CONFIG$validation_lists points to the correct location.")
  vlists <- jsonlite::fromJSON(path)
  required_keys <- c(
    "country", "gsa", "gears_l6", "species",
    "sampling_type", "aggregation_level",
    "catch_registration", "species_registration", "catch_category",
    "sex", "maturity_method", "maturity_scale", "maturity_stages_by_scale",
    "ageing_method", "length_code", "commercial_size_category"
  )
  missing_keys  <- setdiff(required_keys, names(vlists))
  if (length(missing_keys) > 0)
    stop("validation_lists.json is missing required top-level keys: ",
         paste(missing_keys, collapse = ", "))
  vlists
}

# Resolve bundled reference files from an installed package or from the
# source tree during development.
stat4rdbfis_extdata_path <- function(filename, path = NULL, must_work = TRUE) {
  candidates <- character(0)

  if (!is.null(path)) {
    candidates <- c(candidates, path)
  }

  package_path <- system.file(
    "extdata",
    filename,
    package = "stat4RDBFIS",
    mustWork = FALSE
  )
  if (nzchar(package_path)) {
    candidates <- c(candidates, package_path)
  }

  candidates <- c(
    candidates,
    file.path("inst", "extdata", filename),
    filename
  )
  candidates <- unique(candidates[nzchar(candidates)])

  existing <- candidates[file.exists(candidates)]
  if (length(existing) > 0) {
    return(existing[[1]])
  }

  if (!must_work) {
    return("")
  }

  stop(
    "Could not locate required reference file '", filename, "'. Checked:\n  ",
    paste(candidates, collapse = "\n  ")
  )
}

# ============================================================
# Small helpers shared across QC scripts
# ============================================================
#' Treat NA, the string "NA", and empty strings as missing.
#'
#' @param x vector.
#' @return Logical vector.
#' @author stat4RDBFIS Core Team
#' @export is_missing_token
is_missing_token <- function(x) {
  is.na(x) | trimws(as.character(x)) %in% c("", "NA")
}

#' Collapse repeated PSU or haul total weights stored on multiple LFD rows.
#'
#' @param x numeric-like vector.
#' @return Single numeric value or \code{NA_real_}.
#' @author stat4RDBFIS Core Team
#' @export collapse_repeated_weight
collapse_repeated_weight <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  max(unique(x))
}

# ============================================================
# log_check()
# ============================================================
#' Build a one-row tibble summarising one failed QC check.
#'
#' @param df              data.frame. Rows that failed the check.
#' @param check_id        character. Check identifier, e.g. "3.3.1".
#' @param severity        character. "ERROR" or "WARNING".
#' @param message         character. Human-readable rule description.
#' @param found_values    character vector or NULL. Invalid values found in the data.
#' @param expected_values character vector or NULL. Valid values per specification.
#' @param scope           character or NULL. Optional scope label such as
#'   \code{"record"}, \code{"haul"}, \code{"trip"}, or \code{"stratum"}.
#' @return A one-row tibble for appending to the QC check log.
#' @author stat4RDBFIS Core Team
#' @export log_check

log_check <- function(df,
                      check_id,
                      severity,
                      message,
                      found_values    = NULL,
                      expected_values = NULL,
                      scope           = NA_character_) {
  detail <- ""
  if (!is.null(found_values) && !is.null(expected_values)) {
    found_unique <- sort(unique(as.character(found_values)))
    exp_show     <- expected_values[seq_len(min(10L, length(expected_values)))]
    detail <- paste0(
      "FOUND: ",    paste(found_unique, collapse = ", "),
      " | VALID: ", paste(exp_show,     collapse = ", "),
      if (length(expected_values) > 10L)
        paste0(" ... (", length(expected_values), " valid codes total)")
      else ""
    )
  }
  dplyr::tibble(
    check_id     = as.character(check_id),
    severity     = as.character(severity),
    scope        = as.character(scope),
    n_records    = nrow(df),
    message      = as.character(message),
    detail       = detail,
    flag_country = if ("flag_country" %in% names(df))
                     paste(sort(unique(df$flag_country)), collapse = ", ")
                   else NA_character_,
    year         = if ("year" %in% names(df))
                     paste(sort(unique(df$year)), collapse = ", ")
                   else NA_character_,
    area         = if ("area" %in% names(df))
                     paste(sort(unique(df$area)), collapse = ", ")
                   else NA_character_,
    species      = if ("species" %in% names(df))
                     paste(sort(unique(df$species)), collapse = ", ")
                   else NA_character_,
    timestamp    = Sys.time()
  )
}

# ============================================================
# print_check_report()
# ============================================================
#' Print a formatted console block for one QC check result.
#'
#' @param invalid_df      data.frame. Rows that failed the check.
#' @param check_id        character. Check identifier, e.g. "3.3.1".
#' @param severity        character. "ERROR" or "WARNING".
#' @param message         character. Human-readable rule description.
#' @param found_col       character or NULL. Column name to tabulate.
#' @param expected_values character vector or NULL. Valid values per specification.
#' @param error_file      character or NULL. Name of the error CSV for the HOW TO FIX message.
#' @author stat4RDBFIS Core Team
#' @export print_check_report

print_check_report <- function(invalid_df,
                               check_id,
                               severity,
                               message,
                               found_col       = NULL,
                               expected_values = NULL,
                               error_file      = NULL) {
  icon <- if (severity == "ERROR") "[ERROR]" else "[WARN]"
  cat("  ", icon, " ", severity, " ", check_id, ": ", message,
      " (", nrow(invalid_df), " records)\n", sep = "")
}

# ============================================================
# check_maturity_stage_valid()
# ============================================================
#' Validate that a maturity stage is compatible with its declared scale.
#'
#' Returns TRUE when the stage is valid for the declared scale, or when
#' either field is NA / empty (missing data is not flagged here).
#' Returns FALSE when the scale is declared but the stage is not in the
#' allowed list for that scale (includes unknown scale codes).
#'
#' @param stage       character. The maturity stage value to validate.
#' @param scale       character. The maturity scale code (e.g. "MEDITS").
#' @param stages_list named list. Element name = scale code,
#'                    element value = character vector of valid stage codes.
#'                    Typically \code{VLISTS$maturity_stages_by_scale}.
#' @return logical. TRUE = valid or not applicable; FALSE = invalid combination.
#' @author stat4RDBFIS Core Team
#' @export check_maturity_stage_valid
check_maturity_stage_valid <- function(stage, scale, stages_list) {
  # Missing or empty on either side: no rule to enforce
  if (is.na(stage) || stage == "" || is.na(scale) || scale == "") return(TRUE)
  valid_stages <- stages_list[[scale]]
  # Unknown scale code: no valid list -> flag as invalid
  if (is.null(valid_stages)) return(FALSE)
  stage %in% valid_stages
}
