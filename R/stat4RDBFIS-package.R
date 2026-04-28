#' stat4RDBFIS
#'
#' Quality control, data preparation, and statistical raising tools for RCG
#' landings (CL) and sampling (CS) data from Mediterranean and Black Sea
#' fisheries.
#'
#' The package was developed within the RDBFIS III project (Activity 10) by
#' the stat4RDBFIS Core Team: Vicky Sgardeli (HCMR), Ilaria Costantini
#' (CNR-IRBIM), Kostas Touloumis (FRI), Alessandro Mannini (CNR-IRBIM), and
#' Stefanos Kavadas (HCMR).
#'
#' The main workflow includes landings and sampling quality control,
#' crosschecks between CL and CS templates, landings and discard LFD raising,
#' maturity-at-age and maturity-at-length estimation, sex-ratio estimation,
#' age-length keys, and discard ratio estimation.
#'
#' @author stat4RDBFIS Core Team
#' @keywords internal
#' @import dplyr
#' @import tidyr
#' @import tibble
#' @import stringr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats aggregate anova binomial coef fitted glm lm logLik na.omit predict rstandard setNames update
#' @importFrom tidyselect all_of
#' @importFrom utils head read.csv write.csv
"_PACKAGE"

utils::globalVariables(c(
  ".flag", ".iqr", ".label", ".lc_max", ".outlier", ".price_per_kg",
  ".q1", ".q3", ".q_landings", ".q_sampling", ".resid_st", ".row_id",
  ".upper_fence", ".wl_ratio", "A", "B", "COUNTRY", "Dis", "GSA", "Lan",
  "Lmax_mm", "N", "N_cc", "N_final", "N_haul", "N_tr1", "N_trip_stnd",
  "N_trip_str", "SOP", "Tot_sample_w", "age", "age_num", "ageing_method",
  "ages_found", "aggregation_level", "all_same", "area", "area_dis",
  "area_lan", "catch_category", "catch_registration", "catch_w",
  "check_id", "check_label", "check_label_text", "commercial_category",
  "commercial_size_category", "commercial_size_category_scale", "concat",
  "conflict", "coverage", "coverage_fill", "coverage_frac", "coverage_pct",
  "date_parsed", "date_year", "days_at_sea", "depth_fishing_operation",
  "duration_fishing_operation", "expected_quarter", "final_latitude",
  "final_longitude", "fish_id", "fishing_activity_category_eu_l6",
  "fishing_activity_category_eu_l6_dis",
  "fishing_activity_category_eu_l6_lan",
  "fishing_activity_category_national", "flag_country", "gear", "harbour",
  "has_LFD", "has_indiv", "has_individual", "has_pooled", "has_subsample",
  "haul", "haul_ratio", "haul_sample_weight", "in_landings",
  "individual_weight", "initial_latitude", "initial_longitude", "iqr",
  "is_duplicate", "label_n", "land", "landings_harbours", "landings_species",
  "lc", "length_class", "length_class_orig", "length_code", "length_num",
  "lmax_mm", "lower", "lower_fence", "mature", "maturity_method",
  "maturity_scale", "maturity_stage", "maturity_stage_valid", "metier",
  "month", "month_from_date", "month_orig", "n_diff", "n_distinct_ages",
  "n_female", "n_fish_aged", "n_fish_measured", "n_len",
  "n_length_classes", "n_male", "n_missing", "n_pairs", "n_records",
  "n_same", "n_sets_hauls", "n_sexed", "n_species_landings",
  "n_species_sampling", "number_at_length", "number_at_length_orig",
  "official_landings_value", "official_landings_weight", "pooled_n",
  "pooled_wt", "psu", "q1", "q3", "quarter", "quarter_cc",
  "quarter_declared", "quarter_from_date", "quarter_from_date_dis",
  "quarter_from_date_lan", "quarter_from_month", "ratio",
  "resid_standardised", "rf", "same_val", "same_wt", "sample_w",
  "sampling_harbours", "sampling_species", "sampling_type", "scope",
  "scope_label", "scope_prefix", "severity", "severity_fill", "sex",
  "sex_ratio_F", "species", "species_registration", "station_code",
  "stock", "stratum", "stratum_key", "sub_w", "sub_w_est", "sub_w_est_i",
  "subsample_weight", "subsample_weight_orig", "total_indiv_n",
  "total_indiv_wt", "total_individual_weight", "total_landings_weight",
  "total_landings_weight_kg", "total_sample_weight",
  "total_sample_weight_kg", "total_subsample_weight", "trip", "trip_code",
  "upper", "upper_fence", "valid_combination", "value_orig", "w",
  "water_depth", "weight", "weight_diff_pct", "weight_orig", "wt_pct_diff",
  "year", "year_num", "year_orig"
))
