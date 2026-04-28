#' Maturity at age outputs from cleaned sampling data
#'
#' Applies the maturity-at-age workflow to the cleaned sampling template and
#' writes the resulting tables to disk in MED&BS format.
#'
#' @param sampling_df data.frame containing cleaned sampling data.
#' @param SP optional character vector of species codes.
#' @param GSA optional character vector of GSA codes.
#' @param YEAR optional numeric vector of years.
#' @param Imm numeric vector indicating the maturity stages to be treated as
#'   immature. Default is `c(0, 1)`.
#' @param out_dir character. Output directory where csv files are written.
#'
#' @return A nested list of maturity-at-age results by stock and year.
#'
#' @author stat4RDBFIS Core Team
#' @examples
#' \dontrun{
#' maturity_age <- ma_rdbfis(
#'   sampling_df = qc_cross$sampling_clean
#' )
#' }
#' @export
ma_rdbfis <- function(sampling_df, SP = NULL, GSA = NULL, YEAR = NULL, Imm=c(0,1),out_dir = "maturity") {
  
  # 1) get the MS, subset and stop if there are no sampling records 
  MS <- unique(sampling_df$flag_country)
  
  if (length(MS) != 1) {
    stop("Input dataframe must contain only one country.")
  }
  
  # 2) Create output folder
  
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
 
  # 3) subset to user selection (GSA, SP, YEAR)
  
  if (!is.null(GSA)){
    sampling_df <- subset(sampling_df, area %in% GSA)
    if (nrow(sampling_df) == 0) {
      stop("  No sampling records for ", MS, " for the selected area(s): ", paste(GSA, collapse=', ' ))
    }
  }
  
  if (!is.null(SP)){
    sampling_df <- subset(sampling_df, species %in% SP)
    if (nrow(sampling_df) == 0) {
      stop("  No sampling records for ", MS, " for the selected GSA(s) for species(s): ", paste(SP, collapse=', ' ))
    }
  }
  
  if (!is.null(YEAR)){
    sampling_df <- subset(sampling_df, year %in% YEAR)
    if (nrow(sampling_df) == 0) {
      stop("  No sampling records for ", MS, " for the selected GSA(s) and species(s) in years(s): ", paste(YEAR, collapse=', ' ))
    }
  }
  
 
  # 4) Define the stocks
  sampling_df <- sampling_df %>%
    mutate(stock = paste(
      area, species,
      sep = "_")) 
  
  stocks <- sampling_df %>%
    mutate(stock = paste(area, species, sep = "_")) %>%
    distinct(stock, area, species)
  
  # 5) Apply ma_rdbfis sequentially to all stocks found in sampling_df
  st <- stocks$stock
  ma_out <- list()
  ma_medbs <- data.frame()
  fit_warning_log <- list()
  
  for (s in st) {
    yr_out <- list()
    temp1 <- sampling_df[sampling_df$stock == s, ]
    Species <- stocks$species[stocks$stock == s]
    Area    <- stocks$area[stocks$stock == s]
    years=unique(temp1$year)
    for (yr in 1:length(years)) { 
      styear <-endyear <-years[yr]
      temp1_ <- temp1[temp1$year == styear, ]
      yr_result <- ma(temp1_, Species, MS, Area, styear, endyear,Imm)
      yr_out[[yr]] <- yr_result
      yr_warnings <- unique(c(
        yr_result$Common_Sex$fit_warnings,
        yr_result$Per_sex$fit_warnings
      ))
      if (length(yr_warnings) > 0) {
        fit_warning_log[[paste(s, styear, sep = "|")]] <- yr_warnings
      }
    }
    names(yr_out) =years
    ma_out[[s]] <-   yr_out
  }
  attr(ma_out, "fit_warnings") <- fit_warning_log
  if (length(fit_warning_log) > 0) {
    message(
      "ma_rdbfis: glm fit warnings were recorded for ",
      length(fit_warning_log),
      " stock-year combinations. Inspect attr(result, 'fit_warnings') for details."
    )
  }
 
  per_sex_all_raw <- do.call(
    rbind,
    lapply(names(ma_out), function(stock_name) {
      stock <- ma_out[[stock_name]]
      do.call(
        rbind,
        lapply(names(stock), function(year_name) {
          df <- stock[[year_name]]$Per_sex$Output_raw
          df$stock <- stock_name
          df$year  <- year_name
          df}))}))
  per_sex_all_raw=per_sex_all_raw[,1:10]
  
  common_sex_all_raw <- do.call(
    rbind,
    lapply(names(ma_out), function(stock_name) {
      stock <- ma_out[[stock_name]]
      do.call(
        rbind,
        lapply(names(stock), function(year_name) {
          df <- stock[[year_name]]$Common_Sex$Output_raw
          df$stock <- stock_name
          df$year  <- year_name
          df}))}))
  common_sex_all_raw=common_sex_all_raw[,1:10]
  
  
  per_sex_all_model <- do.call(
    rbind,
    lapply(names(ma_out), function(stock_name) {
      stock <- ma_out[[stock_name]]
      do.call(
        rbind,
        lapply(names(stock), function(year_name) {
          df <- stock[[year_name]]$Per_sex$Output_model
          df$stock <- stock_name
          df$year  <- year_name
          df}))}))
  per_sex_all_model=per_sex_all_model[,1:10]
  per_sex_all_model$PRM=round(per_sex_all_model$PRM,4)
  
  common_sex_all_model <- do.call(
    rbind,
    lapply(names(ma_out), function(stock_name) {
      stock <- ma_out[[stock_name]]
      do.call(
        rbind,
        lapply(names(stock), function(year_name) {
          df <- stock[[year_name]]$Common_Sex$Output_model
          df$stock <- stock_name
          df$year  <- year_name
          df}))}))
  common_sex_all_model=common_sex_all_model[,1:10]
  common_sex_all_model$PRM=round(common_sex_all_model$PRM,4)
  
  
  # 6) Output csv in MED&BS format
  suffix_parts <- MS
  if (any(!is.na(GSA))) suffix_parts <- c(suffix_parts, paste(GSA, collapse = "-"))
  if (any(!is.na(SP)))  suffix_parts <- c(suffix_parts, paste(SP,  collapse = "-"))
  if (any(!is.na(YEAR)))  suffix_parts <- c(suffix_parts, paste(YEAR,  collapse = "-"))
  file_suffix <- paste(suffix_parts, collapse = "_")
  
  write.csv(common_sex_all_raw, file.path(out_dir, paste0("ma_common_sex_all_raw_", file_suffix, ".csv")), row.names=F)
  write.csv(per_sex_all_raw, file.path(out_dir, paste0("ma_per_sex_all_raw_", file_suffix, ".csv")), row.names=F)
  write.csv(per_sex_all_model, file.path(out_dir, paste0("ma_per_sex_all_model_", file_suffix, ".csv")), row.names=F)
  write.csv(common_sex_all_model, file.path(out_dir, paste0("ma_common_sex_all_model_", file_suffix, ".csv")), row.names=F)
  
  # 7) output result and write csv file
  return(ma_out)
  
}
