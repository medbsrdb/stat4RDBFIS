#' Sex ratio at age outputs from cleaned sampling data
#'
#' Applies the sex-ratio-at-age workflow to the cleaned sampling template and
#' writes the resulting tables to disk in MED&BS format.
#'
#' @param sampling_df data.frame containing cleaned sampling data.
#' @param SP optional character vector of species codes.
#' @param GSA optional character vector of GSA codes.
#' @param YEAR optional numeric vector of years.
#' @param out_dir character. Output directory where csv files are written.
#'
#' @return A nested list of sex-ratio-at-age results by stock and year.
#'
#' @author stat4RDBFIS Core Team
#' @examples
#' \dontrun{
#' sex_ratio_age <- sra_rdbfis(
#'   sampling_df = qc_cross$sampling_clean
#' )
#' }
#' @export
sra_rdbfis <- function(sampling_df, SP = NULL, GSA = NULL, YEAR = NULL, out_dir = "sex ratio") {
  
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
  
  # 5) Apply sra_rdbfis sequentially to all stocks found in sampling_df
  st <- stocks$stock
  sra_out <- list()
  sra_medbs <- data.frame()

for (s in st) {
  yr_out <- list()
  temp1 <- sampling_df[sampling_df$stock == s, ]
  Species <- stocks$species[stocks$stock == s]
  Area    <- stocks$area[stocks$stock == s]
  years=unique(temp1$year)
  for (yr in 1:length(years)) { 
    styear <-endyear <-years[yr]
    temp1_ <- temp1[temp1$year == styear, ]
    yr_out[[yr]] <- sra(temp1_, Species, MS, Area, styear, endyear) 
  }
  names(yr_out) =years
  sra_out[[s]] <-   yr_out
}

sra_out_ <- do.call(
  rbind,
  unlist(sra_out, recursive = FALSE)
)

  # 6) Output csv in MED&BS format
  suffix_parts <- MS
  if (any(!is.na(GSA))) suffix_parts <- c(suffix_parts, paste(GSA, collapse = "-"))
  if (any(!is.na(SP)))  suffix_parts <- c(suffix_parts, paste(SP,  collapse = "-"))
  if (any(!is.na(YEAR)))  suffix_parts <- c(suffix_parts, paste(YEAR,  collapse = "-"))
  file_suffix <- paste(suffix_parts, collapse = "_")
  
  write.csv(sra_out_, file.path(out_dir, paste0("sra_all_", file_suffix, ".csv")), row.names=F)

  # 7) output result and write csv file
  return(sra_out)
  
}
