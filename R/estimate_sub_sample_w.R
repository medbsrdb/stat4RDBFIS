#' Estimate trip subsample weights
#'
#' Estimate theoretical trip subsample weights from the bundled
#' length-weight relationship table and compare them with the observed
#' subsample weights in the cleaned sampling data.
#'
#' This helper is used by the LFD raising functions to flag large differences
#' between observed and estimated subsample weights. Large ratios may indicate
#' wrong length units, wrong weight units, or other reporting inconsistencies.
#'
#' @param temp data.frame. Prepared sampling data for one stock, as created by
#'   the LFD raising workflow.
#' @param country character. Member State code.
#' @param st character vector. Stock identifiers in the form `GSA_SPECIES`,
#'   e.g. `"GSA22_HKE"`.
#'
#' @return A data.frame with the prepared sampling data plus observed trip
#'   subsample weight, estimated trip subsample weight, and their ratio.
#'
#' @author stat4RDBFIS Core Team
#' @export estimate_sub_sample_w
#' @importFrom readr write_csv
#' @examples
#' \dontrun{
#' estimate_sub_sample_w(temp, country, st)
#' }

estimate_sub_sample_w <- function(temp, country, st){

  # estimate observed sub_sample weight of the trips. If aggregation level='T' then this is just sub_w.
  # If aggregation_level='H' then it is the sum of subsample weights per trip

  old_opts <- options(dplyr.summarise.inform = FALSE)
  on.exit(options(old_opts), add = TRUE)
  w_ob  <- temp %>%
    select(-lc, -N) %>%
    distinct() %>%
    group_by(year, quarter, trip, metier, stratum)  %>%
    summarise(sub_w_trip_ob=sum(sub_w,na.rm=T))

  # estimate sub_sample trip weights from the L-W relationship bundled with the package
  lw_path <- system.file("extdata", "LW-relationship.csv", package = "stat4RDBFIS")
  if (!file.exists(lw_path))
    stop("LW-relationship.csv not found at: ", lw_path,
         "\n  Reinstall stat4RDBFIS so the bundled reference files are available.")
  LW <- read.csv(file = lw_path, header = TRUE)
  lw_transform <- get0(
    "LW_unit_transformation",
    envir = environment(estimate_sub_sample_w),
    inherits = TRUE
  )
  if (is.null(lw_transform)) {
    stop("Required internal function not found in the stat4RDBFIS namespace: LW_unit_transformation",
         "\n  Rebuild/reinstall the package so all files under R/ are loaded.")
  }
  LW <- subset(LW, LW$A3_ASFIS %in% temp$species)  # subset species
  if (nrow(LW)!=0){
    if (country %in% LW$COUNTRY){   # if there is specific LW by country
      LW <- subset(LW, COUNTRY %in% country) # subset country
      if (unique(temp$area) %in% LW$GSA){     # subset for area
        LW <- subset(LW, GSA == unique(temp$area))
      }
      LW <- LW[1,]  # currently select just one of the records (in the future to check for start_year and sex)
      LW <- lw_transform(LW)
      LW$species <- LW$A3_ASFIS
      temp <- temp %>% left_join(LW, by = "species") %>% mutate(w_est = A * lc^B * N)
      w_est <- temp %>% group_by(year, quarter,  metier, stratum, trip) %>%
      summarise(sub_w_trip_est=sum(w_est,na.rm = T))
      temp  <- temp %>% left_join(w_est, by = c('year', 'quarter', 'metier', 'stratum', "trip") )
      temp  <- temp %>% left_join(w_ob, by = c('year', 'quarter', 'metier', 'stratum', "trip") )
      temp$ratio <- temp$sub_w_trip_ob/temp$sub_w_trip_est
      drop <- which(colnames(temp) %in% c("COUNTRY","GSA","start_y","end_y","SPECIES","A3_ASFIS","U_FLAG", "U_FLAG_w","R2","SOURCE_INFO","w_est"))
      temp <- temp[, -drop]
    } else if ('ALL' %in% LW$COUNTRY) {
      LW <- subset(LW, COUNTRY %in% 'ALL')  # subset country (use default LW)
      LW <- LW[1,]  # currently there is just one record for country='All'
      LW <- lw_transform(LW)
      LW$species <- LW$A3_ASFIS
      temp <- temp %>% left_join(LW, by = "species") %>% mutate(w_est = A * lc^B * N)
      w_est <- temp %>%
      group_by(year, quarter,  metier, stratum, trip) %>%
      summarise(sub_w_trip_est=sum(w_est,na.rm = T))
      temp  <- temp %>% left_join(w_est, by = c('year', 'quarter', 'metier', 'stratum', "trip") )
      temp  <- temp %>% left_join(w_ob, by = c('year', 'quarter', 'metier', 'stratum', "trip") )
      temp$ratio <- temp$sub_w_trip_ob/temp$sub_w_trip_est
      drop <- which(colnames(temp) %in% c("COUNTRY","GSA","start_y","end_y","SPECIES","A3_ASFIS","U_FLAG", "U_FLAG_w","R2","SOURCE_INFO","w_est"))
      temp <- temp[, -drop]
    }else{
      print(paste0('No LW relationship available for ', unique(temp$stock)))
      temp  <- temp %>% left_join(w_ob, by = c('year', 'quarter', 'metier', 'stratum', "trip") )
      temp[c("sub_w_trip_est", "ratio", "A", "B", "SEX")] <- NA

    }
 }

return(temp)
}
