#' ITA3 landings LFD raising method
#'
#' Apply the Italy-specific raising method used for GSAs 10, 11, and 16.
#'
#' This low-level method is called by `landings_rdbfis()`. It raises
#' commercial categories within trips, aggregates the corrected numbers, and
#' raises them to landings at stratum level.
#'
#' @param data data.frame. Prepared sampling LFD data for one stock.
#' @param land data.frame. Landings totals aggregated by stratum.
#'
#' @return A data.frame with raised numbers-at-length in long format.
#'
#' @author stat4RDBFIS Core Team
#' @export method_ita3

method_ita3 <- function(data, land){
  # quarterly

  # step 1: raise to catch weight by trip and commercial category
  df_3B   <- data %>%
    group_by(year, quarter, trip, commercial_category, metier, stratum, lc) %>%
    summarise(N=sum(N,na.rm = T))

  df_w_cc <- data %>%
    select(-lc, -N) %>%
    distinct() %>%
    group_by(year, quarter, trip, commercial_category, metier, stratum)  %>%
    summarise(sample_w=sum(sub_w,na.rm=T), catch_w=sum(w,na.rm=T))

  df_3B   <- df_3B %>% left_join(df_w_cc, by = c("stratum", "metier", "year", "quarter", "trip", "commercial_category") )
  df_3B$N_cc   <- df_3B$N* (df_3B$catch_w/ df_3B$sample_w)

  # step 2: aggregate by trip and raise to catch weight of the trip
  df_3C   <- df_3B %>%
    group_by(trip, stratum, year, quarter, metier, lc) %>%
    summarise(N_trip= sum(N_cc,na.rm = T))

  df_w    <- data %>%
    select(-lc, -N) %>%
    distinct() %>%
    group_by(trip, stratum, year, quarter, metier) %>%
    summarise(sub_w=sum(sub_w,na.rm=T), w=sum(w,na.rm=T))
  df_3D    <- df_3C %>% left_join(df_w, by = c("trip", "stratum", "metier", "year", "quarter") )
  df_3D    <- df_3D %>% left_join(land, by = c("metier", "year", "quarter", "stratum") )
  df_3D$N_trip_stnd <- df_3D$N_trip * (df_3D$sub_w /df_3D$w)

  # step 3: raise to the landings of the stratum
  df_w2        <- df_w %>%
    group_by(stratum, year, quarter, metier) %>%
    summarise(Tot_sample_w=sum(sub_w,na.rm=T))
  df_3E        <- df_3D %>% left_join(df_w2, by = c("stratum", "metier", "year", "quarter"))
  df_3F        <- df_3E %>% group_by(stratum, species, area, flag_country, year, quarter, metier,land,Tot_sample_w,lc)%>%summarise(N_str=sum(N_trip_stnd,na.rm = T))
  df_3F$N_final <- df_3F$N_str*( (1000*df_3F$land)/df_3F$Tot_sample_w)
  return(df_3F)
}
