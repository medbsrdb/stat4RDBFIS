#' ESP landings LFD raising method
#'
#' Apply the Spain-specific raising method to prepared LFD inputs.
#'
#' This low-level method is called by `landings_rdbfis()`. It raises onboard
#' samples by haul and onshore samples by trip, applies sampled-haul
#' corrections when needed, reweights trips using the length-weight
#' relationship, and raises to landings at stratum level.
#'
#' @param data data.frame. Prepared sampling LFD data for one stock.
#' @param land data.frame. Landings totals aggregated by stratum.
#'
#' @return A data.frame with raised numbers-at-length in long format.
#'
#' @author stat4RDBFIS Core Team
#' @export method_esp

method_esp <- function(data, land){

  ## step 1: raise to catch weight of the haul (for onboard),  or trip (for onshore)
  df_2B        <- data
  df_2B$N_haul <- data$N* (data$w/ data$sub_w)

  df_2Ba <- subset(df_2B, aggregation_level=='H')
  df_2Bb <- subset(df_2B, aggregation_level=='T')

  # step 2: aggregate onboard and onshore samples by trip. In onboard sampling divide by fraction of sampled hauls
  # on board sampling
  if (nrow(df_2Ba) != 0){
    df_2Ba$haul_ratio <- df_2Ba$n_sets_hauls/df_2Ba$n_sampled_hauls
    df_2Ba     <- df_2Ba %>%
      group_by(year, quarter,  trip, metier, stratum, haul_ratio, lc) %>%
      summarise(N_trip=sum(N_haul,na.rm = T))
    df_2Ba$N_trip    <- df_2Ba$N_trip * df_2Ba$haul_ratio
    df_2Ba$haul_ratio <- NULL
  } else {
    df_2Ba <- NULL
  }

  # on shore sampling
  if (nrow(df_2Bb) != 0){
    df_2Bb     <- df_2Bb %>%
      group_by(year, quarter,  trip, metier, stratum, lc) %>%
      summarise(N_trip=sum(N_haul,na.rm = T))
  } else {
    df_2Bb <- NULL
  }

  # join onshore and offshore samples
  df_2C <- rbind(df_2Ba, df_2Bb)

  # step 3: use theoretical weight of the trip to reweight trip LFD
  if (!is.na(data$A[1]) & !is.na(data$B[1])){  # if there exists LW rel. for the stock
    df_2C$SOP    <- data$A[1] * ((df_2C$lc + 0.5) ^ data$B[1] )* df_2C$N_trip
    df_w_th      <- df_2C %>%
      group_by(year, quarter,  metier, stratum, trip) %>%
      summarise(SOPtrip=sum(SOP,na.rm = T))
    df_w_ob      <- data %>%
      select(-lc, -N) %>%
      distinct() %>%
      group_by(year, quarter, trip, metier, stratum)  %>%
      summarise(catch_w=sum(w,na.rm=T))
    df_w         <- df_w_ob %>% left_join(df_w_th, by= c('year', 'quarter', 'metier', 'stratum', "trip"))
    df_w$wf      <- df_w$catch_w/df_w$SOPtrip
    df_2D         <- df_2C %>% left_join(df_w, by = c('year', 'quarter', 'metier', 'stratum', "trip") )
    df_2D$N_trip_str  <- df_2D$N_trip *df_2D$wf
  } else {
    df_2D <- df_2C
    df_2D$N_trip_str <- df_2D$N_trip
  }

  # Step 4: raised to the landings of the stratum
  df_2E        <- df_2D %>%
    group_by(year, quarter,  metier, stratum, lc) %>%
    summarise(N_str=sum(N_trip_str,na.rm = T))
  df_w         <- data %>%
    select(-lc, -N) %>%
    distinct() %>%
    group_by(stratum, year, quarter, metier) %>%
    summarise(sample_w=sum(sub_w,na.rm=T), catch_w=sum(w,na.rm=T))
  df_2E        <- df_2E %>% left_join(df_w, by = c("stratum", "metier", "year", "quarter") )
  df_2E        <- df_2E %>% left_join(land, by=c("year", "quarter", "metier"))
  df_2E$N_final <- round(df_2E$N_str * ( (1000* df_2E$land) / df_2E$catch_w) )
  return(df_2E)
}

