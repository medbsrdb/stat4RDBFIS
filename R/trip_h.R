#' Generic haul or trip LFD raising method
#'
#' Apply a generic raising method based on haul or trip catch weights.
#'
#' This low-level method is called by `landings_rdbfis()` or
#' `discards_rdbfis()`. It raises numbers-at-length using catch weight and
#' then to totals at stratum level.
#'
#' @param data data.frame. Prepared sampling LFD data for one stock.
#' @param land data.frame. Landings or discard totals aggregated by stratum.
#'
#' @return A data.frame with raised numbers-at-length in long format.
#'
#' @author stat4RDBFIS Core Team
#' @export trip_h

trip_h <- function(data, land){
  # quarterly
  # step 1: raise to catch weight by haul
  df_2B        <- data
  df_2B$N_haul <- df_2B$N* (data$w/ data$sub_w)

  # step 2: aggregate by stratum and estimate catch weight of the stratum
  df_2B  <- df_2B %>%
    group_by(year, quarter,  metier, stratum, lc) %>%
    summarise(N_str=sum(N_haul,na.rm = T))

  df_w   <- data %>%
    select(-lc, -N) %>%
    distinct() %>%
    group_by(stratum, year, quarter, metier)  %>%
    summarise(sample_w=sum(sub_w,na.rm=T), catch_w=sum(w,na.rm=T))

  # step 3: raise to the landings of the stratum
  df_2C        <- df_2B %>% left_join(df_w, by = c("stratum", "metier", "year", "quarter") )
  df_2D        <- df_2C %>% left_join(land, by=c("year", "quarter", "metier"))
  df_2D$N_final <- round(df_2D$N_str * ( (1000* df_2D$land) / df_2D$catch_w) )
  return(df_2D)
}
