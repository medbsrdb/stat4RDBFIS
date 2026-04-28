#' ITA2 landings LFD raising method
#'
#' Apply the Italy-specific raising method used for GSAs 9, 17, 18, and 19.
#'
#' This low-level method is called by `landings_rdbfis()`. It raises
#' numbers-at-length to catch weight by trip and commercial category, then to
#' landings at stratum level.
#'
#' @param data data.frame. Prepared sampling LFD data for one stock.
#' @param land data.frame. Landings totals aggregated by stratum.
#'
#' @return A data.frame with raised numbers-at-length in long format.
#'
#' @author stat4RDBFIS Core Team
#' @export method_ita2

method_ita2 <- function(data, land){
  # quarterly

  df_2B   <- data

  # step 1: raise to catch weight by trip and commercial category
  df_2B   <- df_2B %>%
    group_by(year, quarter, trip, commercial_category, metier, stratum, lc) %>%
    summarise(N=sum(N,na.rm = T))

  df_w_cc <- data %>%
    select(-lc, -N) %>% distinct() %>%
    group_by(year, quarter, trip, commercial_category, metier, stratum)  %>%
    summarise(sample_w=sum(sub_w,na.rm=T), catch_w=sum(w,na.rm=T))

  df_2B   <- df_2B %>% left_join(df_w_cc, by = c("stratum", "metier", "year", "quarter", "trip", "commercial_category") )
  df_2B$N_cc   <- df_2B$N* (df_2B$catch_w/ df_2B$sample_w)

  # step 2: sum trips and compute catch and subsample weight of the stratum
  df_2B        <- df_2B %>% group_by(year, quarter,  metier, stratum, lc) %>%
    summarise(N_str=sum(N_cc,na.rm = T))

  df_w         <- data %>%
    select(-lc, -N)  %>%
    distinct() %>%
    group_by(stratum, year, quarter, metier)  %>%
    summarise(sample_w=sum(sub_w,na.rm=T), catch_w=sum(w,na.rm=T))

  # step 3: raise to the landings of the stratum
  df_2C        <- df_2B %>% left_join(df_w, by = c("stratum", "metier", "year", "quarter") )
  df_2D        <- df_2C %>% left_join(land, by=c("year", "quarter", "metier", "stratum"))
  df_2D$N_final <- round(df_2D$N_str * ( (1000* df_2D$land) / df_2D$catch_w) )
  return(df_2D)
}
