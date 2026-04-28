#' FRA landings LFD raising method
#'
#' Apply the France-specific raising method to prepared LFD inputs.
#'
#' This low-level method is called by `landings_rdbfis()`. It applies a
#' Vigneau-Mahevas style raising to market samples and raises the summed
#' samples to landings at stratum level.
#'
#' @param data data.frame. Prepared sampling LFD data for one stock.
#' @param land data.frame. Landings totals aggregated by stratum.
#'
#' @return A data.frame with raised numbers-at-length in long format.
#'
#' @author stat4RDBFIS Core Team
#' @export method_fra

method_fra <- function(data, land){
  data1        <- data
  df_2A        <- data1 %>% group_by(year, quarter, metier, stratum, lc) %>% summarise(N_str=sum(N, na.rm = T))
  df_w_str     <- data %>%
    select(-lc, -N) %>%
    distinct() %>%
    group_by(year, quarter, metier, stratum)  %>%
    summarise(sample_w=sum(sub_w,na.rm=T), catch_w=sum(w, na.rm=T))

  df_2A        <- df_2A %>% left_join(df_w_str, by = c("stratum", "metier", "year", "quarter") )
  df_2A        <- df_2A %>% left_join(land, by=c("year", "quarter", "metier"))
  df_2A$N_final <- round(df_2A$N_str * ( (1000* df_2A$land) / df_2A$sample_w) )
  return(df_2A)
}
