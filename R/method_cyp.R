#' CYP landings LFD raising method
#'
#' Apply the Cyprus-specific raising method to prepared LFD inputs.
#'
#' This low-level method is called by `landings_rdbfis()`. Small-scale fishery
#' samples are raised directly to stratum totals, while other gears are raised
#' through haul, trip, and commercial-category steps before raising to landings.
#'
#' @param data data.frame. Prepared sampling LFD data for one stock.
#' @param land data.frame. Landings totals aggregated by stratum.
#'
#' @return A data.frame with raised numbers-at-length in long format.
#'
#' @author stat4RDBFIS Core Team
#' @export method_cyp

method_cyp <- function(data, land){

  # first estimate weights and sub_sample weights of individuals from L-W and append data
  if (unique(data$species) %in% c('BOG', 'MUT', 'MUR', 'PAC', 'SPC')){ # if there exists LW rel. for the stock
    data$sub_w_est_i   <- data$A[1] * ((data$lc) ^ data$B[1] )* data$N
    # aggregate the estimated individual weights by haul
    df_w_est <- data %>%
      group_by(year, quarter,  metier, stratum, trip, haul, commercial_category) %>%
      summarise(sub_w_est=sum(sub_w_est_i,na.rm = T))

    data    <- data %>% left_join(df_w_est, by= c('year', 'quarter', 'metier', 'stratum', 'trip', 'haul', 'commercial_category'))
    data    <- as.data.frame(data)
    data    <- data[, colnames(data) != "sub_w_est_i"]
  }

  # set gear and split to two dataframes, SSF (GNS, GTR, LLS) and rest of gears
  data$gear <- substr(data$metier, 1, 3)
  data1 <- subset(data, gear %in% c('GNS', 'GTR', 'LLS'))
  data2 <- subset(data, !(gear %in% c('GNS', 'GTR', 'LLS')))

  # step 1a: raise SSF samples to catch weight of the stratum
  if(nrow(data1)!=0){
    df_2A        <- data1 %>%
      group_by(year, quarter, metier, stratum, lc) %>%
      summarise(N_str=sum(N, na.rm = T))

    if (unique(data1$species) %in% c('BOG', 'MUT', 'MUR', 'PAC', 'SPC')){

      df_w_str <- data1 %>%
        select(-N, -lc) %>%
        distinct() %>%
        group_by(year, quarter, metier, stratum) %>%
        summarise(
          sample_w = sum(sub_w_est, na.rm = TRUE))

    } else{

      df_w_str <- data1 %>%
        select(-N, -lc) %>%
        distinct() %>%
        group_by(year, quarter, metier, stratum) %>%
        summarise(
          sample_w = sum(sub_w, na.rm = TRUE))
    }

    df_2A        <- df_2A %>% left_join(df_w_str, by = c("stratum", "metier", "year", "quarter") )
    df_2A        <- df_2A %>% left_join(land, by=c("year", "quarter", "metier"))
    df_2A$N_final <- round(df_2A$N_str * ( (1000* df_2A$land) / df_2A$sample_w) )
  } else {
    df_2A <- NULL
  }

  ### !!!! Careful here if there is no commercial category or haul info, the raising will be done at trip level and will be repeated again below.
  ### Split data depending on aggregation level, to perform haul raising only to aggregation_level='H' data

  if(nrow(data2)!=0){
    data2a <- subset(data2, aggregation_level=='H')
    data2b <- subset(data2, aggregation_level=='T')

    if(nrow(data2a)!=0){
      # step 1: raise non SSF haul samples to the catch weight of the haul and commercial category.
      df_2B   <- data2a %>%
        group_by(year, quarter, trip, haul, commercial_category, metier, stratum, lc) %>%
        summarise(N_cc=sum(N,na.rm = T))

      if (unique(data2a$species) %in% c('BOG', 'MUT', 'MUR', 'PAC', 'SPC')){
        df_w_cc <- data2a %>%
          select(-lc, -N) %>%
          distinct() %>%
          group_by(year, quarter, trip, haul, commercial_category, metier, stratum)  %>%
          summarise(sample_w=sum(sub_w_est,na.rm=T), catch_w=sum(w,na.rm=T))
      } else {
        df_w_cc <- data2a %>%
          select(-lc, -N) %>%
          distinct() %>%
          group_by(year, quarter, trip, haul, commercial_category, metier, stratum)  %>%
          summarise(sample_w=sum(sub_w,na.rm=T), catch_w=sum(w,na.rm=T))

      }
      df_2B   <- df_2B %>% left_join(df_w_cc, by = c("stratum", "metier", "year", "quarter", "trip", "haul", "commercial_category") )
      df_2B$N_cc   <- df_2B$N_cc* (df_2B$catch_w/ df_2B$sample_w)

      # step 2: raise non SSF haul samples to catch weight of the trip by commercial category
      df_2C <- df_2B %>%
        group_by(year, quarter, trip, commercial_category, metier, stratum, lc) %>%
        summarise(N_tr=sum(N_cc,na.rm = T))

      df_w_cc1 <- df_w_cc %>%
        group_by(year, quarter, trip, commercial_category, metier, stratum)  %>%
        summarise(sample_w=sum(sample_w,na.rm=T), catch_w=sum(catch_w,na.rm=T))
      df_2C   <- df_2C %>% left_join(df_w_cc1, by = c("stratum", "metier", "year", "quarter", "trip", "commercial_category") )
      df_2C$N_tr1   <- df_2C$N_tr* (df_2C$catch_w/ df_2C$sample_w)
    } else {
      df_2C <- NULL
    }

    if(nrow(data2b)!=0){
      # step 2a: raise non SSF trip samples to catch weight of the trip by commercial category
      df_2C1 <- data2b %>%
        group_by(year, quarter, trip, commercial_category, metier, stratum, lc) %>%
        summarise(N_tr=sum(N,na.rm = T))

      if (unique(data2b$species) %in% c('BOG', 'MUT', 'MUR', 'PAC', 'SPC')){
        df_w_cc2 <- data2b %>%
          select(-lc, -N) %>%
          distinct() %>%
          group_by(year, quarter, trip, commercial_category, metier, stratum)  %>%
          summarise(sample_w=sum(sub_w_est,na.rm=T), catch_w=sum(w,na.rm=T))
      }else{
        df_w_cc2 <- data2b %>%
          select(-lc, -N) %>%
          distinct() %>%
          group_by(year, quarter, trip, commercial_category, metier, stratum)  %>%
          summarise(sample_w=sum(sub_w,na.rm=T), catch_w=sum(w,na.rm=T))
      }
      df_2C1   <- df_2C1 %>% left_join(df_w_cc2, by = c("stratum", "metier", "year", "quarter", "trip", "commercial_category") )
      df_2C1$N_tr1   <- df_2C1$N_tr* (df_2C1$catch_w/ df_2C1$sample_w)
    } else {
      df_2C1 <- NULL
    }

    df_2D <- rbind(df_2C, df_2C1)

    # step 3: sum trips and compute catch and subsample weight of the stratum
    df_2D   <- df_2D %>%
      group_by(year, quarter,  metier, stratum, lc) %>%
      summarise(N_str=sum(N_tr1,na.rm = T))
    df_w    <- data %>% select(-lc, -N) %>%
      distinct() %>%
      group_by(stratum, year, quarter, metier)  %>%
      summarise(sample_w=sum(sub_w,na.rm=T), catch_w=sum(w,na.rm=T))

    # step 4: raise to the landings of the stratum
    df_2D        <- df_2D %>% left_join(df_w, by = c("stratum", "metier", "year", "quarter") )
    df_2D        <- df_2D %>% left_join(land, by=c("year", "quarter", "metier"))
    df_2D$N_final <- round(df_2D$N_str * ( (1000* df_2D$land) / df_2D$catch_w) )
  }else {
    df_2D <- NULL
  }

  # step 5: add up SSF and ! SSF samples and aggregate
  df_2E <- rbind(df_2A, df_2D)

  return(df_2E)
}
