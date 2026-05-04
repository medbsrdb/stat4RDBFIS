#' Discard LFD raising
#'
#' Prepare cleaned RCG sampling and landings templates and raise discard
#' length-frequency distributions (LFDs).
#'
#' This function is the main discard LFD raising step in the stat4RDBFIS
#' workflow. It estimates discard ratios from cleaned sampling data, combines
#' them with landings totals, applies the selected raising method, and writes
#' raised discard LFD outputs.
#'
#' @param sampling_df data.frame. Clean RCG sampling template.
#' @param landings_df data.frame. Clean RCG landings template.
#' @param SP optional character vector of species FAO 3-alpha codes.
#'   Default is `NULL`, which processes all species available in the data.
#' @param GSA optional character vector of GSA codes. Default is `NULL`, which
#'   processes all areas available in the data.
#' @param YEAR optional numeric vector of years. Default is `NULL`, which
#'   processes all years available in the data.
#' @param method optional character. Raising method to use. Default is `NULL`,
#'   which applies the Member State-specific method. Accepted alternatives
#'   include `"trip_h"`, `"trip_cc"`, and `"vigneau_mahevas"`.
#' @param sample_types optional character vector of sampling types to use in
#'   the raising. Default is `c("S", "D", "M")`.
#' @param use_mat logical. If `TRUE`, samples collected for maturity are kept
#'   in the raising. Default is `FALSE`.
#' @param out_dir character. Output folder where reports, plots, and raised
#'   discard LFDs are written. Default is `"output"`.
#' @param discard_ratio_out_dir character or `NULL`. Output folder for the
#'   discard ratio table created internally. When `NULL`, a separate
#'   `discard_ratio` folder is created next to `out_dir` when possible.
#' @param discard_ratio_df data.frame or `NULL`. Optional discard-ratio table
#'   previously returned by [discard_ratio_rdbfis()]. When supplied, the table
#'   is reused and the function does not write a second discard-ratio file.
#'
#' @return A data.frame with raised discard LFDs in long format. The function
#'   also writes MED&BS discard-template outputs and subsample-weight
#'   diagnostics to `out_dir`.
#'
#' @author stat4RDBFIS Core Team
#' @export discards_rdbfis
#' @importFrom readr write_csv
#' @examples
#' \dontrun{
#' discards_rdbfis(
#'   sampling_df,
#'   landings_df,
#'   SP = c("DPS", "HKE"),
#'   GSA = "GSA22",
#'   YEAR = c(2023, 2024),
#'   method = "trip_cc"
#' )
#' }

# version: 0.0.1 (output MED&BS discards template)
# date: March 2026

discards_rdbfis <- function(sampling_df, landings_df, SP = NULL, GSA = NULL, YEAR = NULL, method = NULL,
                            sample_types=c('S', 'D', 'M'), use_mat=FALSE,
                            out_dir = "output", discard_ratio_out_dir = NULL,
                            discard_ratio_df = NULL) {

  # 1) check dependencies
  # Packages must be loaded before calling discards_rdbfis()
  for (pkg in c("dplyr", "tidyr", "readr", "stringr", "lubridate", "ggplot2")) {
    if (!requireNamespace(pkg, quietly = TRUE))
      stop("Required package not installed: ", pkg,
           "\n  Install with: install.packages(\"", pkg, "\")")
  }
  # Internal raising helpers must be available in the package namespace.
  required_fns <- c("LW_unit_transformation", "estimate_sub_sample_w")
  package_env <- environment(discards_rdbfis)
  missing_fns <- required_fns[!vapply(
    required_fns,
    exists,
    logical(1),
    envir = package_env,
    inherits = TRUE
  )]
  if (length(missing_fns) > 0)
    stop("Required internal functions not found in the stat4RDBFIS namespace: ",
         paste(missing_fns, collapse = ", "),
         "\n  Rebuild/reinstall the package so all files under R/ are loaded.")

  old_opts <- options(dplyr.summarise.inform = FALSE)
  on.exit(options(old_opts), add = TRUE)

  # 1) get the MS, subset and stop if there are no sampling records 
  MS <- unique(sampling_df$flag_country)
  
  if (length(MS) != 1) {
    stop("Input dataframe must contain only one country.")
  }
  
  if (nrow(landings_df) == 0) {
    stop("  No Landings records found for ", MS)
  }
  message(paste("\nProcessing landings' lfd for: ", "MS = ", MS))
  
  # 2) Select samples to raise 
  
  sampling_df <- subset(sampling_df, sampling_type %in% sample_types)
  if (nrow(sampling_df) == 0) {
    stop("  No Sampling records for catch_category = Lan and selected sampling types ", sample_use, " found for  ", MS)
  }
  
  # 2a) Use maturity samples in the raising?
  if(!use_mat){
    sampling_df <- subset(sampling_df, is.na(maturity_method))
  }
  if (nrow(sampling_df) == 0) {
    stop("  No Sampling records remaining after removing samples collected for maturity ", MS)
  }
  
  # 3) subset to user selection (GSA, SP, YEAR)
  
  if (!is.null(GSA)){
    sampling_df <- subset(sampling_df, area %in% GSA)
    landings_df <- subset(landings_df, area %in% GSA)
    if (nrow(sampling_df) == 0) {
      stop("  No sampling records for ", MS, " for the selected area(s): ", paste(GSA, collapse=', ' ))
    }
  }
  
  if (!is.null(SP)){
    sampling_df <- subset(sampling_df, species %in% SP)
    landings_df <- subset(landings_df, species %in% SP)
    if (nrow(sampling_df) == 0) {
      stop("  No sampling records for ", MS, " for the selected GSA(s) for species(s): ", paste(SP, collapse=', ' ))
    }
  }
  
  if (!is.null(YEAR)){
    sampling_df <- subset(sampling_df, year %in% YEAR)
    landings_df <- subset(landings_df, year %in% YEAR)
    if (nrow(sampling_df) == 0) {
      stop("  No sampling records for ", MS, " for the selected GSA(s) and species(s) in years(s): ", paste(YEAR, collapse=', ' ))
    }
  }
  
  message("  Landings records: ", nrow(landings_df))
  message("  Sampling records: ", nrow(sampling_df))
  
  # 4) Create output folder
  
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  # 5) Minimal header checks  global
  
  # We only check for the fields that are strictly necessary for the preparation of lfd_base and discards_totals.
  required_landings <- c(
    "flag_country", "year", "quarter", "area", "species",
    "fishing_activity_category_eu_l6",
    "official_landings_weight"
  )
  
  missing_L <- setdiff(required_landings, names(landings_df))
  if (length(missing_L) > 0) {
    stop(
      paste(
        "The following required columns are missing in Landings file:",
        paste(missing_L, collapse = ", ")
      )
    )
  }
  
  required_sampling <- c(
    "sampling_type", "flag_country", "year", "date", "area", "species",
    "fishing_activity_category_eu_l6", "catch_category", "trip_code", "station_code",
    "length_class", "length_code", "number_at_length",
    "weight", "subsample_weight",
    "commercial_size_category",
    "aggregation_level"
  )
  
  missing_S <- setdiff(required_sampling, names(sampling_df))
  if (length(missing_S) > 0) {
    stop(
      paste(
        "The following required columns are missing in Sampling file:",
        paste(missing_S, collapse = ", ")
      )
    )
  }
  
  # 6) Normalise essential fields  global
  
  landings_df <- landings_df %>%
    mutate(
      flag_country = toupper(trimws(as.character(flag_country))),
      area         = toupper(trimws(as.character(area))),
      species      = toupper(trimws(as.character(species))),
      fishing_activity_category_eu_l6 =
        toupper(trimws(as.character(fishing_activity_category_eu_l6))),
      year    = suppressWarnings(as.integer(year)),
      quarter = suppressWarnings(as.integer(quarter)),
      official_landings_weight =
        suppressWarnings(as.numeric(official_landings_weight))
    )
  
  # Sampling side
  
  sampling_df <- sampling_df %>%
    mutate(
      sampling_type = toupper(trimws(as.character(sampling_type))),
      flag_country  = toupper(trimws(as.character(flag_country))),
      area          = toupper(trimws(as.character(area))),
      species       = toupper(trimws(as.character(species))),
      fishing_activity_category_eu_l6 =
        toupper(trimws(as.character(fishing_activity_category_eu_l6))),
      year          = suppressWarnings(as.integer(year)),
      catch_category = ifelse(
        is.na(catch_category),
        NA_character_,
        str_to_title(trimws(as.character(catch_category)))
      ),
      trip_code    = trimws(as.character(trip_code)),
      station_code = trimws(as.character(station_code)),
      length_code  = trimws(as.character(length_code)),
      length_class = suppressWarnings(as.numeric(length_class)),
      number_at_length    = suppressWarnings(as.integer(number_at_length)),
      weight              = suppressWarnings(as.numeric(weight)),
      subsample_weight    = suppressWarnings(as.numeric(subsample_weight)),
      commercial_size_category = trimws(as.character(commercial_size_category)),
      aggregation_level   = toupper(trimws(as.character(aggregation_level)))
    )
  
  # get the quarter 
  sampling_df$date_parsed <- suppressWarnings(as.Date(sampling_df$date, format = "%d/%m/%Y"))
  sampling_df$quarter <- ceiling(as.integer(month(sampling_df$date_parsed))/3)  
  
  
  # 7) check for and remove NAs in basic numeric and character fields that take part in aggregations
  
  indl <- which(is.na(landings_df$official_landings_weight))
  if(length(indl)!=0){
    landings_df <- landings_df[-indl,]
  }
  
  inds1 <- which(is.na(sampling_df$weight))
  inds2 <- which(is.na(sampling_df$subsample_weight))
  inds3 <- which(is.na(sampling_df$aggregation_level))
  
  inds <- unique(c(inds1, inds2, inds3))
  if(length(inds)!=0){
    sampling_df <- sampling_df[-inds,]
  }
  
  # 8) adds stock and stratum identifiers, and shorter code for metier
  
  # Add stock identifiers
  sampling_df <- sampling_df %>%
    mutate(stock = paste(
      area, species,
      sep = "_")) 
  
  landings_df <- landings_df %>%
    mutate(stock = paste(
      area, species,
      sep = "_")) 
  
  stocks <- sampling_df %>%
    mutate(stock = paste(area, species, sep = "_")) %>%
    distinct(stock, area, species)
  
  # Define stratum (quarter level stratum) 
  sampling_df <- sampling_df %>%
    group_by(year, quarter, fishing_activity_category_eu_l6) %>%
    mutate(stratum = paste0(year, '_Q', quarter, '_', fishing_activity_category_eu_l6)) %>%
    ungroup()
  
  # 9) Estimate total number of sampled hauls and append to sampling_df (these are used for ESP)
  nhauls      <- sampling_df %>% 
    group_by(year, quarter, area, trip_code, fishing_activity_category_eu_l6, stratum) %>% 
    summarize(n_sampled_hauls= n_distinct(station_code, na.rm=T) )
  sampling_df <- sampling_df %>% left_join(nhauls, by = c("year", "area", "quarter", "trip_code", "fishing_activity_category_eu_l6", 'stratum'))
  sampling_df$n_sampled_hauls[sampling_df$station_code == '999'] <- NA  
  
  # 10) Estimate discard ratios
  discard_ratio_res <- discard_ratio_rdbfis(
    sampling_df = sampling_df,
    out_dir = file.path(out_dir, "discard_ratio"),
    R = 500,
    conf = 0.95,
    seed = 123
  )
  
  discard_ratio_res <- discard_ratio_res[, c('year', 'area', 'metier', 'species', 'discard_ratio_landings')]
  
  # 10a) Estimate discard ratio for ESP as number of trips sampled for discards/total number of trips per stratum
  trips_cat <- sampling_df %>% 
    group_by(year, quarter, area, fishing_activity_category_eu_l6, stratum, catch_category) %>% 
    summarize(ntrips= n_distinct(trip_code, na.rm=T) )
  
  trips_total <- sampling_df %>% 
    group_by(year, quarter, area, fishing_activity_category_eu_l6, stratum) %>% 
    summarize(ntrips_tot= n_distinct(trip_code, na.rm=T) )
  
  disc_ratio_ESP <- trips_total %>% left_join(trips_cat, by = c("year", "area", "quarter", "fishing_activity_category_eu_l6", "stratum"))
  disc_ratio_ESP$ratio <- disc_ratio_ESP$ntrips_tot/disc_ratio_ESP$ntrips
  
  # 11) Aggregate official landings at MS, area, species, year, quarter, metier level
  
  landings_totals <- landings_df %>%
    transmute(
      stock=stock,
      flag_country=flag_country,
      area=area,
      species=species,
      year,
      quarter,
      metier = fishing_activity_category_eu_l6,
      stratum = paste0(year, '_Q', quarter, '_', fishing_activity_category_eu_l6),
      land   = ifelse(is.na(official_landings_weight),
                      0, official_landings_weight)
    ) %>%
    group_by(stock, flag_country, area, species, year, quarter, metier, stratum) %>%
    summarise(
      land = sum(land, na.rm = TRUE),
      .groups = "drop"
    )
  
  
  # 12) Estimate total discards by stratum
  
  discards_totals <- landings_totals %>% left_join(discard_ratio_res, by = c("year", "area", "metier", "species"))
  discards_totals$disc <- discards_totals$land * discards_totals$discard_ratio_landings 
  
  # 10) Restrict Sampling to LFD Discards
  sampling_df <- subset(sampling_df, catch_category == "Dis")
  
  # 10a) Estimate number of samples (individuals) per stratum
  nsample      <- sampling_df %>% 
    group_by(year, quarter, area, species, fishing_activity_category_eu_l6, stratum) %>% 
    summarize(N_sampled= sum(number_at_length, na.rm=T) )
  
  # 11) Build the base LFD table (lfd_base) - keep only necessary fields
  
  fields_base <- c(
    "stock", 'flag_country', 'area', 'species',
    "commercial_size_category", "sampling_type",
    "aggregation_level", "year", "fishing_activity_category_eu_l6",
    "weight", "subsample_weight",
    "length_class", "number_at_length",
    "quarter", "trip_code", "station_code",
    "stratum", "n_sets_hauls", "n_sampled_hauls"
  )
  
  lfd_base <- sampling_df[, fields_base]
  
  colnames(lfd_base) <- c(
    "stock", 'flag_country', 'area', 'species',
    "commercial_category", "sampling_type", "aggregation_level",
    "year", "metier",
    "w", "sub_w",
    "lc", "N",
    "quarter", "trip", "haul",
    "stratum", "n_sets_hauls", "n_sampled_hauls"
  )
  
  # 10) Estimate trip subsample_weights: observed (sub_w_trip_ob) and estimated from L-W relationship (sub_w_trip_est)
  st     <- stocks$stock
  r_list <- lapply(st, function(s) {
    temp1 <- lfd_base[lfd_base$stock == s, ]
    estimate_sub_sample_w(temp1, MS, st)  # call function
  })
  
  # Combine all stocks back into one dataframe
  lfd_base <- do.call(rbind, r_list)
  
  # flag entries when deviation is more than 50% (ratio less than 0.5 or more than 2)
  report <- subset(lfd_base, ratio>=2 | ratio<=0.5)
  
  vnames <- c("stock", 'flag_country', 'area', 'species',
              "commercial_category", "sampling_type", "aggregation_level",
              "year", "metier", "w", "sub_w", "lc", "N",
              "quarter", "trip", "haul",  "stratum", "n_sets_hauls",
              'n_sampled_hauls', 'SEX', 'A',	'B',
              'sub_w_trip_est',	'sub_w_trip_ob',	'ratio')
  lfd_base <- lfd_base[, vnames]
  
  # 11) Define the length bins (1mm for crustaceans and 10mm all the rest).
  
  crustaceans <- c('DPS', 'ARS', 'ARA', 'NEP', 'TGS', 'MTS')
  idx <- !(lfd_base$species %in% crustaceans)
  lfd_base$lc[idx] <- floor(lfd_base$lc[idx]/10) * 10
  
  
  # 13) Write outputs 
  
  #timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")  # kept for uniqueness within a session
  # Build output filename suffix: MS + GSA + SP + YEAR (mirrors QC scripts naming)
  suffix_parts <- MS
  if (!is.null(GSA))  suffix_parts <- c(suffix_parts, paste(GSA,  collapse = "-"))
  if (!is.null(SP))   suffix_parts <- c(suffix_parts, paste(SP,   collapse = "-"))
  file_suffix <- paste(suffix_parts, collapse = "_")
  
  #write_csv(
  #  lfd_base,
  #  file.path(out_dir, paste0("lfd_base_samples_", file_suffix, ".csv"))
  #)
  
  write_csv(
    report,
    file.path(out_dir, paste0("inconsistent_subsample_w_", file_suffix, ".csv"))
  )
  
  #write_csv(
  #  discards_totals,
  #  file.path(out_dir, paste0("landings_totals_strata_", file_suffix, ".csv"))
  #)
  
  res <- list(
    stocks          = stocks,
    lfd_base        = lfd_base,
    discards_totals = discards_totals, 
    disc_ratio_ESP  = disc_ratio_ESP
  )
  
  if (nrow(res$stocks) == 0) {
    stop("No stocks in sampling template.")
  }
  
  cat("\nLFD Discards input preparation completed for all stocks.\n")
  cat("Combinations processed:\n")
  apply(res$stocks, 1, function(row) cat("  stock =", row["stock"],"\n"))
  cat("Outputs written in: ", out_dir, "\n")
  
  # 13a) Report stocks + year combinations for which no landings were found
  
  inds <- unique(res$lfd_base[ , c('stock', 'year')])
  indl <- unique(res$discards_totals[ , c('stock', 'year')])
  
  inds <- as.character(interaction(inds$stock, inds$year, drop = TRUE))
  indl <- as.character(interaction(indl$stock, indl$year, drop = TRUE))
  
  missl <- inds[!inds %in% indl]
  misss <- indl[!indl %in% inds]
  
  if (length(missl)!=0){
    message(paste(
      "No official landings were found for the following stocks and years:",
      paste(missl, collapse = ", ")
    ))
  }
  
  # 14) Perform LFD raising using MS specific method or user selected method
  
  
  # MLT raising method - not yet implemented
  raise_MLT <- function(res) {
    stop(
      "raise_MLT() is not yet implemented for Malta (MLT).\n",
      "  The Malta-specific method will be added in a future release.\n",
      "  Using default method = 'vigneau_mahevas'"
    )
  }
  
  raise_by_country <- function(res, method) {
    
    country  <- unique(res$lfd_base$flag_country) # switch to lfd method depending on country
    if (is.null(method)) {
      method <- country
      raised_lfd <- switch(
        method,
        GRC = raise_GRC(res),
        ITA = raise_ITA(res),
        ESP = raise_ESP(res),
        MLT = raise_VM(res),
        CYP = raise_CYP(res),
        FRA = raise_FRA(res),
        HRV = raise_trip_h(res),
        stop(paste("No method implemented for country:", country))
      )
    } else {          
      raised_lfd <- switch(     # or apply method selected by user 
        method,
        trip_cc = raise_trip_cc(res),
        trip_h = raise_trip_h(res),
        vigneau_mahevas = raise_VM(res),
        method_grc = raise_GRC(res),
        method_cyp = raise_CYP(res),
        method_esp = raise_ESP(res),
        method_fra = raise_FRA(res),
        method_mlt = raise_VM(res),
        method_ita2 = raise_ITA2(res),
        method_ita3 = raise_ITA3(res),
        stop(paste("Not known method:", method))
      )
    }
    
    return(raised_lfd)
  }
  
  # Auxiliary function to handle raising by MS and default methods
  
  raise_VM <- function(res) {
    stocks <- unique(res$lfd_base$stock)
    
    result_list <- lapply(stocks, function(s) {
      lfd_stock <- res$lfd_base[res$lfd_base$stock == s, ]
      land_stock <- res$discards_totals[res$discards_totals$stock == s, ]
      
      vigneau_mahevas(lfd_stock, land_stock)  # call stock-level raising function
    })
    
    # Combine all stocks back into one dataframe
    x <- do.call(rbind, result_list)
    return(x)
  }
  
  raise_trip_cc <- function(res) {
    stocks <- unique(res$lfd_base$stock)
    
    result_list <- lapply(stocks, function(s) {
      lfd_stock <- res$lfd_base[res$lfd_base$stock == s, ]
      land_stock <- res$discards_totals[res$discards_totals$stock == s, ]
      
      trip_cc(lfd_stock, land_stock)  # call stock-level raising function
    })
    
    # Combine all stocks back into one dataframe
    x <- do.call(rbind, result_list)
    return(x)
  }
  
  raise_trip_h <- function(res) {
    stocks <- unique(res$lfd_base$stock)
    
    result_list <- lapply(stocks, function(s) {
      lfd_stock <- res$lfd_base[res$lfd_base$stock == s, ]
      land_stock <- res$discards_totals[res$discards_totals$stock == s, ]
      
      trip_h(lfd_stock, land_stock)  # call stock-level raising function
    })
    
    # Combine all stocks back into one dataframe
    x <- do.call(rbind, result_list)
    return(x)
  }
  
  raise_GRC <- function(res) {
    stocks <- unique(res$lfd_base$stock)
    
    result_list <- lapply(stocks, function(s) {
      lfd_stock <- res$lfd_base[res$lfd_base$stock == s, ]
      land_stock <- res$discards_totals[res$discards_totals$stock == s, ]
      
      method_grc(lfd_stock, land_stock)  # call stock-level raising function
    })
    
    # Combine all stocks back into one dataframe
    x <- do.call(rbind, result_list)
    return(x)
  }
  
  raise_FRA <- function(res) {
    stocks <- unique(res$lfd_base$stock)
    
    result_list <- lapply(stocks, function(s) {
      lfd_stock <- res$lfd_base[res$lfd_base$stock == s, ]
      land_stock <- res$discards_totals[res$discards_totals$stock == s, ]
      
      method_fra(lfd_stock, land_stock)  # call stock-level raising function
    })
    
    # Combine all stocks back into one dataframe
    x <- do.call(rbind, result_list)
    return(x)
  }
  
  raise_ESP <- function(res) {
    stocks <- unique(res$lfd_base$stock)
    
    result_list <- lapply(stocks, function(s) {
      lfd_stock <- res$lfd_base[res$lfd_base$stock == s, ]
      disc_ratio_ESP  <- res$disc_ratio_ESP
      method_esp_dis(lfd_stock, disc_ratio_ESP)  # call stock-level raising function
    })
    
    # Combine all stocks back into one dataframe
    x <- do.call(rbind, result_list)
    return(x)
  }
  
  raise_CYP <- function(res) {
    stocks <- unique(res$lfd_base$stock)
    
    result_list <- lapply(stocks, function(s) {
      lfd_stock <- res$lfd_base[res$lfd_base$stock == s, ]
      land_stock <- res$discards_totals[res$discards_totals$stock == s, ]
      
      method_cyp(lfd_stock, land_stock)  # call stock-level raising function
    })
    
    # Combine all stocks back into one dataframe
    x <-do.call(rbind, result_list)
    return(x)
  }
  
  raise_ITA <- function(res) {
    
    # Map areas (GSAs) to methods
    ITAmethods <- c(
      "GSA9" = "ITA2",
      "GSA10" = "ITA3",
      "GSA11" = "ITA3",
      "GSA16" = "ITA3",
      "GSA17" = "ITA2",
      "GSA18" = "ITA2",
      "GSA19" = "ITA2"
    )
    
    # Map each ITA method to its function
    method_map <- list(
      'ITA2' = raise_ITA2,
      'ITA3' = raise_ITA3
    )
    
    # add method to res by gsa according to ITAmethods
    res <- lapply(res, function(df) {
      df$method <- ITAmethods[df$area]
      df
    })
    
    # split the res list to one list per method
    method <- unique(res$lfd_base$method)
    
    res_split <- vector("list", length(method))
    names(res_split) <- method
    for (i in 1: length(method)){
      res_split[[i]]$stocks   <- subset(res$stocks, method==method[i])
      res_split[[i]]$lfd_base <- subset(res$lfd_base, method==method[i])
      res_split[[i]]$discards_totals <- subset(res$discards_totals, method==method[i])
    }
    
    # apply the method according to map
    res_list <- lapply(names(res_split), function(m) {
      method_map[[m]](res_split[[m]])
    })
    
    final_res <- bind_rows(res_list)
  }
  
  raise_ITA2 <- function(res) {
    stocks <- unique(res$lfd_base$stock)
    
    result_list <- lapply(stocks, function(s) {
      lfd_stock <- res$lfd_base[res$lfd_base$stock == s, ]
      land_stock <- res$discards_totals[res$discards_totals$stock == s, ]
      
      method_ita2(lfd_stock, land_stock)  # call stock-level raising function
    })
    
    # Combine all stocks back into one dataframe
    x <- do.call(rbind, result_list)
    return(x)
  }
  
  raise_ITA3 <- function(res) {
    stocks <- unique(res$lfd_base$stock)
    
    result_list <- lapply(stocks, function(s) {
      lfd_stock <- res$lfd_base[res$lfd_base$stock == s, ]
      land_stock <- res$discards_totals[res$discards_totals$stock == s, ]
      
      method_ita3(lfd_stock, land_stock)  # call stock-level raising function
    })
    
    # Combine all stocks back into one dataframe
    x <- do.call(rbind, result_list)
    return(x)
  }
  
  # 15) Raised lfds 
  raised_lfd <- raise_by_country(res, method)
  
  raised_lfd <- raised_lfd[, c("flag_country", "year", "quarter", "area",  "species", "metier",
                               "lc", "land", "N_final")]
  
  # remove stocks for which no landings records were found in landings template
  indr <- which(is.na(raised_lfd$flag_country))
  if (length(indr) > 0) {
    raised_lfd <- raised_lfd[-indr,]
  }
  
  if (is.null(method)){
    method <- paste0('method_', MS)
    raised_lfd$method  <- method
  } else {
    raised_lfd$method  <- method
  }
  
  # append with sampled individuals
  colnames(nsample) <- c('year', 'quarter','area', 'species', 'metier', 'stratum', 'N_sampled')
  raised_lfd <- raised_lfd %>% left_join(nsample, by = c("year", "area", "quarter", "species", "metier"))
  
  # 16) Make and save plots
  
  lfd_plot_rdbfis1(raised_lfd, out_dir, 'Disc')
  
  # 17) Prepare MED&BS discards output
  
  # merge with discards totals to get also the discards of strata for which no lfd was sampled
  medbs_out <- merge(raised_lfd, discards_totals, by=c("flag_country", "year", "quarter", "area",  "species", "metier"), all.x=T, all.y=T)
  
  medbs_out    <- medbs_out[, c("flag_country", "year", "quarter", "area", "species", "metier",
                                "lc", "land.y", "N_final", "method", "N_sampled")]
  colnames(medbs_out) <-  c("country", "year", "quarter", "area",  "species", "gear",
                            "lc", "discards", "N_final", "method", "sample_size")
  
  # get discards in tons, numbers in 1000 individuals and length bins in cm 
  medbs_out$lc <- medbs_out$lc/10  
  medbs_out$discards <- medbs_out$discards/1000 
  medbs_out$N_final  <- medbs_out$N_final/1000
  
  medbs_out    <- medbs_out %>%
    pivot_wider(
      names_from = lc,
      values_from = N_final
    )
  lcnames <- seq(0,100,1)
  lcnew   <- lcnames[which(!(lcnames %in% colnames(medbs_out)))]
  
  medbs_add <- as.data.frame(matrix(ncol=length(lcnew), nrow=nrow(medbs_out))) 
  colnames(medbs_add) <- lcnew
  
  medbs_out <- cbind(medbs_out, medbs_add)
  
  medbs_add1 <- as.data.frame(matrix(ncol=5, nrow=nrow(medbs_out)))
  colnames(medbs_add1) <- c("vessel_length", "mesh_size_range", "fishery", "specon", "unit")
  medbs_out <- cbind(medbs_out, medbs_add1)
  medbs_out$id <- paste(medbs_out$country, medbs_out$year, medbs_out$quarter, medbs_out$gear, medbs_out$area, sep='-')
  
  cols <- c("id", "country","year","quarter","vessel_length","gear",
            "mesh_size_range","fishery", "area","specon","species","discards","unit",
            "0","1","2","3","4", "5","6","7","8","9", "10","11","12","13","14",
            "15","16","17","18","19","20","21","22","23","24", 
            "25","26","27","28","29","30","31","32","33","34",
            "35","36","37","38","39","40","41","42","43","44",
            "45","46","47","48","49","50","51","52","53","54",
            "55","56","57","58","59","60","61","62","63","64",
            "65","66","67","68","69","70","71","72","73","74",
            "75","76","77","78","79","80","81","82","83","84",
            "85","86","87","88","89","90","91","92","93","94",
            "95","96","97","98","99","100", "sample_size")
  
  medbs_out <- medbs_out[,cols]
  medbs_out$unit <- 'cm'
  medbs_out$vessel_length <- NA 
  medbs_out$mesh_size_range <- substr(medbs_out$gear, 9, 16)
  medbs_out$fishery <- substr(medbs_out$gear, 5, 7)
  medbs_out$gear <- substr(medbs_out$gear, 1, 3)
  
  medbs_out[,14:114][is.na(medbs_out[,14:114])]=-1
  
  colnames(medbs_out)[14:114] <- paste0('lengthclass',colnames(medbs_out)[14:114])
  
  # 18) write result in file 
  
  readr::write_csv(medbs_out, file.path(out_dir, paste0("discards_", file_suffix, "-" , method, ".csv")))
  
  return(raised_lfd)
}
