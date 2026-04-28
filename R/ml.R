ml=function(ML_DATA,Species, Country, Area,styear,endyear, Imm, LC) {
  fit_glm_quietly <- function(formula, data) {
    fit_warnings <- character(0)
    fit <- withCallingHandlers(
      stats::glm(formula, data = data, family = stats::binomial()),
      warning = function(w) {
        fit_warnings <<- c(fit_warnings, conditionMessage(w))
        tryInvokeRestart("muffleWarning")
      }
    )
    attr(fit, "fit_warnings") <- unique(fit_warnings)
    fit
  }

  JRC_ML_Template=c("COUNTRY","AREA","START_YEAR","END_YEAR","SPECIES","SEX","LENGTHCLASS","UNIT","SAMPLE_SIZE","PRM", "METHOD_USED")
  ML_DATA=as.data.frame(ML_DATA)
  ML_outcome=list(
    Common_Sex  = list(),
    Per_sex = list()
  )
  
  if (Country %in% ML_DATA$flag_country) {
    ML_DATA=ML_DATA[ML_DATA$flag_country==Country,]} else {
      stop("ERROR: The country is not included in the dataset. Choose correct country", call. = FALSE)
    }
  
  if (Species %in% ML_DATA$species) {
    ML_DATA=ML_DATA[ML_DATA$species==Species,]} else {
      stop("ERROR: The species is not included in the dataset. Choose correct species", call. = FALSE)
    }
  
  if (Area %in% ML_DATA$area) {
    ML_DATA=ML_DATA[ML_DATA$area==Area,]} else {
      stop("ERROR: The area is not included in the dataset. Choose correct area", call. = FALSE)
    }
  
  df_mat=ML_DATA[!is.na(ML_DATA$maturity_stage),]
  
  # COMMON SEX
  lc_unit=unique(df_mat$length_code)
  
  if (length(lc_unit)!=1) {
    stop("ERROR: All length code units should be identical.Length code units.", call. = FALSE)
  }
  
  
  if (is.null(LC)) {
  if( Species %in% c("DPS","ARS","ARA","NEP","TGS","MTS")) {
      df_mat$length_class  =floor(  df_mat$length_class)
      df_mat$length_code  ="mm"
  } else {
    df_mat$length_class  =floor(  df_mat$length_class/10)
        df_mat$length_code  ="cm"
  }} else if (LC=="cm") {
    df_mat$length_class  =floor(  df_mat$length_class/10)
    df_mat$length_code  ="cm"
  } else if (LC=="mm") {
    df_mat$length_class  =floor(  df_mat$length_class)
    df_mat$length_code  ="mm"
  } else {
    stop("ERROR: The length code unit is wrong.Accepted length code units: cm, mm. set LC='cm', or LC='mm'. ", call. = FALSE)
  }
  #lc_unit=unique(df_mat$length_code)
   #df_mat$length_class  =floor(  df_mat$length_class)


   df_mat$mature=ifelse(df_mat$maturity_stage %in% Imm,0,1)
   df_mat=as.data.frame(df_mat)
   df_mat=df_mat[df_mat$year>=styear,]
   df_mat=df_mat[df_mat$year<=endyear,]
   
   
  # ######### COMMON SEX
  #  # cat("DEBUG: nrow(df_mat) =", nrow(df_mat), "\n")
  #  # str(df_mat[, c("maturity_stage", "length_class", "sex")])
  #  
   fit <- fit_glm_quietly(mature~length_class,data=df_mat)
   ML_outcome[["Common_Sex"]][["binomial_fit"]]=fit
 ML_outcome[["Common_Sex"]][["summary_fit"]]=summary(fit)
 ML_outcome[["Common_Sex"]][["fit_warnings"]] <- attr(fit, "fit_warnings")
 
 x2 <- fit_glm_quietly(mature ~ 1, data = df_mat)
 M1 <- fit$null.deviance - fit$deviance
 M2 <- x2$null.deviance - x2$deviance
 M3 <- abs(M1 - M2)
 r2 <- as.numeric((1 - exp(-M3/nrow(fit$model)))/(1 - exp(2 * 
                                                          as.numeric(logLik(x2)/nrow(fit$model)))))
 
  ML_outcome[["Common_Sex"]][["R2_fit"]]=r2
 ML_outcome[["Common_Sex"]][["L50_fit"]]=(log(0.5/(1- 0.5))-coef( fit)[1])/coef( fit)[2]

 df_mat_common_fit <- aggregate(
   rep(1, nrow(df_mat)) ~ flag_country + area + species + length_class + length_code,
   data = df_mat,
   FUN = sum
 )
 names(df_mat_common_fit)[6] <- "specims"
 
  df_mat_common_fit$MAT_RATIO <- predict(fit,
                                          newdata =  data.frame(length_class = df_mat_common_fit$length_class),type = "response")

  df_mat_common_fit=add_column(df_mat_common_fit, SEX="C", .after  = "species")

  df_mat_common_fit=add_column(df_mat_common_fit, METHOD_="GLM, binomial model", .after  = "MAT_RATIO")
  df_mat_common_fit=add_column(df_mat_common_fit, Start_Year=min(df_mat$year), .before  = "species")
  df_mat_common_fit=add_column(df_mat_common_fit, End_Year=max(df_mat$year), .before  = "species")

  df_mat_common_fit$area=paste("GSA", as.numeric(gsub("[^0-9.]", "", df_mat_common_fit$area)))
  colnames(df_mat_common_fit)=JRC_ML_Template

  df_mat_common_fit=as.data.frame(df_mat_common_fit)
  ML_outcome[["Common_Sex"]][["Output_model"]]=df_mat_common_fit

  df_mat_common=df_mat%>%group_by(flag_country,area,species,length_class,length_code)%>%
    summarise(specims= length(species),
              MAT_RATIO=length(mature[mature==1])/
                (length(mature[mature==0])+length(mature[mature==1])),.groups = "drop")%>%ungroup()
  
  
  df_mat_common$MAT_RATIO=round(df_mat_common$MAT_RATIO,digits = 3)

  df_mat_common_fit$PRM= df_mat_common$MAT_RATIO
  df_mat_common_fit$METHOD_USED=""
  ML_outcome[["Common_Sex"]][["Output_raw"]]=df_mat_common_fit

  ######### Per SEX
  df_mat=df_mat[df_mat$sex %in% c("F","M"),]
 fitsex <- fit_glm_quietly(mature~length_class*sex,data=as.data.frame(df_mat))
 fit_nosex <- fit_glm_quietly(mature~length_class,data=as.data.frame(df_mat))
 
  ML_outcome[["Per_sex"]][["binomial_fit"]]=fitsex
  ML_outcome[["Per_sex"]][["summary_fit"]]=summary(fitsex)
  ML_outcome[["Per_sex"]][["fit_warnings"]] <- unique(c(
    attr(fitsex, "fit_warnings"),
    attr(fit_nosex, "fit_warnings")
  ))
  p_sex = anova(fit_nosex, fitsex, test="Chisq")$'Pr(>Chi)'[2]
  ML_outcome[["Per_sex"]][["p_value_sex"]] = p_sex
  
  x2 <- fit_glm_quietly(mature ~ 1, data = as.data.frame(df_mat))
  M1 <- fitsex$null.deviance - fitsex$deviance
  M2 <- x2$null.deviance - x2$deviance
  M3 <- abs(M1 - M2)
  r2 <- as.numeric((1 - exp(-M3/nrow(fitsex$model)))/(1 - exp(2 * 
                                                             as.numeric(logLik(x2)/nrow(fitsex$model)))))
  ML_outcome[["Per_sex"]][["R2_fit"]]=r2
  ML_outcome[["Per_sex"]][["L50_fit"]]=data.frame(L50_Females= (log(0.5/(1- 0.5))-coef(fitsex)[1])/coef(fitsex)[2],
                                                  L50_Males=     (log(0.5/(1- 0.5))-(coef(fitsex)[1] + coef(fitsex)[3]))/
                                                    (coef(fitsex)[2]+coef(fitsex)[4])
  )
  df_mat_per_sex_fit=df_mat%>%group_by(flag_country,area,species,sex ,length_class,length_code)%>%
    summarise(specims= length(species),.groups = "drop")%>%ungroup()

  df_mat_per_sex_fit$MAT_RATIO <- predict(fitsex,newdata =  df_mat_per_sex_fit[,c(4,5)],type = "response")
  df_mat_per_sex_fit=add_column(df_mat_per_sex_fit, METHOD_="GLM, binomial model", .after  = "MAT_RATIO")
  df_mat_per_sex_fit=add_column(df_mat_per_sex_fit, Start_Year=min(df_mat$year), .before  = "species")
  df_mat_per_sex_fit=add_column(df_mat_per_sex_fit, End_Year=max(df_mat$year), .before  = "species")
  df_mat_per_sex_fit$area=paste("GSA", as.numeric(gsub("[^0-9.]", "", df_mat_per_sex_fit$area)))
  colnames(df_mat_per_sex_fit)=JRC_ML_Template
  df_mat_per_sex_fit=as.data.frame(df_mat_per_sex_fit)

  ML_outcome[["Per_sex"]][["Output_model"]]=df_mat_per_sex_fit

  df_mat_per_sex=df_mat%>%group_by(flag_country,area,species,sex,length_class,length_code)%>%
    summarise(specims= length(species),
              MAT_RATIO=length(mature[mature==1])/
                (length(mature[mature==0])+length(mature[mature==1])),.groups = "drop")%>%ungroup()
  df_mat_per_sex$MAT_RATIO=round(df_mat_per_sex$MAT_RATIO,digits = 3)

  df_mat_per_sex_fit$PRM= df_mat_per_sex$MAT_RATIO
  df_mat_per_sex_fit$METHOD_USED=""
  ML_outcome[["Per_sex"]][["Output_raw"]]=df_mat_per_sex_fit

  return(ML_outcome)
}
