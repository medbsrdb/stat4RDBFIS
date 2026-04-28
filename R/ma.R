ma = function(MA_DATA,Species, Country, Area,styear,endyear,Imm) {
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
  
  
  JRC_MA_Template=c("COUNTRY","AREA","START_YEAR","END_YEAR","SPECIES","SEX","AGECLASS","SAMPLE_SIZE","PRM", "METHOD_USED")
  MA_DATA=as.data.frame(MA_DATA)
  MA_outcome=list(
    Common_Sex  = list(),
    Per_sex = list()
  )
  
  if (Country %in% MA_DATA$flag_country) {
    MA_DATA=MA_DATA[MA_DATA$flag_country==Country,]} else {
      stop("ERROR: The country is not included in the dataset. Choose correct country", call. = FALSE)
    }
  
  if (Species %in% MA_DATA$species) {
    MA_DATA=MA_DATA[MA_DATA$species==Species,]} else {
      stop("ERROR: The species is not included in the dataset. Choose correct species", call. = FALSE)
    }
  
  if (Area %in% MA_DATA$area) {
    MA_DATA=MA_DATA[MA_DATA$area==Area,]} else {
      stop("ERROR: The area is not included in the dataset. Choose correct area", call. = FALSE)
    }
  
  df_mat=MA_DATA[!is.na(MA_DATA$maturity_stage),]
  df_mat=df_mat[!is.na(df_mat$age),]
  
  
   df_mat$mature=ifelse(df_mat$maturity_stage %in% Imm,0,1)
   df_mat=as.data.frame(df_mat)
   df_mat=df_mat[df_mat$year>=styear,]
   df_mat=df_mat[df_mat$year<=endyear,]
   ######## !!!!!!!!!!!!!!!!!!
   df_mat$age=floor(df_mat$age)
   
  # ######### COMMON SEX
     fit <- fit_glm_quietly(mature~age,data=df_mat)
   MA_outcome[["Common_Sex"]][["binomial_fit"]]=fit
 MA_outcome[["Common_Sex"]][["summary_fit"]]=summary(fit)
 MA_outcome[["Common_Sex"]][["fit_warnings"]] <- attr(fit, "fit_warnings")
 
 x2 <- fit_glm_quietly(mature ~ 1, data = df_mat)
 M1 <- fit$null.deviance - fit$deviance
 M2 <- x2$null.deviance - x2$deviance
 M3 <- abs(M1 - M2)
 r2 <- as.numeric((1 - exp(-M3/nrow(fit$model)))/(1 - exp(2 * 
                                                          as.numeric(logLik(x2)/nrow(fit$model)))))
 
  MA_outcome[["Common_Sex"]][["R2_fit"]]=r2
 MA_outcome[["Common_Sex"]][["L50_fit"]]=(log(0.5/(1- 0.5))-coef( fit)[1])/coef( fit)[2]

  df_mat_common_fit=df_mat%>%group_by(flag_country,area,species,age)%>%
    summarise(specims= length(species),.groups = "drop")%>%ungroup()

  df_mat_common_fit$MAT_RATIO <- predict(fit,
                                          newdata =  data.frame(age = df_mat_common_fit$age),type = "response")

  df_mat_common_fit=add_column(df_mat_common_fit, SEX="C", .after  = "species")

  df_mat_common_fit=add_column(df_mat_common_fit, METHOD_="GLM, binomial model", .after  = "MAT_RATIO")
  df_mat_common_fit=add_column(df_mat_common_fit, Start_Year=min(df_mat$year), .before  = "species")
  df_mat_common_fit=add_column(df_mat_common_fit, End_Year=max(df_mat$year), .before  = "species")

  df_mat_common_fit$area=paste("GSA", as.numeric(gsub("[^0-9.]", "", df_mat_common_fit$area)))
  colnames(df_mat_common_fit)=JRC_MA_Template

  df_mat_common_fit=as.data.frame(df_mat_common_fit)
  MA_outcome[["Common_Sex"]][["Output_model"]]=df_mat_common_fit

  df_mat_common=df_mat%>%group_by(flag_country,area,species,age)%>%
    summarise(specims= length(species),
              MAT_RATIO=length(mature[mature==1])/
                (length(mature[mature==0])+length(mature[mature==1])),.groups = "drop")%>%ungroup()
  df_mat_common$MAT_RATIO=round(df_mat_common$MAT_RATIO,digits = 3)

  df_mat_common_fit$PRM= df_mat_common$MAT_RATIO
  df_mat_common_fit$METHOD_USED=""
  MA_outcome[["Common_Sex"]][["Output_raw"]]=df_mat_common_fit

  ######### Per SEX
  df_mat=df_mat[df_mat$sex %in% c("F","M"),]
 fitsex <- fit_glm_quietly(mature~age*sex,data=as.data.frame(df_mat))
 fitnosex <- fit_glm_quietly(mature~age,data=as.data.frame(df_mat))
   MA_outcome[["Per_sex"]][["binomial_fit"]]=fitsex
  MA_outcome[["Per_sex"]][["summary_fit"]]=summary(fitsex)
  MA_outcome[["Per_sex"]][["fit_warnings"]] <- unique(c(
    attr(fitsex, "fit_warnings"),
    attr(fitnosex, "fit_warnings")
  ))
  p_sex = anova(fitnosex, fitsex, test="Chisq")$'Pr(>Chi)'[2]
  MA_outcome[["Per_sex"]][["p_value_sex"]] = p_sex
  
  x2 <- fit_glm_quietly(mature ~ 1, data = as.data.frame(df_mat))
  M1 <- fitsex$null.deviance - fitsex$deviance
  M2 <- x2$null.deviance - x2$deviance
  M3 <- abs(M1 - M2)
  r2 <- as.numeric((1 - exp(-M3/nrow(fitsex$model)))/(1 - exp(2 * 
                                                             as.numeric(logLik(x2)/nrow(fitsex$model)))))
  MA_outcome[["Per_sex"]][["R2_fit"]]=r2
  MA_outcome[["Per_sex"]][["L50_fit"]]=data.frame(L50_Females= (log(0.5/(1- 0.5))-coef(fitsex)[1])/coef(fitsex)[2],
                                                  L50_Males=     (log(0.5/(1- 0.5))-(coef(fitsex)[1] + coef(fitsex)[3]))/
                                                    (coef(fitsex)[2]+coef(fitsex)[4])
  )
  df_mat_per_sex_fit=df_mat%>%group_by(flag_country,area,species,sex ,age)%>%
    summarise(specims= length(species),.groups = "drop")%>%ungroup()

  df_mat_per_sex_fit$MAT_RATIO <- predict(fitsex,newdata =  df_mat_per_sex_fit[,c(4,5)],type = "response")
  df_mat_per_sex_fit=add_column(df_mat_per_sex_fit, METHOD_="GLM, binomial model", .after  = "MAT_RATIO")
  df_mat_per_sex_fit=add_column(df_mat_per_sex_fit, Start_Year=min(df_mat$year), .before  = "species")
  df_mat_per_sex_fit=add_column(df_mat_per_sex_fit, End_Year=max(df_mat$year), .before  = "species")
  df_mat_per_sex_fit$area=paste("GSA", as.numeric(gsub("[^0-9.]", "", df_mat_per_sex_fit$area)))
  colnames(df_mat_per_sex_fit)=JRC_MA_Template
  df_mat_per_sex_fit=as.data.frame(df_mat_per_sex_fit)

  MA_outcome[["Per_sex"]][["Output_model"]]=df_mat_per_sex_fit

  df_mat_per_sex=df_mat%>%group_by(flag_country,area,species,sex,age)%>%
    summarise(specims= length(species),
              MAT_RATIO=length(mature[mature==1])/
                (length(mature[mature==0])+length(mature[mature==1])),.groups = "drop")%>%ungroup()
  df_mat_per_sex$MAT_RATIO=round(df_mat_per_sex$MAT_RATIO,digits = 3)

  df_mat_per_sex_fit$PRM= df_mat_per_sex$MAT_RATIO
  df_mat_per_sex_fit$METHOD_USED=""
  MA_outcome[["Per_sex"]][["Output_raw"]]=df_mat_per_sex_fit

  return(MA_outcome)
}
