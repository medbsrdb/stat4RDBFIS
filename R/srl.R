srl=function(srl_DATA,Species, Country, Area,styear,endyear, LC,Comment=-1) {
  
 
  JRC_srl_Template=c("COUNTRY","AREA","START_YEAR","END_YEAR","SPECIES","LENGTHCLASS","UNIT","SEX_RATIO"  ,"COMMENTS")
  srl_DATA=as.data.frame(srl_DATA)
  #srl_outcome=list(
  #  Common_Sex  = list(),
 #   Per_sex = list()
  #)
  
  if (Country %in% srl_DATA$flag_country) {
    srl_DATA=srl_DATA[srl_DATA$flag_country==Country,]} else {
      stop("ERROR: The country is not included in the dataset. Choose correct country", call. = FALSE)
    }
  
  if (Species %in% srl_DATA$species) {
    srl_DATA=srl_DATA[srl_DATA$species==Species,]} else {
      stop("ERROR: The species is not included in the dataset. Choose correct species", call. = FALSE)
    }
  
  if (Area %in% srl_DATA$area) {
    srl_DATA=srl_DATA[srl_DATA$area==Area,]} else {
      stop("ERROR: The area is not included in the dataset. Choose correct area", call. = FALSE)
    }
  
  df_srl=srl_DATA[!is.na(srl_DATA$sex),]
  
  ####### BE CAREFUL! keep only biological sampling!
  #df_srl=srl_DATA[!is.na(srl_DATA$maturity_stage),]
  
  df_srl=srl_DATA[srl_DATA$sex=="F" | srl_DATA$sex=="M",]
  df_srl=df_srl[df_srl$year>=styear,]
  df_srl=df_srl[df_srl$year<=endyear,]
  
  
  lc_unit=unique(df_srl$length_code)
  
  if (length(lc_unit)!=1) {
    stop("ERROR: All length code units should be identical.Length code units.", call. = FALSE)
  }
  

  if (is.null(LC)) {
    if( Species %in% c("DPS","ARS","ARA","NEP","TGS","MTS")) {
      df_srl$length_class  =floor(  df_srl$length_class)
      df_srl$length_code  ="mm"
    } else {
      df_srl$length_class  =floor(  df_srl$length_class/10)
      df_srl$length_code  ="cm"
    }} else if (LC=="cm") {
      df_srl$length_class  =floor(  df_srl$length_class/10)
      df_srl$length_code  ="cm"
    } else if (LC=="mm") {
      df_srl$length_class  =floor(  df_srl$length_class)
      df_srl$length_code  ="mm"
    } else {
      stop("ERROR: The length code unit is wrong.Accepted length code units: cm, mm. set LC='cm', or LC='mm'. ", call. = FALSE)
    }
  
   df_srl=as.data.frame(df_srl)
 
   df_srl_=df_srl%>%group_by(flag_country,area,species,length_class,length_code)%>%
     summarise(SEX_RATIO=length(sex[sex=="F"])/
                 (length(sex[sex=="F"])+length(sex[sex=="M"])), .groups = "drop")%>%ungroup()
   
   df_srl_$SEX_RATIO=round(df_srl_$SEX_RATIO,digits = 3)
   
   
   df_srl_=add_column(df_srl_, METHOD_="-1", .after  = "SEX_RATIO")


   df_srl_=add_column(df_srl_, Start_Year=min(df_srl$year), .before  = "species")
   df_srl_=add_column(df_srl_, End_Year=max(df_srl$year), .before  = "species")
   df_srl_$area  =paste("GSA", as.numeric(gsub("[^0-9.]", "", df_srl_$area)))
   
   colnames(df_srl_)=JRC_srl_Template
   
   srl_outcome=as.data.frame(df_srl_)
   srl_outcome$COMMENTS=Comment
   return(srl_outcome)
   
}
