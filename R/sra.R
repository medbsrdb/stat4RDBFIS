sra = function(sra_DATA,Species, Country, Area,styear,endyear,Comment=-1) {
  
  
  JRC_sra_Template=c("COUNTRY","AREA","START_YEAR","END_YEAR","SPECIES","AGECLASS","SEX_RATIO"  ,"COMMENTS")
  sra_DATA=as.data.frame(sra_DATA)
  #sra_outcome=list(
  #  Common_Sex  = list(),
 #   Per_sex = list()
  #)
  
  if (Country %in% sra_DATA$flag_country) {
    sra_DATA=sra_DATA[sra_DATA$flag_country==Country,]} else {
      stop("ERROR: The country is not included in the dataset. Choose correct country", call. = FALSE)
    }
  
  if (Species %in% sra_DATA$species) {
    sra_DATA=sra_DATA[sra_DATA$species==Species,]} else {
      stop("ERROR: The species is not included in the dataset. Choose correct species", call. = FALSE)
    }
  
  if (Area %in% sra_DATA$area) {
    sra_DATA=sra_DATA[sra_DATA$area==Area,]} else {
      stop("ERROR: The area is not included in the dataset. Choose correct area", call. = FALSE)
    }
  
  df_sra=sra_DATA[!is.na(sra_DATA$sex),]
  
  ####### BE CAREFUL! keep only biological sampling!
  #df_sra=sra_DATA[!is.na(sra_DATA$maturity_stage),]
  
  df_sra=sra_DATA[sra_DATA$sex=="F" | sra_DATA$sex=="M",]
  df_sra=sra_DATA[!is.na(sra_DATA$age),]
  df_sra=df_sra[df_sra$year>=styear,]
  df_sra=df_sra[df_sra$year<=endyear,]
  
   df_sra=as.data.frame(df_sra)
   
   ######## !!!!!!!!!!!!!!!!!!
   df_sra$age=floor(df_sra$age)
   
   
   df_sra_=df_sra%>%group_by(flag_country,area,species,age)%>%
     summarise(SEX_RATIO=length(sex[sex=="F"])/
                 (length(sex[sex=="F"])+length(sex[sex=="M"])), .groups = "drop")%>%ungroup()
   
   df_sra_$SEX_RATIO=round(df_sra_$SEX_RATIO,digits = 3)
   
   
   df_sra_=add_column(df_sra_, METHOD_="-1", .after  = "SEX_RATIO")


   df_sra_=add_column(df_sra_, Start_Year=min(df_sra$year), .before  = "species")
   df_sra_=add_column(df_sra_, End_Year=max(df_sra$year), .before  = "species")
   df_sra_$area  =paste("GSA", as.numeric(gsub("[^0-9.]", "", df_sra_$area)))
   
   colnames(df_sra_)=JRC_sra_Template
   
   sra_outcome=as.data.frame(df_sra_)
   sra_outcome$COMMENTS=Comment
   return(sra_outcome)
}
