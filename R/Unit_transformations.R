# Description: functions to convert length units to 'mm' or 'cm'
# for the length and the LW relationship 
# version 0.0.1
# author: stat4RDBFIS Core Team

LW_unit_transformation <- function(lw){
  # input: vector containing, a, b parameters of LW relationship and units
  # transform the length units to mm. Assumed that weight is in grams.
  # 'a' parameter is divided by conversion_factor ^ b, e.g. 10^b to translate to mm from cm  
   cf <- c('cm'=10,
           'mm'=1,
           'scm'=5,
           '25 mm'=25,
           '5 cm'=50)
  
  lw$A <- lw$A / cf[[lw$U_FLAG]] ^ lw$B
  lw$U_FLAG <- 'mm'
  
  return(lw)
  
}

L_unit_transformation_mm <- function(dfl){
  # input: lfd_base
  # length is divided by conversion_factor (cf)
    cf <- c('cm'=10,
          'mm'=1,
          'scm'=5,
          '25 mm'=25,
          '5 cm'=50)
  
  dfl$length_class <- cf[dfl$length_code] * dfl$length_class 
  dfl$length_code <- 'mm'
  return(dfl)
}

L_unit_transformation_cm <- function(dfl){
  # input: lfd_base
  # length is divided by conversion_factor (cf)
  cf <- c('cm'= 1,
          'mm'= 0.1,
          'scm'= 0.5,
          '25 mm'= 2.5,
          '5 cm'= 5)
  
  dfl$length_class <- cf[dfl$length_code] * dfl$length_class 
  dfl$length_code <- 'cm'
  return(dfl)
}
