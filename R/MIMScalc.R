


###########################################
# MIMScalc FUNCTION to convert raw numbers to ratios/concentrations
# Original code from Hall and Madinger 2016
# Updated by Luke Loken Aug 2018
###########################################	

# MIMScalc function first calculates raw ratios again for decimal accuracy and calculates saturation values for future use in the function. 
# Next, the function divides the data file in to the calibration data and sample data (Calibrate and Samp)
# In a loop, it calculates a linear regression for each group of samples using the pre and post calibration numbers for that sample. 
# The samples are then fit to the line for each regression. 
# At the end of the function, the data is all put into an output datafile that is saved to the computer. 

library(RcppRoll)
library(dplyr)

MIMScalc<-function(MIMSdata){
  
  nar<- (MIMSdata$N2.Ar) #Measured N2:Ar ratio
  MIMSdata$nar<-nar
  O2ar<-(MIMSdata$O2.Ar) #Measured O2:Ar ratio     
  MIMSdata$O2ar<-O2ar
  watdensv<-watdens(MIMSdata$Temp) #Calculated water density
  MIMSdata$watdensv<-watdensv
  O2satv<-osat1(MIMSdata$Temp, MIMSdata$Pressure) #Calculated saturation O2 conc
  MIMSdata$O2satv<-O2satv
  nsatv<-nsat(MIMSdata$Temp, MIMSdata$Pressure)  #Calculated saturation N2 conc
  MIMSdata$nsatv<-nsatv
  arsatv<-arsat(MIMSdata$Temp, MIMSdata$Pressure)  #Calculated saturation Ar conc
  MIMSdata$arsatv<-arsatv
  narsatv<- (MIMSdata$nsatv / MIMSdata$arsatv) #Calculated saturation N2:Ar ratio
  MIMSdata$narsatv<-narsatv
  O2arsatv<- (MIMSdata$O2satv / MIMSdata$arsatv) #Calculated saturation O2:Ar ratio
  MIMSdata$O2arsatv<-O2arsatv
  
  #subsampling
  calibMIMSdata<-MIMSdata[MIMSdata$Sampletype=="Calibrate",]
  samplesMIMSdata<-MIMSdata[MIMSdata$Sampletype=="Samp",]
  
  
  # ##############################################################
  # ##############################################################
  # Loken 2018: Correct the sample ratios to align with calibration solutions
  # This is based on a single point calibration with a forced y-intercept of zero
  # Thus this assumes that the ratio is linear
  
  #Calculate summaries for each calibration batch
  calmeans <- calibMIMSdata %>% 
    group_by(Calibnum) %>%
    summarize_at(c('narsatv', 'nar', 'O2arsatv', 'O2ar', 'arsatv', 'X40', 'nsatv', 'X28', 'O2satv', 'X32'), mean)
  
  calmeans_bysamp<-as.data.frame(sapply(calmeans, roll_mean, n=2))
  calmeans_bysamp$Calibnum<-floor(calmeans_bysamp$Calibnum)
  
  # Select which calibration ratio to use for each sample (need to change this so it uses both the pre and post cal solution)
  samplesMIMSdata$nar_coef1<-calmeans_bysamp$nar[as.numeric(samplesMIMSdata$Sampnum)]/calmeans_bysamp$narsatv[as.numeric(samplesMIMSdata$Sampnum)]
  samplesMIMSdata$O2ar_coef1<-calmeans_bysamp$O2ar[as.numeric(samplesMIMSdata$Sampnum)]/calmeans_bysamp$O2arsatv[as.numeric(samplesMIMSdata$Sampnum)]
  samplesMIMSdata$ar_coef1<-calmeans_bysamp$X40[as.numeric(samplesMIMSdata$Sampnum)]/calmeans_bysamp$arsatv[as.numeric(samplesMIMSdata$Sampnum)]
  samplesMIMSdata$n_coef1<-calmeans_bysamp$X28[as.numeric(samplesMIMSdata$Sampnum)]/calmeans_bysamp$nsatv[as.numeric(samplesMIMSdata$Sampnum)]
  samplesMIMSdata$O2_coef1<-calmeans_bysamp$X32[as.numeric(samplesMIMSdata$Sampnum)]/calmeans_bysamp$O2satv[as.numeric(samplesMIMSdata$Sampnum)]
  
  # Calculate corrected (_calc) for ratios and concentrations
  samplesMIMSdata$narcalc<-samplesMIMSdata$nar/samplesMIMSdata$nar_coef1
  samplesMIMSdata$O2arcalc<-samplesMIMSdata$O2ar/samplesMIMSdata$O2ar_coef1
  samplesMIMSdata$arcalc<-samplesMIMSdata$X40/samplesMIMSdata$ar_coef1
  samplesMIMSdata$ncalc<-samplesMIMSdata$X28/samplesMIMSdata$n_coef1
  samplesMIMSdata$O2calc<-samplesMIMSdata$X32/samplesMIMSdata$O2_coef1
  
  # Calculate (_calc) the ratios another way using the calculated concentrations
  samplesMIMSdata$narcalc2<-samplesMIMSdata$ncalc/samplesMIMSdata$arcalc
  samplesMIMSdata$O2arcalc2<-samplesMIMSdata$O2calc/samplesMIMSdata$arcalc
  
  # Calculate (_calc) for n and O2 using the calculated ratio and argon 
  samplesMIMSdata$ncalc2<-samplesMIMSdata$narcalc*samplesMIMSdata$arcalc
  samplesMIMSdata$O2calc2<-samplesMIMSdata$O2arcalc*samplesMIMSdata$arcalc
  
  outdf<-samplesMIMSdata
  return(outdf)
}

#End function





# ##################################################################
# Old code below from Bob Hall
# For this to work you need multiple temps for calibration solutions
# ##################################################################
# 
# 
# ## For each ratio, I use a linear model from the calibration data to convert the MIMS data (nar) into a corrected ratio (narcalc). 
# ## The loop calculates the values for samples inbetween each calibration interval. 
# #This only works if there are multiple temperatures used in calibration. 
# 
# #linear model for nar
# lmresult<-matrix(nrow=max(as.numeric(calibMIMSdata$Calibnum))-1,ncol=2)
# for (i in 1:(max(as.numeric(calibMIMSdata$Calibnum))-1)) {
#   calibs<-calibMIMSdata[as.numeric(calibMIMSdata$Calibnum)==i | as.numeric(calibMIMSdata$Calibnum) == i+1,]
#   output<-lm(calibs$narsatv~calibs$nar)
#   lmresult[i,]<-coef(output)
# }
# 
# samplesMIMSdata$coef1<-lmresult[as.numeric(samplesMIMSdata$Sampnum),1]
# samplesMIMSdata$coef2<-lmresult[as.numeric(samplesMIMSdata$Sampnum),2]
# 
# #calc concentration for nar
# samplesMIMSdata$narcalc<-samplesMIMSdata$coef1+samplesMIMSdata$coef2*samplesMIMSdata$nar
# 
# 
# 
# 
# ## Using the corrected ratio, I have another linear model to calculate the argon concentration (arconc)
# 
# #linear model for ar
# lmresultar<-matrix(nrow=max(as.numeric(calibMIMSdata$Calibnum))-1,ncol=2)
# for (i in 1:(max(as.numeric(calibMIMSdata$Calibnum))-1)) {
#   calibs<-calibMIMSdata[as.numeric(calibMIMSdata$Calibnum)==i | as.numeric(calibMIMSdata$Calibnum) == i+1,]
#   output<-lm(calibs$arsatv~as.numeric(calibs$X40))
#   lmresultar[i,]<-coef(output)
# }
# 
# samplesMIMSdata$arcoef1<-lmresultar[as.numeric(samplesMIMSdata$Sampnum),1]
# samplesMIMSdata$arcoef2<-lmresultar[as.numeric(samplesMIMSdata$Sampnum),2]
# 
# #calc concentration for ar
# samplesMIMSdata$arconc<-samplesMIMSdata$arcoef1+samplesMIMSdata$arcoef2*as.numeric(samplesMIMSdata$X40)
# 
# #calc n by proportion with nar and ar
# samplesMIMSdata$nconc<-samplesMIMSdata$nar*samplesMIMSdata$arconc
# 
# ## Argon and Oxygen have nearly identical Schmidt numbers so the saturation values for argon and oxygen do not vary by temperature compared to one another. 
# ## As a result, we only need information from one temperature. In this case I am using the warmer temperature for calculations. 
# ## The calculation pattern is otherwise similar to calculating nitrogen values. 
# 
# #subset just the high temperature data for O2 concentration calculation
# HighO2<-calibMIMSdata[calibMIMSdata$Calibtype=="Only",]
# 
# #X32 current O2 concentration calculation
# O2result<-matrix(nrow=max(as.numeric(HighO2$Calibnum))-1,ncol=1)
# for (i in 1:(max(as.numeric(HighO2$Calibnum))-1)) {
#   calibs<-HighO2[as.numeric(HighO2$Calibnum) == i | as.numeric(HighO2$Calibnum) == i+1,]
#   output<-mean(calibs$X32)
#   O2result[i,]<-(output)
# }
# 
# #O2 saturation calculation
# O2satresult<-matrix(nrow=max(as.numeric(HighO2$Calibnum))-1,ncol=1)
# for (i in 1:(max(as.numeric(HighO2$Calibnum))-1)) {
#   calibs<-HighO2[as.numeric(HighO2$Calibnum) == i | as.numeric(HighO2$Calibnum) == i+1,]
#   output<-mean(calibs$osat1v)
#   O2satresult[i,]<-(output)
# }
# 
# O2calibresult<-O2satresult/O2result 
# 
# samplesMIMSdata$O2conc<-(O2calibresult[as.numeric(samplesMIMSdata$Sampnum),1])*samplesMIMSdata$X32
# 
# #calculating the O2/Ar with only one calculated value to reduce the error 
# #O2ar current calculation
# O2arresult<-matrix(nrow=max(as.numeric(HighO2$Calibnum))-1,ncol=1)
# for (i in 1:(max(as.numeric(HighO2$Calibnum))-1)) {
#   calibs<-HighO2[as.numeric(HighO2$Calibnum) == i | as.numeric(HighO2$Calibnum) == i+1,]
#   output<-mean(calibs$O2ar)
#   O2arresult[i,]<-(output)
# }
# 
# #O2ar saturation calculation
# O2arsatresult<-matrix(nrow=max(as.numeric(HighO2$Calibnum))-1,ncol=1)
# for (i in 1:(max(as.numeric(HighO2$Calibnum))-1)) {
#   calibs<-HighO2[as.numeric(HighO2$Calibnum) == i | as.numeric(HighO2$Calibnum) == i+1,]
#   output<-mean(calibs$O2arsatv)
#   O2arsatresult[i,]<-(output)
# }
# 
# O2arcalibresult<-O2arsatresult/O2arresult 
# samplesMIMSdata$O2arcalc<-(O2arcalibresult[as.numeric(samplesMIMSdata$Sampnum),1])*samplesMIMSdata$O2ar
# 
# 
# outdf<-samplesMIMSdata
# outdf
# 
# # End Bob Hall's old code
# 
