


# ##############################################################################################
# Don't change below
# Code calculated metabolism using mysheets which is done after you read in your excel file
# Jar data needs to be in the first sheet
# DO and Time data need to be in the second sheet
# ##############################################################################################


CalculateIncubationMetabolism <- function (mysheets, Date){

  #Load libraies
  
  library(plyr)
  library(dplyr)
  library(tidyr)
  
  library(ggplot2)
  library(gridExtra)
  library(RColorBrewer)
  
  library(lubridate)
  

#Jar sheet
JarData<-mysheets[[1]]

#DO sheet
DOdata<-mysheets[[2]]

time_cols<-names(DOdata)[startsWith(names(DOdata), 'Date')]
DO_cols<-names(DOdata)[startsWith(names(DOdata), 'Oxygen')]

time_df <- DOdata[time_cols]
DO_df <- DOdata[DO_cols]

if (identical(dim(time_df), dim(DO_df))==FALSE){
  stop('Time and oxygen data are different dimensions. Check data file')
}

if (ncol(time_df) %% 2 == 0){
  extra<-1
  warning('Even number of timepoints. There should be an odd number')
} else { extra=0}

#Remove NAs from Jar vector and generate a vector of the Jar reads
JarSampleOrder<- JarData %>%
  dplyr::select(starts_with("Jar")) %>%
  dplyr::select(1) 

TreatmentSampleOrder<- JarData %>%
  dplyr::select(starts_with("Treatment")) %>%
  dplyr::select(1) 

SiteSampleOrder<- JarData %>%
  dplyr::select(starts_with("Site")) %>%
  dplyr::select(1) 


Jar<-rep(JarSampleOrder[,1], each=nrow(DO_df)/length(JarSampleOrder[,1]))

#Average multiple observations of the same jar/time
#Note that these summary tables sort the data based on Jar, so they are no longer in the JarRead order
DOmean <- data.frame(Jar, DO_df) %>%
  dplyr::group_by(Jar) %>%
  summarize_all(mean, na.rm=T)

DOmedian <- data.frame(Jar, DO_df) %>%
  group_by(Jar) %>%
  summarize_all(median, na.rm=T)

DOsd <-  data.frame(Jar, DO_df) %>%
  group_by(Jar) %>%
  summarize_all(sd, na.rm=T)

timemean <- data.frame(Jar, time_df) %>%
  group_by(Jar) %>%
  summarize_all(mean, na.rm=T)

#Calculate difference between successive measurements
DO_diff_mean<-as.data.frame(apply(DOmean[,-1], 1, function(x) diff(x)))
names(DO_diff_mean)<-DOmean$Jar

DO_diff_median<-as.data.frame(apply(DOmedian[,-1], 1, function(x) diff(x)))
names(DO_diff_median)<-DOmedian$Jar

#Convert time data.frame to seconds and calculate time difference
seconds_df<-sapply(timemean[,-1], seconds)
time_diff_mean<-as.data.frame(apply(seconds_df, 1, function(x) diff(x)))
names(time_diff_mean)<-timemean$Jar

# If there is an extra column of data (i.e., last measurement was at the end of a light period);
# extra == 1, the last measurement will be NEP and there will not be a final ER or GPP calcualtion 
if (extra==0){
row.names(DO_diff_mean)<-paste0(rep(c('NEP', 'ER'), nrow(DO_diff_mean)*.5), rep(1:(nrow(DO_diff_mean)*.5), each=2))
} else {
  row.names(DO_diff_mean)<-paste0(rep(c('NEP', 'ER'), (nrow(DO_diff_mean)+1)*.5), rep(1:((nrow(DO_diff_mean)+1)*.5), each=2))[1:nrow(DO_diff_mean)]
}
row.names(DO_diff_median)<-row.names(DO_diff_mean)
row.names(time_diff_mean)<-row.names(DO_diff_mean)

# ######################################
# Calculate metabolism for each jar/time
# Note that ER is negative
# ######################################

#Create empty dataframes
DOrate_mean<-as.data.frame(matrix(nrow=ncol(DO_diff_mean), ncol=nrow(DO_diff_mean)+1))
names(DOrate_mean)<-c('Jar', row.names(DO_diff_mean))
DOrate_mean[,1]<-names(DO_diff_mean)
DOrate_median <- DOrate_mean

# Calculate ER and NEP (mg O2 per liter per hour)
row=1
for (row in 1:nrow(DOrate_mean)){
  timediff <- time_diff_mean[,row] #seconds
  if (Date=="050718"){ timediff<-c(12.5, 10.5, 12.5, 10.5, 12.5, 10.5, 12.5)*3600}
  
  DOdiff_mean <- DO_diff_mean[,row]
  DOdiff_mean_per_h <- DOdiff_mean/timediff*3600 #convert to hours
  DOrate_mean[row,2:ncol(DOrate_mean)]<- round(DOdiff_mean_per_h,3)
  
  DOdiff_median <- DO_diff_median[,row]
  DOdiff_median_per_h <- DOdiff_median/timediff*3600 #convert to hours
  DOrate_median[row,2:ncol(DOrate_median)]<- round(DOdiff_median_per_h,3)
}

#Calculate GPP from difference between daytime NEP and successive ER (mg O2 per liter per hour)
day=1
for (day in 1:((ncol(DOrate_mean)-1)/2)){
  #mean
  NEP<-DOrate_mean[,paste0('NEP', day)]
  ER<-DOrate_mean[,paste0('ER', day)]
  GPP<-NEP-ER
  
  DOrate_mean[,ncol(DOrate_mean)+1] <- GPP
  names(DOrate_mean)[ncol(DOrate_mean)]<-paste0('GPP', day)
  
  #median
  NEP_median<-DOrate_median[,paste0('NEP', day)]
  ER_median<-DOrate_median[,paste0('ER', day)]
  GPP_median<-NEP_median-ER_median
  
  DOrate_median[,ncol(DOrate_median)+1] <- GPP
  names(DOrate_median)[ncol(DOrate_median)]<-paste0('GPP', day)
}

#Link Treatments and Sites to Jars

DOrate_mean$Treatment<-factor(TreatmentSampleOrder[,1][match(DOrate_mean$Jar, JarSampleOrder[,1])])
DOrate_mean$Site<-SiteSampleOrder[,1][match(DOrate_mean$Jar, JarSampleOrder[,1])]

DOrate_median$Treatment<-factor(TreatmentSampleOrder[,1][match(DOrate_median$Jar, JarSampleOrder[,1])])
DOrate_median$Site<-SiteSampleOrder[,1][match(DOrate_median$Jar, JarSampleOrder[,1])]

#Format data for ggplot and output csv
DOrate_mean_long_table<- DOrate_mean %>%
  gather("Metric", "Value", 2:(ncol(DOrate_mean)-2))

DOrate_mean_long_table$Day<-as.numeric(gsub("[^0-9]", "", DOrate_mean_long_table$Metric) )
DOrate_mean_long_table$Metric<-gsub('[0-9]+', '', DOrate_mean_long_table$Metric)
DOrate_mean_long_table$SampleDate<-rep(mdy(Date), nrow(DOrate_mean_long_table))


DOrate_median_long_table<- DOrate_median %>%
  gather("Metric", "Value", 2:(ncol(DOrate_median)-2))

DOrate_median_long_table$Day<-as.numeric(gsub("[^0-9]", "", DOrate_median_long_table$Metric) )
DOrate_median_long_table$Metric<-gsub('[0-9]+', '', DOrate_median_long_table$Metric)
DOrate_median_long_table$SampleDate<-rep(mdy(Date), nrow(DOrate_median_long_table))

return(list(DOrate_mean_long_table, DOsd))
}

