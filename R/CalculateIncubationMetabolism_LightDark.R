


# ##############################################################################################
# Don't change below
# Code calculated metabolism using mysheets which is done after you read in your excel file
# Jar data needs to be in the first sheet
# DO and Time data need to be in the second sheet
# ##############################################################################################


CalculateIncubationMetabolism_LightDark <- function (mysheets, reads){

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

time_df<-DOdata[time_cols]
DO_df<-DOdata[DO_cols]

if (identical(dim(time_df), dim(DO_df))==FALSE){
  stop('Time and oxygen data are different dimensions. Check data file')
}


#Remove NAs from Jar vector and generate a vector of the Jar reads
JarSampleOrder<-JarData$Jar__1[!is.na(JarData$Jar__1)]

reads_est<-nrow(DO_df)/length(JarSampleOrder)
if (reads_est == reads){
Jar<-rep(JarSampleOrder, each=nrow(DO_df)/length(JarSampleOrder))
} else {
  stop('Number of reads entered does not match data. Check data file (sheet 1) for missing or extra Jar names and/or extra dissolved oxygen measurements on data file (sheet 2)')
}

#Average multiple observations of the same jar/time
#Note that these summary tables sort the data based on Jar, so they are no longer in the JarRead order
DOmean <- data.frame(Jar, DO_df) %>%
  group_by(Jar) %>%
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
DO_diff_median<-as.data.frame(apply(DOmedian[,-1], 1, function(x) diff(x)))


#Convert time data.frame to seconds and calculate time difference
seconds_df<-sapply(timemean[,-1], seconds)
time_diff_mean<-as.data.frame(apply(seconds_df, 1, function(x) diff(x)))

#If only two timepoints need to transpose dataframes
if (ncol(timemean)==3){
  DO_diff_mean<-t(DO_diff_mean)
  DO_diff_median<-t(DO_diff_median)
  time_diff_mean<-t(time_diff_mean)
}

#Change names of columns to match jar labels
colnames(DO_diff_mean)<-DOmean$Jar
colnames(DO_diff_median)<-DOmedian$Jar
colnames(time_diff_mean)<-timemean$Jar

# If there is an extra column of data (i.e., last measurement was at the end of a light period);
# extra == 1, the last measurement will be NEP and there will not be a final ER or GPP calcualtion 
row.names(DO_diff_mean)<-paste0('Measure', seq(1, nrow(DO_diff_mean),1))

row.names(DO_diff_median)<-row.names(DO_diff_mean)
row.names(time_diff_mean)<-row.names(DO_diff_mean)

# ######################################
# Calculate metabolism for each jar/time
# Note that ER is negative
# ######################################

#Create empty dataframes
DOrate_mean<-as.data.frame(matrix(nrow=ncol(DO_diff_mean), ncol=nrow(DO_diff_mean)+1))
names(DOrate_mean)<-c('Jar', row.names(DO_diff_mean))
DOrate_mean[,1]<-colnames(DO_diff_mean)
DOrate_median <- DOrate_mean

# Calculate ER (dark) and NEP (light) (mg O2 per liter per hour)
row=1
for (row in 1:nrow(DOrate_mean)){
  timediff <- time_diff_mean[,row] #seconds
  
  DOdiff_mean <- DO_diff_mean[,row]
  DOdiff_mean_per_h <- DOdiff_mean/timediff*3600 #convert to hours
  DOrate_mean[row,2:ncol(DOrate_mean)]<- round(DOdiff_mean_per_h,3)
  
  DOdiff_median <- DO_diff_median[,row]
  DOdiff_median_per_h <- DOdiff_median/timediff*3600 #convert to hours
  DOrate_median[row,2:ncol(DOrate_median)]<- round(DOdiff_median_per_h,3)
}


#Link Treatments and Sites to Jars
if (ncol(DOrate_mean)==2){
  DOrate_mean$Total<-DOrate_mean$Measure1
  DOrate_median$Total<-DOrate_median$Measure1
} else if  (ncol(DOrate_mean)>2){
DOrate_mean$Total<-rowSums(DOrate_mean[,grep('Measure', colnames(DOrate_mean))])
DOrate_median$Total<-rowSums(DOrate_median[,grep('Measure', names(DOrate_median))])
}
DOrate_mean$Treatment<-factor(JarData$Treatment__1[match(DOrate_mean$Jar, JarData$Jar__1)])
DOrate_mean$Site<-JarData$Site__1[match(DOrate_mean$Jar, JarData$Jar__1)]

DOrate_mean$Metric<-NA
DOrate_mean$Metric[which(DOrate_mean$Treatment=='Light')]<-'NEP'
DOrate_mean$Metric[which(DOrate_mean$Treatment=='Dark')]<-'ER'



DOrate_median$Treatment<-JarData$Treatment__1[match(DOrate_median$Jar, JarData$Jar__1)]
DOrate_median$Site<-JarData$Site__1[match(DOrate_median$Jar, JarData$Jar__1)]

DOrate_median$Metric<-NA
DOrate_median$Metric[which(DOrate_median$Treatment=='Light')]<-'NEP'
DOrate_median$Metric[which(DOrate_median$Treatment=='Dark')]<-'ER'

return(DOrate_mean)
}

