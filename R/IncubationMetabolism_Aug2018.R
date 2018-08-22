
#Load libraies

library(readxl)
library(openxlsx)

library(plyr)
library(dplyr)
library(tidyr)

library(ggplot2)
library(gridExtra)
library(RColorBrewer)

library(lubridate)

#Function to read in all sheets from an excel file
read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}


# Project folder where outputs are stored
dropbox_dir<-'C:/Dropbox/USBR Delta Project'

#Where data come from
google_dir<-'C:/GoogleDrive/DeltaNutrientExperiment'

#Set date %m%d%y
Date<-'081618'



#Don't change below


files<-list.files(paste0(dropbox_dir, '/Data/Incubations/', Date))

goodfile<-files[startsWith(files, 'Delta')]

mysheets <- read_excel_allsheets(paste0(dropbox_dir, '/Data/Incubations/', Date, '/', goodfile))

str(mysheets)

#Jar Codes
JarData<-mysheets[[1]]
Jar<-rep(JarData$Jar__1, each=5)

#DO sheet
DOdata<-mysheets[[2]]

time_cols<-names(DOdata)[startsWith(names(DOdata), 'Date')]
DO_cols<-names(DOdata)[startsWith(names(DOdata), 'Oxygen')]

time_df<-DOdata[time_cols]
DO_df<-DOdata[DO_cols]

if (identical(dim(time_df), dim(DO_df))==FALSE){
  warning('Time and oxygen data are different dimensions. Check data file')
}



#Average multiple observations of the same jar/time

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
names(DO_diff_mean)<-DOmean$Jar

DO_diff_median<-as.data.frame(apply(DOmedian[,-1], 1, function(x) diff(x)))
names(DO_diff_median)<-DOmedian$Jar

#Convert to seconds
seconds_df<-sapply(timemean[,-1], seconds)

time_diff_mean<-as.data.frame(apply(seconds_df, 1, function(x) diff(x)))
names(time_diff_mean)<-timemean$Jar

row.names(DO_diff_mean)<-paste0(rep(c('NEP', 'ER'), nrow(DO_diff_mean)*.5), rep(1:(nrow(DO_diff_mean)*.5), each=2))
row.names(DO_diff_median)<-row.names(DO_diff_mean)
row.names(time_diff_mean)<-row.names(DO_diff_mean)



#Calculate metabolism for each jar/time
#Note that ER is negative
DOrate_mean<-as.data.frame(matrix(nrow=ncol(DO_diff_mean), ncol=nrow(DO_diff_mean)+1))
names(DOrate_mean)<-c('Jar', 'NEP1', 'ER1', 'NEP2', 'ER2', 'NEP3', 'ER3', 'NEP4', 'ER4')
DOrate_mean[,1]<-names(DO_diff_mean)
DOrate_median <- DOrate_mean


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

#Calculate GPP as the difference between daytime GPP and the ER from the following night
DOrate_mean$GPP1<-DOrate_mean$NEP1-DOrate_mean$ER1
DOrate_mean$GPP2<-DOrate_mean$NEP2-DOrate_mean$ER2
DOrate_mean$GPP3<-DOrate_mean$NEP3-DOrate_mean$ER3
DOrate_mean$GPP4<-DOrate_mean$NEP4-DOrate_mean$ER4

DOrate_median$GPP1<-DOrate_median$NEP1-DOrate_median$ER1
DOrate_median$GPP2<-DOrate_median$NEP2-DOrate_median$ER2
DOrate_median$GPP3<-DOrate_median$NEP3-DOrate_median$ER3
DOrate_median$GPP4<-DOrate_median$NEP4-DOrate_median$ER4

#Link Treatments and Sites to Jars
DOrate_mean$Treatment<-factor(JarData$Treatment__1[match(DOrate_mean$Jar, JarData$Jar__1)])
DOrate_mean$Site<-JarData$Site__1[match(DOrate_mean$Jar, JarData$Jar__1)]

DOrate_median$Treatment<-JarData$Treatment__1[match(DOrate_median$Jar, JarData$Jar__1)]
DOrate_median$Site<-JarData$Site__1[match(DOrate_median$Jar, JarData$Jar__1)]

#Format data for ggplot
DOrate_mean_long_table<- DOrate_mean %>%
  gather("Metric", "Value", 2:13)

DOrate_mean_long_table$Day<-as.numeric(gsub("[^0-9]", "", DOrate_mean_long_table$Metric) )
DOrate_mean_long_table$Metric<-gsub('[0-9]+', '', DOrate_mean_long_table$Metric)
DOrate_mean_long_table$SampleDate<-rep(mdy(Date), nrow(DOrate_mean_long_table))


DOrate_median_long_table<- DOrate_median %>%
  gather("Metric", "Value", 2:13)

DOrate_median_long_table$Day<-as.numeric(gsub("[^0-9]", "", DOrate_median_long_table$Metric) )
DOrate_median_long_table$Metric<-gsub('[0-9]+', '', DOrate_median_long_table$Metric)
DOrate_median_long_table$SampleDate<-rep(mdy(Date), nrow(DOrate_median_long_table))

#Metabolism estimates are in mg O2 per liter per hour
write.csv(DOrate_mean_long_table, file=paste0(dropbox_dir, '/Data/Incubations/MetabolismCalculations/JarMetabolism_', Date, '.csv'), row.names=F)



#Create tables of each metabolism metric
GPPtable<- DOrate_mean_long_table %>%
  filter(Metric=='GPP')
GPPrange<-range(GPPtable$Value, na.rm=T)

ERtable<- DOrate_mean_long_table %>%
  filter(Metric=='ER')
ERrange<-range(ERtable$Value, na.rm=T)

NEPtable<- DOrate_mean_long_table %>%
  filter(Metric=='NEP')
NEPrange<-range(NEPtable$Value, na.rm=T)

#Create tables of each metabolism metric (median)
GPPtable_median<- DOrate_median_long_table %>%
  filter(Metric=='GPP')
GPPrange_median<-range(GPPtable_median$Value, na.rm=T)

ERtable_median<- DOrate_median_long_table %>%
  filter(Metric=='ER')
ERrange_median<-range(ERtable_median$Value, na.rm=T)

NEPtable_median<- DOrate_median_long_table %>%
  filter(Metric=='NEP')
NEPrange_median<-range(NEPtable_median$Value, na.rm=T)


# #####################
# plotting
# #####################

#Plotting parameters
jitterwidth=0.15
colorset<-'Dark2'
colors<-brewer.pal(3, colorset)[c(1,3,2)]

#Common theme for all metabolism panels
commonTheme<-list(
  scale_colour_manual(values = colors),
  geom_jitter(size=2, width=jitterwidth, height=0),
  geom_smooth(method='lm', se=F),
  theme_bw(),
  theme(plot.title = element_text(hjust=0.5), legend.position="none")
)

GPP_site34 <- ggplot(GPPtable[GPPtable$Site=='NL34',], aes(Day, Value, colour=Treatment)) + 
  labs(x='Day', y='GPP') +
  ggtitle('Site 34') +
  commonTheme + 
  theme(plot.title = element_text(hjust=0.5), legend.position = c(0.25, 0.7), legend.background = element_rect(fill=NA)) 

GPP_site64 <-ggplot(GPPtable[GPPtable$Site=='NL64',], aes(Day, Value, colour=Treatment)) + 
  labs(x='Day', y='GPP') +
  ggtitle('Site 64') +
  commonTheme

GPP_site70 <- ggplot(GPPtable[GPPtable$Site=='NL70',], aes(Day, Value, colour=Treatment)) + 
  labs(x='Day', y='GPP') +
  ggtitle('Site 70') +
  commonTheme

GPP_site74 <- ggplot(GPPtable[GPPtable$Site=='NL74',], aes(Day, Value, colour=Treatment)) + 
  labs(x='Day', y='GPP') +
  ggtitle('Site 74') +
  commonTheme

#ER
ER_site34 <- ggplot(ERtable[ERtable$Site=='NL34',], aes(Day, Value, colour=Treatment)) + 
  labs(x='Day', y='ER') +
  ggtitle('Site 34') +
  commonTheme 

ER_site64 <-ggplot(ERtable[ERtable$Site=='NL64',], aes(Day, Value, colour=Treatment)) + 
  labs(x='Day', y='ER') +
  ggtitle('Site 64') +
  commonTheme

ER_site70 <- ggplot(ERtable[ERtable$Site=='NL70',], aes(Day, Value, colour=Treatment)) + 
  labs(x='Day', y='ER') +
  ggtitle('Site 70') +
  commonTheme

ER_site74 <- ggplot(ERtable[ERtable$Site=='NL74',], aes(Day, Value, colour=Treatment)) + 
  labs(x='Day', y='ER') +
  ggtitle('Site 74') +
  commonTheme

#NEP
NEP_site34 <- ggplot(NEPtable[NEPtable$Site=='NL34',], aes(Day, Value, colour=Treatment)) + 
  labs(x='Day', y='NEP') +
  ggtitle('Site 34') +
  commonTheme 

NEP_site64 <-ggplot(NEPtable[NEPtable$Site=='NL64',], aes(Day, Value, colour=Treatment)) + 
  labs(x='Day', y='NEP') +
  ggtitle('Site 64') +
  commonTheme

NEP_site70 <- ggplot(NEPtable[NEPtable$Site=='NL70',], aes(Day, Value, colour=Treatment)) + 
  labs(x='Day', y='NEP') +
  ggtitle('Site 70') +
  commonTheme

NEP_site74 <- ggplot(NEPtable[NEPtable$Site=='NL74',], aes(Day, Value, colour=Treatment)) + 
  labs(x='Day', y='NEP') +
  ggtitle('Site 74') +
  commonTheme



png(paste0(dropbox_dir, '/Figures/Incubations/', Date, 'Metabolism_Timeseries.png'), width=8, height=12, units='in', res=200)

grid.arrange(GPP_site34, GPP_site64, GPP_site70, GPP_site74, ER_site34, ER_site64, ER_site70, ER_site74, NEP_site34, NEP_site64, NEP_site70, NEP_site74 , ncol=3, as.table=F)

dev.off()


png(paste0(dropbox_dir, '/Figures/Incubations/', Date, 'Metabolism_WithinSampleSD_boxplot.png'), width=5, height=4, units='in', res=200)
par(mar=c(3,3,0.5,0.5))
par(mgp=c(3,0.5,0))
xlabels<-paste0('T', 1:(ncol(DOsd)-1))

boxplot(DOsd[2:10], names=xlabels, col=c(rep(c('grey50', 'darkgreen'),4), 'grey50'), boxwex=0.5, cex=0.8)
axis(1, at=1:9, line=1, labels=c(rep(c('AM', 'PM'),4), 'AM'), tick=F, lty=0)
mtext('Within measurement SD (mg O2/L)', 2, 2)
mtext('Timepoint', 1, 2.5)

dev.off()
