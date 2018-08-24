
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


source('R/read_excel_allsheets.R')
source('R/g_legend.R')

# Project folder where outputs are stored
dropbox_dir<-'C:/Dropbox/USBR Delta Project'

#Where data come from
google_dir<-'C:/GoogleDrive/DeltaNutrientExperiment'

#Set date %m%d%y
Date<-'081618'
# Date<-'071218'
# Date<-'050718'

# ##############################################################################################
# Don't change below
# Code looks into the dropbox directory/incubation data and loads the correct file using 'Date'
# ##############################################################################################

files<-list.files(paste0(dropbox_dir, '/Data/Incubations/', Date))

goodfile<-files[startsWith(files, 'Delta')]

mysheets <- read_excel_allsheets(paste0(dropbox_dir, '/Data/Incubations/', Date, '/', goodfile))

#Jar sheet
JarData<-mysheets[[1]]

#DO sheet
DOdata<-mysheets[[2]]

time_cols<-names(DOdata)[startsWith(names(DOdata), 'Date')]
DO_cols<-names(DOdata)[startsWith(names(DOdata), 'Oxygen')]

time_df<-DOdata[time_cols]
DO_df<-DOdata[DO_cols]

if (identical(dim(time_df), dim(DO_df))==FALSE){
  warning('Time and oxygen data are different dimensions. Check data file')
}

if (ncol(time_df) %% 2 == 0){
  extra<-1
  warning('Even number of timepoints. There should be an odd number')
} else { extra=0}

#Remove NAs from Jar vector and generate a vector of the Jar reads
JarSampleOrder<-JarData$Jar__1[is.finite(JarData$Jar__1)]
Jar<-rep(JarSampleOrder, each=nrow(DO_df)/length(JarSampleOrder))


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
DOrate_mean$Treatment<-factor(JarData$Treatment__1[match(DOrate_mean$Jar, JarData$Jar__1)])
DOrate_mean$Site<-JarData$Site__1[match(DOrate_mean$Jar, JarData$Jar__1)]

DOrate_median$Treatment<-JarData$Treatment__1[match(DOrate_median$Jar, JarData$Jar__1)]
DOrate_median$Site<-JarData$Site__1[match(DOrate_median$Jar, JarData$Jar__1)]

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

# Save to directory listed at the start
# Metabolism estimates are in mg O2 per liter per hour
write.csv(DOrate_mean_long_table, file=paste0(dropbox_dir, '/Data/Incubations/MetabolismCalculations/JarMetabolism_', Date, '.csv'), row.names=F)

# #####################
# Plotting
# #####################


# ##############################################
# multi panel of metabolism estimates over time 
# Each panel is a site/metric
# X is day, y is value, color is treatment
# #############################################


#Plotting parameters
jitterwidth=0.15
colorset<-'Dark2'
colors<-brewer.pal(3, colorset)[c(1,3,2)]

se.colorset<-'Set2'
se.colors<-brewer.pal(3, se.colorset)[c(1,3,2)]

#Common theme for all metabolism timeseries panels
commonTheme<-list(
  scale_colour_manual(values = colors),
  scale_fill_manual(values = colors),
  # geom_smooth(method='loess',  se=F),
  geom_smooth(method='auto', se=T, alpha=.2),
  geom_jitter(size=2, width=jitterwidth, height=0),
  theme_bw(),
  theme(plot.title = element_text(hjust=0.5), legend.position="none")
)

# Make a table to determine how many panels are needed
# Each panel is a site and a metric
uniquetable<-unique(DOrate_mean_long_table[c('Metric', 'Site')])
uniquetable$Metric<-factor(uniquetable$Metric, c('GPP', 'ER', 'NEP'))
uniquetable<-uniquetable[order(uniquetable$Site),]
uniquetable<-uniquetable[order(uniquetable$Metric),]

ranges<-sapply(uniquetable$Metric, function(x) extendrange(DOrate_mean_long_table$Value[DOrate_mean_long_table$Metric ==x], f=0.05))

# Loop through metrics and sites and make a gg object
plot_list<-list()
plot_nu<-1
for (plot_nu in 1:nrow(uniquetable)){
  
  site<-uniquetable$Site[plot_nu]
  metric<-uniquetable$Metric[plot_nu]
  
  table<-DOrate_mean_long_table[DOrate_mean_long_table$Metric==metric & 
                                  DOrate_mean_long_table$Site==site,]

  plot_list[[plot_nu]] <- ggplot(table, aes(Day, Value, colour=Treatment, fill=Treatment)) + 
  labs(x='Day', y=metric) +
  ggtitle(site) +
  # ylim(ranges[1,plot_nu], ranges[2,plot_nu]) +
  # scale_y_continuous(limits=ranges[,plot_nu]) +
  coord_cartesian(ylim=ranges[,plot_nu]) + 
  commonTheme 
  
}

#Add and extract legend from first plot
plot_withlegend <- plot_list[[1]] + 
  theme(legend.position='bottom')

mylegend<-g_legend(plot_withlegend)

# p_cols1<-grid.arrange(grobs=plot_list[c(1,4,7,11)], ncol=1, as.table=F, top = "GPP")

# arrange plots without legend
p2<-grid.arrange(grobs=plot_list, ncol=length(unique(uniquetable$Metric)), as.table=F)

#Add legend to bottom of figure and save
png(paste0(dropbox_dir, '/Figures/Incubations/', Date, 'Metabolism_Timeseries.png'), width=8, height=12, units='in', res=200)

grid.arrange(p2, mylegend, nrow=2,heights=c(10, length(unique(uniquetable$Site))/16))

dev.off()


# ##########################################
# Boxplot of within jar standard deviations
# ##########################################

png(paste0(dropbox_dir, '/Figures/Incubations/', Date, 'Metabolism_WithinSampleSD_boxplot.png'), width=5, height=4, units='in', res=200)
par(mar=c(2.5,3,0.5,0.5))
par(mgp=c(3,0.5,0))
xlabels<-paste0('T', 1:(ncol(DOsd)-1))

boxplot(DOsd[2:ncol(DOsd)], names=xlabels, col=c(rep(c('grey50', 'darkgreen'),4), 'grey50'), boxwex=0.5, cex=0.8)
# axis(1, at=1:9, line=1, labels=c(rep(c('AM', 'PM'),4), 'AM'), tick=F, lty=0)
mtext('Within measurement SD (mg O2/L)', 2, 2)
mtext('Timepoint', 1, 1.5)
legend('topleft', inset=0.02, bty='n', pt.bg=c('grey50', 'darkgreen'), c('AM', 'PM'), pt.cex=3, pch=22, cex=1, y.intersp=2)

dev.off()



# ##########################################
# Boxplot of treatment effect
# ##########################################

# 
# uniquetable<-unique(DOrate_mean_long_table[c('Metric', 'Site')])
# uniquetable$Metric<-factor(uniquetable$Metric, c('GPP', 'ER', 'NEP'))
# uniquetable<-uniquetable[order(uniquetable$Site),]
# uniquetable<-uniquetable[order(uniquetable$Metric),]
# 
# boxplot(DOsd[2:10], names=xlabels, col=c(rep(c('grey50', 'darkgreen'),4), 'grey50'), boxwex=0.5, cex=0.8)
# 
# table<-DOrate_mean_long_table[DOrate_mean_long_table$Metric==metric & 
#                                 DOrate_mean_long_table$Site==site,]
# 
# 
# #Create tables of each metabolism metric
# GPPtable<- DOrate_mean_long_table %>%
#   filter(Metric=='GPP')
# GPPrange<-range(GPPtable$Value, na.rm=T)
# 
# ERtable<- DOrate_mean_long_table %>%
#   filter(Metric=='ER')
# ERrange<-range(ERtable$Value, na.rm=T)
# 
# NEPtable<- DOrate_mean_long_table %>%
#   filter(Metric=='NEP')
# NEPrange<-range(NEPtable$Value, na.rm=T)
# 
# #Create tables of each metabolism metric (median)
# GPPtable_median<- DOrate_median_long_table %>%
#   filter(Metric=='GPP')
# GPPrange_median<-range(GPPtable_median$Value, na.rm=T)
# 
# ERtable_median<- DOrate_median_long_table %>%
#   filter(Metric=='ER')
# ERrange_median<-range(ERtable_median$Value, na.rm=T)
# 
# NEPtable_median<- DOrate_median_long_table %>%
#   filter(Metric=='NEP')
# NEPrange_median<-range(NEPtable_median$Value, na.rm=T)
# 
# 
# #Plotting parameters
# colorset<-'Dark2'
# colors<-brewer.pal(3, colorset)[c(1,3,2)]



uniquemetrics<-unique(DOrate_mean_long_table[c('Metric')])
uniquemetrics<-factor(uniquemetrics[,1], c('GPP', 'ER', 'NEP'))
uniquemetrics<-uniquemetrics[order(uniquemetrics)]


box_list<-list()
plot_nu<-1
for (plot_nu in 1:length(uniquemetrics)){
  
  metric<-uniquemetrics[plot_nu]
  box_table<-DOrate_mean_long_table[DOrate_mean_long_table$Metric==metric,]
  

#Common theme for all metabolism panels
commonTheme_boxplot<-list(
  scale_fill_manual(values = colors),
  scale_colour_manual(values = colors),
  theme_bw(),
  theme(plot.title = element_text(hjust=0.5), legend.position="none"),
  geom_boxplot(outlier.size=0.5)
)

box_list[[plot_nu]] <- ggplot(aes(y = Value, x = Site, fill = Treatment), data = box_table) + 
  labs(x='Site', y=metric) +
  commonTheme_boxplot
}


#Add and extract legend from first plot
box_withlegend <- box_list[[1]] + 
  theme(legend.position='bottom') 

mylegend_box<-g_legend(box_withlegend)


# arrange plots without legend
p2_box<-grid.arrange(grobs=box_list, ncol=3, as.table=F)



#Add legend to bottom of figure and save
png(paste0(dropbox_dir, '/Figures/Incubations/', Date, 'Metabolism_Boxplot.png'), width=8, height=3, units='in', res=200)

grid.arrange(p2_box, mylegend_box, nrow=2,heights=c(10, 1))

dev.off()

