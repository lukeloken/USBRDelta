


library(readr) # read in data
library(dplyr,quietly = T) # clean data
library(tidyr) # clean data
library(rLakeAnalyzer) # lake analyses
library(lubridate) # working with time
library(LakeMetabolizer) # lake analyses
library(RcppRoll) # Rolling operations

source('R/g_legend.R')

# Project folder where outputs are stored
dropbox_dir<-'C:/Dropbox/USBR Delta Project'

#Where data come from
google_dir<-'C:/GoogleDrive/DeltaNutrientExperiment'

zone<-c('NL76', rep('upper', 2), rep('middle', 4), rep('lower', 4), 'NL70')
zone_def<-data.frame(buoy_numbers, zone)

metab.df <- read.csv(file=paste0(dropbox_dir, '/Data/NutrientExperiment/Buoys/MetabolismEstimates_v1.csv'))

metab.df$date<-as.Date(metab.df$date)
metab.df$zone<-zone_def$zone[match(metab.df$Buoy, zone_def$buoy_numbers)]
metab.df$zone<-factor(metab.df$zone, unique(zone))

metab.summary<- metab.df %>%
  dplyr::select(date, NEP, GPP, R, zone) %>%
  drop_na(NEP) %>%
  group_by(date, zone) %>%
  summarize_all(mean)

ggplot(aes(x=date, y=NEP, group=zone, colour=zone), data=metab.summary) +
  geom_path() + 
  geom_point() + 
  theme_bw()


write.table(metab.summary, file=paste0(dropbox_dir, '/Data/NutrientExperiment/Buoys/MetabolismSummaries_v1.csv'), row.names=F, sep=',')


