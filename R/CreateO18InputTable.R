

#Create table for oxygen 18 metabolism estimation

library(viridis)
library(ggplot2)
library(gridExtra)
library(plyr)
library(dplyr)
library(tidyr)
library(lubridate)
library(RColorBrewer)
library(RcppRoll)

# source('R/CompilePhytos.R')

# Project folder where outputs are stored
dropbox_dir<-'C:/Dropbox/USBR Delta Project'

#Where data come from
google_dir<-'C:/GoogleDrive/DeltaNutrientExperiment'


#Met data
weather_df<-read.csv(paste(dropbox_dir, 'Data', 'NutrientExperiment', 'Oxygen18', 'SacramentoWeather_Oct2018.csv', sep='/'), stringsAsFactors = F)
summary(weather_df)
weather_df$Date<-as.Date(weather_df$DATE, format='%m/%d/%Y')

wind_df<- weather_df %>% 
  select(Date, LATITUDE, LONGITUDE, STATION, ELEVATION, AWND, WDF2, WDF5) %>% 
  tidyr::drop_na(AWND) 
  # dplyr::filter(STATION == 'USW00093225')

wind_speed <- wind_df %>%
  select(Date, STATION, AWND) %>%
  spread(key=STATION, value=AWND)

wind_direction5 <- wind_df %>%
  select(Date, STATION, WDF5) %>%
  spread(key=STATION, value=WDF5)

wind_direction2 <- wind_df %>%
  select(Date, STATION, WDF2) %>%
  spread(key=STATION, value=WDF2)

wind_avg<- wind_df %>%
  select(-LATITUDE, -LONGITUDE, -ELEVATION, -STATION) %>%
  group_by(Date) %>% 
  summarize_all(mean)

summary(wind_df)  

# ggplot(wind_df, aes(Date, AWND, group=STATION, color=STATION)) +
#   geom_point(size=2) + 
#   geom_line()
# 
# ggplot(wind_df, aes(Date, WDF5, group=STATION, color=STATION)) +
#   geom_point(size=2) + 
#   geom_line()
# 
# ggplot(wind_speed, aes(x=USW00023232, y=USW00093225)) +
#   geom_point(size=2) + 
#   labs(y='Wind speed SMF', x='Wind speed Sac Exe Airport') + 
#   geom_abline(intercept=0,slope=1)
#   
# ggplot(wind_direction5, aes(x=USW00023232, y=USW00093225)) +
#   geom_point(size=2) +
#   labs(y='Wind dir SMF', x='Wind dir Sac Exe Airport') +
#   geom_abline(intercept=0,slope=1) +
#   scale_x_continuous(limits=c(0,360)) +
#   scale_y_continuous(limits=c(0,360))
# 
# ggplot(wind_direction2, aes(x=USW00023232, y=USW00093225)) +
#   geom_point(size=2) + 
#   labs(y='Wind dir SMF', x='Wind dir Sac Exe Airport') + 
#   geom_abline(intercept=0,slope=1) + 
#   scale_x_continuous(limits=c(0,360)) + 
#   scale_y_continuous(limits=c(0,360))


#Calculate 3 day means and standard deviations
wind_avg$wind.ms<-roll_mean(wind_avg$AWND, n=3, fill=NA, align='right')
wind_avg$sd.wind.ms<-roll_sd(wind_avg$AWND, n=3, fill=NA, align='right')

#assume a wind height of 10m (completely made up. Should check what the wind height is at the airports)
wind_avg$wind.height.ms=10

wind_avg

merge_df <- read.csv(file=paste0(dropbox_dir, '/Data/NutrientExperiment/SurfaceChemistry/YSIMetabolismSurface.csv'), stringsAsFactors = F)
merge_df$Date<-as.Date(merge_df$Date)
merge_df$Site<-factor(merge_df$Site, c('NL70', 'EC2','EC3','EC4','EC5','EC6','EC7','EC8','NL76'))

merge_df_O18<-full_join(merge_df, wind_avg[c('Date', 'wind.ms', 'sd.wind.ms', 'wind.height.ms')])

waterdepths<- merge_df %>%
  group_by(Site) %>%
  summarize_at("TotalDepth", mean, na.rm=T)

waterO18table<-data.frame(Site = c('NL70', 'EC2','EC3','EC4','EC5','EC6','EC7','EC8','NL76'), delo18.h2o=(-1)*c(6.73, 6.1, 6.01, 6.01, 6.01, 6.01, 6.01, 5.95, 5.83))

merge_df_O18<-drop_na(merge_df_O18, d180_02.vs.VSMOW)

merge_df_O18$zmix.m<- waterdepths$TotalDepth[match(merge_df_O18$Site, waterdepths$Site)]/3.28
merge_df_O18$delo18.h2o<-waterO18table$delo18.h2o[match(merge_df_O18$Site, waterO18table$Site)]


write.csv(merge_df_O18, file=paste0(dropbox_dir, '/Data/NutrientExperiment/Oxygen18/InputTermsO18Metabolism_SSCN_Oct2018.csv'), row.names=F)

