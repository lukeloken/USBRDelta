


# Code to extract surface, middle, and deep water measurements from profile data at fixed sites
# Export 4 files, all data merged, surface, middle, and deep

library(readxl)
library(plyr)
library(dplyr)
library(tidyr)
library(viridis)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(gtools)

# source('R/read_excel_allsheets.R')
# source('R/g_legend.R')
# 
# # Project folder where outputs are stored
# dropbox_dir<-'C:/Dropbox/USBR Delta Project'
# 
# #Where data come from
# google_dir<-'C:/GoogleDrive/DeltaNutrientExperiment'


# Find all filenames in directory
# These will be used to loop through all old data

ysi_directory<-file.path(onedrive_dir, "RData", "MonthlyCruises", "YSIVerticalProfiles")

filenames<-list.files(ysi_directory)

filename<-filenames[1]

rm(YSI_AllDepths)
for (filename in filenames){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("YSI_AllDepths")){
    YSI_AllDepths <- readRDS(paste(ysi_directory, filename, sep='/'))
  } else {
  # if the merged dataset does exist, append to it
    temp_dataset <-readRDS(paste(ysi_directory, filename, sep='/'))
    YSI_AllDepths<-smartbind(YSI_AllDepths, temp_dataset)
    rm(temp_dataset)
  }
  
}

row.names(YSI_AllDepths)<-NULL
YSI_AllDepths$DateTime.PT<-as.POSIXct(YSI_AllDepths$DateTime.PT, tz='America/Los_Angeles')
YSI_AllDepths$Date<-as.Date(YSI_AllDepths$DateTime.PT, tz='America/Los_Angeles')

YSI_AllDepths<-YSI_AllDepths[which(!is.na(YSI_AllDepths$Date)),]

YSI_AllDepths <- YSI_AllDepths %>%
  drop_na(Date, Station, Depth.feet) %>%
  arrange(Date, Station)


YSI_surf <- YSI_AllDepths %>%
  subset(Depth.feet<3) %>%
  group_by(Station, Date) %>% 
  summarize_all(mean, na.rm=T) %>%
  mutate(DepthStrata = 'lessthan3')


YSI_mid <- YSI_AllDepths %>%
  subset(Depth.feet>8 & Depth.feet<12) %>%
  group_by(Station, Date) %>% 
  summarize_all(mean, na.rm=T) %>%
  mutate(DepthStrata = '8to10')


YSI_deep <- YSI_AllDepths %>%
  subset(Depth.feet>20) %>%
  group_by(Station, Date) %>% 
  summarize_all(mean, na.rm=T) %>%
  mutate(DepthStrata = 'morethan20')


YSI_ThreeDepths<-bind_rows(YSI_surf, YSI_mid, YSI_deep) %>%
  dplyr::arrange(Date, Station, Depth.feet)


write.csv(YSI_AllDepths, file=paste0(google_dir, '/DataOutputs/YSILongTermSites_AllDepths.csv'), row.names=F)
saveRDS(YSI_AllDepths , file=paste0(dropbox_dir, '/Data/Rdata/YSI_AllDepths.rds'))

write.csv(YSI_ThreeDepths, file=paste0(google_dir, '/DataOutputs/YSILongTermSites_ThreeDepths.csv'), row.names=F)
saveRDS(YSI_ThreeDepths , file=paste0(dropbox_dir, '/Data/Rdata/YSI_ThreeDepths.rds'))


write.table(YSI_AllDepths , file=file.path(onedrive_dir, 'OutputData', 'MonthlyCruises', 'YSILongTermSites_AllDepths.csv'), row.names=F)
saveRDS(YSI_AllDepths , file=file.path(onedrive_dir, 'RData', 'MonthlyCruises', 'YSI_AllDepths.rds'))

write.table(YSI_ThreeDepths , file=file.path(onedrive_dir, 'OutputData', 'MonthlyCruises', 'YSILongTermSites_ThreeDepths.csv'), row.names=F)
saveRDS(YSI_ThreeDepths , file=file.path(onedrive_dir, 'RData', 'MonthlyCruises', 'YSI_ThreeDepths.rds'))



rm(YSI_deep, YSI_mid, YSI_surf, filename, filenames, ysi_directory)


#View data to see if it makes sense
# ggplot(YSI_ThreeDepths, aes(Date, Depth.feet, group=DepthStrata, colour=DepthStrata)) +
#   geom_point() +
#   geom_line() +
#   facet_wrap(~Station)

