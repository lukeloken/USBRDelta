


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


ysi_directory<-paste0(dropbox_dir, "/Data/Rdata_SSCN2/YSIProfiles")

filenames<-list.files(ysi_directory)

filename<-filenames[1]

rm(YSI_AllDepths)
for (filename in filenames){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("YSI_AllDepths")){
    YSI_AllDepths <- readRDS(paste(ysi_directory, filename, sep='/')) %>%
      dplyr::select(-Time)
  } else {
  # if the merged dataset does exist, append to it
    temp_dataset <-readRDS(paste(ysi_directory, filename, sep='/')) %>%
      dplyr::select(-Time)
    YSI_AllDepths<- bind_rows(YSI_AllDepths, temp_dataset)
    # YSI_AllDepths<-smartbind(YSI_AllDepths, temp_dataset)
    rm(temp_dataset)
  }
  
}

row.names(YSI_AllDepths)<-NULL

YSI_AllDepths<-YSI_AllDepths[which(!is.na(YSI_AllDepths$Date)),] %>%
  drop_na(Date, Site, Depth_m) %>%
  arrange(Date, Site) %>%
  mutate(Site = gsub("\\s*\\([^\\)]+\\)","",as.character(Site)))


YSI_surf <- YSI_AllDepths %>%
  subset(Depth_m<1.5) %>%
  group_by(Site, Date) %>% 
  summarize_all(mean, na.rm=T) %>%
  mutate(DepthStrata = 'lessthan1.5m')


YSI_mid <- YSI_AllDepths %>%
  subset(Depth_m>3 & Depth_m<5) %>%
  group_by(Site, Date) %>% 
  summarize_all(mean, na.rm=T) %>%
  mutate(DepthStrata = '3to5m')


YSI_deep <- YSI_AllDepths %>%
  subset(Depth_m>6) %>%
  group_by(Site, Date) %>% 
  summarize_all(mean, na.rm=T) %>%
  mutate(DepthStrata = 'morethan6m')


YSI_ThreeDepths<-bind_rows(YSI_surf, YSI_mid, YSI_deep) %>%
  dplyr::arrange(Date, Site, Depth_m)


write.csv(YSI_AllDepths, file=paste0(google_dir, '/SSCN2_DataOutputs/YSISSNC2Sites_AllDepths.csv'), row.names=F)
saveRDS(YSI_AllDepths , file=paste0(dropbox_dir, '/Data/Rdata_SSCN2/YSI_AllDepths.rds'))

write.csv(YSI_ThreeDepths, file=paste0(google_dir, '/SSCN2_DataOutputs/YSISSCN2Sites_ThreeDepths.csv'), row.names=F)
saveRDS(YSI_ThreeDepths , file=paste0(dropbox_dir, '/Data/Rdata_SSCN2/YSI_ThreeDepths.rds'))


rm(YSI_deep, YSI_mid, YSI_surf, filename, filenames, ysi_directory)


#View data to see if it makes sense
# ggplot(YSI_ThreeDepths, aes(Date, Depth_m, group=DepthStrata, colour=DepthStrata)) +
#   geom_point() +
#   geom_line() +
#   facet_wrap(~Site)

