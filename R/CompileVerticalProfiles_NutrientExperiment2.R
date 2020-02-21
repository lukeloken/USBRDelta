
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
library(rLakeAnalyzer)
library(LakeMetabolizer)
library(wql)


#Hypso curve
Depth_df <- readRDS(file=file.path(onedrive_dir, 'RData', 'HypsoCurveNL74.rds'))

# Find all filenames in directory
# These will be used to loop through all old data


ysi_directory<-file.path(onedrive_dir, "RData", "NutrientExperiment2", "YSIProfiles")

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

#Calculate mgL saturation because sensor did not log it first few days
YSI_AllDepths$DO_mgL <- o2.at.sat.base(YSI_AllDepths$Temp_C)*  YSI_AllDepths$DO_perSat /100


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


write.csv(YSI_AllDepths, file=file.path(onedrive_dir, 'OutputData', 'NutrientExperiment2', 'YSIProfiles_AllDepths.csv'), row.names=F)
saveRDS(YSI_AllDepths , file=file.path(onedrive_dir, 'RData', 'NutrientExperiment2', 'YSIProfiles_AllDepths.rds'))

write.csv(YSI_ThreeDepths, file=file.path(onedrive_dir, 'OutputData', 'NutrientExperiment2', 'YSIProfiles_ThreeDepths.csv'), row.names=F)
saveRDS(YSI_ThreeDepths , file=file.path(onedrive_dir, 'RData', 'NutrientExperiment2', 'YSIProfiles_ThreeDepths.rds'))

#Calculate volume weighted averages and Schmidt Stability

avg_vars<-names(YSI_AllDepths)[-which(names(YSI_AllDepths) %in% c('Date', 'Site', "AirPressure_mmHg", "Depth_m", "DateTime"))]

YSI_dates<-unique(YSI_AllDepths$Date)
day=1
avg_list<-list()
for (day in 1:length(YSI_dates)){
  YSI_i<-YSI_AllDepths[YSI_AllDepths$Date == YSI_dates[day],]
  Sites_i <- unique(YSI_i$Site)
  
  avg_df <- YSI_i %>%
    group_by(Date, Site) %>%
    dplyr::select(-Depth_m) %>%
    summarize_all(mean)
  
  avg_df2 <- avg_df
  avg_df2$Schmidt_Jm2 <- rep(NA, nrow(avg_df2))
  site=1
  for (site in 1:length(Sites_i)){
    YSI_i_j<-YSI_i[YSI_i$Site == Sites_i[site],]
    Depth_closest<-sapply(Depth_df$Depth_m, function (x) which.min(abs(YSI_i_j$Depth_m-x)))
    
    YSI_everydepth<-YSI_i_j[Depth_closest,avg_vars]
    
    Depth_df2<-data.frame(Depth_df, Depth_closest, YSI_everydepth)
    
    var=avg_vars[1]
    
    var_avg <- sapply(avg_vars, function (y) sum(Depth_df2[y]*Depth_df2$Volume_percent))

    avg_df2[site,avg_vars]<-var_avg
    
    #Schmidt Stability
    Salinity <-ec2pss(YSI_i_j$SPC_uScm/1000, YSI_i_j$Temp_C, p=0)
    
    Schmidt<-schmidt.stability(wtr=YSI_i_j$Temp_C, depths=YSI_i_j$Depth_m, bthA = Depth_df$Area_m2, bthD = Depth_df$Depth_m, sal=Salinity)
    avg_df2$Schmidt_Jm2[site] <- Schmidt
  }
  
  avg_list[[day]] <- avg_df2
  
}
YSI_avg_out <- ldply(avg_list, data.frame)

write.csv(YSI_avg_out, file=paste0(google_dir, '/SSCN2_DataOutputs/YSISSCN2_VerticalAverage.csv'), row.names=F)
saveRDS(YSI_avg_out , file=paste0(dropbox_dir, '/Data/Rdata_SSCN2/YSI_VerticalAverage.rds'))


write.csv(YSI_avg_out, file=file.path(onedrive_dir, 'OutputData', 'NutrientExperiment2', 'YSI_VerticalAverage.csv'), row.names=F)
saveRDS(YSI_avg_out , file=file.path(onedrive_dir, 'RData', 'NutrientExperiment2', 'YSI_VerticalAverage.rds'))


rm(YSI_deep, YSI_mid, YSI_surf, filename, filenames, ysi_directory, avg_list, avg_df2, site,avg_vars, var_avg, Depth_df, Depth_df2, YSI_everydepth,YSI_i_j,Depth_closest, YSI_i, avg_df, Schmidt, day, Salinity, Sites_i, var, YSI_dates)


#View data to see if it makes sense
# ggplot(YSI_ThreeDepths, aes(Date, Depth_m, group=DepthStrata, colour=DepthStrata)) +
#   geom_point() +
#   geom_line() +
#   facet_wrap(~Site)

