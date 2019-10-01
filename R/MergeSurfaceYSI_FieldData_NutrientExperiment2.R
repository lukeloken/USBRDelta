


#Code to extract surface water measurements from profile data at fixed sites

library(readxl)
library(plyr)
library(tidyr)
library(dplyr)
library(viridis)
library(lubridate)
library(ggplot2)
library(gridExtra)


source('R/read_excel_allsheets.R')
source('R/g_legend.R')

# # Project folder where outputs are stored
# dropbox_dir<-'C:/Dropbox/USBR Delta Project'
# 
# #Where data come from
# google_dir<-'C:/GoogleDrive/DeltaNutrientExperiment'


#Field notes
site_df<-readRDS(file=paste0(dropbox_dir, '/Data/Rdata_SSCN2/SSCN2_FieldData.rds'))
site_df_withFlame<- readRDS(file=paste0(dropbox_dir, '/Data/Rdata_SSCN2/FlameSiteData.rds'))

#Water chemistry
# nutrient_df<-read.csv(file=paste0(dropbox_dir, '/Data/NutrientExperiment/NutrientData.csv'), sep=',', header=T, stringsAsFactors = F)
# nutrient_df$Date<-as.Date(nutrient_df$Date)
# nutrient_df<-nutrient_df[which(nutrient_df$Site %in% sitetable$site1),]
# nutrient_surface<-nutrient_df[grep('_S', nutrient_df$SampleLabel),]
# nutrient_surface<-dplyr::select(nutrient_surface, -Turbidity, -EC, -PH, -Lab..) 

#YSI data
YSI_ThreeDepths <- readRDS(file=paste0(dropbox_dir, '/Data/Rdata_SSCN2/YSI_ThreeDepths.rds'))

YSI_surf<- YSI_ThreeDepths %>%
  dplyr::filter(DepthStrata=="lessthan1.5m") %>%
  group_by() %>%
  mutate(Site = factor(Site, levels(site_df$Site))) %>%
  select(-DepthStrata) %>%
  arrange(Date, Site) %>%
  dplyr::rename(DateTime_UTC = DateTime)

names(YSI_surf)[-which(names(YSI_surf) %in% c("Site", "Date", "DateTime_UTC", "AirPressure_mmHg"))] <- paste0(
  "YSISurf_", names(YSI_surf)[-which(names(YSI_surf) %in% c("Site", "Date", "DateTime_UTC", "AirPressure_mmHg"))]
)


  

# incubation metabolism results
# summary_df<-read.csv(file=paste0(dropbox_dir, '/Data/NutrientExperiment/IncubationMetabolism/', 'LightDarkMetabolism.csv'), header=T, stringsAsFactors = F)
# str(summary_df)
# summary_df$Date<-as.Date(summary_df$Date)
# 
# incubation_df<-summary_df %>%
#   dplyr::select(Date, Site, Metric, mean) %>%
#   spread(key=Metric, value=mean) %>%
#   mutate(ER=ER*24, 
#          NEP=NEP*24, 
#          GPP=NEP-ER)

#Light profiles
# kd_alldates<-readRDS(file=paste0(dropbox_dir, '/Data/Rdata/kd_alldates.rds'))



#Merge everything together

# merge1<-full_join(site_df, YSI_surf)
merge1<-full_join(site_df_withFlame, YSI_surf)


#Field notes
# merge3<-full_join(merge2, field_df_withFlame)
# merge4<-dplyr::select(merge3, -Date.1)

#Nutrient data
# merge5<-full_join(merge4, nutrient_surface)

#Light Profiles
# merge6<-full_join(merge5, kd_alldates)

#Merge water chemistry data
merge_df<-merge1

saveRDS(merge_df, file=paste0(dropbox_dir, '/Data/Rdata_SSCN2/SiteData_Merged.rds'))
write.csv(merge_df, file=paste0(google_dir, '/SSCN2_DataOutputs/SiteData_Merged.csv'))

