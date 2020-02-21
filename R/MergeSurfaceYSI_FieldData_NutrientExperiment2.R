


#Code to extract surface water measurements from profile data at fixed sites

library(readxl)
library(plyr)
library(tidyr)
library(dplyr)
library(viridis)
library(lubridate)
library(ggplot2)
library(gridExtra)


#Field notes
site_df<-readRDS(file=file.path(onedrive_dir, 'RData', 'NutrientExperiment2', 'SSCN2_FieldData.rds'))
site_df_withFlame<- readRDS(file=file.path(onedrive_dir, 'RData', 'NutrientExperiment2', 'FlameSiteData.rds'))

site_df_deep<-site_df_withFlame %>%
  mutate(DepthCode= "D") %>%
  dplyr::select(1:27) %>%
  dplyr::select(-c(11:12, 15:18, 20:23)) %>%
  mutate(DepthCode="D")

site_df_twodepths<- site_df_withFlame %>% mutate(DepthCode = "S") %>%
  bind_rows(site_df_deep) %>%
  mutate(SampleCode = paste(SampleCode, DepthCode, sep="_"))


#Water chemistry
full_chem_df <- readRDS(file.path(onedrive_dir, 'RData', 'NutrientExperiment2', 'AllWaterChemistry.rds')) %>%
  mutate(Site = factor(Site, levels(site_df_withFlame$Site))) %>%
  drop_na(SampleCode) %>%
  dplyr::filter(Date<as.Date("2019-08-27"))


#YSI data
YSI_ThreeDepths <- readRDS(file.path(onedrive_dir, 'RData', 'NutrientExperiment2', 'YSIProfiles_ThreeDepths.rds'))

names(YSI_ThreeDepths)[-which(names(YSI_ThreeDepths) %in% c("Site", "Date", "DateTime", "AirPressure_mmHg", "DepthStrata"))] <- paste0(
  "YSI_", names(YSI_ThreeDepths)[-which(names(YSI_ThreeDepths) %in% c("Site", "Date", "DateTime", "AirPressure_mmHg", "DepthStrata"))]
)

YSI_surf<- YSI_ThreeDepths %>%
  dplyr::filter(DepthStrata=="lessthan1.5m") %>%
  dplyr::group_by() %>%
  mutate(Site = factor(Site, levels(site_df$Site)), 
         DepthCode = "S") %>%
  dplyr::select(-DepthStrata) %>%
  arrange(Date, Site) %>%
  dplyr::rename(DateTime_UTC = DateTime)


YSI_deep<- YSI_ThreeDepths %>%
  dplyr::filter(DepthStrata=="morethan6m") %>%
  group_by() %>%
  mutate(Site = factor(Site, levels(site_df$Site)), 
         DepthCode = "D") %>%
  dplyr::select(-DepthStrata) %>%
  arrange(Date, Site) %>%
  dplyr::rename(DateTime_UTC = DateTime)

YSI_TwoDepths<- bind_rows(YSI_surf, YSI_deep)

#Vertically weighted average
YSI_avg_out <- readRDS(file.path(onedrive_dir, 'RData', 'NutrientExperiment2', 'YSI_VerticalAverage.rds'))

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
kd_alldates<-readRDS(file.path(onedrive_dir, 'RData', 'NutrientExperiment2', 'SSCN2_Kd.rds'))
str(kd_alldates)


#O18 results
O18_files<-list.files(file.path(onedrive_dir, 'RawData', 'NutrientExperiment2', 'Oxygen18', 'Results'))

O18_list<-lapply(file.path(onedrive_dir, 'RawData', 'NutrientExperiment2', 'Oxygen18', 'Results', O18_files), read.csv, header=T)
O18_df <- ldply(O18_list, data.frame) %>%
  dplyr::select(Group.1, d180_02.vs.air, d180_02.vs.VSMOW, d15N_N2.vs.air, d13C.CO2, d13C.TotalDIC) %>%
  rename(SampleCode = Group.1) %>%
  mutate(SampleCode = gsub('_a', '', SampleCode)) %>%
  mutate(SampleCode = gsub('_b', '', SampleCode)) %>%
  mutate(SampleCode = gsub('_c', '', SampleCode)) %>%
  mutate(SampleCode = gsub('_d', '', SampleCode)) %>%
  group_by(SampleCode) %>%
  summarize_all(mean)

#Water Isotopes
H2O_18<- read_excel_allsheets(file.path(onedrive_dir, 'RawData', 'NutrientExperiment2', 'WaterChemistry', 'SSCN2_WaterIsotopes.xlsx'))[[2]]

Code_String <- H2O_18$`Sample ID`
Code1<-gsub(" EV", "_", Code_String)
H2O_18$SampleCode<-gsub(" Site ", "_0", Code1)

#Change name of deltas
names(H2O_18)[grep('2HVSMOW', names(H2O_18))] <- "d2HVSMOW"
names(H2O_18)[grep('18OVSMOW', names(H2O_18))] <- "d18OVSMOW"

H2O_18_SSCN2 <- H2O_18 %>%
  filter(str_detect(SampleCode, "SSCN2")) %>%
  mutate(SampleCode = paste0(SampleCode, '_S')) %>%
  dplyr::select(SampleCode, d18OVSMOW, d2HVSMOW) %>%
  group_by(SampleCode) %>%
  summarize_all(mean, na.rm=T)


#Merge everything together

# merge1<-full_join(site_df, YSI_surf)
merge1<-full_join(site_df_twodepths, full_chem_df)
  
#Nutrient data
merge2<-full_join(merge1, YSI_TwoDepths)

#O18-DO data
merge3<- left_join(merge2, O18_df)

#O18-H2O data
merge4 <- left_join(merge3, H2O_18_SSCN2)


#Merge water chemistry data
merge_df<-merge4

saveRDS(merge_df, file=file.path(onedrive_dir, 'RData', 'NutrientExperiment2', 'SiteData_Merged.rds'))
write.csv(merge_df, file.path(onedrive_dir, 'OutputData', 'NutrientExperiment2', 'SiteData_Merged.csv'))


rm(YSI_surf, merge1, merge2, merge3, merge4, Code_String, Code1, H2O_18_SSCN2, H2O_18, O18_df, O18_list, O18_files)
