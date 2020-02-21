


#Code to download meta and field sheet data

library(readxl)
library(tidyr)
library(dplyr)
library(plyr)
library(viridis)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(stringr)

nutrient_df<-read_excel(file.path(onedrive_dir, "RawData", "NutrientExperiment2" ,"WaterChemistry", "SSCN2_NutrientData.xlsx"), skip=1)

nutrient_df_names<-names(read_excel(file.path(onedrive_dir, "RawData", "NutrientExperiment2" ,"WaterChemistry", "SSCN2_NutrientData.xlsx")))

names(nutrient_df)<-nutrient_df_names
rm(nutrient_df_names)

#Reduce number of columns and calcuate new ones
nutrient_df <- dplyr::select(nutrient_df, -starts_with('...')) %>%
  dplyr::select(-starts_with('Lab')) %>%
  dplyr::select(-starts_with('AnalyteName')) %>%
  drop_na(SampleCode) %>%
  dplyr::mutate(
    `DIN-ppm` = `NH4-ppm` + `NO3-ppm`, 
    `DON-ppm` = `TDN-ppm` - `NH4-ppm` - `NO3-ppm`, 
    `TPN-ppm` = `TN-ppm` - `TDN-ppm`, 
    Date = as.Date(Date),
    Site = SiteName
  ) #%>%
  #rename(LocationName = SiteName)

#head(nutrient_df)


#TSS data
tss_df<-read_excel(file.path(onedrive_dir, "RawData", "NutrientExperiment2", "WaterChemistry", "SSCN2_TSSData.xlsx"), skip=1)

tss_df_names<-names(read_excel(file.path(onedrive_dir, "RawData", "NutrientExperiment2", "WaterChemistry", "SSCN2_TSSData.xlsx")))

names(tss_df)[1:7]<-tss_df_names[1:7]
rm(tss_df_names)

tss_df_sub<-tss_df[c("SampleCode", "Date", "Event", "Site", 'SiteCode', 'TSS', 'VSS')] %>%
  mutate(Date=as.Date(Date))

#ChlA

chla_df<-read_excel(file.path(onedrive_dir, "RawData", "NutrientExperiment2", "WaterChemistry", "SSCN2_chla.xlsx"), skip=1)


chla_df_names<-names(read_excel(file.path(onedrive_dir, "RawData", "NutrientExperiment2", "WaterChemistry", "SSCN2_chla.xlsx")))

names(chla_df)<-chla_df_names
rm(chla_df_names)

chla_df_mean <- dplyr::select(chla_df, -starts_with('...')) %>%
  dplyr::select(-starts_with('Lab')) %>%
  drop_na(SampleCode) %>%
  mutate(SampleCode = gsub("_a", "", SampleCode)) %>%
  mutate(SampleCode = gsub("_b", "", SampleCode)) %>%
  mutate(Date = as.Date(Date, format='%m/%d/%Y')) %>%
  group_by(SampleCode, LocationCode, LocationName, Date) %>%
  dplyr::summarize(
    chla_mean = mean(ChlA, na.rm=T),
    chla_sd = sd(ChlA, na.rm=T),
    pheo_mean = mean(Pheo, na.rm=T),
    pheo_sd = sd(Pheo, na.rm=T)) %>% 
  dplyr::arrange(Date, LocationCode) %>%
  dplyr::rename(Site = LocationName)
  

nut_tss_df<-full_join(nutrient_df, tss_df_sub) 

nut_tss_chla_df<-left_join(nut_tss_df, chla_df_mean)

# nut_tss_O18_df$Site<-sitetable$site1[match(nut_tss_O18_df$Site, sitetable$site3)]

full_chem_df <- nut_tss_chla_df

head(as.data.frame(full_chem_df))

write.table(full_chem_df, file=file.path(onedrive_dir, 'OutputData', 'NutrientExperiment2', 'AllWaterChemistry.csv'), row.names=F, sep=',')
saveRDS(full_chem_df, file=file.path(onedrive_dir, 'RData', 'NutrientExperiment2', 'AllWaterChemistry.rds'))


rm(nutrient_df, tss_df, tss_df_sub, nut_tss_df, nut_tss_chla_df, chla_df, chla_df_mean)
