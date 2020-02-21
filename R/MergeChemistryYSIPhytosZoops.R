
# Merge nutrient, ysi, phytoplannkton, and zooplankton data
# Can add more as necessary


# plot phytoplankton community

library(viridis)
library(ggplot2)
library(gridExtra)
library(plyr)
library(dplyr)
library(tidyr)
library(lubridate)
library(RColorBrewer)
# library(MASS)



# # Project folder where outputs are stored
# dropbox_dir<-'C:/Dropbox/USBR Delta Project'
# 
# #Where data come from
# google_dir<-'C:/GoogleDrive/DeltaNutrientExperiment'


stationfactors<-c("16", "34", "44", "Pro", "56", "62", "64", "66" ,"70" ,"74" ,"76" ,"84" ,"WSP")


#Input Data
#nutrients
WQ_stations<-readRDS(file=paste0(dropbox_dir, '/Data/Rdata/WQ_stations'))
WQ_stations$Station<-factor(WQ_stations$Station, stationfactors)
WQ_stations <- dplyr::select(WQ_stations, -NH4_20_ftppm, -NO3_20_ftppm, -PO4_20_ft)

#YSI
YSI_ThreeDepths<-readRDS(file=paste0(dropbox_dir, '/Data/Rdata/YSI_ThreeDepths.rds'))
YSI_surf<-dplyr::filter(YSI_ThreeDepths, DepthStrata=='lessthan3')

#Phyto
Phyto_summary_spread <- readRDS(file=paste0(dropbox_dir, '/Data/Rdata/Phyto_summary_spread.rds'))

#Zoops
Zoo_summary_spread <- readRDS(file=paste0(dropbox_dir, '/Data/Rdata/Zoo_summary_spread.rds'))

#Light extinction
kd_alldates<-readRDS(file=paste0(dropbox_dir, '/Data/Rdata/kd_alldates.rds')) %>%
  dplyr::rename(Station=Site) %>%
  mutate(Station = gsub("NL", "", Station)) %>%
  mutate(Station = gsub("RB", "", Station)) %>%
  mutate(Station = factor(Station, stationfactors))

# others...

SSC_joined_data<-full_join(WQ_stations, YSI_surf) %>%
  full_join(kd_alldates) %>%
  full_join(Phyto_summary_spread) %>%
  full_join(Zoo_summary_spread)


#Plotting factors for consistent plots
SSC_joined_data$Month<-as.character(month(SSC_joined_data$Date, label = TRUE))
SSC_joined_data$Month<-factor(SSC_joined_data$Month, month.abb[1:12])

SSC_joined_data$Zone<-NA
SSC_joined_data$Zone[SSC_joined_data$Station %in% c('WSP', '84')]<-'5'
SSC_joined_data$Zone[SSC_joined_data$Station %in% c('70', '74', '76')]<-'4'
SSC_joined_data$Zone[SSC_joined_data$Station %in% c('62', '64', '66')]<-'3'
SSC_joined_data$Zone[SSC_joined_data$Station %in% c('44', '56', 'Pro')]<-'2'
SSC_joined_data$Zone[SSC_joined_data$Station %in% c('16', '34')]<-'1'


write.table(SSC_joined_data, file=paste0(google_dir, '/DataOutputs/SSC_JoinedSurfaceData.csv'), row.names=F, sep=',')
saveRDS(SSC_joined_data , file=paste0(dropbox_dir, '/Data/Rdata/SSC_joined_data.rds'))



light_data<-SSC_joined_data %>%
  filter(!is.na(kd_meters)) %>%
  dplyr::select(Date, Station, Time, LabTurbidity, FieldTurbidity, Secchicm, Turbid..NTU, kd_meters, PhoticDepth_m) %>%
  rename(LabTurbidity_NTU = LabTurbidity,
         FieldTurbidity_NTU = FieldTurbidity,
         Secchi_cm = Secchicm, 
         SensorTurbidity_NTU = Turbid..NTU)

write.table(light_data, file=paste0(google_dir, '/DataOutputs/light_data.csv'), row.names=F, sep=',')
saveRDS(light_data , file=paste0(dropbox_dir, '/Data/Rdata/light_data.rds'))


