

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


wind_df_summary <- readRDS(file=paste0(dropbox_dir, '/Data/Rdata_SSCN2/WindDataAvg.rds'))

wind_avg <- wind_df_summary %>%
  mutate(Date = as.Date(DateTime, tz="Etc/GMT+8")) %>%
  dplyr::select(-DateTime) %>%
  group_by(Date) %>%
  summarize_all(mean) %>%
  mutate(WS_ms_3day = roll_mean(WS_ms_roll, 3, align='right', fill=NA)) %>%
  mutate(WS_ms_3day_sd = roll_sd(WS_ms_roll, 3, align='right', fill=NA)) %>%
  dplyr::rename(WS_ms_daily = WS_ms_roll)

  
#assume a wind height of 10m (completely made up. Should check what the wind height is at the airports)
wind_avg$wind.height.ms=10


#Big data frame
merge_df_IncMetab <- readRDS(file=paste0(dropbox_dir, '/Data/Rdata_SSCN2/SiteData_withIncMetab_Merged.rds'))


merge_df_wind <- left_join(merge_df_IncMetab, wind_avg)

waterO18table <- merge_df_wind %>%
  select(Site, d18OVSMOW, d2HVSMOW) %>%
  group_by(Site) %>%
  dplyr::summarize(d18OVSMOW_mean = mean(d18OVSMOW, na.rm=T),
                   d2HVSMOW_mean = mean(d2HVSMOW, na.rm=T))

merge_df_wind <- left_join(merge_df_wind, waterO18table)

saveRDS(merge_df_wind, file=paste0(dropbox_dir, '/Data/Rdata_SSCN2/InputTermsO18Metabolism_SSCN2.rds'))
write.csv(merge_df_wind, file=paste0(google_dir, '/SSCN2_DataOutputs/InputTermsO18Metabolism_SSCN2.csv'), row.names=F)

