

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

wind_df_summary <- readRDS(file=file.path(onedrive_dir, 'Rdata', 'NutrientExperiment2', 'WindDataAvg.rds'))


wind_avg <- wind_df_summary %>%
  mutate(Date = as.Date(DateTime, tz="Etc/GMT+8")) %>%
  dplyr::select(-DateTime) %>%
  group_by(Date) %>%
  summarize_all(mean) %>%
  mutate(WS_ms_3day = roll_mean(WS_ms_roll, 3, align='right', fill=NA)) %>%
  mutate(WS_ms_3day_sd = roll_sd(WS_ms_roll, 3, align='right', fill=NA)) %>%
  dplyr::rename(WS_ms_daily = WS_ms_roll)

  
#assume a wind height of 10m (completely made up. Should check what the wind height is at the airports)
wind_avg$wind.height.ms=2.5


#k models based on ustar
k_out_wind <- readRDS(file=file.path(onedrive_dir, 'Rdata', 'NutrientExperiment2', 'k_estimates.rds'))

k_out_avg <- k_out_wind %>%
  mutate(Date = as.Date(Datetime_PST , tz="Etc/GMT+8")) %>%
  dplyr::select(Date, k_O2, k_O2_theory_low, k_O2_theory_high) %>%
  group_by(Date) %>%
  summarize_all(mean, na.rm=T) %>%
  mutate(k_O2 = k_O2*3600*24,
         k_O2_theory_low = k_O2_theory_low*3600*24,
         k_O2_theory_high = k_O2_theory_high*3600*24) %>%
  mutate(k_O2_3day = roll_mean(k_O2, 3, align='right', fill=NA)) %>%
  mutate(k_O2_3day_sd = roll_sd(k_O2, 3, align='right', fill=NA)) %>%
  dplyr::rename(k_O2_daily = k_O2) 


#Big data frame
merge_df_IncMetab <- readRDS(file=file.path(onedrive_dir, 'Rdata', 'NutrientExperiment2', 'SiteData_withIncMetab_Merged.rds'))


merge_df_wind <- left_join(merge_df_IncMetab, wind_avg) %>%
  left_join(k_out_avg)

waterO18table <- merge_df_wind %>%
  select(Site, d18OVSMOW, d2HVSMOW) %>%
  group_by(Site) %>%
  dplyr::summarize(d18OVSMOW_mean = mean(d18OVSMOW, na.rm=T),
                   d2HVSMOW_mean = mean(d2HVSMOW, na.rm=T))

merge_df_wind <- left_join(merge_df_wind, waterO18table)

saveRDS(merge_df_wind, file=file.path(onedrive_dir, 'Rdata', 'NutrientExperiment2', 'InputTermsO18Metabolism_SSCN2.rds'))
write.csv(merge_df_wind, file=file.path(onedrive_dir, 'OutputData', 'NutrientExperiment2', 'InputTermsO18Metabolism_SSCN2.csv'), row.names=F)

