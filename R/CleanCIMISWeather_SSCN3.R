

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

library(stringr)
library(openair)
library(circular)

dir.create(file.path(box_dir, "Figures", "Wind"), showWarnings = F)


#CIMIS data
weather_df<-read.csv(file.path(box_dir, 'DWSC_Data_Raw', 'CIMIS_weather_hourly_Nov2024.csv'), stringsAsFactors = F)
summary(weather_df)
weather_df$Date<-as.Date(weather_df$Date, format='%m/%d/%Y')

weather_df2<-read.csv(file.path(box_dir, 'DWSC_Data_Raw', 'CIMIS_weather_hourly_Nov2023.csv'), stringsAsFactors = F)
weather_df2$Date<-as.Date(weather_df2$Date, format='%m/%d/%Y')

weather_df <- bind_rows(weather_df2, weather_df) %>%
  distinct() %>%
  arrange(Stn.Id, Date, Hour..PST.)


wind_df<- weather_df %>% 
  mutate(DateTime = as.POSIXct(paste(Date, Hour..PST./100, sep=' '), format='%Y-%m-%d %H', tz="Etc/GMT+8")) %>%
  dplyr::select(Date, DateTime, Stn.Id, Hour..PST., Sol.Rad..W.sq.m.,  Air.Temp..C. , Wind.Speed..m.s., Wind.Dir..0.360.) %>% 
  tidyr::drop_na(Date) %>%
  dplyr::rename(Station = Stn.Id,
                Hour = Hour..PST.,
                SolRad_Wsqm = Sol.Rad..W.sq.m.,
                AirTemp_C = Air.Temp..C.,
                WS_ms = Wind.Speed..m.s.,
                WD_deg = Wind.Dir..0.360.
                ) %>%
  mutate(Station = as.character(Station)) %>%
  filter(Station != "155")
  
  #Plot wind by station
ggplot(wind_df, aes(DateTime, WS_ms, group=Station, color=Station)) +
  geom_point(size=2, alpha=.3) +
  # geom_line() + 
  facet_grid(rows=vars(Station)) + 
  scale_y_continuous(limits=c(0,15))

ggplot(wind_df, aes(DateTime, WD_deg, group=Station, color=Station)) +
  geom_point(size=2, alpha=.3) +
  facet_grid(rows=vars(Station)) + 
  scale_y_continuous(limits=c(0,360))


ggplot(wind_df, aes(y=WS_ms, x=Station, fill=Station)) +
  geom_boxplot() + 
  scale_y_continuous(limits=c(0,15))

#Combine two stations
#Calculate average wind direction
wind_dir_mean <-   wind_df %>%
  filter(Station %in% c('212', '6', '243'), !is.na(WD_deg)) %>% #updated in 2024, three stations surrounding DWSC
  group_by(DateTime) %>%
  dplyr::summarize(WD_deg = mean(circular(WD_deg, units = "degrees"), na.rm=T))

wind_dir_mean$WD_deg[which(wind_dir_mean$WD_deg<0)]=360+wind_dir_mean$WD_deg[which(wind_dir_mean$WD_deg<0)]

#Summarize other variables and merge with direction
wind_df_summary <- wind_df %>%
  filter(Station %in% c('212', '6', '243')) %>%
  group_by(DateTime) %>%
  select(-Date, -Hour, -Station, -WD_deg) %>%
  summarize_all(mean, na.rm = TRUE) %>%
  full_join(wind_dir_mean)
  
  
#plot wind summary
ggplot(wind_df_summary, aes(DateTime, WS_ms)) +
  geom_point(size=2, alpha=.3) +
  # geom_line() + 
  scale_y_continuous(limits=c(0,15))

ggplot(wind_df_summary, aes(DateTime, WD_deg)) +
  geom_point(size=2, alpha=.3) +
  # geom_line() + 
  scale_y_continuous(limits=c(0,360))



png(file.path(box_dir, 'Figures', 'Wind', 'AnnualWind_Davis.png'), width=4, height=4, units='in', res=200)
par(mar = c(0,0.5,0.5,0.5),mgp=c(1.5,0.4,0),tck=-0.02)

windRose(wind_df[wind_df$Station == 6,], ws="WS_ms", wd="WD_deg", cols='hue', paddle=F, auto.text=F, border='black', grid.line=list(value=10, lty=5, col="gray"), offset=4, main=paste('Aug 2022 - Nov 2023 average wind: Davis', sep=""), dig.lab=3, angle.scale=45, key.footer=expression(paste('Wind speed (m s'^'-1', ')')))

dev.off()

png(file.path(box_dir, 'Figures', 'Wind', 'AnnualWind_Hastings.png'), width=4, height=4, units='in', res=200)
par(mar = c(0,0.5,0.5,0.5),mgp=c(1.5,0.4,0),tck=-0.02)

windRose(wind_df[wind_df$Station == 212,], ws="WS_ms", wd="WD_deg", cols='hue', paddle=F, auto.text=F, border='black', grid.line=list(value=10, lty=5, col="gray"), offset=4, main=paste('Aug 2022 - Nov 2024 average wind: Hastings', sep=""), dig.lab=3, angle.scale=45, key.footer=expression(paste('Wind speed (m s'^'-1', ')')))

dev.off()

png(file.path(box_dir, 'Figures', 'Wind', 'AnnualWind_Ryde.png'), width=4, height=4, units='in', res=200)
par(mar = c(0,0.5,0.5,0.5),mgp=c(1.5,0.4,0),tck=-0.02)

windRose(wind_df[wind_df$Station == 243,], ws="WS_ms", wd="WD_deg", cols='hue', paddle=F, auto.text=F, border='black', grid.line=list(value=10, lty=5, col="gray"), offset=4, main=paste('Aug 2022 - Nov 2024 average wind: Ryde', sep=""), dig.lab=3, angle.scale=45, key.footer=expression(paste('Wind speed (m s'^'-1', ')')))

dev.off()


png(file.path(box_dir, 'Figures', 'Wind', 'AnnualWind_3StationAverage.png'), width=4, height=4, units='in', res=200)
par(mar = c(0,0.5,0.5,0.5),mgp=c(1.5,0.4,0),tck=-0.02)

windRose(wind_df_summary, ws="WS_ms", wd="WD_deg", cols='hue', paddle=F, auto.text=F, border='black', grid.line=list(value=10, lty=5, col="gray"), offset=4, main=paste('Aug 2022 - Nov 2024 average wind: 3 stations', sep=""), dig.lab=3, angle.scale=45, key.footer=expression(paste('Wind speed (m s'^'-1', ')')))

dev.off()


#Calculate 3 hour means and standard deviations
wind_df_summary$WS_ms_roll <- roll_mean(wind_df_summary$WS_ms, n = 5, fill = NA, align = 'center')


write.csv(wind_df_summary, file=file.path(box_dir, 'DWSC_Data_Processed', 'WindDataAvg.csv'), row.names=F)
saveRDS(wind_df_summary, file=file.path(box_dir, 'DWSC_Data_Processed', 'WindDataAvg.rds'))


rm(weather_df, wind_df, wind_df_summary, wind_dir_mean)
