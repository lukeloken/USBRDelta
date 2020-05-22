

#Estimate gas transfer velocity using friction velocity
library(LakeMetabolizer)
library(dplyr)


#Prep Temp data
Temp_df_clean2 <- readRDS(file=file.path(onedrive_dir, 'Rdata', 'NutrientExperiment2', 'Buoy', 'Buoy_Temp_cleaned.rds'))
Temp_df_clean2 <- Temp_df_clean2 %>%
  mutate(Datetime_PST = Datetime_UTC)
attributes(Temp_df_clean2$Datetime_PST)$tzone = 'Etc/GMT+8'
Temp_df_kmerge <- Temp_df_clean2 %>%
  mutate(Datetime_PST_round = round_date(Datetime_PST, unit="5 minutes")) %>%
  filter(Depth == 0.5, Site == 'NL74') %>%
  select(Temp_C, Datetime_PST_round) %>%
  rename(Datetime_PST = Datetime_PST_round)

head(Temp_df_kmerge)

#gas transfer input data
k_input<-read.csv(file.path(onedrive_dir, 'OutputData', 'NutrientExperiment2', 'gastransferVariables.csv'), stringsAsFactors = F)
k_input <- k_input %>%
  mutate(Datetime_PST = as.POSIXct(Timestamp..PST., format='%m/%d/%Y %H:%M', tz="Etc/GMT+8")) %>%
  rename(ustar_air = ustar_air..m.s.,
         rho_air = rho_air..kg.m3.,
         rho_water = rho_water..kg.m3.) %>%
  left_join(Temp_df_kmerge) %>%
  select(-Timestamp..PST.)


#Equations in Esters et al. 2017
# ustar_air^2*rho_air = ustar_water^2*rho_water

k_out <- k_input %>%
  mutate(ustar_water = sqrt(ustar_air^2 * rho_air / rho_water),
         SC_CO2 = getSchmidt(Temp_C, 'CO2'),
         SC_O2 = getSchmidt(Temp_C, 'O2')) %>%
  mutate(n =  (.13 - .22 * log10(ustar_water))) %>%
  mutate(k_CO2 = .224 * ustar_water * SC_CO2 ^ (-n)) %>%
  mutate(k_O2 = k_CO2 * (SC_CO2 / SC_O2)^(-n)) %>%
  mutate(k_600_theory_high = ustar_water / 6.7 * 600^-.5,
         k_600_theory_low = ustar_water / 12.2 * 600^-.666) %>%
  mutate(k_O2_theory_low = k_600_theory_low * (600 / SC_O2)^(-.666),
         k_O2_theory_high = k_600_theory_high * (600 / SC_O2)^(-.5))

summary(k_out)
head(k_out)

ggplot(k_out, aes(x=ustar_water)) +
  geom_point(aes(y=k_O2), col='black') + 
  geom_point(aes(y=k_O2_theory_low), col='red') + 
  geom_point(aes(y=k_O2_theory_high), col='blue')  
  # geom_abline(col='red')
  # geom_smooth(method='lm', col='blue')

ggplot(k_out, aes(x=k_O2_theory_low, y=k_O2)) +
  geom_point() + 
  geom_abline(col='red')


ggplot(k_out, aes(x=ustar_water, y= (k_CO2 * 100 * 3600))) +
  geom_point()
# geom_abline(col='red') +
# geom_smooth(method='lm')

ggplot(k_out, aes(x=ustar_water, y=n)) +
  geom_point()


wind_df_summary <- readRDS(file=file.path(onedrive_dir, 'Rdata', 'NutrientExperiment2', 'WindDataAvg.rds'))

wind_pred <- data.frame(approx(x=wind_df_summary$DateTime, y=wind_df_summary$WS_ms_roll, xo=seq.POSIXt(min(k_out$Datetime_PST), max(k_out$Datetime_PST), by="5 mins")))
names(wind_pred)<-c("Datetime_PST", "WS_ms")

wind_pred$SolRad_Wsqm <- approx(x=wind_df_summary$DateTime, y=wind_df_summary$SolRad_Wsqm, xo=seq.POSIXt(min(k_out$Datetime_PST), max(k_out$Datetime_PST), by="5 mins"))$y

attributes(wind_pred$Datetime_PST)$tzone

k_out_wind <- k_out %>%
  left_join(wind_pred)

saveRDS(k_out_wind, file=file.path(onedrive_dir, 'Rdata', 'NutrientExperiment2', 'k_estimates.rds'))

#Old code from Bogard
z.mean<-8
wind.height <- 2.5
area <- .153

u10 <- k_out_wind$WS_ms * (1+ (((0.0013^(0.5))/0.41) * (log(10/wind.height)))) #converting wind speed from 3m to 10m height following equation 3 Vachon & Prairie (2013)
k600cmh <- 2.51 + 1.48*u10 + 0.39*u10*(log10(area)) #k600 in cm/h from table 2 equation B vachon & prairie 2013
k600md <- k600cmh * 24/100 #converting k600 to m/d
sco2 <- 1800.6 - (120.1*k_out_wind$Temp_C) + (3.7818 * (k_out_wind$Temp_C^2)) - (0.047608*(k_out_wind$Temp_C^3))#calculating schmidt number for oxygen from Jahne et al (1987)
sco2_v2 <- getSchmidt(k_out_wind$Temp_C, 'O2')

ko2md <- k600md * ((sco2/600)^(-2/3)) #converting k600 to ko2 in m/d for use in mass balance

k_buoy<-ko2md / 24 / 3600


#Plots for assessing calculations

ggplot(k_out_wind, aes(x=sco2_v2, y=sco2, col=Temp_C)) +
  geom_point() + 
  geom_abline()

ggplot(k_out_wind, aes(x=k_O2, y= k_buoy)) +
  geom_point(alpha=.5) + 
  geom_abline(col='red') +
  theme_bw()
# geom_smooth(method='lm')

ggplot(k_out_wind, aes(x=k_O2_theory, y= k_buoy)) +
  geom_point(alpha=.5) + 
  geom_abline(col='red') +
  theme_bw()

ggplot(k_out_wind, aes(x=WS_ms , y= k_buoy-k_O2)) +
  geom_point(alpha=.5) + 
  geom_hline(yintercept=0, col='red') +
  theme_bw()

