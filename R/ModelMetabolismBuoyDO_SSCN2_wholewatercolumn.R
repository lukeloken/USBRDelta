
library(RcppRoll)
library(tidyr)
library(LakeMetabolizer)
library(akima)
library(zoo)



#Load buoy data
Temp_df_clean2 <- readRDS(file=file.path(onedrive_dir, 'Rdata', 'NutrientExperiment2', 'Buoy', 'Buoy_Temp_cleaned.rds'))
Cond_df_clean2 <- readRDS(file=file.path(onedrive_dir, 'Rdata', 'NutrientExperiment2', 'Buoy', 'Buoy_Cond_cleaned.rds'))
DO_df_clean2   <- readRDS(file=file.path(onedrive_dir, 'Rdata', 'NutrientExperiment2', 'Buoy', 'Buoy_DO_cleaned.rds'))



#Prep Temp data
Temp_df_clean2 <- Temp_df_clean2 %>%
  mutate(Datetime_PDT = Datetime_UTC)
attributes(Temp_df_clean2$Datetime_PDT)$tzone = 'America/Los_Angeles'
Temp_df_clean2$Datetime_PDT_round = round_date(Temp_df_clean2$Datetime_PDT, unit="5 minutes")


#Prep DO data
DO_depths<-unique(DO_df_clean2$Depth)
DO_depths_num<-as.numeric(DO_depths[order(DO_depths)])

DO_df_clean2 <- DO_df_clean2 %>%
  mutate(Datetime_PDT = Datetime_UTC)
attributes(DO_df_clean2$Datetime_PDT)$tzone = 'America/Los_Angeles'
DO_df_clean2$Datetime_PDT_round = round_date(DO_df_clean2$Datetime_PDT, unit="5 minutes")

#Load gage data for water level
GageData <- readRDS(file=file.path(onedrive_dir, 'RData', 'NutrientExperiment2', 'USGSGageData.rds')) %>%
  filter(site_no == '11455095') %>%
  dplyr::select_if(not_all_na) %>%
  dplyr::select(dateTime, Wtemp_Inst, Turb_Inst, Flow_Inst, GH_Inst, SpecCond_Inst, DO_Inst, pH_Inst,  X_32316_Inst) %>%
  rename(Datetime_PDT_round =dateTime) %>%
  mutate(GH_Inst = approx(x=Datetime_PDT_round, y=GH_Inst, xo=Datetime_PDT_round)$y) %>%
  mutate(Flow_Inst = approx(x=Datetime_PDT_round, y=Flow_Inst, xo=Datetime_PDT_round)$y) %>%
  mutate(GH_m = round(GH_Inst*0.3048, 2)) %>%
  drop_na(GH_Inst)

#k models based on ustar
k_out_wind <- readRDS(file=file.path(onedrive_dir, 'Rdata', 'NutrientExperiment2', 'k_estimates.rds'))
attributes(k_out_wind$Datetime_PST)$tzone <- 'America/Los_Angeles'

#wind and solar raddata
wind_df_summary <- readRDS(file=file.path(onedrive_dir, 'Rdata', 'NutrientExperiment2', 'WindDataAvg.rds'))

wind_pred <- data.frame(approx(x=wind_df_summary$DateTime, y=wind_df_summary$WS_ms_roll, xo=seq.POSIXt(min(DO_df_clean2$Datetime_PDT_round), max(DO_df_clean2$Datetime_PDT_round), by="5 mins")))
names(wind_pred)<-c("Datetime_PDT_round", "WS_ms")

wind_pred$SolRad_Wsqm <- approx(x=wind_df_summary$DateTime, y=wind_df_summary$SolRad_Wsqm, xo=seq.POSIXt(min(DO_df_clean2$Datetime_PDT_round), max(DO_df_clean2$Datetime_PDT_round), by="5 mins"))$y

attributes(wind_pred$Datetime_PDT_round)$tzone

wind_pred$k_O2 <- approx(x=k_out_wind$Datetime_PST, y=k_out_wind$ k_O2, xo=seq.POSIXt(min(DO_df_clean2$Datetime_PDT_round), max(DO_df_clean2$Datetime_PDT_round), by="5 mins"))$y


#load hypso data
Depth_df <- readRDS(file=file.path(onedrive_dir, 'Rdata', 'HypsoCurveNL74.rds'))
# plot(Depth_df$Area_m2, Depth_df$Depth_m, ylim=c(12,0), type='o', pch=16)

#Approximate volume by depth
Depth_pred25 <- data.frame(approx(x=Depth_df$Depth_m, y=.5*(Depth_df$Volume_m3), xo=seq(0, max(Depth_df$Depth_m), by=0.25)))
Total_volume = sum(Depth_df$Volume_m3, na.rm=T)
# Total_volume = sum(Depth_pred25$y, na.rm=T)
Surface_area = Depth_df$Area_m2[which(Depth_df$Depth_m==0)]
Mean_depth <- Total_volume/Surface_area

Depth_pred100 <- data.frame(approx(x=Depth_df$Depth_m, y=(Depth_df$Area_m2), 
                                   xo=seq(0, max(Depth_df$Depth_m), by=0.01))) %>%
  mutate(Volume_m3 = y*.01) %>%
  rename(Area_m2 = y)



#New code to include variation in depth through time
WL_times <- GageData$Datetime_PDT_round
WL_z <- GageData$GH_m
range_z <- range(WL_z, na.rm=T)

Depth_pred100 <- Depth_pred100 %>%
  mutate(GH_m = range_z[2] - x) 

x <- Depth_pred100$GH_m[1]
Vol_top <- sapply(Depth_pred100$GH_m, function (x) 
  sum(Depth_pred100$Volume_m3[match(x, Depth_pred100$GH_m):
                        (match(x, Depth_pred100$GH_m)+100*mean(DO_depths_num[1:2])-1)
                      ]))

Vol_mid <- sapply(Depth_pred100$GH_m, function (x) 
  sum(Depth_pred100$Volume_m3[(match(x, Depth_pred100$GH_m)+100*mean(DO_depths_num[1:2])):
                        (match(x, Depth_pred100$GH_m)+100*mean(DO_depths_num[2:3])-1)
                      ]))

Vol_bot <- sapply(Depth_pred100$GH_m, function (x) 
  sum(Depth_pred100$Volume_m3[(match(x, Depth_pred100$GH_m)+100*mean(DO_depths_num[2:3])):
                        nrow(Depth_pred100)
                      ]))

#Table includes WL (gage height) and three columns with volumes associated with the three DO sensors
Volume_table <- Depth_pred100 %>%
  dplyr::select(GH_m, Area_m2) %>%
  bind_cols(data.frame(Vol_top = Vol_top, 
                       Vol_mid = Vol_mid, 
                       Vol_bot = Vol_bot)) %>%
  mutate(Vol_sum = Vol_top + Vol_mid + Vol_bot) %>%
  mutate(GH_m = factor(round(GH_m, 2), seq(range_z[1], range_z[2], by=.01)))

GageData <- GageData %>%
  drop_na(GH_m) %>%
  mutate(GH_m = factor(round(GH_m, 2), seq(range_z[1], range_z[2], by=.01))) %>%
  left_join(Volume_table)


#Volumes of three depth strata
Vol_top_v1 <- sum(Depth_pred25$y[which(Depth_pred25$x < mean(DO_depths_num[1:2]))], na.rm=T)
Vol_mid_v1 <- sum(Depth_pred25$y[which(Depth_pred25$x < mean(DO_depths_num[2:3]))], na.rm=T) - Vol_top_v1
Vol_bot_v1 <- sum(Depth_pred25$y[which(Depth_pred25$x >= mean(DO_depths_num[2:3]))], na.rm=T)


# Vol_top <- sum(Depth_pred100$y[which(Depth_pred100$x < mean(DO_depths_num[1:2]))], na.rm=T)
# Vol_mid <- sum(Depth_pred100$y[which(Depth_pred100$x < mean(DO_depths_num[2:3]))], na.rm=T) - Vol_top
# Vol_bot <- sum(Depth_pred100$y[which(Depth_pred100$x >= mean(DO_depths_num[2:3]))], na.rm=T)
# 
# data.frame(Vol_top, Vol_mid, Vol_bot)

#Allow volume to vary



#constants
#these might not be useful or accurate
z.mean<-8
wind.height <- 10
area <- .153

#Average of data used for Oxygen 18 metabolism SSCN 1
ko2md_mean<-2.307741

#Code to loop through each buoy and whole water column integrated metabolism
buoy_names<-unique(DO_df_clean2$Site)

metab.list<-list()
buoy_nu <- 3
for (buoy_nu in 1:length(buoy_names)){
  
  site_name <- buoy_names[buoy_nu]
  
  #prepare temp data
  temp_buoy <- Temp_df_clean2 %>%
    dplyr::filter(Site == site_name) %>%
    tidyr::drop_na(Temp_C)
  
  temp_times = temp_buoy$Datetime_PDT_round
  temp_depths=as.numeric(temp_buoy$Depth)
  temp_values =temp_buoy$Temp_C
  
  # Temp_Est <- interp(temp_times,temp_depths,temp_values, yo=seq(min(temp_depths),max(temp_depths), by=0.25), xo=seq(min(temp_times) , max(temp_times), "30 mins"))
  # names(Temp_Est)<-c('DateTime_PDT', 'SensorDepth', 'Temp_C')
  # 
  # Temp_Est$DateTime_PDT<-seq(ceiling_date(min(dates), "mins"),floor_date(max(dates), "mins"), by='mins')
  
  #Calculate thermocline depth
  wrt1<-spread(temp_buoy[c('Datetime_PDT_round', 'Depth', 'Temp_C')], key=Depth, value=Temp_C, fill=NA )
  wrt3<-wrt1[which(!is.na(rowSums(wrt1[,2:10]))),]
  
  # Calculating rolling thermocline depth
  t.d<-apply(wrt3[,2:10], 1, function (x) thermo.depth(x, depths=as.numeric(names(wrt3[,2:10])), mixed.cutoff = 0.2)[1])
  
  #Calculate upper mixed layer depth (top of metalimnion)
  m.d.top <- apply(wrt3[,2:10], 1, function (x) meta.depths(x, depths=as.numeric(names(wrt3[,2:10])), mixed.cutoff = 0.5)[1])
  m.d.bot <- apply(wrt3[,2:10], 1, function (x) meta.depths(x, depths=as.numeric(names(wrt3[,2:10])), mixed.cutoff = 0.5)[2])
  
  t.d[which(is.na(t.d))]<- 6
  m.d.top[which(is.na(m.d.top))]<- 6
  m.d.bot[which(is.na(m.d.bot))]<- 6
  
  #run a rolling mean to smooth
  t.d.roll<-roll_mean(t.d, n=5, align='center', fill=NA)
  m.d.top.roll<-roll_mean(m.d.top, n=5, align='center', fill=NA)
  m.d.bot.roll<-roll_mean(m.d.bot, n=5, align='center', fill=NA)
  
  #Look at thermocline and metalimnion demarcations
  # plot(wrt3[,1],t.d, lwd=2, col='black', type='l', ylim=c(6,0))
  # points(wrt3[,1],m.d.top, lwd=1, col='red', type='l')
  # points(wrt3[,1],m.d.bot, lwd=1, col='blue', type='l')
  # 
  # #Look at thermocline and metalimnion demarcations (smoothed)
  # plot(wrt3[,1],t.d.roll, lwd=2, col='black', type='l', ylim=c(6,0))
  # points(wrt3[,1],m.d.top.roll, lwd=1, col='red', type='l')
  # points(wrt3[,1],m.d.bot.roll, lwd=1, col='blue', type='l')
  # 
  # #Look at just metalimnion demarcations (smoothed)
  # # plot(wrt3[,1],t.d.roll, lwd=2, col='black', type='l', ylim=c(6,0))
  # plot(wrt3[,1],m.d.top.roll, lwd=2, col='red', type='l', ylim=c(6,0))
  # points(wrt3[,1],m.d.bot.roll, lwd=1, col='blue', type='l', lty=2)
  # 
  
  zmix_buoy<-data.frame(DateTime = wrt3[,1], Zmix_m=m.d.top.roll) %>%
    tidyr::fill(Zmix_m, .direction='down') %>%
    tidyr::fill(Zmix_m, .direction='up')
  
  ####################################################################################################
  
  #Prepare DO data
  #Metab date starts at 6am
  DO_buoy <- DO_df_clean2 %>%
    dplyr::filter(Site == site_name) %>%
    mutate(Date = as.Date(Datetime_PDT_round, tz='America/Los_Angeles'),
           Date_metab = as.Date(Datetime_PDT_round-6*60*60, tz='America/Los_Angeles')) 
  
  head(DO_buoy)
  
  DO_spread_mgL <- DO_buoy %>%
    dplyr::select(Date_metab, Datetime_PDT_round, Depth, DO_mgL) %>%
    spread(key=Depth, value=DO_mgL) %>%
    drop_na(DO_depths)
  
  #Calculate rolling mean (7 observations 30 minute windows)
  DO_spread_mgL_roll <- rollapply(DO_spread_mgL[DO_depths], 7, mean, align='center', fill=NA) 
  colnames(DO_spread_mgL_roll) <- paste0("roll_", DO_depths)
  
  head(DO_spread_mgL_roll)
  
  DO_spread_mgL_join <- bind_cols(DO_spread_mgL, data.frame(DO_spread_mgL_roll))
  
  DO_spread_mgL_join <- GageData  %>% 
    select(Datetime_PDT_round, GH_m, Vol_top, Vol_mid, Vol_bot, Vol_sum, Area_m2) %>%
    right_join(DO_spread_mgL_join) %>%
    drop_na(Vol_top) %>%
    left_join(wind_pred)
  
  head(DO_spread_mgL_join)
  
  
  #whole lake depth integrated DO concentration
  DO_spread_mgL_join$DO_WLDI_v1 <- (DO_spread_mgL_join$roll_1   * Vol_top_v1 +
                                   DO_spread_mgL_join$`roll_2.5` * Vol_mid_v1 +
                                   DO_spread_mgL_join$roll_4   * Vol_bot_v1) / Total_volume
  
  # whole lake depth integrated DO concentration
  #allow volume to vary based on water level
  DO_spread_mgL_join$DO_WLDI <- (DO_spread_mgL_join$roll_1   * DO_spread_mgL_join$Vol_top +
                                   DO_spread_mgL_join$`roll_2.5` * DO_spread_mgL_join$Vol_mid +
                                   DO_spread_mgL_join$roll_4   * DO_spread_mgL_join$Vol_bot) / 
    DO_spread_mgL_join$Vol_sum
  
  DO_spread_mgL_join <- DO_spread_mgL_join %>%
    tidyr::fill(DO_WLDI, .direction='down') %>%
    tidyr::fill(DO_WLDI, .direction='up') %>%
    tidyr::fill(DO_WLDI_v1, .direction='down') %>%
    tidyr::fill(DO_WLDI_v1, .direction='up')
  
  head(DO_spread_mgL_join)
  
  #Look at daily Whole Water Column DO curves
  DO_curves <- ggplot(aes(x=Datetime_PDT_round, y=DO_WLDI), data=DO_spread_mgL_join) +
    geom_vline(xintercept=DO_spread_mgL_join$Datetime_PDT_round[which(DO_spread_mgL_join$SolRad_Wsqm<50)], col='grey', size=.9) + 
    geom_vline(xintercept=fert_posix, col='green', linetype='solid', size=2) +
    geom_path() +
    theme_bw() +
    scale_x_datetime(date_breaks = '6 hours', date_labels = "%H") + 
    facet_wrap(~Date_metab, scales="free", ncol=9) +
    labs(x='Hour', y='Whole water column DO (mg/L)')
    # geom_point(inherit.aes=F, data=DO_spread_mgL_join[which(DO_spread_mgL_join$SolRad_Wsqm<50),], 
              # aes(x=Datetime_PDT_round, y=6), col='black', size=2)
  print(DO_curves)

ggsave(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'Buoys', 'DO', paste0('WholeWaterColumnDOCurves_', site_name, '.png')), DO_curves, width=14, height=10, units='in')
  


  input_df<-left_join(DO_spread_mgL_join, wind_pred) 
  
  input_df <- DO_buoy %>% filter(Depth==1) %>%
    dplyr::select(Datetime_PDT_round, Temp_C) %>%
    right_join(input_df) %>%
    distinct()
  
  
  #Loop through each day
  metab.dates<-unique(input_df$Date_metab)
  metab.out <-data.frame(Date = metab.dates, Site = rep(site_name, length(metab.dates)), GPP=NA, ER=NA, NEP=NA, GPP_v1=NA, ER_v1=NA, NEP_v1=NA)
  
  m=2
  for (m in 1:length(metab.dates)){
  
    index <- which(input_df$Date_metab==metab.dates[m])
    
  #inputs for metabolism model
  datetime <-  input_df$Datetime_PDT_round[index]
  
  Vol_sum <-  input_df$Vol_sum[index]
  Area_m2 <- input_df$Area_m2[index]
  
  #Only calculate metabolism if sampling was at least most of the day
  if (sum(as.numeric(diff(datetime), units='days'))>.8){
  
  freq<-1/as.numeric((diff(datetime)), units='days')
  
  wtr <- input_df$Temp_C[index]
  
  do_surf <- input_df$`1`[index]
  do_wldi <- input_df$DO_WLDI[index]
  do_wldi_v1 <- input_df$DO_WLDI_v1[index]
  
  do_surf_sat<- o2.at.sat.base(wtr, altitude=0)
  
  wind_ms <- input_df$WS_ms[index]
  SolRad_Wsqm <- input_df$SolRad_Wsqm[index]
  
  k_O2 <- input_df$k_O2[index]
  
  #calculating the gas exchange coefficient for O2 empirically from lake area and wind speed:
  u10 <- wind_ms * (1+ (((0.0013^(0.5))/0.41) * (log(10/wind.height)))) #converting wind speed from 3m to 10m height following equation 3 Vachon & Prairie (2013)
  k600cmh <- 2.51 + 1.48*u10 + 0.39*u10*(log10(area)) #k600 in cm/h from table 2 equation B vachon & prairie 2013
  k600md <- k600cmh * 24/100 #converting k600 to m/d
  sco2 <- 1800.6 - (120.1*wtr) + (3.7818 * (wtr^2)) - (0.047608*(wtr^3))#calculating schmidt number for oxygen from Jahne et al (1987)
  ko2md <- k600md * ((sco2/600)^(-2/3)) #converting k600 to ko2 in m/d for use in mass balance
  
  # k_buoy<-ko2md
  
  k_buoy<-k_O2*24*3600
  
  isday <- which(SolRad_Wsqm>100)
  isnight <- which(SolRad_Wsqm<10)
  isnight <-isnight[which(isnight>max(isday, na.rm=T))]

  
    # isday = as.integer(is.day(datetime,38.5)) #Sparkling Lake is at 48 degrees N latitude
  # dayI <- isday == 1L
  # nightI <- isday == 0L
  # 
  zmix <- approx(x=zmix_buoy$DateTime, y=zmix_buoy$Zmix_m, xo=datetime, rule=2)$y

  #Model metabolism
  delta.do <- diff(do_wldi)
  miss.delta <- sum(is.na(delta.do))
  delta.do_v1 <- diff(do_wldi_v1)
  miss.delta_v1 <- sum(is.na(delta.do_v1))
  if (miss.delta != 0) {
    warning(paste(miss.delta, " missing values (", miss.delta/length(delta.do), 
                  "%) in diff(do.obs)", sep = ""))
  }
  
  #flux in g O2 per day for whole channel. Big volume and area
  gas.flux <- ((do_surf_sat - do_surf) * k_buoy * Area_m2)[-length(datetime)]
  gas.flux_v1 <- ((do_surf_sat - do_surf) * k_buoy * Surface_area)[-length(datetime)]
  
  
  #metab is in g O2 per day for whole channel
  delta.do.metab <- (delta.do*freq*Vol_sum[-length(datetime)]) - gas.flux 
  
  delta.do.metab_v1 <- (delta.do_v1*freq*Vol_sum[-length(datetime)]) - gas.flux 
  
  
  #Convert to area and summarize by day/night (g O2 per m2 per day)
  delta.do.metab.area <- delta.do.metab/Area_m2[-length(datetime)]
  delta.do.metab.area_v1 <- delta.do.metab_v1/Surface_area  
  
  
  nep.day <- delta.do.metab.area[isday]
  nep.night <- delta.do.metab.area[isnight]
  ER <- mean(nep.night, na.rm = TRUE)
  NEP <- mean(delta.do.metab.area, na.rm = TRUE)
  # GPP <- mean(nep.day, na.rm = TRUE) - ER
  GPP <- sum((nep.day - ER)/freq[isday], na.rm=T)
  
  nep.day_v1 <- delta.do.metab.area_v1[isday]
  nep.night_v1 <- delta.do.metab.area_v1[isnight]
  ER_v1 <- mean(nep.night_v1, na.rm = TRUE)
  NEP_v1 <- mean(delta.do.metab.area_v1, na.rm = TRUE)
  # GPP_v1 <- mean(nep.day_v1, na.rm = TRUE) - ER
  GPP_v1 <- sum((nep.day_v1 - ER_v1)/freq[isday], na.rm=T)
  
  # metab <- data.frame(GPP = GPP, R = R, NEP = NEP)
  
  metab.out$GPP[m]<-GPP
  metab.out$ER[m]<-ER
  metab.out$NEP[m]<-NEP
  
  metab.out$GPP_v1[m]<-GPP_v1
  metab.out$ER_v1[m]<-ER_v1
  metab.out$NEP_v1[m]<-NEP_v1
  }
  }
  

  
  metab.roll <- rollapply(metab.out[c('GPP', 'ER', 'NEP', 'GPP_v1', 'ER_v1', 'NEP_v1')], 3, mean, align='center', fill=NA)
  colnames(metab.roll)<-paste0(colnames(metab.roll), '_roll')
  
  metab.out2 <- bind_cols(metab.out, data.frame(metab.roll))
  
  metab.list[[buoy_nu]]<-metab.out2
  
  
  png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'Buoys', 'Metabolism', paste('Metabolism_', site_name, '_TS.png')), width=5, height=4, units='in', res=200)
  par(mar=c(3,3,1.5,.5), mgp=c(3,.5,0), tck=-0.02)
  
  plot(metab.out2$Date, metab.out2$NEP, type='l', ylim=range(metab.out[,3:5], na.rm=T), lwd=.5, xlab='', ylab='', las=1)
  abline(h=0, lty=3, lwd=.5)
  points(metab.out2$Date, metab.out2$GPP, type='l', col='darkgreen', lwd=.5)
  points(metab.out2$Date, metab.out2$ER, type='l', col='sienna4', lwd=.5)
  
  points(metab.out2$Date, metab.out2$NEP_roll, type='l', lwd=2)
  points(metab.out2$Date, metab.out2$GPP_roll, type='l', lwd=2, col='darkgreen')
  points(metab.out2$Date, metab.out2$ER_roll, type='l', lwd=2, col='sienna4')
  
  # points(metab.out$Date, roll_mean(metab.out$NEP, 5, fill=NA), type='l', lwd=3)
  # points(metab.out$Date, roll_mean(metab.out$GPP, 5, fill=NA), type='l', lwd=3, col='darkgreen')
  # points(metab.out$Date, roll_mean(metab.out$ER, 5, fill=NA), type='l', lwd=3, col='sienna4')
  
  
  mtext(expression(paste('g ', O[2], ' m'^'-2', ' d'^'-1')), 2, 1.5)
  mtext(site_name, 3, 0.1)
  
  legend('topright', inset=0.02, c('GPP', 'NEP', 'ER'), text.col=c('darkgreen', 'black', 'sienna4'), lty=0, bty='n')
  
  dev.off()  
  
  png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'Buoys', 'Metabolism', paste('Metabolism_', site_name, '_TS_v1.png')), width=5, height=4, units='in', res=200)
  par(mar=c(3,3,1.5,.5), mgp=c(3,.5,0), tck=-0.02)
  
  plot(metab.out2$Date, metab.out2$NEP_v1, type='l', ylim=range(metab.out[,6:8], na.rm=T), lwd=.5, xlab='', ylab='', las=1)
  abline(h=0, lty=3, lwd=.5)
  points(metab.out2$Date, metab.out2$GPP_v1, type='l', col='darkgreen', lwd=.5)
  points(metab.out2$Date, metab.out2$ER_v1, type='l', col='sienna4', lwd=.5)
  
  points(metab.out2$Date, metab.out2$NEP_v1_roll, type='l', lwd=2)
  points(metab.out2$Date, metab.out2$GPP_v1_roll, type='l', lwd=2, col='darkgreen')
  points(metab.out2$Date, metab.out2$ER_v1_roll, type='l', lwd=2, col='sienna4')
  
  # points(metab.out$Date, roll_mean(metab.out$NEP, 5, fill=NA), type='l', lwd=3)
  # points(metab.out$Date, roll_mean(metab.out$GPP, 5, fill=NA), type='l', lwd=3, col='darkgreen')
  # points(metab.out$Date, roll_mean(metab.out$ER, 5, fill=NA), type='l', lwd=3, col='sienna4')
  
  
  mtext(expression(paste('g ', O[2], ' m'^'-2', ' d'^'-1')), 2, 1.5)
  mtext(site_name, 3, 0.1)
  
  legend('topright', inset=0.02, c('GPP', 'NEP', 'ER'), text.col=c('darkgreen', 'black', 'sienna4'), lty=0, bty='n')
  
  dev.off()  
  
  
}

#combine all buoys
metab.df<-ldply(metab.list, data.frame)

#Save to file
write.table(metab.df, file=file.path(onedrive_dir, 'OutputData', 'NutrientExperiment2', 'BuoyMetabolism.csv'), row.names=F, sep=',')

saveRDS(metab.df, file=file.path(onedrive_dir, 'RData', 'NutrientExperiment2', 'BuoyMetabolism.rds'))


metab.df.daily <- metab.df %>%
  group_by(Date) %>%
  select(-Site) %>%
  summarize_all(mean)

metab.df.wind <- input_df %>%
  rename(Date = Date_metab) %>%
  group_by(Date) %>%
  summarize_at(vars(WS_ms), list(max=max, mean=mean, median=median, min=min), na.rm=T) %>%
  right_join(metab.df.daily)

metab.df.wind$windplus1 <- c(NA, metab.df.wind$mean[1:(nrow(metab.df.wind)-1)])

ggplot(metab.df.wind, aes(x=mean, y=NEP)) +
  geom_point() +
  geom_text(aes(label=Date)) +
  geom_text(aes(x=mean, y=NEP,label=Date), data=metab.df.wind[metab.df.wind$Date %in% as.Date(c('2019-07-24', '2019-07-27', '2019-08-12', '2019-08-13', '2019-08-14', '2019-08-15', '2019-08-22')),], col='red')


# ####################
#Plotting
# ####################

color.palette = colorRampPalette(c(viridis(6, begin=.2, end=.98), rev(magma(5, begin=.35, end=.98))), bias=1)

colors<-(color.palette(length(unique(metab.df$Site))))
xlim<-range(metab.df$Date[which(is.finite(metab.df$NEP))], na.rm=T)

#Common theme for all metabolism timeseries panels
commonThemePrint<-list(
  scale_colour_manual(values = colors),
  scale_fill_manual(values = colors),
  scale_shape_manual(values=c(23, 22,22,22,21,21,21,21,22,22,22,23)),
  # geom_smooth(method='loess',  se=F),
  # geom_smooth(method='auto', se=T, alpha=.2),
  # geom_jitter(size=2, width=jitterwidth, height=0, aes(fill=Site, shape=Site)),
  geom_hline(yintercept=0, color='lightgrey', linetype=1.5, size=1), 
  # geom_vline(xintercept=shipdate, color='grey', linetype=2, size=1),
  geom_vline(xintercept=fert_dates, color='green', linetype=2, size=1),
  theme_bw(),
  theme(plot.title = element_text(hjust=0.5), legend.position="none", axis.title.x=element_blank()), 
  scale_x_date(limits=xlim, date_minor_breaks= "weeks", date_breaks = "2 weeks", date_labels="%b %d"), 
  guides(color = guide_legend(nrow = 1, title.position='top', title.hjust=0.5)) 
)

NEPplot<-ggplot(metab.df, aes(Date, NEP, group=(Site))) + 
  commonThemePrint + 
  labs(x='Date', y=expression(paste('Daily NEP (g ', O[2], ' m'^'-2', ' d'^'-1', ')'))) +
  geom_path(aes(color=Site, group=Site), size=0.5) + 
  # geom_point(size=2, aes(fill=Site, shape=Site)) + 
  ggtitle('Free-water metabolism')

GPPplot<-ggplot(metab.df, aes(Date, GPP, group=(Site))) + 
  commonThemePrint + 
  labs(x='Date', y=expression(paste('Daily GPP (g ', O[2], ' m'^'-2', ' d'^'-1', ')'))) +
  geom_path(aes(color=Site, group=Site), size=0.5) 
  # geom_point(size=2, aes(fill=Site, shape=Site))

ERplot<-ggplot(metab.df, aes(Date, ER, group=(Site))) + 
  commonThemePrint + 
  labs(x='Date', y=expression(paste('Daily ER (g ', O[2], ' m'^'-2', ' d'^'-1', ')'))) +
  geom_path(aes(color=Site, group=Site), size=0.5) 
  # geom_point(size=2, aes(fill=Site, shape=Site))


# arrange plots without legend
p2<-grid.arrange(grobs=list(GPPplot, ERplot, NEPplot), ncol=1, as.table=F)

p1<-ERplot + 
  theme(legend.position='bottom') + 
  guides(colour = guide_legend(nrow = 1, title.position='left', title.hjust=0.5))
mylegend<-g_legend(p1)

grid.arrange(p2, mylegend, nrow=2,heights=c(10, 1.5))



png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'Buoys', 'Metabolism_TS.png'), width=5, height=7, units='in', res=200)

grid.newpage()
plots<-grid.draw(rbind(ggplotGrob(NEPplot), ggplotGrob(GPPplot),  ggplotGrob(p1), size = "first"))

dev.off()




NEPplot<-ggplot(metab.df, aes(Date, NEP_roll, group=(Site))) + 
  commonThemePrint + 
  labs(x='Date', y=expression(paste('Daily NEP (g ', O[2], ' m'^'-2', ' d'^'-1', ')'))) +
  geom_path(aes(color=Site, group=Site), size=1.5) + 
  # geom_point(size=2, aes(fill=Site, shape=Site)) + 
  ggtitle('Free-water metabolism: 3 day rolling mean')

GPPplot<-ggplot(metab.df, aes(Date, GPP_roll, group=(Site))) + 
  commonThemePrint + 
  labs(x='Date', y=expression(paste('Daily GPP (g ', O[2], ' m'^'-2', ' d'^'-1', ')'))) +
  geom_path(aes(color=Site, group=Site), size=1.5) 
# geom_point(size=2, aes(fill=Site, shape=Site))

ERplot<-ggplot(metab.df, aes(Date, ER_roll, group=(Site))) + 
  commonThemePrint + 
  labs(x='Date', y=expression(paste('Daily ER (g ', O[2], ' m'^'-2', ' d'^'-1', ')'))) +
  geom_path(aes(color=Site, group=Site), size=1.5) 
# geom_point(size=2, aes(fill=Site, shape=Site))


# arrange plots without legend
p2<-grid.arrange(grobs=list(GPPplot, ERplot, NEPplot), ncol=1, as.table=F)

p1<-ERplot + 
  theme(legend.position='bottom') + 
  guides(colour = guide_legend(nrow = 1, title.position='left', title.hjust=0.5))
mylegend<-g_legend(p1)

grid.arrange(p2, mylegend, nrow=2,heights=c(10, 1.5))



png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'Buoys', 'Metabolism_Rollmean_TS.png'), width=5, height=7, units='in', res=200)

grid.newpage()
plots<-grid.draw(rbind(ggplotGrob(NEPplot), ggplotGrob(GPPplot),  ggplotGrob(p1), size = "first"))

dev.off()




commonBox<-list(
  geom_vline(xintercept=fert_dates, color='green', linetype=2, size=1),
  geom_hline(yintercept=0, color='lightgrey', linetype=1.5, size=1), 
  theme_bw(),
  theme(plot.title = element_text(hjust=0.5), legend.position="none", axis.title.x=element_blank()), 
  scale_x_date(limits=xlim, date_minor_breaks= "weeks", date_breaks = "2 weeks", date_labels="%b %d"), 
  guides(color = guide_legend(nrow = 1, title.position='top', title.hjust=0.5)) 
)


GPPbox<-ggplot(metab.df, aes(x=Date, group=Date, y=GPP_roll)) + 
  commonBox +
  labs(x='Date', y=expression(paste('Daily GPP (g ', O[2], ' m'^'-2', ' d'^'-1', ')'))) +
  geom_boxplot(fill='darkgreen', outlier.size=0.5) 

ERbox<-ggplot(metab.df, aes(x=Date, group=Date, y=ER_roll)) + 
  commonBox +
  labs(x='Date', y=expression(paste('Daily ER (g ', O[2], ' m'^'-2', ' d'^'-1', ')'))) +
  geom_boxplot(fill='sienna4', outlier.size=0.5) 

NEPbox<-ggplot(metab.df, aes(x=Date, group=Date, y=NEP_roll)) + 
  commonBox +
  labs(x='Date', y=expression(paste('Daily NEP (g ', O[2], ' m'^'-2', ' d'^'-1', ')'))) +
  geom_boxplot(fill='grey30', outlier.size=0.5) 


png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'Buoys', 'Metabolism_Boxplot_TS.png'), width=5, height=7, units='in', res=200)


grid.newpage()
boxes<-grid.draw(rbind(ggplotGrob(GPPbox), ggplotGrob(ERbox),  ggplotGrob(NEPbox), size = "first"))

dev.off()
