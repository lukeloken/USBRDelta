
library(RcppRoll)
library(tidyr)
library(LakeMetabolizer)
library(akima)
library(zoo)

Temp_df_clean2 <- readRDS(file=paste0(box_dir, "/Outputs/BuoyTempCleaned.RDS"))
Cond_df_clean2 <- readRDS(file=paste0(box_dir, "/Outputs/BuoyCondCleaned.RDS"))
DO_df_clean2   <- readRDS(file=paste0(box_dir, "/Outputs/BuoyDOCleaned.RDS"))

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

#wind and solar raddata
wind_df_summary <- readRDS(file=paste0(dropbox_dir, '/Data/Rdata_SSCN2/WindDataAvg.rds'))

wind_pred <- data.frame(approx(x=wind_df_summary$DateTime, y=wind_df_summary$WS_ms_roll, xo=seq.POSIXt(min(DO_df_clean2$Datetime_PDT_round), max(DO_df_clean2$Datetime_PDT_round), by="5 mins")))
names(wind_pred)<-c("Datetime_PDT_round", "WS_ms")

wind_pred$SolRad_Wsqm <- approx(x=wind_df_summary$DateTime, y=wind_df_summary$SolRad_Wsqm, xo=seq.POSIXt(min(DO_df_clean2$Datetime_PDT_round), max(DO_df_clean2$Datetime_PDT_round), by="5 mins"))$y


#upload hypso data
Depth_df <- readRDS(file=paste0(dropbox_dir, '/Data/Rdata_SSCN2/HypsoCurveNL74.rds'))
# plot(Depth_df$Area_m2, Depth_df$Depth_m, ylim=c(12,0), type='o', pch=16)

#Approximate volume by depth
Depth_pred25 <- data.frame(approx(x=Depth_df$Depth_m, y=.5*(Depth_df$Volume_m3), xo=seq(0, max(Depth_df$Depth_m), by=0.25)))
Total_volume = sum(Depth_pred25$y, na.rm=T)
Surface_area = Depth_df$Area_m2[which(Depth_df$Depth_m==0)]
Mean_depth <- Total_volume/Surface_area

#Volumes of three depth strata
Vol_top <- sum(Depth_pred25$y[which(Depth_pred25$x < mean(DO_depths_num[1:2]))], na.rm=T)
Vol_mid <- sum(Depth_pred25$y[which(Depth_pred25$x < mean(DO_depths_num[2:3]))], na.rm=T) - Vol_top
Vol_bot <- sum(Depth_pred25$y[which(Depth_pred25$x >= mean(DO_depths_num[2:3]))], na.rm=T)


#constants
z.mean<-8
wind.height <- 10
area <- .153

#Average of data used for Oxygen 18 metabolism
ko2md_mean<-2.307741



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
  # 
  #Calculate thermocline depth
  wrt1<-spread(temp_buoy[c('Datetime_PDT_round', 'Depth', 'Temp_C')], key=Depth, value=Temp_C, fill=NA )
  wrt3<-wrt1[which(!is.na(rowSums(wrt1[,2:10]))),]
  
  
  # Calculating rolloing thermocline depth
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
  DO_buoy <- DO_df_clean2 %>%
    dplyr::filter(Site == site_name) %>%
    mutate(Date = as.Date(Datetime_PDT_round, tz='America/Los_Angeles'),
           Date_metab = as.Date(Datetime_PDT_round-6*60*60, tz='America/Los_Angeles')) #Metab date starts at 6am
  
  
  head(DO_buoy)
  
  DO_spread_mgL <- DO_buoy %>%
    dplyr::select(Date_metab, Datetime_PDT_round, Depth, DO_mgL) %>%
    spread(key=Depth, value=DO_mgL) %>%
    drop_na(DO_depths)
  
  DO_spread_mgL_roll <- rollapply(DO_spread_mgL[DO_depths], 7, mean, align='center', fill=NA) 
  colnames(DO_spread_mgL_roll) <- paste0("roll_", DO_depths)
  
  head(DO_spread_mgL_roll)
  
  DO_spread_mgL_join <- bind_cols(DO_spread_mgL, data.frame(DO_spread_mgL_roll))
  
  head(DO_spread_mgL_join)
  
  
  #whole lake depth integrated DO concentration
  DO_spread_mgL_join$DO_WLDI <- (DO_spread_mgL_join$roll_1   * Vol_top +
                                   DO_spread_mgL_join$`roll_2.5` * Vol_mid +
                                   DO_spread_mgL_join$roll_4   * Vol_bot) / Total_volume
  
  DO_spread_mgL_join <- DO_spread_mgL_join %>%
    tidyr::fill(DO_WLDI, .direction='down') %>%
    tidyr::fill(DO_WLDI, .direction='up')
  
  
  
  #Look at daily Whole Water Column DO curves
  # ggplot(aes(x=Datetime_PDT_round, y=DO_WLDI), data=DO_spread_mgL_join) +
  #   geom_vline(xintercept=fert_posix, col='green', linetype='dashed') + 
  #   geom_path() + 
  #   theme_bw() + 
  #   facet_wrap(~Date_metab, scales="free")
  # 

  
  input_df<-left_join(DO_spread_mgL_join, wind_pred) 
  
  input_df <- DO_buoy %>% filter(Depth==1) %>%
    dplyr::select(Datetime_PDT_round, Temp_C) %>%
    right_join(input_df)
  
  
  #Loop through each day
  metab.dates<-unique(input_df$Date_metab)
  metab.out <-data.frame(Date = metab.dates, Site = rep(site_name, length(metab.dates)), GPP=NA, ER=NA, NEP=NA)
  
  m=2
  for (m in 1:length(metab.dates)){
  
    index <- which(input_df$Date_metab==metab.dates[m])
    
  #inputs for metabolism model
  datetime <-  input_df$Datetime_PDT_round[index]
  
  #Only calculate metabolism if sampling was at least most of the day
  if (sum(as.numeric(diff(datetime), units='days'))>.8){
  
  freq<-1/as.numeric((diff(datetime)), units='days')
  
  wtr <- input_df$Temp_C[index]
  
  do_surf <- input_df$`1`[index]
  do_wldi <- input_df$DO_WLDI[index]
  
  do_surf_sat<- o2.at.sat.base(wtr, altitude=0)
  
  wind_ms <- input_df$WS_ms[index]
  SolRad_Wsqm <- input_df$SolRad_Wsqm[index]
  
  #calculating the gas exchange coefficient for O2 empirically from lake area and wind speed:
  u10 <- wind_ms * (1+ (((0.0013^(0.5))/0.41) * (log(10/wind.height)))) #converting wind speed from 3m to 10m height following equation 3 Vachon & Prairie (2013)
  k600cmh <- 2.51 + 1.48*u10 + 0.39*u10*(log10(area)) #k600 in cm/h from table 2 equation B vachon & prairie 2013
  k600md <- k600cmh * 24/100 #converting k600 to m/d
  sco2 <- 1800.6 - (120.1*wtr) + (3.7818 * (wtr^2)) - (0.047608*(wtr^3))#calculating schmidt number for oxygen from Jahne et al (1987)
  ko2md <- k600md * ((sco2/600)^(-2/3)) #converting k600 to ko2 in m/d for use in mass balance
  
  k_buoy<-ko2md
  
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
  if (miss.delta != 0) {
    warning(paste(miss.delta, " missing values (", miss.delta/length(delta.do), 
                  "%) in diff(do.obs)", sep = ""))
  }
  
  #flux in g O2 per day for whole channel. Big volume and area
  gas.flux <- ((do_surf_sat - do_surf) * k_buoy * Surface_area)[-length(datetime)]
  
  #metab is in g O2 per day for whole channel
  delta.do.metab <- (delta.do*freq*Total_volume) - gas.flux 
  
  #Convert to area and summarize by day/night (g O2 per m2 per day)
  delta.do.metab.area <- delta.do.metab/Surface_area  
  
  nep.day <- delta.do.metab.area[isday]
  nep.night <- delta.do.metab.area[isnight]
  ER <- mean(nep.night, na.rm = TRUE)
  NEP <- mean(delta.do.metab.area, na.rm = TRUE)
  GPP <- mean(nep.day, na.rm = TRUE) - ER
  # metab <- data.frame(GPP = GPP, R = R, NEP = NEP)
  
  metab.out$GPP[m]<-GPP
  metab.out$ER[m]<-ER
  metab.out$NEP[m]<-NEP
  }
  }
  
  png(paste0(dropbox_dir, '/Figures/NutrientExperiment2/Buoys/Metabolism/Metabolism_', site_name, '_TS.png'), width=5, height=4, units='in', res=200)
  par(mar=c(3,3,1.5,.5), mgp=c(3,.5,0), tck=-0.02)
  
  plot(metab.out$Date, metab.out$NEP, type='l', ylim=range(metab.out[,3:5], na.rm=T), lwd=.5, xlab='', ylab='', las=1)
  abline(h=0, lty=3, lwd=.5)
  points(metab.out$Date, metab.out$GPP, type='l', col='darkgreen', lwd=.5)
  points(metab.out$Date, metab.out$ER, type='l', col='sienna4', lwd=.5)

  points(metab.out$Date, roll_mean(metab.out$NEP, 3, fill=NA), type='l', lwd=2)
  points(metab.out$Date, roll_mean(metab.out$GPP, 3, fill=NA), type='l', lwd=2, col='darkgreen')
  points(metab.out$Date, roll_mean(metab.out$ER, 3, fill=NA), type='l', lwd=2, col='sienna4')
  
  # points(metab.out$Date, roll_mean(metab.out$NEP, 5, fill=NA), type='l', lwd=3)
  # points(metab.out$Date, roll_mean(metab.out$GPP, 5, fill=NA), type='l', lwd=3, col='darkgreen')
  # points(metab.out$Date, roll_mean(metab.out$ER, 5, fill=NA), type='l', lwd=3, col='sienna4')
  
  
  mtext(expression(paste('g ', O[2], ' m'^'-2', ' d'^'-1')), 2, 1.5)
  mtext(site_name, 3, 0.1)

  legend('topright', inset=0.02, c('GPP', 'NEP', 'ER'), text.col=c('darkgreen', 'black', 'sienna4'), lty=0, bty='n')
  
  dev.off()  
  
  
  metab.list[[buoy_nu]]<-metab.out
  
}

  