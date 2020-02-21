



#Code to estimate Production using Jassby and Cloern models


source('R/lightmodel.R')

# p = (4.61 * iota * B * I )/k

cloern_gpp <-function(B, I, k, psi=0.82){
  GPP <- (4.61 * psi * B * I )/k
  return(GPP)
}

deploydates <- as.POSIXct(c("2019-07-02 16:00:00", "2019-09-18 08:00:00"), tz = 'America/Los_Angeles')

times<-seq.POSIXt(deploydates[1], deploydates[2], by='hour')

localHour<-hour(times) +  minute(times)/60 + second(times)/3600
DayOfYear<-yday(times)

Lat=38.5064
Long= -121.5847
Altitude <- 0.001 #using non-zero numbers is better
Aspect <- 0.001 #using non-zero numbers is better
Slope <- 0.05 
TimeZone <- -8
DST <- 1 #if clock is in daylight savings time use "1", if in standard time use "0"
Transmissivity <- 0.999 #use 0.999 for full sun
# timeStep <- 5/60



PAR<-lightModel(Altitude, Aspect, Slope, Lat, localHour, Long, DayOfYear, TimeZone, DST, Transmissivity)


plot(PAR~times, col='red', pch=16, type='l')

PAR_df <- data.frame(times, PAR) %>%
  mutate(Date = as.Date(times, tz='America/Los_Angeles')) %>%
  group_by(Date) %>%
  summarize(PAR = mean(PAR),
            n=n()) %>%
  dplyr::filter(n==24) %>%
  dplyr::select(-n)

merge_df_IncMetab$PAR <- PAR_df$PAR[match(merge_df_IncMetab$Date, PAR_df$Date)]

#g O2 per meter square per day
GPP_C<-cloern_gpp(B=merge_df_IncMetab$chla_mean,
           I=(merge_df_IncMetab$PAR*86400/1000000), #Convert from uE s-1 to E d-1
           k=merge_df_IncMetab$Kd_est) * 32/12.01/1000 #Convert from mg C to g O2

plot(merge_df_IncMetab$Date, GPP_C, ylab='Modeled GPP (g O2 m-2 d-1)')
