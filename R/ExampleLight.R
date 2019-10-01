

#Playing with Par model and Production estimates from literature


#Estimate P using Jassby and/or Cloern model
endtimes<-as.POSIXct(c('2019-09-30 00:00', '2019-10-31 00:00'), tz='America/Los_Angeles')

times<-seq.POSIXt(endtimes[1], endtimes[2], by='15 mins')

# times<-times[order(times)]
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




#From Jassby et al 2002
Pro_Jassby <- function(B, I, k){
  P = 4.61 * 0.728 * B * I / k
  return(P)
}

#From Cloern et al 2007
Pro_Cloern<-function(B, I, k){
  P = 4.6*0.82 * B * I / k
  return(P)
}




#Old script. Do not run


summary(P_Cloern)
Pro_Cloern(B=mean(merge_df$EXOChlugL, na.rm=T), I=mean(PAR, na.rm=T)*3600*24/1000000, k=mean(merge_df$kd_meters+1, na.rm=T))
mean(mean(merge_df$EXOChlugL, na.rm=T)* mean(PAR, na.rm=T)*3600*24/1000000 * (mean(merge_df$PhoticDepth_m, na.rm=T)), na.rm=T)

P_Jassby<-Pro_Jassby(B=mean(merge_df$EXOChlugL, na.rm=T), I=PAR*3600*24/1000000, k=mean(merge_df$kd_meters, na.rm=T))
P_Cloern<-Pro_Cloern(B=mean(merge_df$EXOChlugL, na.rm=T), I=PAR*3600*24/1000000, k=mean(merge_df$kd_meters, na.rm=T))


plot(P_Jassby~times, xlab='Date', ylab='Primary production (mg C m-2, d-1)', ylim=c(0,800), col='black', type='p', pch=20)
points(P_Cloern~times, col='red', type='p', pch=20)
