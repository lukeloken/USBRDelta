

# Code to make example figure\
# Nitrate concentrations over time in the DWSC experiment 2





T0<-as.POSIXct("2019-07-20 00:00:00", tz='America/Los_Angeles')
T1<-as.POSIXct("2019-07-30 00:00:00", tz='America/Los_Angeles')

fert_times<-as.POSIXct(paste(c("2019-07-22", "2019-07-23", "2019-07-24", "2019-07-25"),  "09:00:00", sep=' '), tz='America/Los_Angeles')

x_times<-seq.POSIXt(T0, T1, by='hours')

x<-seq(1,length(x_times),1)


base_no3<-0.05


y_no3<-rep(base_no3, length(x_times))


plot(x_times, y_no3)
abline(v=fert_times, lty=2)

y <- 1 * (1 - 0.05)^x + 0.05

plot(x, y)
abline(v=fert_times, lty=2)



y_no3_2<-rep(NA, length(x_times))
y_no3_3<-rep(NA, length(x_times))

time<-x[1]
for (time in x){
  
  if (time==1){
    new  <- base_no3
    new2 <- base_no3
    
  } else {
    old<-y_no3_2[time-1]
    old2<-y_no3_3[time-1]
    
    if (x_times[time] %in% fert_times){
      new  <- old-(old-base_no3)*0.05+0.5
      new2 <- 0.5
    } else {
    new  <- (old-(old-base_no3)*0.05)
    new2 <- (old2-(old2-base_no3)*0.05)
    }
    
  }
  
  y_no3_2[time]<-new
  y_no3_3[time]<-new2
  
}

png(paste0(dropbox_dir, '/Figures/NutrientExperiment/NitrateExpectationTimeSeries.png'), width=6, height=4, units='in', res=200)

par(mfrow=c(1,1), mar=c(3,4,0.5,0.5), tck=-0.02, mgp=c(3,0.7,0), xaxs='i')
plot(x_times, y_no3_2, type='l', xlab='', ylab='', las=1)
points(x_times, y_no3_3, type='l', col='red')
# points(x_times, y_no3, col='red')
# abline(v=fert_times, lty=2)
abline(h=base_no3, lty=2, col='grey')
mtext(expression(paste("Nitrate-N (mg N L"^"-1", ")")), 2, 2)

text(x=mean(c(T0, T1), na.rm=T), y=base_no3, 'background', pos=3, col='grey')
text(x=mean(c(T0, T1, T1), na.rm=T), y=0.37, 'expected', pos=3, col='black')

dev.off()


table_out<-data.frame(time=x_times, expected_no3=round(y_no3_2, 3))

write.table(table_out, file=paste(google_dir, '/DataOutputs/ExpectedNO3_FertExp2.csv',  sep=''), row.names=F, sep=',')
