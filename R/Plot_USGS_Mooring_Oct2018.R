
#Plot USGS buoy data from NL74

source('R/read_excel_allsheets.R')
source('R/g_legend.R')

dropbox_dir<-'C:/Dropbox/USBR Delta Project'

USGS_buoy_allsheets<-read_excel_allsheets(paste(dropbox_dir, 'Data', 'NutrientExperiment', 'Buoys', 'DWS_Fert_2018-12-17_KO_BB.xlsx', sep='/'))

summary(USGS_buoy_allsheets)

BuoyData<-USGS_buoy_allsheets$`DWS Fert`
RawData<-USGS_buoy_allsheets$RawData

BuoyData$`SUNA NO3 (uM)`<-RawData$`SUNA NO3 (uM)`[match(BuoyData$Timestamp, RawData$Timestamp)]
BuoyData$`SUNA NO3 (mg/L)`<-RawData$`SUNA NO3 (mg/L)`[match(BuoyData$Timestamp, RawData$Timestamp)]

#Mooring is in PST
BuoyData$Time.PST<-strftime(force_tz(BuoyData$Timestamp, "Etc/GMT+8"), format="%H:%M:%S")
BuoyData$Date.PST<-strftime(force_tz(BuoyData$Timestamp, "Etc/GMT+8"), format="%Y-%m-%d")

BuoyData$DateTime.PST<-as.POSIXct(paste(BuoyData$Date.PST, BuoyData$Time.PST), tz="Etc/GMT+8")

RangeDates<-as.POSIXct(c('2018-09-25 00:00:00', '2018-11-20  00:00:00'), tz="Etc/GMT+8")

deploydates<-as.POSIXct(c("2018-09-26 20:00:00", "2018-10-10 17:00:00"), format='%Y-%m-%d %H:%M:%S', tz='America/Los_Angeles')

# shipdate<-as.POSIXct(c("2018-10-07 14:00:00", "2018-10-06 11:20:00", "2018-10-05 02:00:00"), format='%Y-%m-%d %H:%M:%S', tz='America/Los_Angeles')
shipdate<-as.POSIXct(c("2018-10-07 14:00:00", "2018-10-06 11:20:00"), format='%Y-%m-%d %H:%M:%S', tz='America/Los_Angeles')
FertDates<-as.POSIXct(c('2018-09-30 00:00:00', '2018-10-07  00:00:00'), tz="Etc/GMT+8")
ferttime<-as.POSIXct("2018-10-01 12:30:00", format='%Y-%m-%d %H:%M:%S', tz="Etc/GMT+8") #13:30 PDT

prefertNO3<-mean(BuoyData$`SUNA NO3 (uM)`[which(BuoyData$DateTime.PST<=ferttime & BuoyData$DateTime.PST>=(ferttime-3600*24))], na.rm=T)
postfertNO3<-mean(BuoyData$`SUNA NO3 (uM)`[which(BuoyData$DateTime.PST<=(ferttime+3*3600*24) & BuoyData$DateTime.PST>=(ferttime+2*3600*24))], na.rm=T)

diffNO3<-postfertNO3-prefertNO3

#Calculate the volume enriched
# 3000 pounds of CaNO32
# 15% N by weight
# pounds to kg (/2.204)
# kg to kmol (/14)
# kmol to umol (*1000*1000*1000)
uMapplied<-3000*.15/2.204/14*10^9

Liters<-uMapplied/diffNO3
cubicmeters<-Liters/1000
lengthmeters<-cubicmeters/8/153


#Determine how deep the fertilizer sank

meanNO3<-72.4 #uM
meanNO3_mg<-meanNO3*14/1000 #mg/L
width<-120 #m
length<-600 #m

depth<-uMapplied/width/length/1000/meanNO3

#Old way
BuoyData_sub<-BuoyData[which(BuoyData$DateTime.PST<=FertDates[2] & BuoyData$DateTime.PST>=FertDates[1]), ]

#Match timeseries with oxygen data
BuoyData_sub<-BuoyData[which(BuoyData$DateTime.PST<=deploydates[2] & BuoyData$DateTime.PST>=deploydates[1]), ]


BuoyData_fert<-BuoyData[which(BuoyData$'SpCond (µS/cm)'<=875 & BuoyData$'SpCond (µS/cm)'>=865 & BuoyData$DateTime.PST>=ferttime), ]

str(BuoyData)       


png(paste0(dropbox_dir, '/Figures/NutrientExperiment/Buoys/USGSMooring_NO3_Timeseries_2weeks.png'), width=8, height=4, units='in', res=200)

par(mar=c(3,3,1,3), mgp=c(3,.5, 0), tck=-.02)

plot(BuoyData_sub$DateTime.PST, BuoyData_sub$`SUNA NO3 (uM)`, type='n', ylab='', xlab='', axes=F, ylim=c(4,24), xaxs='i')
# abline(v=BuoyData_fert$DateTime.PST, col='lightpink', lwd=2)
# abline(v=ferttime, lty=1, col='hotpink4', lwd=3)
abline(v=BuoyData_fert$DateTime.PST, col='grey85', lwd=2)
abline(v=ferttime, lty=1, col='grey35', lwd=3)
# abline(v=shipdate[2], lty=2, col='darkgrey', lwd=2)

# abline(h=prefertNO3)
# abline(h=postfertNO3)
# axis.POSIXct(1, at=seq.POSIXt(FertDates[1], FertDates[2], by='day'), format='%b %d')

points(BuoyData_sub$DateTime.PST, BuoyData_sub$`SUNA NO3 (uM)`, type='o', pch=16, cex=.6)
axis(2)

par(new=T)

plot(BuoyData_sub$DateTime.PST, BuoyData_sub$`SpCond (µS/cm)`, ylim=c(600,920), type='l', col='blue', axes=F, ylab='', xlab='', xaxs='i')

axis.POSIXct(1, at=seq.POSIXt(deploydates[1]+3600*4, deploydates[2], by='day'), format='%b %d')
axis(4, col.ticks='blue', col.axis='blue')

mtext(expression(paste('SPC (', mu, 'S cm'^'-1', ')')), 4, 2, col='blue')
mtext(expression(paste(NO[3], ' (', mu, 'M)')), 2, 2)
mtext('Date', 1, 2)

box(which='plot')

dev.off()



png(paste0(dropbox_dir, '/Figures/NutrientExperiment/Buoys/USGSMooring_NO3_WL_Timeseries_2weeks.png'), width=6, height=3, units='in', res=200)

par(mar=c(1.5,2.5,0.5,2), mgp=c(3,.4, 0), tck=-.02, ps=8)
par(mfrow=c(1,1))

plot(BuoyData_sub$DateTime.PST, BuoyData_sub$`SUNA NO3 (uM)`*14.0064/1000, type='n', ylab='', xlab='', axes=F, ylim=c(0.05,0.34), xaxs='i')
abline(v=BuoyData_fert$DateTime.PST, col='grey85', lwd=2)
abline(v=ferttime, lty=1, col='grey35', lwd=3)
abline(v=shipdate[1:2], lty=2, col='grey35', lwd=2)
# abline(h=prefertNO3)
# abline(h=postfertNO3)
# axis.POSIXct(1, at=seq.POSIXt(FertDates[1], FertDates[2], by='day'), format='%b %d')

points(BuoyData_sub$DateTime.PST, BuoyData_sub$`SUNA NO3 (uM)`*14.0064/1000, type='o', pch=16, cex=.4)
axis(2, las=1)

par(new=T)

plot(BuoyData_sub$DateTime.PST, BuoyData_sub$`Depth (m)`, ylim=c(-4,5), type='l', col='blue', axes=F, ylab='', xlab='', xaxs='i')

axis.POSIXct(1, at=seq.POSIXt(deploydates[1]+3600*4, deploydates[2], by='day'), format='%b %d', mgp=c(3,.1,0))
axis(4, col.ticks='blue', col.axis='blue', at=seq(2,5,1), las=1)

mtext(expression(paste('                                                 Water level (m)')), 4, 0.75, col='blue')
mtext(expression(paste(NO[3], ' (mg N L'^'-1', ')')), 2, 1.5)
# mtext('Date', 1, 2)

box(which='plot')

dev.off()




png(paste0(dropbox_dir, '/Figures/NutrientExperiment/Buoys/USGSMooring_NO3_Timeseries_FullRecord.png'), width=8, height=12, units='in', res=200)

par(mar=c(1.25,3.5,0.25,0.25), oma=c(1.75,0,0,0), mgp=c(3,.3, 0), tck=-.04)
par(mfrow=c(10,1), ps=10)

plot(BuoyData$DateTime.PST, BuoyData$`SUNA NO3 (uM)`, type='n', ylab='', xlab='', yaxt='n', xaxs='i')
# abline(v=BuoyData_fert$DateTime.PST, col='lightgreen', lwd=2)
abline(v=ferttime, lty=2, col='darkgreen', lwd=3)

# abline(h=prefertNO3)
# abline(h=postfertNO3)
axis.POSIXct(1, at=seq.POSIXt(RangeDates[1], RangeDates[2], by='day'), format='%b %d', labels=NA, tck=-.02)

points(BuoyData$DateTime.PST, BuoyData$`SUNA NO3 (uM)`, type='o', pch=16, cex=.6)
axis(2)

abline(v=ferttime, lty=2, col='darkgreen', lwd=3)

mtext(expression(paste(NO[3], ' (', mu, 'M)')), 2, 2)


plot(BuoyData$DateTime.PST, BuoyData$`Temp (°C)`, type='l', col='darkorange', yaxt='n', ylab='', xlab='', xaxs='i')
# axis.POSIXct(1, at=seq.POSIXt(FertDates[1], FertDates[2], by='day'), format='%b %d')
axis(2, col.ticks='darkorange', col.axis='darkorange')
mtext(expression(paste('Temp (', degree, 'C)')), 2, 2, col='darkorange')
abline(v=ferttime, lty=2, col='darkgreen', lwd=3)
axis.POSIXct(1, at=seq.POSIXt(RangeDates[1], RangeDates[2], by='day'), format='%b %d', labels=NA, tck=-.02)

# par(new=T)

plot(BuoyData$DateTime.PST, BuoyData$`SpCond (µS/cm)`, type='l', col='blue', yaxt='n', ylab='', xlab='', xaxs='i')
# axis.POSIXct(1, at=seq.POSIXt(FertDates[1], FertDates[2], by='day'), format='%b %d')
axis(2, col.ticks='blue', col.axis='blue')
mtext(expression(paste('SPC (', mu, 'S cm'^'-1', ')')), 2, 2, col='blue')
abline(v=ferttime, lty=2, col='darkgreen', lwd=3)
axis.POSIXct(1, at=seq.POSIXt(RangeDates[1], RangeDates[2], by='day'), format='%b %d', labels=NA, tck=-.02)

plot(BuoyData$DateTime.PST, BuoyData$`Depth (m)`, type='l', col='grey10', yaxt='n', ylab='', xlab='', xaxs='i')
# axis.POSIXct(1, at=seq.POSIXt(FertDates[1], FertDates[2], by='day'), format='%b %d')
axis(2, col.ticks='grey10', col.axis='grey10')
mtext(expression(paste('Water level (m)')), 2, 2, col='darkgrey')
abline(v=ferttime, lty=2, col='darkgreen', lwd=3)
axis.POSIXct(1, at=seq.POSIXt(RangeDates[1], RangeDates[2], by='day'), format='%b %d', labels=NA, tck=-.02)

plot(BuoyData$DateTime.PST, BuoyData$`ODO (% sat)`, type='l', col='red', yaxt='n', ylab='', xlab='', xaxs='i')
# axis.POSIXct(1, at=seq.POSIXt(FertDates[1], FertDates[2], by='day'), format='%b %d')
axis(2, col.ticks='red', col.axis='red')
mtext(expression(paste('DO (% sat)')), 2, 2, col='red')
abline(v=ferttime, lty=2, col='darkgreen', lwd=3)
axis.POSIXct(1, at=seq.POSIXt(RangeDates[1], RangeDates[2], by='day'), format='%b %d', labels=NA, tck=-.02)

plot(BuoyData$DateTime.PST, BuoyData$`Turbidity (FNU)`, type='l', col='brown', yaxt='n', ylab='', xlab='', xaxs='i')
# axis.POSIXct(1, at=seq.POSIXt(FertDates[1], FertDates[2], by='day'), format='%b %d')
axis(2, col.ticks='brown', col.axis='brown')
mtext(expression(paste('Turbidity (FNU)')), 2, 2, col='brown')
abline(v=ferttime, lty=2, col='darkgreen', lwd=3)
axis.POSIXct(1, at=seq.POSIXt(RangeDates[1], RangeDates[2], by='day'), format='%b %d', labels=NA, tck=-.02)

plot(BuoyData$DateTime.PST, BuoyData$pH, type='l', col='purple', yaxt='n', ylab='', xlab='', xaxs='i')
# axis.POSIXct(1, at=seq.POSIXt(FertDates[1], FertDates[2], by='day'), format='%b %d')
axis(2, col.ticks='purple', col.axis='purple')
mtext(expression(paste('pH')), 2, 2, col='purple')
abline(v=ferttime, lty=2, col='darkgreen', lwd=3)
axis.POSIXct(1, at=seq.POSIXt(RangeDates[1], RangeDates[2], by='day'), format='%b %d', labels=NA, tck=-.02)


plot(BuoyData$DateTime.PST, BuoyData$`fCHLA (µg/L)`, type='l', col='darkgreen', yaxt='n', ylab='', xlab='', xaxs='i')
# axis.POSIXct(1, at=seq.POSIXt(FertDates[1], FertDates[2], by='day'), format='%b %d')
axis(2, col.ticks='darkgreen', col.axis='darkgreen')
mtext(expression(paste('ChlA (', mu, 'g L'^'-1', ')')), 2, 2, col='darkgreen')
abline(v=ferttime, lty=2, col='darkgreen', lwd=3)
axis.POSIXct(1, at=seq.POSIXt(RangeDates[1], RangeDates[2], by='day'), format='%b %d', labels=NA, tck=-.02)

plot(BuoyData$DateTime.PST, BuoyData$`BGA-PC (µg/L)`, type='l', col='cyan', yaxt='n', ylab='', xlab='', xaxs='i')
# axis.POSIXct(1, at=seq.POSIXt(FertDates[1], FertDates[2], by='day'), format='%b %d')
axis(2, col.ticks='cyan', col.axis='cyan')
mtext(expression(paste('BGA-PC (', mu, 'g L'^'-1', ')')), 2, 2, col='cyan')
abline(v=ferttime, lty=2, col='darkgreen', lwd=3)
axis.POSIXct(1, at=seq.POSIXt(RangeDates[1], RangeDates[2], by='day'), format='%b %d', labels=NA, tck=-.02)

plot(BuoyData$DateTime.PST, BuoyData$`fDOM (QSE)`, type='l', col='grey10', yaxt='n', ylab='', xlab='', xaxs='i')
# axis.POSIXct(1, at=seq.POSIXt(FertDates[1], FertDates[2], by='day'), format='%b %d')
axis(2, col.ticks='grey10', col.axis='grey10')
mtext(expression(paste('fDOM (QSE)')), 2, 2, col='grey10')
abline(v=ferttime, lty=2, col='darkgreen', lwd=3)
axis.POSIXct(1, at=seq.POSIXt(RangeDates[1], RangeDates[2], by='day'), format='%b %d', labels=NA, tck=-.02)


mtext('Date', 1, 2)

box(which='plot')

dev.off()


