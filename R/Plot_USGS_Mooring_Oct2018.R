

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

BuoyData$Time.PST<-strftime(force_tz(BuoyData$Timestamp, "Etc/GMT+8"), format="%H:%M:%S")
BuoyData$Date.PST<-strftime(force_tz(BuoyData$Timestamp, "Etc/GMT+8"), format="%Y-%m-%d")

# BuoyData$Time.PST<-strftime(force_tz(BuoyData$Timestamp, "America/Los_Angeles"), format="%H:%M:%S")
# BuoyData$Date.PST<-strftime(force_tz(BuoyData$Timestamp, "America/Los_Angeles"), format="%Y-%m-%d")

BuoyData$DateTime.PST<-as.POSIXct(paste(BuoyData$Date.PST, BuoyData$Time.PST), tz="Etc/GMT+8")

FertDates<-as.POSIXct(c('2018-09-30 00:00:00', '2018-10-07  00:00:00'), tz="Etc/GMT+8")
ferttime<-as.POSIXct("2018-10-01 12:30:00", format='%Y-%m-%d %H:%M:%S', tz="Etc/GMT+8")

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

BuoyData_sub<-BuoyData[which(BuoyData$DateTime.PST<=FertDates[2] & BuoyData$DateTime.PST>=FertDates[1]), ]
BuoyData_fert<-BuoyData[which(BuoyData$'SpCond (µS/cm)'<=875 & BuoyData$'SpCond (µS/cm)'>=865 & BuoyData$DateTime.PST>=ferttime), ]

str(BuoyData)       


png(paste0(dropbox_dir, '/Figures/NutrientExperiment/Buoys/USGSMooring_NO3_Timeseries.png'), width=8, height=4, units='in', res=200)

par(mar=c(3,3,1,3), mgp=c(3,.5, 0), tck=-.02)

plot(BuoyData_sub$DateTime.PST, BuoyData_sub$`SUNA NO3 (uM)`, type='n', ylab='', xlab='', axes=F, ylim=c(4,24), xaxs='i')
abline(v=BuoyData_fert$DateTime.PST, col='lightgreen', lwd=2)
abline(v=ferttime, lty=2, col='darkgreen', lwd=3)

# abline(h=prefertNO3)
# abline(h=postfertNO3)


# axis.POSIXct(1, at=seq.POSIXt(FertDates[1], FertDates[2], by='day'), format='%b %d')

points(BuoyData_sub$DateTime.PST, BuoyData_sub$`SUNA NO3 (uM)`, type='o', pch=16, cex=.6)
axis(2)


par(new=T)

plot(BuoyData_sub$DateTime.PST, BuoyData_sub$`SpCond (µS/cm)`, ylim=c(600,920), type='l', col='blue', axes=F, ylab='', xlab='', xaxs='i')

axis.POSIXct(1, at=seq.POSIXt(FertDates[1], FertDates[2], by='day'), format='%b %d')
axis(4, col.ticks='blue', col.axis='blue')

mtext(expression(paste('SPC (', mu, 'S cm'^'-1', ')')), 4, 2, col='blue')
mtext(expression(paste(NO[3], ' (', mu, 'M)')), 2, 2)
mtext('Date', 1, 2)

box(which='plot')


dev.off()

