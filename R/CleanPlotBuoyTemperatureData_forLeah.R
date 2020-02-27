
#View temperature string data from Nutrient experiment 1
# Date are from NL74 buoy


library(lubridate)
library(plyr)
library(dplyr)
library(viridis)
library(ggplot2)
library(gridExtra)
library(akima)
library(tidyr)
library(rLakeAnalyzer)
library(RcppRoll)

#As of Feb 2020, new single folder where all data and outputs live
onedrive_dir <- 'C:/Users/lloken/OneDrive - DOI/USBR_DWSC'


deploydates<-as.POSIXct(c("2018-09-26 20:00:00", "2018-10-10 17:00:00"), format='%Y-%m-%d %H:%M:%S', tz='America/Los_Angeles')
shipdate<-as.POSIXct(c("2018-10-07 14:00:00", "2018-10-06 11:20:00"), format='%Y-%m-%d %H:%M:%S', tz='America/Los_Angeles')
fertdate<-as.POSIXct("2018-10-01 13:30:00", format='%Y-%m-%d %H:%M:%S', tz='America/Los_Angeles')


#Read in temperature data from outputdata folder
df_alldepth2<- read.csv(file=file.path(onedrive_dir, 'OutputData', 'NutrientExperiment1', 'Buoy', 'Temperature_AllDepths.csv')) %>%
  mutate(DateTime_PDT = as.POSIXct(DateTime_PDT, tz='America/Los_Angeles'))



png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment1', 'Buoy', 'TemperatureTimeseries.png'), width=6, height=3.5, units='in', res=400, bg='white')
par(pch=16)
par(ps=10)
par(mfrow=c(1,1))
par(mar = c(3,3.5,0.5,0.5),mgp=c(1.5,0.4,0),tck=-0.02)
par(lend=2)

print(
ggplot(df_alldepth2, aes(DateTime_PDT, Temp_C, group=as.character(SensorDepth))) + 
  geom_path(aes(colour=SensorDepth)) + 
  theme_bw() + 
  scale_x_datetime(date_breaks = "weeks", date_minor_breaks = "days") + 
  theme(legend.position=c(.85, .7))
)

dev.off()



png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment1', 'Buoy', 'TemperatureTimeseriesTopBottom.png', sep=''), width=6, height=3.5, units='in', res=400, bg='white')
par(pch=16)
par(ps=10)
par(mfrow=c(1,1))
par(mar = c(3,3.5,0.5,0.5),mgp=c(1.5,0.4,0),tck=-0.02)
par(lend=2)

print(
ggplot(df_alldepth2[which(df_alldepth2$SensorDepth %in% c(1,6)),], aes(DateTime_PDT, Temp_C, group=as.character(SensorDepth))) + 
  geom_path(aes(colour=as.character(SensorDepth))) + 
  theme_bw() + 
  scale_x_datetime(date_breaks = "weeks", date_minor_breaks = "days") + 
  theme(legend.position=c(.85, .7), legend.key = element_rect(colour = NA, fill = NA)) + 
  labs(colour='Depth (m)', y='Temp (C)', x='Date') +
  scale_color_manual(values=c("Darkblue", "Darkred")) + 
  theme(legend.background=element_blank())
)

dev.off()

#Vectors of x, y, and z values for filled.contour
dates=df_alldepth2$DateTime_PDT
depths=df_alldepth2$SensorDepth
value=df_alldepth2$Temp_C

#interpolate value between dates/depths prior to plotting with filled.controu
Temp_Est <- interp(dates,depths,value, yo=seq(min(depths),max(depths), by=0.25), xo=seq(ceiling_date(min(dates), "mins"),floor_date(max(dates), "mins"), by='mins'))
names(Temp_Est)<-c('DateTime_PDT', 'SensorDepth', 'Temp_C')

Temp_Est$DateTime_PDT<-seq(ceiling_date(min(dates), "mins"),floor_date(max(dates), "mins"), by='mins')

#Calculate thermocline depth
wrt1<-spread(df_alldepth2[c('DateTime_PDT', 'SensorDepth', 'Temp_C')], key=SensorDepth, value=Temp_C, fill=NA )
wrt3<-wrt1[which(!is.na(rowSums(wrt1[,3:8]))),]


# Calculating rolloing thermocline depth
t.d<-apply(wrt3[,3:8], 1, function (x) thermo.depth(x, depths=1:6, mixed.cutoff = 0.2)[1])

#Calculate upper mixed layer depth (top of metalimnion)
m.d.top <- apply(wrt3[,3:8], 1, function (x) meta.depths(x, depths=1:6, mixed.cutoff = 0.5)[1])
m.d.bot <- apply(wrt3[,3:8], 1, function (x) meta.depths(x, depths=1:6, mixed.cutoff = 0.5)[2])

#for NA values use a thermocline depth of 6m. 
t.d[which(is.na(t.d))]<- 6 
m.d.top[which(is.na(m.d.top))]<- 6 
m.d.bot[which(is.na(m.d.bot))]<- 6 


#run a rolling mean to smooth
t.d.roll<-roll_mean(t.d, n=5, align='center', fill=NA)
m.d.top.roll<-roll_mean(m.d.top, n=5, align='center', fill=NA)
m.d.bot.roll<-roll_mean(m.d.bot, n=5, align='center', fill=NA)


#Look at thermocline and metalimnion demarcations
plot(wrt3[,1],t.d, lwd=2, col='black', type='l', ylim=c(6,0))
points(wrt3[,1],m.d.top, lwd=1, col='red', type='l')
points(wrt3[,1],m.d.bot, lwd=1, col='blue', type='l')

#Look at thermocline and metalimnion demarcations (smoothed)
plot(wrt3[,1],t.d.roll, lwd=2, col='black', type='l', ylim=c(6,0))
points(wrt3[,1],m.d.top.roll, lwd=1, col='red', type='l')
points(wrt3[,1],m.d.bot.roll, lwd=1, col='blue', type='l')

#Look at just metalimnion demarcations (smoothed)
# plot(wrt3[,1],t.d.roll, lwd=2, col='black', type='l', ylim=c(6,0))
plot(wrt3[,1],m.d.top.roll, lwd=2, col='red', type='l', ylim=c(6,0))
points(wrt3[,1],m.d.bot.roll, lwd=1, col='blue', type='l', lty=2)


png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment1', 'Buoy', 'TemperatureVerticalProfileTimeseries.png', sep=''), width=7.15, height=3.5, units='in', res=400, bg='white')
par(pch=16)
par(ps=10)
par(mfrow=c(1,1))
par(mar = c(3,3.5,0,0.5),mgp=c(1.5,0.4,0),tck=-0.02)
par(lend=2)


xticks<-seq(ceiling_date(min(deploydates), "days"),floor_date(max(deploydates), "days"), by='days')
xlabels<-paste(lubridate::month(xticks, label=TRUE, abbr=T), day(xticks), sep=" ")

sunset<-xticks + 60*60*17
sunrise<-xticks + 60*60*8


# Plot
filled.contour(x=Temp_Est[[1]], y=Temp_Est[[2]], z=Temp_Est[[3]], ylim=c(max(depths), min(depths)), nlevels = 40, color.palette = colorRampPalette(c('navy', "blue", "cyan", "green3", "yellow", "orange", "red"), bias = 1, space = "rgb"), ylab="Depth (m)")

# plot with thermocline 
# filled.contour(x=Temp_Est[[1]], y=Temp_Est[[2]], z=Temp_Est[[3]], ylim=c(max(depths), min(depths)), nlevels = 40, color.palette = colorRampPalette(c('navy', "blue", "cyan", "green3", "yellow", "orange", "red"), bias = 1, space = "rgb"), ylab="Depth (m)", plot.axes = { axis(1, at=xticks, labels=xlabels, tck=-0.02); axis(2);lines(wrt3[,1],m.d.top.roll, lwd=2, col='black')})

# plot with mixed layer depth and 5pm lines
# filled.contour(x=Temp_Est[[1]], y=Temp_Est[[2]], z=Temp_Est[[3]], ylim=c(max(depths), min(depths)), nlevels = 40, color.palette = colorRampPalette(c('navy', "blue", "cyan", "green3", "yellow", "orange", "red"), bias = 1, space = "rgb"), ylab="Depth (m)", plot.axes = { axis(1, at=xticks, labels=xlabels, tck=-0.02); axis(2);lines(wrt3[,1],m.d.top.roll, lwd=2, col='black'); axis(1, at=sunset, labels=NA, tck=50, col='white', lwd=2, lty=2); axis(1, at=sunrise, labels=NA, tck=50, col='red', lwd=2, lty=2)})


mtext(expression(paste("Water temperature (", degree, "C)", sep="")), 4, -6)


dev.off()


