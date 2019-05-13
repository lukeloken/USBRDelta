



library(lubridate)
library(viridis)
library(grid)
library(gridExtra)
library(ggplot2)
library(dplyr)
library(LakeMetabolizer)

# Project folder where outputs are stored
dropbox_dir<-'C:/Dropbox/USBR Delta Project'

#Where data come from
google_dir<-'C:/GoogleDrive/DeltaNutrientExperiment'


source('R/read_excel_allsheets.R')
source('R/g_legend.R')
source('R/lightmodel.R')

# setwd("C:/Users/Luke/Dropbox/USBR Delta Project")
# 
# source("R/ReadInMasterData.R")

#New data input (after O18 metabolism)
met.final<-read.csv(file = paste(dropbox_dir, "Data", "NutrientExperiment", "Oxygen18", "O18MetabolismEstimates.csv", sep='/'), stringsAsFactors = F)

met.final$Date<-as.Date(met.final$Date)
met.final$Site<-factor(met.final$Site, c('NL70', 'EC2','EC3','EC4','EC5','EC6','EC7','EC8','NL76'))


#Model photic depth using turbiidyt
Turb<-met.final$EXOTurbFNU
PD<-met.final$PhoticDepth_m

logPDmodel<-lm(PD~log10(Turb))

met.final$PD_est<-predict(logPDmodel, newdata=data.frame(log10(Turb)))

png(paste0(dropbox_dir, '/Figures/NutrientExperiment/TurbidityPhoticDepth.png'), width=4, height=4, units='in', res=200)

par(mgp=c(2,0.5,0), tck=-0.02)
par(mar=c(3,3,0.5,0.5))
plot(log10(Turb), PD, xlab='', ylab='', pch=16)

abline(logPDmodel, lty=3)
mtext("1% photic depth (m)", 2,1.5)
mtext(expression(paste(log[10], ' of turbidity (FNU)')),1,1.5)

summary(logPDmodel)

dev.off()


#Calculate day length
sunrise<-sun.rise.set(as.POSIXct(met.final$Date), lat=38.5064)[,1]
sunset<-sun.rise.set(as.POSIXct(met.final$Date), lat=38.5064)[,2]
daylength<-sunset-sunrise

met.final$daylength_h<-as.numeric(sunset-sunrise, unit='hours')

#Sparkling Lake is at 48 degrees N latitude



#Scale incubation metabolism

hypso_df <- read.csv(file=paste(dropbox_dir, 'Data', 'Bathy', 'Hypso_NL74.csv', sep='/'), sep=',')


head(met.final)



met.final$PhoticVolume<-hypso_df$CumArea_m2[match(round(met.final$PD_est,2), hypso_df$depth)]

plot(hypso_df$depth, hypso_df$CumArea_m2)
points(met.final$PhoticVolume~met.final$PD_est, col='red', pch=17)

MaxVolume<-max(hypso_df$CumArea_m2, na.rm=T)


met.final$ER_inc<-met.final$ER

met.final$GPP_inc<-met.final$GPP* #Incubation rate
  met.final$PhoticVolume/MaxVolume* #Percent of volume in photic zone
  met.final$daylength_h/24  #Percent of day with sun
  
met.final$NEP_inc <- met.final$GPP_inc + met.final$ER_inc 


plot(met.final$NEP_inc, met.final$nepv, xlim=c(-0.3, .1), ylim=c(-.3, .1))
abline(0,1)
abline(h=0, lty=2)
abline(v=0, lty=2)


plot(met.final$GPP_inc, met.final$gppv, xlim=c(0,0.8), ylim=c(0,0.8))
abline(0,1)
abline(h=0, lty=2)
abline(v=0, lty=2)


plot(met.final$ER_inc, met.final$rv, xlim=c(-1.5,0), ylim=c(-1.5,0))
abline(0,1)
abline(h=0, lty=2)
abline(v=0, lty=2)


write.table(met.final, file = paste(dropbox_dir, "Data", "NutrientExperiment", "MergedData1.csv", sep='/'), sep=',', row.names = F)
