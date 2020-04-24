
library(dplyr)


deploy_dates<-as.POSIXct(c("2019-07-12 17:00:00", "2019-08-27 00:00:00"), tz="Etc/GMT+8")
deploy_dates_round = round(deploy_dates, 'day')

huey_dirty_dates<-as.POSIXct(c("2019-08-12 00:00:00", "2019-08-16 12:00:00"), tz="Etc/GMT+8")


# fert_posix<-as.POSIXct(c("2019-07-22 08:00", "2019-07-23 08:00","2019-07-24 08:00", "2019-07-25 08:00", "2019-08-05 08:00", "2019-08-06 08:00","2019-08-07 08:00", "2019-08-08 08:00"), format='%Y-%m-%d %H:%M', tz="Etc/GMT+8")




#plot NO3 data at huey
# huey_files<-list.files(paste0(box_dir, "/Data/BuoyData/USGS"))

huey_files<-list.files(file.path(onedrive_dir, "RawData", "NutrientExperiment2", "BuoyData", "USGS"))


huey_list<-list()

i=1
for (i in 1:length(huey_files)){

huey_data_i<-read.csv(file.path(onedrive_dir, "RawData", "NutrientExperiment2", "BuoyData", "USGS", huey_files[i]), header=F, skip = 4)

huey_header_i<-names(read.csv(file.path(onedrive_dir, "RawData", "NutrientExperiment2", "BuoyData", "USGS", huey_files[i]), header=T, skip = 1))

names(huey_data_i)<-huey_header_i

huey_data_i$DateTime<-as.POSIXct(huey_data_i$Time.America.Yakutat.UTC.08.00, format='%m-%d-%Y %H:%M:%S', tz="Etc/GMT+8")

huey_list[[i]]<-huey_data_i

}

huey_df<-ldply(huey_list, data.frame)
head(huey_df)

huey_df_distinct<-huey_df %>% 
  distinct() %>%
  arrange(DateTime) %>%
  filter(DateTime>deploy_dates[1] & DateTime<deploy_dates[2])
  

#Clean data
huey_df_distinct$Nitrate.mg.L[huey_df_distinct$Nitrate.mg.L==(-1)]<-NA
  
huey_df_distinct$Nitrate.mg.L[huey_df_distinct$DateTime>huey_dirty_dates[1] & huey_df_distinct$DateTime<huey_dirty_dates[2] & huey_df_distinct$Nitrate.mg.L>(0.15)]<-NA

huey_df_distinct$Nitrate.mg.L[huey_df_distinct$DateTime>huey_dirty_dates[1] & huey_df_distinct$DateTime<huey_dirty_dates[2] & huey_df_distinct$Nitrate.mg.L<(0.06)]<-NA


#chlA
huey_df_distinct$Chlorophyll.ug.L[huey_df_distinct$DateTime>huey_dirty_dates[1] & huey_df_distinct$DateTime<huey_dirty_dates[2]]<-NA
huey_df_distinct$Chlorophyll.RFU[huey_df_distinct$DateTime>huey_dirty_dates[1] & huey_df_distinct$DateTime<huey_dirty_dates[2]]<-NA
huey_df_distinct$Chlorophyll.ug.L[huey_df_distinct$Chlorophyll.ug.L>17]<-NA

#BGA
huey_df_distinct$BGA.PC.ug.L[huey_df_distinct$DateTime>huey_dirty_dates[1] & huey_df_distinct$DateTime<huey_dirty_dates[2]]<-NA
huey_df_distinct$BGA.PC.RFU[huey_df_distinct$DateTime>huey_dirty_dates[1] & huey_df_distinct$DateTime<huey_dirty_dates[2]]<-NA
huey_df_distinct$BGA.PC.ug.L[huey_df_distinct$BGA.PC.ug.L>2]<-NA

#Turb
huey_df_distinct$Turbidity.[huey_df_distinct$DateTime>huey_dirty_dates[1] & huey_df_distinct$DateTime<huey_dirty_dates[2]]<-NA


#Temp
huey_df_distinct$Temperature[huey_df_distinct$DateTime>huey_dirty_dates[1] & huey_df_distinct$DateTime<huey_dirty_dates[2] & huey_df_distinct$Temperature<(24)]<-NA


#pH
huey_df_distinct$pH[huey_df_distinct$pH<8.25]<-NA

# plot(huey_df$DateTime, huey_df$ODOSat, type='l')
plot(huey_df_distinct$DateTime, huey_df_distinct$ODOSat, type='l', col='red')

abline(v=fert_posix, col='green')
head(huey_df_distinct)
tail(huey_df_distinct)


write.csv(huey_df_distinct, file=file.path(onedrive_dir, 'OutputData', 'NutrientExperiment2', 'USGSBuoys', 'Huey_cleaned.csv'), row.names=F)
saveRDS(huey_df_distinct , file=file.path(onedrive_dir, 'RData', 'NutrientExperiment2', 'USGSBuoys', 'Huey_cleaned.rds'))



png(file.path(onedrive_dir, "Figures", "NutrientExperiment2", "USGSBuoys", "Huey_NO3_SSCN2.png"), width=8, height=4, units='in', res=200)

par(mar=c(3,3.5,0.5, 0.5), mgp=c(3,0.5,0), tck=-0.02)

plot(huey_df_distinct$DateTime, huey_df_distinct$Nitrate.mg.L, xlab='', ylab='', las=1, type='l', lwd=2)

# points(huey_df_distinct$DateTime, huey_df_distinct$Nitrate.mg.L, xlab='', ylab='', las=1, type='p', pch=20, cex=1, col='gray')

abline(v=fert_posix, lty=3, col='green', lwd=2)

points(huey_df_distinct$DateTime, huey_df_distinct$Nitrate.mg.L, xlab='', ylab='', las=1, type='l', lwd=2)

mtext(expression(paste(NO[3], ' (mg N L'^'-1', ')')), 2, 2)
box(which='plot')

dev.off()




png(file.path(onedrive_dir, "Figures", "NutrientExperiment2", "USGSBuoys", "Huey_ChlA_SSCN2.png"), width=8, height=4, units='in', res=200)

par(mar=c(3,3.5,0.5, 0.5), mgp=c(3,0.5,0), tck=-0.02)

plot(huey_df_distinct$DateTime, huey_df_distinct$Chlorophyll.ug.L, xlab='', ylab='', las=1, type='l', lwd=2)

# points(huey_df_distinct$DateTime, huey_df_distinct$Chlorophyll.ug.L, xlab='', ylab='', las=1, type='p', pch=20, cex=1, col='gray')

abline(v=fert_posix, lty=3, col='green', lwd=2)

points(huey_df_distinct$DateTime, huey_df_distinct$Chlorophyll.ug.L, xlab='', ylab='', las=1, type='l', lwd=2)

mtext(expression(paste("Chl ", italic(a), ' (', mu, 'g L'^'-1', ')')), 2, 2)
box(which='plot')

dev.off()



png(file.path(onedrive_dir, "Figures", "NutrientExperiment2", "USGSBuoys", "Huey_Temp_SSCN2.png"), width=8, height=4, units='in', res=200)

par(mar=c(3,3.5,0.5, 0.5), mgp=c(3,0.5,0), tck=-0.02)

plot(huey_df_distinct$DateTime, huey_df_distinct$Temperature, xlab='', ylab='', las=1, type='l', lwd=2)

# points(huey_df_distinct$DateTime, huey_df_distinct$Temperature, xlab='', ylab='', las=1, type='p', pch=20, cex=1, col='gray')

abline(v=fert_posix, lty=3, col='green', lwd=2)

points(huey_df_distinct$DateTime, huey_df_distinct$Temperature, xlab='', ylab='', las=1, type='l', lwd=2)

mtext(expression(paste("Temperature (", degree, 'C)')), 2, 2)
box(which='plot')

dev.off()



png(file.path(onedrive_dir, "Figures", "NutrientExperiment2", "USGSBuoys", "Huey_DO_SSCN2.png"), width=8, height=4, units='in', res=200)

par(mar=c(3,3.5,0.5, 0.5), mgp=c(3,0.5,0), tck=-0.02)

plot(huey_df_distinct$DateTime, huey_df_distinct$ODO, xlab='', ylab='', las=1, type='l', lwd=2)
# points(huey_df_distinct$DateTime, huey_df_distinct$ODO, xlab='', ylab='', las=1, type='p', pch=20, cex=1, col='gray')

abline(v=fert_posix, lty=3, col='green', lwd=2)
points(huey_df_distinct$DateTime, huey_df_distinct$ODO, xlab='', ylab='', las=1, type='l', lwd=2)

mtext(expression(paste("Dissolved Oxygen (mg ", O[2], ' L'^'-1', ')')), 2, 2)
box(which='plot')

dev.off()




png(file.path(onedrive_dir, "Figures", "NutrientExperiment2", "USGSBuoys", "Huey_Temp_ChlA_DO_SSCN2_fertweek1.png"), width=8, height=12, units='in', res=200)

par(mar=c(3,3.5,0.5, 0.5), mgp=c(3,0.5,0), tck=-0.02, mfrow=c(3,1))



plot(huey_df_distinct$DateTime, huey_df_distinct$Temperature, xlab='', ylab='', las=1, type='l', lwd=2, xlim=c((fert_posix[1]-3600*24*2), (fert_posix[4]+3600*24*3)))

# points(huey_df_distinct$DateTime, huey_df_distinct$Temperature, xlab='', ylab='', las=1, type='p', pch=20, cex=1, col='gray')

abline(v=fert_posix, lty=3, col='green', lwd=2)

points(huey_df_distinct$DateTime, huey_df_distinct$Temperature, xlab='', ylab='', las=1, type='l', lwd=2)

mtext(expression(paste("Temperature (", degree, 'C)')), 2, 2)
box(which='plot')

axis.POSIXct(1, x=seq.POSIXt(fert_posix[1], fert_posix[4], by='day'))




plot(huey_df_distinct$DateTime, huey_df_distinct$Chlorophyll.ug.L, xlab='', ylab='', las=1, type='l', lwd=2, xlim=c((fert_posix[1]-3600*24*2), (fert_posix[4]+3600*24*3)), ylim=c(0,20))

# points(huey_df_distinct$DateTime, huey_df_distinct$Chlorophyll.ug.L, xlab='', ylab='', las=1, type='p', pch=20, cex=1, col='gray')

abline(v=fert_posix, lty=3, col='green', lwd=2)

points(huey_df_distinct$DateTime, huey_df_distinct$Chlorophyll.ug.L, xlab='', ylab='', las=1, type='l', lwd=2)

mtext(expression(paste("Chl ", italic(a), ' (', mu, 'g L'^'-1', ')')), 2, 2)
box(which='plot')

axis.POSIXct(1, x=seq.POSIXt(fert_posix[1], fert_posix[4], by='day'))



plot(huey_df_distinct$DateTime, huey_df_distinct$ODO, xlab='', ylab='', las=1, type='l', lwd=2, xlim=c((fert_posix[1]-3600*24*2), (fert_posix[4]+3600*24*3)))
# points(huey_df_distinct$DateTime, huey_df_distinct$ODO, xlab='', ylab='', las=1, type='p', pch=20, cex=1, col='gray')

abline(v=fert_posix, lty=3, col='green', lwd=2)
points(huey_df_distinct$DateTime, huey_df_distinct$ODO, xlab='', ylab='', las=1, type='l', lwd=2)

mtext(expression(paste("Dissolved Oxygen (mg ", O[2], ' L'^'-1', ')')), 2, 2)
box(which='plot')




dev.off()






png(file.path(onedrive_dir, "Figures", "NutrientExperiment2", "USGSBuoys", "Huey_Temp_ChlA_DO_SSCN2_fertweek2.png"), width=8, height=12, units='in', res=200)

par(mar=c(3,3.5,0.5, 0.5), mgp=c(3,0.5,0), tck=-0.02, mfrow=c(3,1))



plot(huey_df_distinct$DateTime, huey_df_distinct$Temperature, xlab='', ylab='', las=1, type='l', lwd=2, xlim=c((fert_posix[5]-3600*24*2), (fert_posix[8]+3600*24*3)))

# points(huey_df_distinct$DateTime, huey_df_distinct$Temperature, xlab='', ylab='', las=1, type='p', pch=20, cex=1, col='gray')

abline(v=fert_posix, lty=3, col='green', lwd=2)

points(huey_df_distinct$DateTime, huey_df_distinct$Temperature, xlab='', ylab='', las=1, type='l', lwd=2)

mtext(expression(paste("Temperature (", degree, 'C)')), 2, 2)
box(which='plot')

axis.POSIXct(1, x=seq.POSIXt(fert_posix[5], fert_posix[8], by='day'))


plot(huey_df_distinct$DateTime, huey_df_distinct$Chlorophyll.ug.L, xlab='', ylab='', las=1, type='l', lwd=2, xlim=c((fert_posix[5]-3600*24*2), (fert_posix[8]+3600*24*3)), ylim=c(0,20))

# points(huey_df_distinct$DateTime, huey_df_distinct$Chlorophyll.ug.L, xlab='', ylab='', las=1, type='p', pch=20, cex=1, col='gray')

abline(v=fert_posix, lty=3, col='green', lwd=2)

points(huey_df_distinct$DateTime, huey_df_distinct$Chlorophyll.ug.L, xlab='', ylab='', las=1, type='l', lwd=2)

mtext(expression(paste("Chl ", italic(a), ' (', mu, 'g L'^'-1', ')')), 2, 2)
box(which='plot')

axis.POSIXct(1, x=seq.POSIXt(fert_posix[5], fert_posix[8], by='day'))


plot(huey_df_distinct$DateTime, huey_df_distinct$ODO, xlab='', ylab='', las=1, type='l', lwd=2, xlim=c((fert_posix[5]-3600*24*2), (fert_posix[8]+3600*24*3)))
# points(huey_df_distinct$DateTime, huey_df_distinct$ODO, xlab='', ylab='', las=1, type='p', pch=20, cex=1, col='gray')

abline(v=fert_posix, lty=3, col='green', lwd=2)
points(huey_df_distinct$DateTime, huey_df_distinct$ODO, xlab='', ylab='', las=1, type='l', lwd=2)

mtext(expression(paste("Dissolved Oxygen (mg ", O[2], ' L'^'-1', ')')), 2, 2)
box(which='plot')


dev.off()







#Figure for Research Short. 
# No major ecological problems


png(file.path(onedrive_dir, "Figures", "NutrientExperiment2", "USGSBuoys", "Huey_EcosystemHealth_SSCN2.png"), width=6, height=9, units='in', res=200)

par(mar=c(2,4,0.5, 0.5), mgp=c(3,0.5,0), tck=-0.02, mfrow=c(5,1))

#Nitrate
plot(huey_df_distinct$DateTime, huey_df_distinct$Nitrate.mg.L, xlab='', ylab='', las=1, type='l', lwd=2)

# points(huey_df_distinct$DateTime, huey_df_distinct$Nitrate.mg.L, xlab='', ylab='', las=1, type='p', pch=20, cex=1, col='gray')

abline(v=fert_posix, lty=3, col='green', lwd=2)

points(huey_df_distinct$DateTime, huey_df_distinct$Nitrate.mg.L, xlab='', ylab='', las=1, type='l', lwd=2)

mtext(expression(paste(NO[3], ' (mg N L'^'-1', ')')), 2, 2)
box(which='plot')

#ODO
plot(huey_df_distinct$DateTime, huey_df_distinct$ODO, xlab='', ylab='', las=1, type='l', lwd=2)
# points(huey_df_distinct$DateTime, huey_df_distinct$ODO, xlab='', ylab='', las=1, type='p', pch=20, cex=1, col='gray')

abline(v=fert_posix, lty=3, col='green', lwd=2)
points(huey_df_distinct$DateTime, huey_df_distinct$ODO, xlab='', ylab='', las=1, type='l', lwd=2)

mtext(expression(paste("Dissolved oxygen (mg  ", O[2], ' L'^'-1', ')')), 2, 2)
box(which='plot')

#pH
plot(huey_df_distinct$DateTime, huey_df_distinct$pH, xlab='', ylab='', las=1, type='l', lwd=2)
# points(huey_df_distinct$DateTime, huey_df_distinct$pH, xlab='', ylab='', las=1, type='p', pch=20, cex=1, col='gray')

abline(v=fert_posix, lty=3, col='green', lwd=2)
points(huey_df_distinct$DateTime, huey_df_distinct$pH, xlab='', ylab='', las=1, type='l', lwd=2)

mtext(expression(paste("pH")), 2, 2)
box(which='plot')

#BGA
plot(huey_df_distinct$DateTime, huey_df_distinct$BGA.PC.ug.L, xlab='', ylab='', las=1, type='l', lwd=2)

# points(huey_df_distinct$DateTime, huey_df_distinct$Temperature, xlab='', ylab='', las=1, type='p', pch=20, cex=1, col='gray')

abline(v=fert_posix, lty=3, col='green', lwd=2)

points(huey_df_distinct$DateTime, huey_df_distinct$Temperature, xlab='', ylab='', las=1, type='l', lwd=2)

mtext(expression(paste("Blue green algae (", mu, 'g L'^'-1', ')')), 2, 2)
box(which='plot')

axis.POSIXct(1, x=seq.POSIXt(fert_posix[5], fert_posix[8], by='day'))


#ChlA
plot(huey_df_distinct$DateTime, huey_df_distinct$Chlorophyll.ug.L, xlab='', ylab='', las=1, type='l', lwd=2)

# points(huey_df_distinct$DateTime, huey_df_distinct$Chlorophyll.ug.L, xlab='', ylab='', las=1, type='p', pch=20, cex=1, col='gray')

abline(v=fert_posix, lty=3, col='green', lwd=2)

points(huey_df_distinct$DateTime, huey_df_distinct$Chlorophyll.ug.L, xlab='', ylab='', las=1, type='l', lwd=2)

mtext(expression(paste("Chl ", italic(a), " (", mu, 'g L'^'-1', ')')), 2, 2)
box(which='plot')

axis.POSIXct(1, x=seq.POSIXt(fert_posix[5], fert_posix[8], by='day'))




dev.off()






#Figure for metabolism paper
#Important pieces from Huey
png(file.path(onedrive_dir, "Figures", "NutrientExperiment2", "USGSBuoys", "Huey_MetabPaper_SSCN2.png"), width=6, height=7, units='in', res=200)

par(mar=c(2,4.5,0.5, 0.5), mgp=c(3,0.5,0), tck=-0.02, mfrow=c(4,1), oma=c(1,0,1.5,0))

#Nitrate
plot(huey_df_distinct$DateTime, huey_df_distinct$Nitrate.mg.L, xlab='', ylab='', las=1, type='l', lwd=2, xaxt='n', ylim=c(0.05,1.4))

abline(v=fert_posix, lty=3, col='green', lwd=2)
axis.POSIXct(1, at=seq.POSIXt(deploy_dates_round[1], deploy_dates_round[2], by='week'), format='%b %d', tck=-.04)
axis.POSIXct(1, at=seq.POSIXt(deploy_dates_round[1], deploy_dates_round[2], by='day'), format='%b %d', labels=NA, tck=-.01)

points(huey_df_distinct$DateTime, huey_df_distinct$Nitrate.mg.L, xlab='', ylab='', las=1, type='l', lwd=2)

mtext(expression(paste(NO[3], ' (mg N L'^'-1', ')')), 2, 2.5)
box(which='plot')

#Turbidity
plot(huey_df_distinct$DateTime, huey_df_distinct$Turbidity., xlab='', ylab='', las=1, type='l', lwd=2, col='sienna4', xaxt='n', ylim=c(0,60))

abline(v=fert_posix, lty=3, col='green', lwd=2)
axis.POSIXct(1, at=seq.POSIXt(deploy_dates_round[1], deploy_dates_round[2], by='week'), format='%b %d', tck=-.04)
axis.POSIXct(1, at=seq.POSIXt(deploy_dates_round[1], deploy_dates_round[2], by='day'), format='%b %d', labels=NA, tck=-.01)

points(huey_df_distinct$DateTime, huey_df_distinct$Turbidity., xlab='', ylab='', las=1, type='l', lwd=2, col='sienna4')

mtext(expression(paste("Turbidity (FNU)")), 2, 2.5)
box(which='plot')

#BGA
plot(huey_df_distinct$DateTime, huey_df_distinct$BGA.PC.ug.L, xlab='', ylab='', las=1, type='l', lwd=2, col='darkturquoise', xaxt='n', ylim=c(-.5,2))

abline(v=fert_posix, lty=3, col='green', lwd=2)
axis.POSIXct(1, at=seq.POSIXt(deploy_dates_round[1], deploy_dates_round[2], by='week'), format='%b %d', tck=-.04)
axis.POSIXct(1, at=seq.POSIXt(deploy_dates_round[1], deploy_dates_round[2], by='day'), format='%b %d', labels=NA, tck=-.01)

points(huey_df_distinct$DateTime, huey_df_distinct$Temperature, xlab='', ylab='', las=1, type='l', lwd=2, col='darkturquoise')

mtext(expression(paste("Blue green algae (", mu, 'g L'^'-1', ')')), 2, 2.5)
box(which='plot')


#ChlA
plot(huey_df_distinct$DateTime, huey_df_distinct$Chlorophyll.ug.L, xlab='', ylab='', las=1, type='l', lwd=2, col='darkgreen', xaxt='n', ylim=c(1,16))

abline(v=fert_posix, lty=3, col='green', lwd=2)
axis.POSIXct(1, at=seq.POSIXt(deploy_dates_round[1], deploy_dates_round[2], by='week'), format='%b %d', tck=-.04)
axis.POSIXct(1, at=seq.POSIXt(deploy_dates_round[1], deploy_dates_round[2], by='day'), format='%b %d', labels=NA, tck=-.01)

points(huey_df_distinct$DateTime, huey_df_distinct$Chlorophyll.ug.L, xlab='', ylab='', las=1, type='l', lwd=2, col='darkgreen')

mtext(expression(paste("Chl ", italic(a), " (", mu, 'g L'^'-1', ')')), 2, 2.5)
box(which='plot')

mtext("Experiment 2", 3, 0, outer=T)
mtext("2019", 1, 0, outer=T)


dev.off()






#Figure for metabolism paper 1 years

#Important pieces from Huey
png(file.path(onedrive_dir, "Figures", "NutrientExperiment2", "USGSBuoys", "Huey_MetabPaper_SSCN2_3panel.png"), width=4, height=5, units='in', res=200)

par(mar=c(1.5,4,0.5, 0.5), mgp=c(3,0.5,0), tck=-0.02, mfcol=c(3,1), oma=c(1.5,0,1.5,0), ps=10)


#2019

#Nitrate
plot(huey_df_distinct$DateTime, huey_df_distinct$Nitrate.mg.L, xlab='', ylab='', las=1, type='l', lwd=2, xaxt='n', ylim=c(0.05,1.4))

abline(v=fert_posix, lty=3, col='green', lwd=2)
axis.POSIXct(1, at=seq.POSIXt(deploy_dates_round[1], deploy_dates_round[2], by='week'), format='%b %d', tck=-.04)
axis.POSIXct(1, at=seq.POSIXt(deploy_dates_round[1], deploy_dates_round[2], by='day'), format='%b %d', labels=NA, tck=-.01)

points(huey_df_distinct$DateTime, huey_df_distinct$Nitrate.mg.L, xlab='', ylab='', las=1, type='l', lwd=2)

mtext(expression(paste(NO[3], ' (mg N L'^'-1', ')')), 2, 2)

# mtext("Experiment 2", 3, .25)

box(which='plot')

#Turbidity
plot(huey_df_distinct$DateTime, huey_df_distinct$Turbidity., xlab='', ylab='', las=1, type='l', lwd=2, col='sienna4', xaxt='n', ylim=c(0,60))

abline(v=fert_posix, lty=3, col='green', lwd=2)
axis.POSIXct(1, at=seq.POSIXt(deploy_dates_round[1], deploy_dates_round[2], by='week'), format='%b %d', tck=-.04)
axis.POSIXct(1, at=seq.POSIXt(deploy_dates_round[1], deploy_dates_round[2], by='day'), format='%b %d', labels=NA, tck=-.01)

points(huey_df_distinct$DateTime, huey_df_distinct$Turbidity., xlab='', ylab='', las=1, type='l', lwd=2, col='sienna4')

mtext(expression(paste("Turbidity (FNU)")), 2, 2)
box(which='plot')

#BGA
# plot(huey_df_distinct$DateTime, huey_df_distinct$BGA.PC.ug.L, xlab='', ylab='', las=1, type='l', lwd=2, col='darkturquoise', xaxt='n', ylim=c(-.5,2))
# 
# abline(v=fert_posix, lty=3, col='green', lwd=2)
# axis.POSIXct(1, at=seq.POSIXt(deploy_dates_round[1], deploy_dates_round[2], by='week'), format='%b %d', tck=-.04)
# axis.POSIXct(1, at=seq.POSIXt(deploy_dates_round[1], deploy_dates_round[2], by='day'), format='%b %d', labels=NA, tck=-.01)
# 
# points(huey_df_distinct$DateTime, huey_df_distinct$Temperature, xlab='', ylab='', las=1, type='l', lwd=2, col='darkturquoise')
# 
# mtext(expression(paste("Blue green algae ( ", mu, 'g L'^'-1', ')')), 2, 2)
# box(which='plot')


#ChlA
plot(huey_df_distinct$DateTime, huey_df_distinct$Chlorophyll.ug.L, xlab='', ylab='', las=1, type='l', lwd=2, col='darkgreen', xaxt='n', ylim=c(1,16))

abline(v=fert_posix, lty=3, col='green', lwd=2)
axis.POSIXct(1, at=seq.POSIXt(deploy_dates_round[1], deploy_dates_round[2], by='week'), format='%b %d', tck=-.04)
axis.POSIXct(1, at=seq.POSIXt(deploy_dates_round[1], deploy_dates_round[2], by='day'), format='%b %d', labels=NA, tck=-.01)

points(huey_df_distinct$DateTime, huey_df_distinct$Chlorophyll.ug.L, xlab='', ylab='', las=1, type='l', lwd=2, col='darkgreen')

mtext(expression(paste("Chl ", italic(a), " (", mu, 'g L'^'-1', ')')), 2, 2)
box(which='plot')

mtext("2019", 1, 1.75)



dev.off()



# 
# #Figure for metabolism paper two years
# # No BGA
# #Important pieces from Huey
# png(paste0(dropbox_dir, '/Figures/NutrientExperiment2/USGSBuoys/Huey_MetabPaper_2Years_3Vars.png'), width=7.5, height=5, units='in', res=200)
# 
# par(mar=c(1.5,2,0.5, 0.5), mgp=c(3,0.5,0), tck=-0.02, mfcol=c(3,2), oma=c(1.5,2,1.5,0), ps=10)
# 
# #2018
# 
# # #Nitrate
# plot(BuoyData$DateTime.PST, BuoyData$`SUNA NO3 (mg/L)`, xlab='', ylab='', las=1, type='l', lwd=2, xaxt='n', ylim=c(0.05,1.4))
# 
# abline(v=ferttime, lty=3, col='green', lwd=2)
# axis.POSIXct(1, at=seq.POSIXt(RangeDates[1], RangeDates[2], by='week'), format='%b %d', tck=-.04)
# axis.POSIXct(1, at=seq.POSIXt(RangeDates[1], RangeDates[2], by='day'), format='%b %d', labels=NA, tck=-.01)
# 
# points(BuoyData$DateTime.PST, BuoyData$`SUNA NO3 (mg/L)`, xlab='', ylab='', las=1, type='l', lwd=2)
# 
# mtext(expression(paste(NO[3], ' (mg N L'^'-1', ')')), 2, 2)
# box(which='plot')
# 
# mtext("Experiment 1", 3, .25)
# 
# # #Turbidity
# plot(BuoyData$DateTime.PST, BuoyData$`Turbidity (FNU)`, xlab='', ylab='', las=1, type='l', lwd=2, col='sienna4', xaxt='n', ylim=c(0,60))
# 
# abline(v=ferttime, lty=3, col='green', lwd=2)
# axis.POSIXct(1, at=seq.POSIXt(RangeDates[1], RangeDates[2], by='week'), format='%b %d', tck=-.04)
# axis.POSIXct(1, at=seq.POSIXt(RangeDates[1], RangeDates[2], by='day'), format='%b %d', labels=NA, tck=-.01)
# 
# points(BuoyData$DateTime.PST, BuoyData$`Turbidity (FNU)`, xlab='', ylab='', las=1, type='l', lwd=2, col='sienna4')
# 
# mtext(expression(paste("Turbidity (FNU)")), 2, 2)
# box(which='plot')
# 
# 
# # #BGA
# # plot(BuoyData$DateTime.PST, BuoyData$`BGA-PC (µg/L)`, xlab='', ylab='', las=1, type='l', lwd=2, col='darkturquoise', xaxt='n', ylim=c(-.5,2))
# # 
# # abline(v=ferttime, lty=3, col='green', lwd=2)
# # axis.POSIXct(1, at=seq.POSIXt(RangeDates[1], RangeDates[2], by='week'), format='%b %d', tck=-.04)
# # axis.POSIXct(1, at=seq.POSIXt(RangeDates[1], RangeDates[2], by='day'), format='%b %d', labels=NA, tck=-.01)
# # 
# # points(BuoyData$DateTime.PST, BuoyData$`BGA-PC (µg/L)`, xlab='', ylab='', las=1, type='l', lwd=2, col='darkturquoise')
# # 
# # mtext(expression(paste("Blue green algae ( ", mu, 'g L'^'-1', ')')), 2, 2)
# # box(which='plot')
# 
# 
# # #ChlA
# plot(BuoyData$DateTime.PST, BuoyData$`fCHLA (µg/L)`, xlab='', ylab='', las=1, type='l', lwd=2, col='darkgreen', xaxt='n', ylim=c(1,16))
# 
# abline(v=ferttime, lty=3, col='green', lwd=2)
# axis.POSIXct(1, at=seq.POSIXt(RangeDates[1], RangeDates[2], by='week'), format='%b %d', tck=-.04)
# axis.POSIXct(1, at=seq.POSIXt(RangeDates[1], RangeDates[2], by='day'), format='%b %d', labels=NA, tck=-.01)
# 
# points(BuoyData$DateTime.PST, BuoyData$`fCHLA (µg/L)`, xlab='', ylab='', las=1, type='l', lwd=2, col='darkgreen')
# 
# mtext(expression(paste("Chl ", italic(a), " (", mu, 'g L'^'-1', ')')), 2, 2)
# box(which='plot')
# 
# mtext("2018", 1, 1.75)
# 
# 
# 
# #2019
# 
# #Nitrate
# plot(huey_df_distinct$DateTime, huey_df_distinct$Nitrate.mg.L, xlab='', ylab='', las=1, type='l', lwd=2, xaxt='n', ylim=c(0.05,1.4))
# 
# abline(v=fert_posix, lty=3, col='green', lwd=2)
# axis.POSIXct(1, at=seq.POSIXt(deploy_dates_round[1], deploy_dates_round[2], by='week'), format='%b %d', tck=-.04)
# axis.POSIXct(1, at=seq.POSIXt(deploy_dates_round[1], deploy_dates_round[2], by='day'), format='%b %d', labels=NA, tck=-.01)
# 
# points(huey_df_distinct$DateTime, huey_df_distinct$Nitrate.mg.L, xlab='', ylab='', las=1, type='l', lwd=2)
# 
# # mtext(expression(paste(NO[3], ' (mg N L'^'-1', ')')), 2, 2)
# 
# mtext("Experiment 2", 3, .25)
# 
# box(which='plot')
# 
# #Turbidity
# plot(huey_df_distinct$DateTime, huey_df_distinct$Turbidity., xlab='', ylab='', las=1, type='l', lwd=2, col='sienna4', xaxt='n', ylim=c(0,60))
# 
# abline(v=fert_posix, lty=3, col='green', lwd=2)
# axis.POSIXct(1, at=seq.POSIXt(deploy_dates_round[1], deploy_dates_round[2], by='week'), format='%b %d', tck=-.04)
# axis.POSIXct(1, at=seq.POSIXt(deploy_dates_round[1], deploy_dates_round[2], by='day'), format='%b %d', labels=NA, tck=-.01)
# 
# points(huey_df_distinct$DateTime, huey_df_distinct$Turbidity., xlab='', ylab='', las=1, type='l', lwd=2, col='sienna4')
# 
# # mtext(expression(paste("Turbidity (FNU)")), 2, 2)
# box(which='plot')
# 
# #BGA
# # plot(huey_df_distinct$DateTime, huey_df_distinct$BGA.PC.ug.L, xlab='', ylab='', las=1, type='l', lwd=2, col='darkturquoise', xaxt='n', ylim=c(-.5,2))
# # 
# # abline(v=fert_posix, lty=3, col='green', lwd=2)
# # axis.POSIXct(1, at=seq.POSIXt(deploy_dates_round[1], deploy_dates_round[2], by='week'), format='%b %d', tck=-.04)
# # axis.POSIXct(1, at=seq.POSIXt(deploy_dates_round[1], deploy_dates_round[2], by='day'), format='%b %d', labels=NA, tck=-.01)
# # 
# # points(huey_df_distinct$DateTime, huey_df_distinct$Temperature, xlab='', ylab='', las=1, type='l', lwd=2, col='darkturquoise')
# # 
# # mtext(expression(paste("Blue green algae ( ", mu, 'g L'^'-1', ')')), 2, 2)
# # box(which='plot')
# 
# 
# #ChlA
# plot(huey_df_distinct$DateTime, huey_df_distinct$Chlorophyll.ug.L, xlab='', ylab='', las=1, type='l', lwd=2, col='darkgreen', xaxt='n', ylim=c(1,16))
# 
# abline(v=fert_posix, lty=3, col='green', lwd=2)
# axis.POSIXct(1, at=seq.POSIXt(deploy_dates_round[1], deploy_dates_round[2], by='week'), format='%b %d', tck=-.04)
# axis.POSIXct(1, at=seq.POSIXt(deploy_dates_round[1], deploy_dates_round[2], by='day'), format='%b %d', labels=NA, tck=-.01)
# 
# points(huey_df_distinct$DateTime, huey_df_distinct$Chlorophyll.ug.L, xlab='', ylab='', las=1, type='l', lwd=2, col='darkgreen')
# 
# # mtext(expression(paste("Chl ", italic(a), " (", mu, 'g L'^'-1', ')')), 2, 2)
# box(which='plot')
# 
# mtext("2019", 1, 1.75)
# 
# 
# 
# dev.off()
# 
# 
# 
# 
# 
