
library(dplyr)


deploy_dates<-as.POSIXct(c("2019-07-12 17:00:00", "2019-08-27 00:00:00"), tz="Etc/GMT+8")

huey_dirty_dates<-as.POSIXct(c("2019-08-12 00:00:00", "2019-08-16 12:00:00"), tz="Etc/GMT+8")


fert_dates<-as.POSIXct(c("2019-07-22 08:00", "2019-07-23 08:00","2019-07-24 08:00", "2019-07-25 08:00", "2019-08-05 08:00", "2019-08-06 08:00","2019-08-07 08:00", "2019-08-08 08:00"), format='%Y-%m-%d %H:%M', tz="Etc/GMT+8")




#plot NO3 data at huey
huey_files<-list.files(paste0(box_dir, "/Data/BuoyData/USGS"))

huey_list<-list()

i=1
for (i in 1:length(huey_files)){

huey_data_i<-read.csv(paste0(box_dir, "/Data/BuoyData/USGS/", huey_files[i]), header=F, skip = 4)

huey_header_i<-names(read.csv(paste0(box_dir, "/Data/BuoyData/USGS/", huey_files[i]), header=T, skip = 1))

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
huey_df_distinct$Chlorophyll.ug.L[huey_df_distinct$Chlorophyll.ug.L>20]<-NA

#BGA
huey_df_distinct$BGA.PC.ug.L[huey_df_distinct$DateTime>huey_dirty_dates[1] & huey_df_distinct$DateTime<huey_dirty_dates[2]]<-NA
huey_df_distinct$BGA.PC.RFU[huey_df_distinct$DateTime>huey_dirty_dates[1] & huey_df_distinct$DateTime<huey_dirty_dates[2]]<-NA


#Temp
huey_df_distinct$Temperature[huey_df_distinct$DateTime>huey_dirty_dates[1] & huey_df_distinct$DateTime<huey_dirty_dates[2] & huey_df_distinct$Temperature<(24)]<-NA


#pH
huey_df_distinct$pH[huey_df_distinct$pH<8.25]<-NA

# plot(huey_df$DateTime, huey_df$ODOSat, type='l')
plot(huey_df_distinct$DateTime, huey_df_distinct$ODOSat, type='l', col='red')

abline(v=fert_dates, col='green')
head(huey_df_distinct)
tail(huey_df_distinct)




png(paste0(dropbox_dir, '/Figures/NutrientExperiment2/USGSBuoys/Huey_NO3_SSCN2.png'), width=8, height=4, units='in', res=200)

par(mar=c(3,3.5,0.5, 0.5), mgp=c(3,0.5,0), tck=-0.02)

plot(huey_df_distinct$DateTime, huey_df_distinct$Nitrate.mg.L, xlab='', ylab='', las=1, type='l', lwd=2)

# points(huey_df_distinct$DateTime, huey_df_distinct$Nitrate.mg.L, xlab='', ylab='', las=1, type='p', pch=20, cex=1, col='gray')

abline(v=fert_dates, lty=3, col='green', lwd=2)

points(huey_df_distinct$DateTime, huey_df_distinct$Nitrate.mg.L, xlab='', ylab='', las=1, type='l', lwd=2)

mtext(expression(paste(NO[3], ' (mg N L'^'-1', ')')), 2, 2)
box(which='plot')

dev.off()




png(paste0(dropbox_dir, '/Figures/NutrientExperiment2/USGSBuoys/Huey_ChlA_SSCN2.png'), width=8, height=4, units='in', res=200)

par(mar=c(3,3.5,0.5, 0.5), mgp=c(3,0.5,0), tck=-0.02)

plot(huey_df_distinct$DateTime, huey_df_distinct$Chlorophyll.ug.L, xlab='', ylab='', las=1, type='l', lwd=2)

# points(huey_df_distinct$DateTime, huey_df_distinct$Chlorophyll.ug.L, xlab='', ylab='', las=1, type='p', pch=20, cex=1, col='gray')

abline(v=fert_dates, lty=3, col='green', lwd=2)

points(huey_df_distinct$DateTime, huey_df_distinct$Chlorophyll.ug.L, xlab='', ylab='', las=1, type='l', lwd=2)

mtext(expression(paste("Chl ", italic(a), ' (', mu, 'g L'^'-1', ')')), 2, 2)
box(which='plot')

dev.off()



png(paste0(dropbox_dir, '/Figures/NutrientExperiment2/USGSBuoys/Huey_Temp_SSCN2.png'), width=8, height=4, units='in', res=200)

par(mar=c(3,3.5,0.5, 0.5), mgp=c(3,0.5,0), tck=-0.02)

plot(huey_df_distinct$DateTime, huey_df_distinct$Temperature, xlab='', ylab='', las=1, type='l', lwd=2)

# points(huey_df_distinct$DateTime, huey_df_distinct$Temperature, xlab='', ylab='', las=1, type='p', pch=20, cex=1, col='gray')

abline(v=fert_dates, lty=3, col='green', lwd=2)

points(huey_df_distinct$DateTime, huey_df_distinct$Temperature, xlab='', ylab='', las=1, type='l', lwd=2)

mtext(expression(paste("Temperature (", degree, 'C)')), 2, 2)
box(which='plot')

dev.off()



png(paste0(dropbox_dir, '/Figures/NutrientExperiment2/USGSBuoys/Huey_DO_SSCN2.png'), width=8, height=4, units='in', res=200)

par(mar=c(3,3.5,0.5, 0.5), mgp=c(3,0.5,0), tck=-0.02)

plot(huey_df_distinct$DateTime, huey_df_distinct$ODO, xlab='', ylab='', las=1, type='l', lwd=2)
# points(huey_df_distinct$DateTime, huey_df_distinct$ODO, xlab='', ylab='', las=1, type='p', pch=20, cex=1, col='gray')

abline(v=fert_dates, lty=3, col='green', lwd=2)
points(huey_df_distinct$DateTime, huey_df_distinct$ODO, xlab='', ylab='', las=1, type='l', lwd=2)

mtext(expression(paste("Dissolved Oxygen (mg ", O[2], ' L'^'-1', ')')), 2, 2)
box(which='plot')

dev.off()




png(paste0(dropbox_dir, '/Figures/NutrientExperiment2/USGSBuoys/Huey_Temp_ChlA_DO_SSCN2_fertweek1.png'), width=8, height=12, units='in', res=200)

par(mar=c(3,3.5,0.5, 0.5), mgp=c(3,0.5,0), tck=-0.02, mfrow=c(3,1))



plot(huey_df_distinct$DateTime, huey_df_distinct$Temperature, xlab='', ylab='', las=1, type='l', lwd=2, xlim=c((fert_dates[1]-3600*24*2), (fert_dates[4]+3600*24*3)))

# points(huey_df_distinct$DateTime, huey_df_distinct$Temperature, xlab='', ylab='', las=1, type='p', pch=20, cex=1, col='gray')

abline(v=fert_dates, lty=3, col='green', lwd=2)

points(huey_df_distinct$DateTime, huey_df_distinct$Temperature, xlab='', ylab='', las=1, type='l', lwd=2)

mtext(expression(paste("Temperature (", degree, 'C)')), 2, 2)
box(which='plot')

axis.POSIXct(1, x=seq.POSIXt(fert_dates[1], fert_dates[4], by='day'))




plot(huey_df_distinct$DateTime, huey_df_distinct$Chlorophyll.ug.L, xlab='', ylab='', las=1, type='l', lwd=2, xlim=c((fert_dates[1]-3600*24*2), (fert_dates[4]+3600*24*3)), ylim=c(0,20))

# points(huey_df_distinct$DateTime, huey_df_distinct$Chlorophyll.ug.L, xlab='', ylab='', las=1, type='p', pch=20, cex=1, col='gray')

abline(v=fert_dates, lty=3, col='green', lwd=2)

points(huey_df_distinct$DateTime, huey_df_distinct$Chlorophyll.ug.L, xlab='', ylab='', las=1, type='l', lwd=2)

mtext(expression(paste("Chl ", italic(a), ' (', mu, 'g L'^'-1', ')')), 2, 2)
box(which='plot')

axis.POSIXct(1, x=seq.POSIXt(fert_dates[1], fert_dates[4], by='day'))



plot(huey_df_distinct$DateTime, huey_df_distinct$ODO, xlab='', ylab='', las=1, type='l', lwd=2, xlim=c((fert_dates[1]-3600*24*2), (fert_dates[4]+3600*24*3)))
# points(huey_df_distinct$DateTime, huey_df_distinct$ODO, xlab='', ylab='', las=1, type='p', pch=20, cex=1, col='gray')

abline(v=fert_dates, lty=3, col='green', lwd=2)
points(huey_df_distinct$DateTime, huey_df_distinct$ODO, xlab='', ylab='', las=1, type='l', lwd=2)

mtext(expression(paste("Dissolved Oxygen (mg ", O[2], ' L'^'-1', ')')), 2, 2)
box(which='plot')




dev.off()






png(paste0(dropbox_dir, '/Figures/NutrientExperiment2/USGSBuoys/Huey_Temp_ChlA_DO_SSCN2_fertweek2.png'), width=8, height=12, units='in', res=200)

par(mar=c(3,3.5,0.5, 0.5), mgp=c(3,0.5,0), tck=-0.02, mfrow=c(3,1))



plot(huey_df_distinct$DateTime, huey_df_distinct$Temperature, xlab='', ylab='', las=1, type='l', lwd=2, xlim=c((fert_dates[5]-3600*24*2), (fert_dates[8]+3600*24*3)))

# points(huey_df_distinct$DateTime, huey_df_distinct$Temperature, xlab='', ylab='', las=1, type='p', pch=20, cex=1, col='gray')

abline(v=fert_dates, lty=3, col='green', lwd=2)

points(huey_df_distinct$DateTime, huey_df_distinct$Temperature, xlab='', ylab='', las=1, type='l', lwd=2)

mtext(expression(paste("Temperature (", degree, 'C)')), 2, 2)
box(which='plot')

axis.POSIXct(1, x=seq.POSIXt(fert_dates[5], fert_dates[8], by='day'))


plot(huey_df_distinct$DateTime, huey_df_distinct$Chlorophyll.ug.L, xlab='', ylab='', las=1, type='l', lwd=2, xlim=c((fert_dates[5]-3600*24*2), (fert_dates[8]+3600*24*3)), ylim=c(0,20))

# points(huey_df_distinct$DateTime, huey_df_distinct$Chlorophyll.ug.L, xlab='', ylab='', las=1, type='p', pch=20, cex=1, col='gray')

abline(v=fert_dates, lty=3, col='green', lwd=2)

points(huey_df_distinct$DateTime, huey_df_distinct$Chlorophyll.ug.L, xlab='', ylab='', las=1, type='l', lwd=2)

mtext(expression(paste("Chl ", italic(a), ' (', mu, 'g L'^'-1', ')')), 2, 2)
box(which='plot')

axis.POSIXct(1, x=seq.POSIXt(fert_dates[5], fert_dates[8], by='day'))


plot(huey_df_distinct$DateTime, huey_df_distinct$ODO, xlab='', ylab='', las=1, type='l', lwd=2, xlim=c((fert_dates[5]-3600*24*2), (fert_dates[8]+3600*24*3)))
# points(huey_df_distinct$DateTime, huey_df_distinct$ODO, xlab='', ylab='', las=1, type='p', pch=20, cex=1, col='gray')

abline(v=fert_dates, lty=3, col='green', lwd=2)
points(huey_df_distinct$DateTime, huey_df_distinct$ODO, xlab='', ylab='', las=1, type='l', lwd=2)

mtext(expression(paste("Dissolved Oxygen (mg ", O[2], ' L'^'-1', ')')), 2, 2)
box(which='plot')


dev.off()







#Figure for Research Short. 
# No major ecological problems


png(paste0(dropbox_dir, '/Figures/NutrientExperiment2/USGSBuoys/Huey_EcosystemHealth_SSCN2.png'), width=6, height=9, units='in', res=200)

par(mar=c(2,4,0.5, 0.5), mgp=c(3,0.5,0), tck=-0.02, mfrow=c(5,1))

#Nitrate
plot(huey_df_distinct$DateTime, huey_df_distinct$Nitrate.mg.L, xlab='', ylab='', las=1, type='l', lwd=2)

# points(huey_df_distinct$DateTime, huey_df_distinct$Nitrate.mg.L, xlab='', ylab='', las=1, type='p', pch=20, cex=1, col='gray')

abline(v=fert_dates, lty=3, col='green', lwd=2)

points(huey_df_distinct$DateTime, huey_df_distinct$Nitrate.mg.L, xlab='', ylab='', las=1, type='l', lwd=2)

mtext(expression(paste(NO[3], ' (mg N L'^'-1', ')')), 2, 2)
box(which='plot')

#ODO
plot(huey_df_distinct$DateTime, huey_df_distinct$ODO, xlab='', ylab='', las=1, type='l', lwd=2)
# points(huey_df_distinct$DateTime, huey_df_distinct$ODO, xlab='', ylab='', las=1, type='p', pch=20, cex=1, col='gray')

abline(v=fert_dates, lty=3, col='green', lwd=2)
points(huey_df_distinct$DateTime, huey_df_distinct$ODO, xlab='', ylab='', las=1, type='l', lwd=2)

mtext(expression(paste("Dissolved oxygen (mg  ", O[2], ' L'^'-1', ')')), 2, 2)
box(which='plot')

#pH
plot(huey_df_distinct$DateTime, huey_df_distinct$pH, xlab='', ylab='', las=1, type='l', lwd=2)
# points(huey_df_distinct$DateTime, huey_df_distinct$pH, xlab='', ylab='', las=1, type='p', pch=20, cex=1, col='gray')

abline(v=fert_dates, lty=3, col='green', lwd=2)
points(huey_df_distinct$DateTime, huey_df_distinct$pH, xlab='', ylab='', las=1, type='l', lwd=2)

mtext(expression(paste("pH")), 2, 2)
box(which='plot')

#BGA
plot(huey_df_distinct$DateTime, huey_df_distinct$BGA.PC.ug.L, xlab='', ylab='', las=1, type='l', lwd=2)

# points(huey_df_distinct$DateTime, huey_df_distinct$Temperature, xlab='', ylab='', las=1, type='p', pch=20, cex=1, col='gray')

abline(v=fert_dates, lty=3, col='green', lwd=2)

points(huey_df_distinct$DateTime, huey_df_distinct$Temperature, xlab='', ylab='', las=1, type='l', lwd=2)

mtext(expression(paste("Blue green algae (", mu, 'g L'^'-1', ')')), 2, 2)
box(which='plot')

axis.POSIXct(1, x=seq.POSIXt(fert_dates[5], fert_dates[8], by='day'))


#ChlA
plot(huey_df_distinct$DateTime, huey_df_distinct$Chlorophyll.ug.L, xlab='', ylab='', las=1, type='l', lwd=2)

# points(huey_df_distinct$DateTime, huey_df_distinct$Chlorophyll.ug.L, xlab='', ylab='', las=1, type='p', pch=20, cex=1, col='gray')

abline(v=fert_dates, lty=3, col='green', lwd=2)

points(huey_df_distinct$DateTime, huey_df_distinct$Chlorophyll.ug.L, xlab='', ylab='', las=1, type='l', lwd=2)

mtext(expression(paste("Chl ", italic(a), " (", mu, 'g L'^'-1', ')')), 2, 2)
box(which='plot')

axis.POSIXct(1, x=seq.POSIXt(fert_dates[5], fert_dates[8], by='day'))




dev.off()

