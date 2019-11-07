

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

# box folder where oxygen data comes from
box_dir<-'C:/Users/lcloken/Box/SadroLab/Luke/DeltaBuoy'

#Where data come from
google_dir<-'C:/GoogleDrive/DeltaNutrientExperiment'

# Project folder where outputs are stored
dropbox_dir<-'C:/Dropbox/USBR Delta Project'


deploydates<-as.POSIXct(c("2018-09-26 20:00:00", "2018-10-10 17:00:00"), format='%Y-%m-%d %H:%M:%S', tz='America/Los_Angeles')
shipdate<-as.POSIXct(c("2018-10-07 14:00:00", "2018-10-06 11:20:00"), format='%Y-%m-%d %H:%M:%S', tz='America/Los_Angeles')
fertdate<-as.POSIXct("2018-10-01 13:30:00", format='%Y-%m-%d %H:%M:%S', tz='America/Los_Angeles')


folders<-list.files(paste0(box_dir, '/Temperature'))

# Loop through filenames and read in temp data
i=1
df_list<-list()
for (i in 1:length(folders)){
  df_i<-read.table(paste0(box_dir, '/Temperature/', folders[i]), skip=1, sep=',', header=T)
  
  name<-as.character(gsub(".csv", "", folders[i]))
  
  df_i_clean<- df_i %>%
    dplyr::select(1:3) %>%
    dplyr::rename("Obs_Nu" = !!names(.[1]),
           "DateTime_PDT" = !!names(.[2]), 
           "Temp_C" = !!names(.[3])) %>%
    mutate(DateTime_PDT = as.POSIXct(DateTime_PDT, format="%m/%d/%y %I:%M:%S %p", tz="America/Los_Angeles"), 
           SensorName = rep(name, nrow(df_i)))
           
  df_list[[i]]<-df_i_clean
}

str(df_list)


# merge to singel df
df_list_deploy<-lapply(df_list, function(l) l[which(l$DateTime_PDT>=deploydates[1] & l$DateTime_PDT<=deploydates[2]),])

df_deploy<-ldply(df_list_deploy, data.frame)


depth_df<-data.frame(SensorName = c("20174618", "20174614", "20174622", "20174620", "20174634"), depth_m = c(2, 3, 4, 5, 6))

df_deploy$SensorDepth<-depth_df$depth_m[match(df_deploy$SensorName, depth_df$SensorName)]

head(df_deploy)


#Get conductivity temp data as well

df_cond<-read.table(paste0(box_dir, '/Conductivity/20438693.csv'), skip=1, sep=',', header=T)
df_cond_clean<- df_cond %>%
  dplyr::select(1:4) %>%
  dplyr::rename("Obs_Nu" = !!names(.[1]),
         "DateTime_PDT" = !!names(.[2]), 
         "SPC_uScm" = !!names(.[3]), 
         "Temp_C" = !!names(.[4])) %>%
  mutate(DateTime_PDT = as.POSIXct(DateTime_PDT, format="%m/%d/%y %I:%M:%S %p", tz="America/Los_Angeles"), 
         SensorName = rep("20438693", nrow(df_cond)))

df_cond_clean$SensorDepth<-rep(1, nrow(df_cond_clean))



#Get PME temp data as well


df_do<-read.table(paste0(box_dir, '/miniDot_Data/543493/Cat.TXT'), skip=8, sep=',', header=T)
names_df<-names(read.table(paste0(box_dir, '/miniDot_Data/543493/Cat.TXT'), skip=7, sep=',', header=T))
names(df_do)<-names_df


SN_str<-as.character(read.table(paste0(box_dir, '/miniDot_Data/543493/Cat.TXT'), skip=0, sep=',', header=F)[2,1])
SN<-paste0('SN_', sub('.*\\-', '', SN_str))

df_do$DateTime_UTC<-as.POSIXct(df_do$UTC_Date_._Time, format='%Y-%m-%d %H:%M:%S', tz='UTC')
df_do$DateTime_PST<-df_do$DateTime_UTC
attributes(df_do$DateTime_PST)$tzone<-'America/Los_Angeles'

df_do_clean<- df_do %>%
  dplyr::select(DateTime_PST, Temperature) %>%
  dplyr::rename("DateTime_PDT" = DateTime_PST, 
         "Temp_C" = Temperature) %>%
  mutate(SensorName = rep("543493", nrow(df_do)))

df_do_clean$SensorDepth<-rep(0.75, nrow(df_do_clean))


df_alldepth<-bind_rows(df_do_clean, df_cond_clean, df_deploy)

df_alldepth2<- df_alldepth[which(df_alldepth$DateTime_PDT>=deploydates[1] & df_alldepth$DateTime_PDT<=deploydates[2]),]



write.csv(df_alldepth2, file=paste0(dropbox_dir, '/Data/NutrientExperiment/Buoys/Temperature_AllDepths.csv'), row.names=F)


png(paste(dropbox_dir, '/Figures/NutrientExperiment/Buoys/TemperatureTimeseries.png', sep=''), width=6, height=3.5, units='in', res=400, bg='white')
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



png(paste(dropbox_dir, '/Figures/NutrientExperiment/Buoys/TemperatureTimeseriesTopBottom.png', sep=''), width=6, height=3.5, units='in', res=400, bg='white')
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


dates=df_alldepth2$DateTime_PDT
depths=df_alldepth2$SensorDepth
value=df_alldepth2$Temp_C
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


png(paste(dropbox_dir, '/Figures/NutrientExperiment/Buoys/TemperatureVerticalProfileTimeseries.png', sep=''), width=7.15, height=3.5, units='in', res=400, bg='white')
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

 # plot with thermocline and 5pm lines
# filled.contour(x=Temp_Est[[1]], y=Temp_Est[[2]], z=Temp_Est[[3]], ylim=c(max(depths), min(depths)), nlevels = 40, color.palette = colorRampPalette(c('navy', "blue", "cyan", "green3", "yellow", "orange", "red"), bias = 1, space = "rgb"), ylab="Depth (m)", plot.axes = { axis(1, at=xticks, labels=xlabels, tck=-0.02); axis(2);lines(wrt3[,1],t.d.roll, lwd=2, col='black'); axis(1, at=sunset, labels=NA, tck=50, col='white', lwd=2, lty=2)})

# plot with mixed layer depth and 5pm lines
filled.contour(x=Temp_Est[[1]], y=Temp_Est[[2]], z=Temp_Est[[3]], ylim=c(max(depths), min(depths)), nlevels = 40, color.palette = colorRampPalette(c('navy', "blue", "cyan", "green3", "yellow", "orange", "red"), bias = 1, space = "rgb"), ylab="Depth (m)", plot.axes = { axis(1, at=xticks, labels=xlabels, tck=-0.02); axis(2);lines(wrt3[,1],m.d.top.roll, lwd=2, col='black'); axis(1, at=sunset, labels=NA, tck=50, col='white', lwd=2, lty=2); axis(1, at=sunrise, labels=NA, tck=50, col='red', lwd=2, lty=2)})


# filled.contour(x=dates, y=depths, z=wrt, ylim=c(max(depths), 0), nlevels = 40, color.palette = colorRampPalette(c('navy', "blue", "cyan", "green3", "yellow", "orange", "red"), bias = 1, space = "rgb"), ylab="Depth (m)", plot.axes = { axis(1, at=xticks, labels=xlabels); axis(2);lines(wrt3[,1],t.d.roll, lwd=1, col='black')})

mtext(expression(paste("Water temperature (", degree, "C)", sep="")), 4, -6)


dev.off()


physics_df<-data.frame(DateTime = wrt3[,1], Zmix_m=m.d.top.roll) %>%
  mutate(Date = as.Date(DateTime, tz='America/Los_Angeles'),
         DateTime_PDT = as.POSIXct(DateTime, tz="America/Los_Angeles")) %>%
  tidyr::fill(Zmix_m, .direction='down') %>%
  tidyr::fill(Zmix_m, .direction='up')

physics_df$DateTime_PDT_round <- lubridate::round_date(physics_df$DateTime_PDT, unit='5 minutes')


isday = is.day(as.POSIXct(physics_df$DateTime, tz='America/Los_Angeles'),38.5) #Sparkling Lake is at 48 degrees N latitude
physics_df$dayIrr = rep(0,nrow(physics_df))
physics_df$dayIrr[isday] = 1

physics_list<-list()
day_nu = 1
for (day_nu in 1:length(unique(physics_df$Date))){
  physics_list[[day_nu]] <- physics_df %>%
    dplyr::filter(Date == unique(physics_df$Date)[day_nu]) %>%
    dplyr::filter(dayIrr == 1) %>%
    arrange(desc(DateTime_PDT)) %>%
    mutate(Z_new = cummax(Zmix_m))

}

# plot(physics_list[[2]]$DateTime_PST,  physics_list[[2]]$Zmix_m, type='l', ylim=c(6,0))
# points( physics_list[[2]]$DateTime_PST,  physics_list[[2]]$Z_new, type='l', col='red')

physics_out<-ldply(physics_list, data.frame) %>%
  arrange(DateTime_PDT)


physics_out2<-left_join(physics_df, physics_out)

plot(physics_out2$DateTime_PDT, physics_out2$Zmix_m, type='l', ylim=c(6,0))
points(physics_out2$DateTime_PDT, physics_out2$Z_new, type='l', col='red')


physics_df_byday <- physics_df %>%
  mutate(Time = strftime(DateTime, format='%H:%M'),
         LocalHour = as.numeric(strftime(DateTime, format='%H')), 
         LocalMin =  as.numeric(strftime(DateTime, format='%M')) ) %>% 
  mutate(DecHour = LocalHour + LocalMin/60 )



ggplot(aes(x=DateTime_PDT, y=Zmix_m),data=physics_df) + 
  geom_line() + 
  scale_y_reverse() + 
  theme_bw() + 
  scale_x_datetime(date_breaks='weeks', date_minor_breaks = 'days')

ggplot(aes(x=DecHour, y=Zmix_m, group=as.factor(Date), colour=as.factor(Date)),data=physics_df_byday) + 
  geom_line() + 
  scale_y_reverse() + 
  theme_bw() +
  labs(x='Hour (PDT)')
  # facet_wrap(~Date)
  # scale_x_datetime(date_breaks='weeks', date_minor_breaks = 'days')


write.csv(physics_out2, file=paste0(dropbox_dir, '/Data/NutrientExperiment/Buoys/Zmix_Oct2018.csv'), row.names=F)
