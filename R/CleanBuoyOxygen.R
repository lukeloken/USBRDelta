
library(lubridate)
library(plyr)
library(viridis)
library(ggplot2)

# box folder where oxygen data comes from
box_dir<-'C:/Users/lcloken/Box/SadroLab/Luke/DeltaBuoy'

#Where data come from
google_dir<-'C:/GoogleDrive/DeltaNutrientExperiment'

# Project folder where outputs are stored
dropbox_dir<-'C:/Dropbox/USBR Delta Project'

folders<-list.files(paste0(box_dir, '/miniDot_Data'))

# Loop through filenames and read in data
i=8
df_list<-list()
for (i in 1:length(folders)){
  df_i<-read.table(paste0(box_dir, '/miniDot_Data/', folders[i], '/Cat.TXT'), skip=8, sep=',', header=T)
  names_df<-names(read.table(paste0(box_dir, '/miniDot_Data/', folders[i], '/Cat.TXT'), skip=7, sep=',', header=T))
  names(df_i)<-names_df
  SN_str<-as.character(read.table(paste0(box_dir, '/miniDot_Data/', folders[i], '/Cat.TXT'), skip=0, sep=',', header=F)[2,1])
  SN<-paste0('SN_', sub('.*\\-', '', SN_str))
  
  df_i$DateTime_UTC<-as.POSIXct(df_i$UTC_Date_._Time, format='%Y-%m-%d %H:%M:%S', tz='UTC')
  df_i$DateTime_PST<-df_i$DateTime_UTC
  attributes(df_i$DateTime_PST)$tzone<-'America/Los_Angeles'
  df_i$Date<-as.Date(df_i$DateTime_PST)
  df_list[[i]]<-df_i
  names(df_list)[[i]]<-SN
}

str(df_list)

deploydates<-ymd(c("2018-09-27", "2018-10-09"))

shipdate<-as.POSIXct(c("2018-10-07 12:00:00", "2018-10-06 13:00:00", "2018-10-05 02:00:00"), format='%Y-%m-%d %H:%M:%S', tz='America/Los_Angeles')
fertdate<-as.POSIXct("2018-10-01 13:30:00", format='%Y-%m-%d %H:%M:%S', tz='America/Los_Angeles')

SensorIDs<-data.frame(Buoy=1:15, SensorSN=paste0('SN_', c(152972, 468192, 415713, 939315, 1043, 781274, 1044, 543493, 442116, 332834, 598099, 285695, 565424, 970412, 159482)))

df_list_deploy<-lapply(df_list, function(l) l[which(l$Date>=deploydates[1] & l$Date<=deploydates[2]),])



df_deploy<-ldply(df_list_deploy, data.frame)
names(df_deploy)[1]<-'Sensor'
str(df_deploy)

df_deploy$Sensor<-factor(df_deploy$Sensor, SensorIDs$SensorSN)
df_deploy$BuoyNu<-factor(SensorIDs$Buoy[match(df_deploy$Sensor, SensorIDs$SensorSN)], 1:15)

write.csv(df_deploy, file=paste0(dropbox_dir, '/Data/NutrientExperiment/Buoys/MiniDot_Oct2018.csv'), row.names=F)


#colors
color.palette = colorRampPalette(c(viridis(6, begin=.2, end=.98), rev(magma(5, begin=.35, end=.98))), bias=1)
colors<-color.palette(length(unique(df_deploy$Sensor)))


plot_list <- ggplot(df_deploy, aes(DateTime_PST, Dissolved.Oxygen, group=BuoyNu)) + 
  labs(x='Date (PST)', y='Dissolved Oxygen (mg per L)') +
  scale_shape_manual(values=rep(21:25, 5))  + 
  scale_fill_manual(values = colors) + 
  scale_colour_manual(values = colors) + 
  geom_vline(xintercept=shipdate, color='#636363', linetype=2, size=1.5) + 
  geom_vline(xintercept=fertdate, color='#2ca25f', linetype=2, size=1.5) + 
  geom_path(aes(color=BuoyNu), size=1) + 
  # geom_point(size=3, aes(fill=Sensor, shape=Sensor)) + 
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))  + 
  theme(legend.position='bottom') + 
  scale_y_continuous(limits=c(5.5, 11)) + 
  annotate(geom="text", x=(fertdate+70000), y=5.75, label="Fertilization", color="#2ca25f") + 
annotate(geom="text", x=(shipdate[3]+45000), y=5.75, label="Ships", color="#636363")

# print(plot_list)


png(paste0(dropbox_dir, '/Figures/NutrientExperiment/Buoys/DissolvedOxygen_TS.png'), width=8, height=4, units='in', res=200)

print(plot_list)

dev.off()
