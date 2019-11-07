
library(lubridate)
library(plyr)
library(viridis)
library(ggplot2)
library(gridExtra)

source('R/DownloadUSGSBuoy.R')

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
  df_i$Date<-ymd(strptime(df_i$DateTime_PST, format="%Y-%m-%d", tz='America/Los_Angeles'))
  df_list[[i]]<-df_i
  names(df_list)[[i]]<-SN
}

str(df_list)

#old way to clip data
# deploydates<-ymd(c("2018-09-27", "2018-10-09"))

deploydates<-as.POSIXct(c("2018-09-26 20:00:00", "2018-10-10 17:00:00"), format='%Y-%m-%d %H:%M:%S', tz='America/Los_Angeles')
shipdate<-as.POSIXct(c("2018-10-07 14:00:00", "2018-10-06 11:20:00"), format='%Y-%m-%d %H:%M:%S', tz='America/Los_Angeles')
fertdate<-as.POSIXct("2018-10-01 13:30:00", format='%Y-%m-%d %H:%M:%S', tz='America/Los_Angeles')

SensorIDs<-data.frame(Buoy=1:15, SensorSN=paste0('SN_', c(152972, 468192, 415713, 939315, 1043, 781274, 1044, 543493, 442116, 332834, 598099, 285695, 565424, 970412, 159482)))

# old way to do this
# df_list_deploy<-lapply(df_list, function(l) l[which(l$Date>=deploydates[1] & l$Date<=deploydates[2]),])

# use exact times
df_list_deploy<-lapply(df_list, function(l) l[which(l$DateTime_PST>=deploydates[1] & l$DateTime_PST<=deploydates[2]),])



df_deploy<-ldply(df_list_deploy, data.frame)
names(df_deploy)[1]<-'Sensor'
str(df_deploy)

df_deploy$Sensor<-factor(df_deploy$Sensor, SensorIDs$SensorSN)
df_deploy$BuoyNu<-factor(SensorIDs$Buoy[match(df_deploy$Sensor, SensorIDs$SensorSN)], 1:15)
df_deploy$Buoy<-SensorIDs$Buoy[match(df_deploy$Sensor, SensorIDs$SensorSN)]

BuoyNames<-unique(df_deploy$Buoy)[order(unique(df_deploy$Buoy))]
BuoyNames[which(BuoyNames==15)]<-'NL76'
BuoyNames[which(BuoyNames==1)]<-'NL70'

df_deploy$Buoy[which(df_deploy$Buoy==15)]<-'NL76'
df_deploy$Buoy[which(df_deploy$Buoy==1)]<-'NL70'
df_deploy$Buoy<-factor(df_deploy$Buoy, rev(BuoyNames))

df_deploy$Dissolved.Oxygen[which(df_deploy$BuoyNu=="15" & df_deploy$DateTime_PST>(shipdate[2]-3600))]<-NA


write.csv(df_deploy, file=paste0(dropbox_dir, '/Data/NutrientExperiment/Buoys/MiniDot_Oct2018.csv'), row.names=F)



#colors
color.palette = colorRampPalette(c(viridis(6, begin=.2, end=.98), rev(magma(5, begin=.35, end=.98))), bias=1)
colors<-rev(color.palette(length(unique(df_deploy$Sensor))))
xlim<-range(df_deploy$DateTime_PST, na.rm=T)

plot_list<-list()

plot_list[[1]] <- ggplot(df_deploy, aes(DateTime_PST, Dissolved.Oxygen, group=Buoy)) + 
  labs(x='Date (PDT)', y=expression(paste('Dissolved oxygen (mg  ', O[2], ' L'^'-1', ')', sep=''))) +
  scale_shape_manual(values=rep(21:25, 5))  + 
  scale_fill_manual(values = colors) + 
  scale_colour_manual(values = colors) + 
  geom_vline(xintercept=shipdate[1:2], color='grey', linetype=2, size=1) + 
  geom_vline(xintercept=fertdate, color='black', linetype=1, size=1) + 
  geom_path(aes(color=Buoy), size=1) + 
  # geom_point(size=3, aes(fill=Sensor, shape=Sensor)) + 
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))  + 
  theme(legend.position='bottom') + 
  scale_y_continuous(limits=c(6.5, 11)) + 
  scale_x_datetime(limits=xlim, date_minor_breaks= "1 days", date_breaks = "2 days", date_labels="%b %d") +
  annotate(geom="text", x=(fertdate+70000), y=5.75, label="Fertilization", color="#2ca25f") + 
annotate(geom="text", x=(shipdate[3]+45000), y=5.75, label="Ships", color="#636363") + 
  guides(color = guide_legend(nrow = 1, title.position='top', title.hjust=0.5)) 

plot_list[[2]] <- ggplot(RawData, aes(dateTime, DO_Inst, group=site_no)) + 
  labs(x='Date (PDT)', y='Dissolved Oxygen (mg per L)') +
  scale_shape_manual(values=rep(21:25, 5))  + 
  scale_fill_manual(values = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")) + 
  scale_colour_manual(values = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")) + 
  geom_vline(xintercept=shipdate[1:2], color='grey', linetype=2, size=1) + 
  geom_vline(xintercept=fertdate, color='black', linetype=1, size=1) + 
  geom_path(aes(color=site_no), size=1) + 
  # geom_point(size=3, aes(fill=Sensor, shape=Sensor)) + 
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))  + 
  theme(legend.position='bottom') + 
  scale_y_continuous(limits=c(6.5, 11)) + 
  scale_x_datetime(limits=xlim, date_minor_breaks= "1 days", date_breaks = "2 days", date_labels="%b %d") +
  annotate(geom="text", x=(fertdate+70000), y=6.75, label="Fertilization", color="#2ca25f") + 
  annotate(geom="text", x=(shipdate[3]+45000), y=6.75, label="Ships", color="#636363") + 
  guides(color = guide_legend(nrow = 1, title.position='top', title.hjust=0.5)) 

plot_list

png(paste0(dropbox_dir, '/Figures/NutrientExperiment/Buoys/DissolvedOxygen_TS.png'), width=8, height=8, units='in', res=200)

grid.arrange(grobs=plot_list, nrow=2)

dev.off()

#Moved to another scipt. 
# png(paste0(dropbox_dir, '/Figures/NutrientExperiment/Buoys/DissolvedOxygen_TS_SSCN.png'), width=8, height=4, units='in', res=200)
# 
# plot_list[[1]] + theme(axis.title.x =element_blank())
# 
# dev.off()


facet_plot<-plot_list[[1]] + facet_wrap(~Buoy ,nrow=6, ncol=2, dir="v", as.table=T) + 
  theme(legend.position='right') + 
  guides(color = guide_legend(ncol = 1, title.position='top', title.hjust=0.5, reverse=F)) +
  theme(strip.background = element_blank(), strip.text.x = element_blank()) + 
  theme(panel.background = element_rect(fill = "white",
                                  colour = "black",
                                  size = 0.5, linetype = "solid"))
  # theme( strip.background = element_blank(), strip.placement = "inside") 

print(facet_plot)

  
png(paste0(dropbox_dir, '/Figures/NutrientExperiment/Buoys/DissolvedOxygen_TS_facet.png'), width=8, height=8, units='in', res=200)

print(facet_plot)

dev.off()


#Gauge SPC and Height
gauge_list<-ggplot(RawData, aes(dateTime, GH_Inst, group=site_no)) + 
  labs(x='Date (PDT)', y='Gauge height (ft)') +
  scale_shape_manual(values=rep(21:25, 5))  + 
  scale_fill_manual(values = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")) + 
  scale_colour_manual(values = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")) + 
  # geom_vline(xintercept=shipdate, color='#636363', linetype=2, size=1.5) + 
  geom_vline(xintercept=fertdate, color='#2ca25f', linetype=2, size=1.5) +
  geom_path(aes(color=site_no), size=1) + 
  # geom_point(size=3, aes(fill=Sensor, shape=Sensor)) + 
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))  + 
  theme(legend.position='bottom') + 
  # scale_y_continuous(limits=c(6.5, 11)) + 
  scale_x_datetime(limits=xlim, date_minor_breaks= "1 days", date_breaks = "2 days", date_labels="%b %d") +
  # annotate(geom="text", x=(fertdate+70000), y=6.75, label="Fertilization", color="#2ca25f") + 
  # annotate(geom="text", x=(shipdate[3]+45000), y=6.75, label="Ships", color="#636363") + 
  guides(color = guide_legend(nrow = 1, title.position='top', title.hjust=0.5)) 

SPC_list<-ggplot(RawData, aes(dateTime, SpecCond_Inst, group=site_no)) + 
  labs(x='Date (PDT)', y='SPC (uS/cm)') +
  scale_shape_manual(values=rep(21:25, 5))  + 
  scale_fill_manual(values = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")) + 
  scale_colour_manual(values = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")) + 
  # geom_vline(xintercept=shipdate, color='#636363', linetype=2, size=1.5) + 
  geom_vline(xintercept=fertdate, color='#2ca25f', linetype=2, size=1.5) +
  geom_path(aes(color=site_no), size=1) + 
  # geom_point(size=3, aes(fill=Sensor, shape=Sensor)) + 
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))  + 
  theme(legend.position='bottom') + 
  # scale_y_continuous(limits=c(6.5, 11)) + 
  scale_x_datetime(limits=xlim, date_minor_breaks= "1 days", date_breaks = "2 days", date_labels="%b %d") +
  # annotate(geom="text", x=(fertdate+70000), y=6.75, label="Fertilization", color="#2ca25f") + 
  # annotate(geom="text", x=(shipdate[3]+45000), y=6.75, label="Ships", color="#636363") + 
  guides(color = guide_legend(nrow = 1, title.position='top', title.hjust=0.5)) 

png(paste0(dropbox_dir, '/Figures/NutrientExperiment/Buoys/WaterLevelSPC_TS.png'), width=8, height=8, units='in', res=200)

grid.arrange(grobs=list(gauge_list, SPC_list), nrow=2)

dev.off()
