

#Code to extract surface water measurements from profile data at fixed sites

library(readxl)
library(plyr)
library(dplyr)
library(viridis)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(anytime)


light<-read.csv(file.path(onedrive_dir, 'RawData', 'NutrientExperiment2', 'LightProfiles', 'SSCN2_LightProfiles.csv'), stringsAsFactors = F)

# light_df<-light[light$Depth_m>0,]
light_df<-light
light_df$PAR_umol<-as.numeric(light_df$PAR_umol)
# light_df$Date<-as.Date(light_df$Date, format='%Y-%m-%d')
light_df$Date<-anydate(light_df$Date)

light_df<-light_df[which(light_df$Date > as.Date('2019-07-08') & light_df$Date < as.Date('2019-08-27')),]

light_df$Site[which(light_df$Site=="Site1")]<-"NL70"

light_df$Site<-factor(light_df$Site, c("NL70", "Site2", "Site3", "NL74", "Site5", "Site6", "NL76"))
  
  
light_df<-light_df[which(light_df$Depth_m!="surface"),]
light_df$Depth_m<-as.numeric(light_df$Depth_m)

#light_df$Site<-factor(light_df$Site, sitetable$site1)

light_df$ln_par<-log(light_df$PAR_umol)

# model<-lm(light_df$PAR_unit ~ light_df$Depth_m | light_df$Station)
# 
# summary(model)
# anova(model)

dates<-unique(light_df$Date)
stations<-unique(light_df$Site)

day=1
output_list<-list()

for (day in 1:length(dates)){

  data_i<-light_df[which(light_df$Date ==dates[day]), ]
  

  water_data<-data_i[which(data_i$Depth_m>0),]
  
  
good_stations<-unique(water_data$Site)
  
  
  models<-lapply(good_stations, function (x) lm(ln_par ~ Depth_m, data=water_data[water_data$Site == x,]))
  slopes<-sapply(models, function (l) coef(l)[2])
  
  out_df<-data.frame(good_stations, slopes*(-1))
  names(out_df)<-c("Site", "kd_meters")
  out_df$Date<-rep(dates[day], nrow(out_df))
  
output_list[[day]]<-out_df



}
kd_alldates<-ldply(output_list, data.frame)

kd_alldates$PhoticDepth_m=log(1/100)/(kd_alldates$kd_meters*(-1))



color.palette = colorRampPalette(c(viridis(6, begin=.2, end=.98), rev(magma(5, begin=.35, end=.98))), bias=1)
colors<-color.palette(length(levels(kd_alldates$Site)))
shapes<-rep(21:25, 5)


#Common theme for all metabolism timeseries panels
commonTheme_kd<-list(
  scale_colour_manual(values = colors),
  scale_fill_manual(values = colors),
  scale_shape_manual(values=rep(21:25, 5)),
  # geom_smooth(method='loess',  se=F),
  # geom_smooth(method='auto', se=T, alpha=.2),
  # geom_jitter(size=2, width=jitterwidth, height=0, aes(fill=Site, shape=Site)),
  geom_vline(xintercept=fert_dates, linetype="dashed", color = "green", size=0.5),
  theme_bw(),
  theme(plot.title = element_text(hjust=0.5), legend.position="bottom", axis.title.x=element_blank())
)


png(file.path(onedrive_dir, "Figures", "NutrientExperiment2", "LightExtinction_SSCN2.png"), width = 12, height = 8, units = "in", res = 250)

par(mar=c(3.25,3.25,2.5,2.5), mgp=c(3,1,0), tck = -.02)

print(
  ggplot(aes(x=Date, y=kd_meters, group=Site, colour=Site), data=kd_alldates)+
  commonTheme_kd + 
  geom_path() + 
  geom_point() +
  theme_bw() +
  theme(text = element_text(size=15)) +
  labs(y=expression(paste('Light extinction (m'^'-1', ')'
)), x=expression(paste (""))) +
theme(legend.position = "bottom")
)

dev.off()

png(file.path(onedrive_dir, "Figures", "NutrientExperiment2", "PhoticDepths_SSCN2_.png"), width = 12, height = 8, units = "in", res = 250)

print(
  ggplot(aes(x=Date, y=PhoticDepth_m, group=Site, colour=Site), data=kd_alldates)+
  commonTheme_kd + 
  geom_path() + 
  geom_point() +
  theme_bw() + 
  theme(text = element_text(size=15)) +
  labs(y=expression(paste('Photic Depth (m)')), x=expression(paste (""))) +
  theme(legend.position = "bottom")
)

dev.off()

# plot(kd_alldates$kd_meters, kd_alldates$PhoticDepth_m)

write.csv(kd_alldates, file=file.path(onedrive_dir, 'OutputData', 'NutrientExperiment2', 'SSCN2_Kd.csv'), row.names=F)

saveRDS(kd_alldates , file=file.path(onedrive_dir, 'RData', 'NutrientExperiment2', 'SSCN2_Kd.rds'))


rm(data_i, commonTheme_kd, kd_alldates, light, light_df, models, out_df, output_list, water_data, colors, dates, day, good_stations, shapes, slopes, stations)

