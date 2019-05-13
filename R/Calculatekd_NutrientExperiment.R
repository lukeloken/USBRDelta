

#Code to extract surface water measurements from profile data at fixed sites

library(readxl)
library(plyr)
library(dplyr)
library(viridis)
library(lubridate)
library(ggplot2)
library(gridExtra)


source('R/read_excel_allsheets.R')
source('R/g_legend.R')
source('R/lightmodel.R')

# # Project folder where outputs are stored
# dropbox_dir<-'C:/Dropbox/USBR Delta Project'
# 
# #Where data come from
# google_dir<-'C:/GoogleDrive/DeltaNutrientExperiment'


light<-read.csv(paste0(dropbox_dir, '/Data/NutrientExperiment/LightProfiles/LightProfiles_SSCN_Oct2018.csv'), stringsAsFactors = F)



# light_df<-light[light$Depth_m>0,]
light_df<-light
light_df$PAR_unit<-as.numeric(light_df$PAR_unit)
light_df$Date<-as.Date(light_df$Date, format='%m/%d/%Y')
light_df<-light_df[which(light_df$Date > as.Date('2018-09-15') & light_df$Date < as.Date('2018-10-26')),]
light_df$Station<-factor(light_df$Station, sitetable$site1)

light_df$ln_par<-log(light_df$PAR_unit)

# model<-lm(light_df$PAR_unit ~ light_df$Depth_m | light_df$Station)
# 
# summary(model)
# anova(model)

dates<-unique(light_df$Date)
stations<-unique(light_df$Station)

day=1
output_list<-list()

for (day in 1:length(dates)){

  data_i<-light_df[which(light_df$Date ==dates[day]), ]
  
  surface_data<-data_i[which(data_i$Depth_m==0),]
  water_data<-data_i[which(data_i$Depth_m>0),]
  
  models<-lapply(stations, function (x) lm(ln_par ~ Depth_m, data=water_data[water_data$Station == x,]))
  slopes<-sapply(models, function (l) coef(l)[2])
  
  out_df<-data.frame(stations, slopes*(-1))
  names(out_df)<-c("Site", "kd_meters")
  out_df$Date<-rep(dates[day], nrow(out_df))
  
output_list[[day]]<-out_df



}
kd_alldates<-ldply(output_list, data.frame)

kd_alldates$PhoticDepth_m=log(1/100)/(kd_alldates$kd_meters*(-1))

ggplot(aes(x=Date, y=kd_meters, group=Site, colour=Site), data=kd_alldates)+
  geom_path() + 
  geom_point() +
  theme_bw() + 
  labs(y=expression(paste('Light extinction (m'^'-1', ')')))

# ggplot(aes(x=Date, y=PhoticDepth_m, group=Site, colour=Site), data=kd_alldates)+
#   geom_path() + 
#   geom_point() +
#   theme_bw() + 
#   labs(y=expression(paste('Photic Depth (m)')))

ggplot(aes(x=Date, y=PhoticDepth_m, group=Site, colour=Site), data=kd_alldates)+
  geom_path() + 
  geom_point() +
  theme_bw() + 
  labs(y=expression(paste('Photic Depth (m)')))

plot(kd_alldates$kd_meters, kd_alldates$PhoticDepth_m)

write.csv(kd_alldates, file=paste(google_dir, 'DataOutputs', 'Kd_NutrientExperiment.csv', sep='/'), row.names=F)

saveRDS(kd_alldates , file=paste0(dropbox_dir, '/Data/Rdata/kd_alldates.rds'))

