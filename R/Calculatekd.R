

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

# Project folder where outputs are stored
dropbox_dir<-'C:/Dropbox/USBR Delta Project'

#Where data come from
google_dir<-'C:/GoogleDrive/DeltaNutrientExperiment'


light<-read.csv(paste0(dropbox_dir, '/Data/LightProfiles/081618.csv'))
light_df<-light[light$Depth_m>0,]

light_df$ln_par<-log(light_df$PAR_unit)

model<-lm(light_df$PAR_unit ~ light_df$Depth_m | light_df$Station)

summary(model)
anova(model)

stations<-unique(light_df$Station)
models<-lapply(stations, function (x) lm(ln_par ~ Depth_m, data=light_df[light_df$Station == x,]))

slopes<-sapply(models, function (l) coef(l)[2])

out_df<-data.frame(stations, slopes*(-1))
names(out_df)<-c("Station", "kd_meters-1")

plot(out_df$`kd_meters-1` ~ out_df$Station, xlab='Station', ylab=expression(paste('Kd (m'^'-1', ')')))

write.csv(out_df, file=paste0(dropbox_dir, '/Data/LightProfiles/CalculatedAttenuation081618.csv'), row.names=F)
