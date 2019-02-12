


#Code to extract surface water measurements from profile data at fixed sites

library(readxl)
library(plyr)
library(tidyr)
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

sitetable<-data.frame(site1=c('NL70', 'EC2','EC3','EC4','EC5','EC6','EC7','EC8','NL76'), site2=c( "SSCN01_NV70", "SSCN02", "SSCN03", "SSCN04", "SSCN05", "SSCN06", "SSCN07", "SSCN08", "SSCN09 NL76"), site4=1:9)

#Field notes and flame data
field_df_withFlame<-read.csv(file=paste0(dropbox_dir, '/Data/NutrientExperiment/FlameSiteData.csv'),sep=',')
field_df_withFlame$Date<-as.Date(field_df_withFlame$Date)
field_df_withFlame$Site<-sitetable$site1[match(field_df_withFlame$Location, sitetable$site4)]


#Water chemistry
nutrient_df<-read.csv(file=paste0(dropbox_dir, '/Data/NutrientExperiment/NutrientData.csv'), sep=',', header=T, stringsAsFactors = F)
nutrient_df$Date<-as.Date(nutrient_df$Date)
nutrient_df<-nutrient_df[which(nutrient_df$Site %in% sitetable$site1),]
nutrient_surface<-nutrient_df[grep('_S', nutrient_df$SampleLabel),]
nutrient_surface<-select(nutrient_surface, -Turbidity, -EC, -PH, -Lab..) 

#YSI surface data
bigdf <- read.csv(file=paste0(dropbox_dir, '/Data/NutrientExperiment/SurfaceChemistry/YSISurface.csv'), stringsAsFactors = F)
bigdf$Date<-as.Date(bigdf$Date, format='%Y-%m-%d')

bigdf$Site<-sitetable$site1[match(bigdf$Station, sitetable$site2)]

# incubation metabolism results
summary_df<-read.csv(file=paste0(dropbox_dir, '/Data/NutrientExperiment/IncubationMetabolism/', 'LightDarkMetabolism.csv'), header=T, stringsAsFactors = F)
str(summary_df)
summary_df$Date<-as.Date(summary_df$Date)

incubation_df<-summary_df %>%
  select(Date, Site, Metric, mean) %>%
  spread(key=Metric, value=mean)


#Merge everything together

merge1<-full_join(incubation_df, bigdf)
merge2<-select(merge1, -Station, -Depth.feet,- DateTime.PT)

#Field notes
merge3<-full_join(merge2, field_df_withFlame)
merge4<-select(merge3, -Date.1)

#Nutrient data
merge5<-full_join(merge4, nutrient_surface)


#Merge water chemistry data
merge_df<-merge5
write.csv(merge_df, file=paste0(dropbox_dir, '/Data/NutrientExperiment/SurfaceChemistry/YSIMetabolismSurface.csv'), row.names=F)
