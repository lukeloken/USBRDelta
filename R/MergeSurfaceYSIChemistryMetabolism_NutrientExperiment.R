


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

# Find all filenames in directory
# These will be used to loop through all old data

bigdf <- read.csv(file=paste0(dropbox_dir, '/Data/NutrientExperiment/SurfaceChemistry/YSISurface.csv'), stringsAsFactors = F)
bigdf$Date<-as.Date(bigdf$Date, format='%Y-%m-%d')

sitetable<-data.frame(site1=c('NL70', 'EC2','EC3','EC4','EC5','EC6','EC7','EC8','NL76'), site2=c( "SSCN01_NV70", "SSCN02", "SSCN03", "SSCN04", "SSCN05", "SSCN06", "SSCN07", "SSCN08", "SSCN09 NL76"))

bigdf$Site<-sitetable$site1[match(bigdf$Station, sitetable$site2)]

# Merge metabolism results

summary_df<-read.csv(file=paste0(dropbox_dir, '/Data/NutrientExperiment/IncubationMetabolism/', 'LightDarkMetabolism.csv'), header=T, stringsAsFactors = F)
str(summary_df)
summary_df$Date<-as.Date(summary_df$Date)

incubation_df<-summary_df %>%
  select(Date, Site, Metric, mean) %>%
  spread(key=Metric, value=mean)



# MergeWaterChemistry
# No Data yet

# #Data file as two rows as a header with mixed units/labels
# df1<-read_excel(paste0(google_dir, '/Data/WaterQuality/SSC Nutrients August 2018 Update.xlsx'), skip=1)
# df2<-read_excel(paste0(google_dir, '/Data/WaterQuality/SSC Nutrients August 2018 Update.xlsx'), skip=0)
# 
# names1A<-gsub("\\_.*","",names(df1))
# names1B<-gsub("X", '', names1A)
# names1C<-gsub("2012", '', names1B)
# 
# names2A<-gsub("\\X.*","",names(df2))
# 
# names<-paste0(names2A, names1C)
# 
# names(df1)<-names
# 
# df_sub<-df1[,-which(names(df1) %in% c("", "Chlo+Pheoppb",  "Pre-HClPost-HCl"))]
# names(df_sub)[which(names(df_sub)=="Chlo+Pheo__1ppb")]<-"Chlo+Pheoppb"
# names(df_sub)[which(names(df_sub)=="location")]<-'Station'
# names(df_sub)[which(names(df_sub)=="pH")]<-'pH_WQ'
# 
# 
# df_sub_noNA<-df_sub[!is.na(df_sub$Date),]
# 
# unique(df_sub_noNA$Station)
# 
# goodstations<-c('NL 16', 'NL 34', 'NL 44', 'NL 56', 'NL 62', 'NL 66', 'NL 70', 'NL 74', 'NL 76', 'NL 84', 'WSP', 'PS', 'RB 34')
# 
# df_stations<-df_sub_noNA[df_sub_noNA$Station %in% goodstations,]
# df_stations$Station<-gsub('PS', 'Pro', df_stations$Station)
# df_stations$Station<-gsub('NL ', '', df_stations$Station)
# df_stations$Station<-gsub('RB ', '', df_stations$Station)
# 
# df_stations$Date<-as.Date(df_stations$Date, format='%Y-%m-%d')
# 
# merge_df<-full_join(df_stations, bigdf)
# 
# head(merge_df)

merge1<-full_join(incubation_df, bigdf)
merge2<-select(merge1, -Station, -Depth.feet,- DateTime.PT)


#Merge water chemistry data
merge_df<-merge2
write.csv(merge_df, file=paste0(dropbox_dir, '/Data/NutrientExperiment/SurfaceChemistry/YSIMetabolismSurface.csv'), row.names=F)
