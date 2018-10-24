


#Code to download meta and field sheet data

library(readxl)
library(plyr)
library(viridis)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(stringr)

source('R/read_excel_allsheets.R')
source('R/g_legend.R')

# Project folder where outputs are stored
dropbox_dir<-'C:/Dropbox/USBR Delta Project'

#Where data come from
google_dir<-'C:/GoogleDrive/DeltaNutrientExperiment'


nutrient_df<-read_excel(paste0(google_dir, "/Data/NutrientExperiment/WaterChemistry/SSCN_NutrientData.xlsx"), skip=1)

nutrient_df_names<-names(read_excel(paste0(google_dir, "/Data/NutrientExperiment/WaterChemistry/SSCN_NutrientData.xlsx")))

names(nutrient_df)<-nutrient_df_names
rm(nutrient_df_names)


#Clean Times
nutrient_df$Date<-as.Date(nutrient_df$Date)

#Add site names
sitetable<-data.frame(site1=c('NL70', 'EC2','EC3','EC4','EC5','EC6','EC7','EC8','NL76'), site2=c( "SSCN01_NV70", "SSCN02", "SSCN03", "SSCN04", "SSCN05", "SSCN06", "SSCN07", "SSCN08", "SSCN09 NL76"), site3=str_pad(1:9, 2, pad='0'))

nutrient_df$Site<-sitetable$site1[match(nutrient_df$Site, sitetable$site3)]

head(as.data.frame(nutrient_df))

nutrient_df<-  nutrient_df[,-grep('X__', names(nutrient_df))]

write.table(nutrient_df, file=paste0(dropbox_dir, '/Data/NutrientExperiment/NutrientData.csv'), row.names=F, sep=',')
