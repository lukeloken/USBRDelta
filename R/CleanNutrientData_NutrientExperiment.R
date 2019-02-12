


#Code to download meta and field sheet data

library(readxl)
library(dplyr)
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

nutrient_df$`DIN-ppm`<-nutrient_df$`NH4-ppm` + nutrient_df$`NO3-ppm` 
nutrient_df$`DON-ppm`<-nutrient_df$`TDN-ppm` - nutrient_df$`DIN-ppm`
nutrient_df$`TPN-ppm`<-nutrient_df$`TN-ppm` - nutrient_df$`TDN-ppm`


#TSS data
tss_df<-read_excel(paste0(google_dir, "/Data/NutrientExperiment/WaterChemistry/SSCN_TSSData.xlsx"), skip=1)

tss_df_names<-names(read_excel(paste0(google_dir, "/Data/NutrientExperiment/WaterChemistry/SSCN_TSSData.xlsx")))

names(tss_df)[1:7]<-tss_df_names[1:7]
rm(tss_df_names)

tss_df_sub<-tss_df[c("SampleCode", "Date", "Event", "Site", 'SampleLabel', 'TSS', 'VSS')]

#Oxygen18data
O18<-read.table(paste0(dropbox_dir,"/Data/NutrientExperiment/Oxygen18/LokenSadro_ExetData_decomposed.txt"), sep='\t', skip=71, header=T)

O18$SampleLabel<-sub('_[ab]', '', O18$Group.1 )

O18avg<- O18 %>%
  select(-Group.1, -flag.smallArea32) %>%
  group_by(SampleLabel) %>%
  summarize_all(mean)

#Clean Times
nutrient_df$Date<-as.Date(nutrient_df$Date)
tss_df_sub$Date<-as.Date(tss_df_sub$Date)

nut_tss_df<-full_join(nutrient_df, tss_df_sub)

nut_tss_O18_df<-left_join(nut_tss_df, O18avg, by='SampleLabel')

#Add site names
sitetable<-data.frame(site1=c('NL70', 'EC2','EC3','EC4','EC5','EC6','EC7','EC8','NL76'), site2=c( "SSCN01_NV70", "SSCN02", "SSCN03", "SSCN04", "SSCN05", "SSCN06", "SSCN07", "SSCN08", "SSCN09 NL76"), site3=str_pad(1:9, 2, pad='0'))

nut_tss_O18_df$Site<-sitetable$site1[match(nut_tss_O18_df$Site, sitetable$site3)]

nut_tss_O18_df<-  nut_tss_O18_df[,-grep('X__', names(nut_tss_O18_df))]

head(as.data.frame(nut_tss_O18_df))

write.table(nut_tss_O18_df, file=paste0(dropbox_dir, '/Data/NutrientExperiment/NutrientData.csv'), row.names=F, sep=',')
