


#Code to download meta and field sheet data

library(readxl)
library(dplyr)
library(plyr)
library(viridis)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(stringr)

# source('R/read_excel_allsheets.R')
# source('R/g_legend.R')
# 
# # Project folder where outputs are stored
# dropbox_dir<-'C:/Dropbox/USBR Delta Project'
# 
# #Where data come from
# google_dir<-'C:/GoogleDrive/DeltaNutrientExperiment'


#Find the correct water chemistry filename
WQ_files<-list.files(paste0(google_dir, '/Data/WaterQuality'))
SSC_WQ_files<-WQ_files[grep('SSC Nutrients', WQ_files)]

if (length(grep('~', SSC_WQ_files))>0){
  SSC_WQ_files<-SSC_WQ_files[-grep('~', SSC_WQ_files)]
}

if (length(SSC_WQ_files)!=1){
  stop('Incorrect number of files in Data/WaterQuality folder. Only include one file in this folder that is the most updated version')
}


#Load water chemistry file and format
#Data file as two rows as a header with mixed units/labels
WQ_df1<-read_excel(paste0(google_dir, '/Data/WaterQuality/', SSC_WQ_files, sep=''), skip=1)
WQ_df2<-read_excel(paste0(google_dir, '/Data/WaterQuality/', SSC_WQ_files, sep=''), skip=0)


names1A<-gsub("\\_.*","",names(WQ_df1))
names1B<-gsub("X", '', names1A)
names1C<-gsub("2012", '', names1B)

names2A<-gsub("\\X.*","",names(WQ_df2))

names<-paste0(names2A, names1C)

names(WQ_df1)<-names

#Change weird header names
df_sub<-WQ_df1[,-which(names(WQ_df1) %in% c("", "Chlo+Pheoppb",  "Pre-HClPost-HCl"))]
names(df_sub)[which(names(df_sub)=="Chlo+Pheo__1ppb")]<-"ChloPheoppb"
names(df_sub)[which(names(df_sub)=="location")]<-'Station'
names(df_sub)[which(names(df_sub)=="pH")]<-'pH_WQ'
names(df_sub)[grep('Lab #', names(df_sub))]<-'Lab_nu'

names(df_sub)<-gsub(' ', '_', names(df_sub))
names(df_sub)<-gsub(';', '', names(df_sub))
names(df_sub)<-gsub('N4', 'NH4', names(df_sub))
names(df_sub)<-gsub('-', '', names(df_sub))
names(df_sub)<-gsub('/', '', names(df_sub))

#Exclude rows without data and calculate additional values
WQ_AllSamples<-df_sub %>%
  drop_na(Date, Lab_nu) %>% 
  dplyr::select (-c('NH4+NO3')) %>%
    mutate(DINppm = NH4Nppm + NO3Nppm, 
         DONppm = TDNppm - DINppm,
         TPNppm = TNppm - TDNppm,
         TPPppm = TPppm - TDPppm, 
         Date = as.Date(Date)
  )
  

write.table(WQ_AllSamples, file=paste0(google_dir, '/DataOutputs/WaterChemistryAllSamples.csv'), row.names=F, sep=',')
saveRDS(WQ_AllSamples , file=paste0(dropbox_dir, '/Data/Rdata/WQ_AllSamples'))

#Exclude weird station names
unique(WQ_AllSamples$Station)

goodstations<-c('NL 16', 'NL 34', 'NL 44', 'NL 56', 'NL 62', 'NL 64', 'NL 66', 'NL 70', 'NL 74', 'NL 76', 'NL 84', 'WSP', 'PS', 'RB 34')

WQ_stations<-WQ_AllSamples[WQ_AllSamples$Station %in% goodstations,]
WQ_stations$Station<-gsub('PS', 'Pro', WQ_stations$Station)
WQ_stations$Station<-gsub('NL ', '', WQ_stations$Station)
WQ_stations$Station<-gsub('RB ', '', WQ_stations$Station)


write.table(WQ_stations, file=paste0(google_dir, '/DataOutputs/WaterChemistryLongTermStations.csv'), row.names=F, sep=',')
saveRDS(WQ_stations , file=paste0(dropbox_dir, '/Data/Rdata/WQ_stations'))


rm(df_sub, WQ_df1, WQ_df2, SSC_WQ_files, WQ_files, goodstations)
rm(list=as.character(paste("names", c("", "1A", "1B", "1C", "2A"), sep='')))
#End

