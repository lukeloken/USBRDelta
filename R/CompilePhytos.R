# Consolidate phyto data

library(readxl)
library(plyr)
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

PhytoFiles<-list.files(paste(google_dir, 'Data', 'Phyto', sep='/'))
PhytoFiles<-PhytoFiles[grep('.xls', PhytoFiles)]

KeepNames<-c('STATION', 'SAMPLE', 'GENUS', 'DIVISION', 'TALLY', 'DENSITY', 'TOTAL BV', 'DENSITY (cells/L)', 'NOTES')

File_i=1
Phyto_list<-list()
for (File_i in 1:length(PhytoFiles)){
  Phyto_list[[File_i]]<-read_excel(paste(google_dir, 'Data', 'Phyto', PhytoFiles[File_i], sep='/'), skip=1)
  PhytoNames<-names(read_excel(paste(google_dir, 'Data', 'Phyto', PhytoFiles[File_i], sep='/'), skip=0))
  
  PhytoNames[which(PhytoNames=="DENSITY (cells/L)")]<-"DENSITY"
  names(Phyto_list[[File_i]])<-PhytoNames
 
  Phyto_list[[File_i]]<-Phyto_list[[File_i]][,intersect(KeepNames, PhytoNames)]

}

Phyto_df<-ldply(Phyto_list, data.frame)

head(Phyto_df)
head(Phyto_list[[1]])
(Phyto_list)

Phyto_df$DATE<-as.Date(Phyto_df$SAMPLE)
