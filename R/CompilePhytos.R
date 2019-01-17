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

Phyto_df<-Phyto_df[which(!is.na(Phyto_df$STATION) | !is.na(Phyto_df$DATE)),]


#Rename stations
AllStations<-unique(Phyto_df$STATION)

greps<-c(16,34,44,56,62,64,66,70,74,76,84,'Pro', 'WSP')

names16<-c(16, AllStations[grep('16', AllStations)])
names34<-c(34, AllStations[grep('34', AllStations)])
names44<-c(44, AllStations[grep('44', AllStations)])
names56<-c(56, AllStations[grep('56', AllStations)])
names62<-c(62, AllStations[grep('62', AllStations)])
names64<-c(64, AllStations[grep('64', AllStations)])
names66<-c(66, AllStations[grep('66', AllStations)])
names70<-c(70, AllStations[grep('70', AllStations)])
names74<-c(74, AllStations[grep('74', AllStations)])
names76<-c(76, AllStations[grep('76', AllStations)])
names84<-c(84, AllStations[grep('84', AllStations)])

namesPro <- c("Pro", "Prospect", "Prospect-1/PS" , "PSL", "Prospect/Stair Steps", "Prospect 1", "Prospect 51", "Prospect-1",  "Prospect AM")
namesWSP<-c("WSP","COE West Sac", "COE Gate/W. Sac", "West Sac Port", "WS-Port", "COE Gate / W. Sac. Port", "W. Sac. Port","West Sac.", "W. Sac PM", "W. Sac AM", "West Sac", "W. Sac", "W.S.P." )

names_list<-list(names16, names34, names44, names56, names62, names64, names66, names70, names74, names76, names84, namesPro, namesWSP)

Phyto_df$STATIONclean<-NA

station<-1
for (station in 1:length(names_list)){
  Phyto_df$STATIONclean[which(Phyto_df$STATION %in% names_list[[station]])]<-names_list[[station]][1]
}
head(Phyto_df)

unique(Phyto_df$STATION[is.na(Phyto_df$STATIONclean)])

write.csv(Phyto_df, file=paste(dropbox_dir, 'Data', 'Phyto', 'PhytoCountsAll.csv', sep='/'))




#Zooplankton

ZooFiles<-list.files(paste(google_dir, 'Data', 'Zoops', sep='/'))
ZooFiles<-ZooFiles[grep('.xls', ZooFiles)]
ZooFiles<-ZooFiles[-grep('SSC Zoops Date comparison', ZooFiles)]

ZooKeepNames<-c('bottle ID', 'date', 'genus', 'species', 'division', 'notes', "tow length (m)", "net radius (cm)", "tow volume filtered (L)", "total sample volume (ml)", "aliquot (ml)", "count factor", "#individuals counted", "# / L" , "biomass factor", "species biomass (µg d.w./L)")

File_i=1
Zoo_list<-list()
for (File_i in 1:length(ZooFiles)){
  col1<-read_excel(paste(google_dir, 'Data', 'Zoops', ZooFiles[File_i], sep='/'), skip=0)[,1]
  
  headerrow<-which(col1=='bottle ID')
  if(length(headerrow)==0){
    zoo_i<-read_excel(paste(google_dir, 'Data', 'Zoops', ZooFiles[File_i], sep='/'))
  } else if (length(headerrow)>0){
    zoo_i<-read_excel(paste(google_dir, 'Data', 'Zoops', ZooFiles[File_i], sep='/'), skip=(headerrow))
  }
  
  zoo_i<-zoo_i[,intersect(ZooKeepNames, names(zoo_i))]
  
  zoo_i<-zoo_i[which(!is.na(zoo_i$`bottle ID`) | !is.na(zoo_i$date)),]
  zoo_i$date<-as.Date(zoo_i$date, tryFormats=c('%m/%d/%Y'))
  
  Zoo_list[[File_i]]<-zoo_i

}

Zoo_df<-ldply(Zoo_list, data.frame)
Zoo_df$species.biomass..µg.d.w..L.<-as.numeric(Zoo_df$species.biomass..µg.d.w..L.)

Zoo_df$biomass.factor <-as.numeric(Zoo_df$biomass.factor)
