# Load all phytoplankton and zooplankton data and merge to a common file

library(readxl)
library(plyr)
library(lubridate)

# source('R/read_excel_allsheets.R')
# source('R/g_legend.R')
# 
# # Project folder where outputs are stored
# dropbox_dir<-'C:/Dropbox/USBR Delta Project'
# 
# #Where data come from
# google_dir<-'C:/GoogleDrive/DeltaNutrientExperiment'

# PhytoFiles<-list.files(paste(google_dir, 'Data', 'Phyto', sep='/'))
# PhytoFiles<-PhytoFiles[grep('.xls', PhytoFiles)]

PhytoFiles<-list.files(file.path(onedrive_dir, 'RawData', 'MonthlyCruises', 
                             'Phyto2'))
PhytoFiles<-PhytoFiles[grep('.xls', PhytoFiles)]

KeepNames<-c('STATION', 'SAMPLE', 'GENUS', 'DIVISION', 'TALLY', 'DENSITY', 'TOTAL BV', 'DENSITY (cells/L)', 'NOTES', 'DATE')

File_i=14
Phyto_list<-list()
for (File_i in 1:length(PhytoFiles)){
  Phyto_list[[File_i]] <- read_excel(file.path(onedrive_dir, 
                                             'RawData', 
                                             'MonthlyCruises',
                                             'Phyto2', 
                                             PhytoFiles[File_i]),
                                   skip=1)
  PhytoNames <- names(read_excel(file.path(onedrive_dir, 
                                         'RawData', 
                                         'MonthlyCruises',
                                         'Phyto2', 
                                         PhytoFiles[File_i]),
                               skip=0))
  
  PhytoNames[which(PhytoNames=="DENSITY (cells/L)")]<-"DENSITY"
  
  PhytoNames[grepl("DATE",names(Phyto_list[[File_i]]), ignore.case = TRUE)] <- "DATE"
  names(Phyto_list[[File_i]]) <- PhytoNames
 
  Phyto_list[[File_i]]<-Phyto_list[[File_i]][,intersect(KeepNames, PhytoNames)]

}

if (length(Phyto_list) != length(PhytoFiles)) {
  warning("Not all files in Phyto list")
}

Phyto_df<-ldply(Phyto_list, data.frame) %>% distinct()




# head(Phyto_df)
# head(Phyto_list[[1]])

Phyto_FullRecord<- Phyto_df %>%
  drop_na(STATION) %>%
  dplyr::rename(bottle.ID = STATION) %>%
  mutate(DATE=as.Date(DATE), Station=NA)

rm(Phyto_list, Phyto_df, PhytoFiles, PhytoNames, KeepNames, File_i)

ggplot(Phyto_FullRecord, aes(x = Date, y = bottle.ID)) + geom_point()


# ###########
# Zooplankton
# ###########

ZooFiles<-list.files(file.path(onedrive_dir, 'RawData', 'MonthlyCruises', 
                                 'Zoops2'))

# ZooFiles<-list.files(paste(google_dir, 'Data', 'Zoops', sep='/'))
ZooFiles<-ZooFiles[grep('.xls', ZooFiles)]
# ZooFiles<-ZooFiles[-grep('SSC Zoops Date comparison', ZooFiles)]



File_i=1
Zoo_list<-list()
for (File_i in 1:length(ZooFiles)){
  col1<-read_excel(file.path(onedrive_dir, 'RawData', 'MonthlyCruises', 
                             'Zoops2', ZooFiles[File_i]), skip=0)[,1]
  
  headerrow<-which(col1=='bottle ID')
  if(length(headerrow)==0){
    zoo_i<-read_excel(file.path(onedrive_dir, 'RawData', 'MonthlyCruises', 
                                'Zoops2', ZooFiles[File_i]))
  } else if (length(headerrow)>0){
    zoo_i<-read_excel(file.path(onedrive_dir, 'RawData', 'MonthlyCruises', 
                                'Zoops2', ZooFiles[File_i]), skip=(headerrow))
  }
  

  zoo_i<-zoo_i[which(!is.na(zoo_i$`bottle ID`) | !is.na(zoo_i$date)),]
  zoo_i$date<-as.Date(zoo_i$date, tryFormats=c('%m/%d/%Y'))
  # zoo_i$date<-as.Date(zoo_i$date)
  
  Zoo_list[[File_i]]<-zoo_i

  rm(headerrow)
}

if (length(Zoo_list) != length(ZooFiles)) {
  warning("Not all files in Phyto list")
}


Zoo_df<-ldply(Zoo_list, data.frame)

# dplyr::select(Zoo_df, starts_with("species.biomass"))

ZooKeepNames<-c('bottle.ID', 'Date', 'genus', 'species', 'division', 'notes', "NetRadius_cm", "TowVolume_L", "TotalSampleVolume_mL", "Aliquot_mL", "CountFactor", "NumberIndividualsCounted", "NumberPerLiter" , "BiomassFactor", "SpeciesBiomass_ugdwL")



Zoo_FullRecord <- Zoo_df %>%
  dplyr::rename(Date = date,
         SpeciesBiomass_ugdwL = starts_with("species.biomass"),
         BiomassFactor = starts_with("biomass.factor"),
         Aliquot_mL = starts_with("aliquot"),
         NetRadius_cm = starts_with("net.radius"),
         TowVolume_L = starts_with("tow.volume.filtered"),
         TotalSampleVolume_mL = starts_with("total.sample.volume"),
         CountFactor = starts_with("count.factor"),
         NumberIndividualsCounted = starts_with("X.individuals.counted"),
         NumberPerLiter = starts_with("X....L")) %>%
         # TowLength = starts_with("tow.length")) %>%
  dplyr::select(one_of(ZooKeepNames)) %>%
  mutate(SpeciesBiomass_ugdwL = as.numeric(SpeciesBiomass_ugdwL),
         BiomassFactor = as.numeric(BiomassFactor), 
         Station=NA) %>% distinct()


ggplot(Zoo_FullRecord, aes(x = Date, y = bottle.ID)) + geom_point()


rm(Zoo_list, Zoo_df, zoo_i, File_i, headerrow, ZooKeepNames, ZooFiles, col1)

# #####################################
# Rename phyto and zooplankton stations
# #####################################


AllStations<-unique(c(Phyto_FullRecord$bottle.ID, Zoo_FullRecord$bottle.ID))

#SSC known sites
LongTermSites<-c(16,34,44,'Pro',56,62,64,66,70,74,76,84, 'WSP')

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

namesPro <- c("Pro", "Prospect", "Prospect-1/PS" , "PSL", "Prospect 1", "Prospect 51", "Prospect-1",   "Prospect -1", "Prospect AM", "PS")
namesWSP<-c("WSP","COE West Sac", "COE Gate/W. Sac", "West Sac Port", "WS-Port", "COE Gate / W. Sac. Port", "W. Sac. Port","West Sac.", "W. Sac PM", "W. Sac AM", "West Sac", "W. Sac", "W.S.P.", "West Sacs", "COE Gate W. Sac Port", "Port", "Port-gate", "COE Gate", "COE Gates", "West Steps", "West Steps/W. Sac", "West Steps/W. Sac.")

names_list<-list(names16, names34, names44, namesPro, names56, names62, names64, names66, names70, names74, names76, names84, namesWSP)


station<-1
for (station in 1:length(names_list)){
  Phyto_FullRecord$Station[which(Phyto_FullRecord$bottle.ID %in% names_list[[station]])]<-names_list[[station]][1]
  Zoo_FullRecord$Station[which(Zoo_FullRecord$bottle.ID %in% names_list[[station]])]<-names_list[[station]][1]
}
# head(Phyto_FullRecord)
# head(Zoo_FullRecord)

Phyto_FullRecord$Station<-factor(Phyto_FullRecord$Station, LongTermSites)
Zoo_FullRecord$Station<-factor(Zoo_FullRecord$Station, LongTermSites)

Phyto_FullRecord <- Phyto_FullRecord %>% 
  arrange(Date, Station)
  
Phyto_badIDs <- unique(Phyto_FullRecord$bottle.ID[is.na(Phyto_FullRecord$Station)])
Zoo_badIDs <- unique(Zoo_FullRecord$bottle.ID[is.na(Zoo_FullRecord$Station)])

badIDs<-c(Phyto_badIDs, Zoo_badIDs)

write.csv(Phyto_FullRecord, file=paste(google_dir, 'DataOutputs', 'PhytoCountsAll.csv', sep='/'), row.names=F)
write.csv(Zoo_FullRecord, file=paste(google_dir, 'DataOutputs', 'ZoopsCountsAll.csv', sep='/'), row.names=F)

saveRDS(Phyto_FullRecord , file=paste0(dropbox_dir, '/Data/Rdata/Phyto_FullRecord.rds'))
saveRDS(Zoo_FullRecord , file=paste0(dropbox_dir, '/Data/Rdata/Zoo_FullRecord.rds'))

saveRDS(Phyto_FullRecord , file = file.path(onedrive_dir, "RData", "MonthlyCruises", "Phyto_FullRecord.rds"))
saveRDS(Zoo_FullRecord , file = file.path(onedrive_dir, "RData", "MonthlyCruises", 'Zoo_FullRecord.rds'))

write.csv(Phyto_FullRecord, file = file.path(onedrive_dir, "OutputData", "MonthlyCruises", 'PhytoCountsAll.csv'), row.names = FALSE)
write.csv(Zoo_FullRecord, file = file.path(onedrive_dir, "OutputData", "MonthlyCruises", 'ZoopsCountsAll.csv'), row.names=FALSE)


rm(list=as.character(paste("names", LongTermSites, sep='')))
rm(AllStations, LongTermSites, station, names_list)
   
# End

ggplot(Phyto_FullRecord, aes(x = DATE, y = Station)) + geom_point()
ggplot(Zoo_FullRecord, aes(x = Date, y = Station)) + geom_point()

