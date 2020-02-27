
# Loop through zooplankton, phytoplankton, and picplankton
# from nutrient experiment
# Export a clean data table with each

library(readxl)
library(plyr)
library(tidyr)
library(lubridate)

# #############
# Phytoplankton
# #############

PhytoFiles<-list.files(file.path(onedrive_dir, 'RawData', 'NutrientExperiment2', 'Phytos'))
PhytoFiles<-PhytoFiles[grepl('.xls', PhytoFiles)]

if (length(PhytoFiles)>0){
KeepNames<-c('STATION', 'SAMPLE', 'GENUS', 'DIVISION', 'TALLY', 'DENSITY', 'TOTAL BV', 'DENSITY (cells/L)', 'NOTES')

File_i=1
Phyto_list<-list()
for (File_i in 1:length(PhytoFiles)){
  Phyto_list[[File_i]]<-read_excel(file.path(onedrive_dir, 'RawData', 'NutrientExperiment2', 'Phytos', PhytoFiles[File_i]), skip=1)
  PhytoNames<-names(read_excel(file.path(onedrive_dir, 'RawData', 'NutrientExperiment2', 'Phytos', PhytoFiles[File_i]), skip=0))

  PhytoNames[which(PhytoNames=="DENSITY (cells/L)")]<-"DENSITY"
  names(Phyto_list[[File_i]])<-PhytoNames

  Phyto_list[[File_i]]<-Phyto_list[[File_i]][,intersect(KeepNames, PhytoNames)]

}

Phyto_df<-ldply(Phyto_list, data.frame)

Phyto_FullRecord<- Phyto_df %>%
  drop_na(STATION, SAMPLE) %>%
  dplyr::rename(bottle.ID = STATION) 
Phyto_FullRecord <- Phyto_FullRecord %>% 
  mutate(Date=as.Date(SAMPLE), 
         site3=paste0("", str_split(Phyto_FullRecord$bottle.ID, "_", simplify=T)[,3])) %>%
  left_join(sitetable) %>%
  mutate(Station=factor(site1, (sitetable$site1)))

head(Phyto_FullRecord)

#Save files
write.csv(Phyto_FullRecord, file=file.path(onedrive_dir, 'OutputData', 'NutrientExperiment2', 'Phytos', 'PhytosCountsAll_NutExp2.csv'), row.names=F)
saveRDS(Phyto_FullRecord , file=file.path(onedrive_dir, 'RData', 'NutrientExperiment2', 'Phytos', 'Phyto_FullRecord_NutExp2.rds'))

}

rm(Phyto_list, Phyto_df, PhytoFiles, PhytoNames, KeepNames, File_i)

# ###########
# Zooplankton
# ###########

ZooFiles<-list.files(file.path(onedrive_dir, 'RawData', 'NutrientExperiment2', 'Zoops'))
ZooFiles<-ZooFiles[grep('.xls', ZooFiles)]

if(length(ZooFiles)>0){

File_i=1
Zoo_list<-list()
for (File_i in 1:length(ZooFiles)){
  col1<-read_excel(file.path(onedrive_dir, 'RawData', 'NutrientExperiment2', 'Zoops', ZooFiles[File_i]), skip=0)[,1]
  
  headerrow<-which(col1=='sample code')
  if(length(headerrow)==0){
    zoo_i<-read_excel(file.path(onedrive_dir, 'RawData', 'NutrientExperiment2', 'Zoops', ZooFiles[File_i]))
  } else if (length(headerrow)>0){
    zoo_i<-read_excel(file.path(onedrive_dir, 'RawData', 'NutrientExperiment2', 'Zoops', ZooFiles[File_i]), skip=(headerrow))
  }
  
  
  zoo_i<-zoo_i[which(!is.na(zoo_i$`sample code`) | !is.na(zoo_i$date)),]
  zoo_i$date<-as.Date(zoo_i$date, tryFormats=c('%m/%d/%Y'))
  # zoo_i$date<-as.Date(zoo_i$date)
  
  Zoo_list[[File_i]]<-zoo_i
  
}

Zoo_df<-ldply(Zoo_list, data.frame)

ZooKeepNames<-c('SampleCode', 'Date', 'Event', 'Site', 'LocationCode', 'genus', 'species', 'division', 'notes', "TowVolume_L", "TotalSampleVolume_mL", "Aliquot_mL", "NumberIndividualsCounted", "NumberPerLiter" , "BiomassFactor", "SpeciesBiomass_ugdwL")

Zoo_FullRecord <- Zoo_df %>%
  dplyr::rename(Date = date,
                SampleCode = starts_with('sample.code'),
                Event = event,
                LocationCode = location.code,
                Site = location.name,
                SpeciesBiomass_ugdwL = starts_with("species.biomass"),
                BiomassFactor = starts_with("biomass.factor"),
                Aliquot_mL = starts_with("aliquot"),
                TowVolume_L = starts_with("tow.volume.filtered"),
                TotalSampleVolume_mL = starts_with("total.sample.volume"),
                NumberIndividualsCounted = starts_with("X.individuals.counted"),
                NumberPerLiter = starts_with("X....L")) %>%
  dplyr::select(one_of(ZooKeepNames)) %>%
  mutate(SpeciesBiomass_ugdwL = as.numeric(SpeciesBiomass_ugdwL),
         BiomassFactor = as.numeric(BiomassFactor), 
         Station=NA)


Zoo_FullRecord$Station<-factor(Zoo_FullRecord$Site, sitetable$site1)

write.csv(Zoo_FullRecord, file=file.path(onedrive_dir, 'OutputData', 'NutrientExperiment2', 'Zoops', 'ZoopsCountsAll_NutExp2.csv'), row.names=F)
saveRDS(Zoo_FullRecord , file=file.path(onedrive_dir, 'RData', 'NutrientExperiment2', 'Zoops', 'Zoo_FullRecord_NutExp2.rds'))

}

rm(Zoo_list, Zoo_df, zoo_i, File_i, headerrow, ZooKeepNames, ZooFiles, col1)


# #########################################################
# Picoplankton
# One excel sheet with pico on sheet 1, bacteria on sheet 2
# #########################################################
