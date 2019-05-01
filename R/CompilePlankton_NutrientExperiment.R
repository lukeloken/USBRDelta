
# Loop through zooplankton, phytoplankton, and picplankton
# from nutrient experiment
# Export a clean data table with each

library(readxl)
library(plyr)
library(lubridate)

# #############
# Phytoplankton
# #############

PhytoFiles<-list.files(paste(google_dir, 'Data', 'NutrientExperiment', 'Phytoplankton', sep='/'))
PhytoFiles<-PhytoFiles[grep('.xls', PhytoFiles)]

KeepNames<-c('STATION', 'SAMPLE', 'GENUS', 'DIVISION', 'TALLY', 'DENSITY', 'TOTAL BV', 'DENSITY (cells/L)', 'NOTES')

File_i=1
Phyto_list<-list()
for (File_i in 1:length(PhytoFiles)){
  Phyto_list[[File_i]]<-read_excel(paste(google_dir, 'Data', 'NutrientExperiment', 'Phytoplankton', PhytoFiles[File_i], sep='/'), skip=1)
  PhytoNames<-names(read_excel(paste(google_dir,  'Data', 'NutrientExperiment', 'Phytoplankton', PhytoFiles[File_i], sep='/'), skip=0))

  PhytoNames[which(PhytoNames=="DENSITY (cells/L)")]<-"DENSITY"
  names(Phyto_list[[File_i]])<-PhytoNames

  Phyto_list[[File_i]]<-Phyto_list[[File_i]][,intersect(KeepNames, PhytoNames)]

}

Phyto_df<-ldply(Phyto_list, data.frame)

Phyto_FullRecord<- Phyto_df %>%
  drop_na(STATION, SAMPLE) %>%
  dplyr::rename(bottle.ID = STATION) %>%
  mutate(Date=as.Date(SAMPLE), 
         Station=paste0("EC", str_split(Phyto_FullRecord$bottle.ID, "_", simplify=T)[,3]))

rm(Phyto_list, Phyto_df, PhytoFiles, PhytoNames, KeepNames, File_i)

#Save files
write.csv(Phyto_FullRecord, file=paste(google_dir, 'DataOutputs', 'PhytosCountsAll_NutExp1.csv', sep='/'), row.names=F)
saveRDS(Phyto_FullRecord , file=paste0(dropbox_dir, '/Data/Rdata/Phyto_FullRecord_NutExp1.rds'))


# ###########
# Zooplankton
# ###########

ZooFiles<-list.files(paste(google_dir, 'Data', 'NutrientExperiment', 'Zooplankton', sep='/'))
ZooFiles<-ZooFiles[grep('.xls', ZooFiles)]


File_i=1
Zoo_list<-list()
for (File_i in 1:length(ZooFiles)){
  col1<-read_excel(paste(google_dir, 'Data', 'NutrientExperiment', 'Zooplankton', ZooFiles[File_i], sep='/'), skip=0)[,1]
  
  headerrow<-which(col1=='sample code')
  if(length(headerrow)==0){
    zoo_i<-read_excel(paste(google_dir, 'Data', 'NutrientExperiment', 'Zooplankton', ZooFiles[File_i], sep='/'))
  } else if (length(headerrow)>0){
    zoo_i<-read_excel(paste(google_dir, 'Data', 'NutrientExperiment', 'Zooplankton', ZooFiles[File_i], sep='/'), skip=(headerrow))
  }
  
  
  zoo_i<-zoo_i[which(!is.na(zoo_i$`sample code`) | !is.na(zoo_i$date)),]
  zoo_i$date<-as.Date(zoo_i$date, tryFormats=c('%m/%d/%Y'))
  # zoo_i$date<-as.Date(zoo_i$date)
  
  Zoo_list[[File_i]]<-zoo_i
  
}

Zoo_df<-ldply(Zoo_list, data.frame)

ZooKeepNames<-c('SampleCode', 'Date', 'event', 'site', 'rep', 'genus', 'species', 'division', 'notes', "TowVolume_L", "TotalSampleVolume_mL", "Aliquot_mL", "NumberIndividualsCounted", "NumberPerLiter" , "BiomassFactor", "SpeciesBiomass_ugdwL")

Zoo_FullRecord <- Zoo_df %>%
  dplyr::rename(Date = date,
                SampleCode = starts_with('sample.code'),
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


rm(Zoo_list, Zoo_df, zoo_i, File_i, headerrow, ZooKeepNames, ZooFiles, col1)


Zoo_FullRecord$Station<-sitetable$site1[match(Zoo_FullRecord$site, sitetable$site3)]

write.csv(Zoo_FullRecord, file=paste(google_dir, 'DataOutputs', 'ZoopsCountsAll_NutExp1.csv', sep='/'), row.names=F)

saveRDS(Zoo_FullRecord , file=paste0(dropbox_dir, '/Data/Rdata/Zoo_FullRecord_NutExp1.rds'))


# #########################################################
# Picoplankton
# One excel sheet with pico on sheet 1, bacteria on sheet 2
# #########################################################

PicoFiles<-list.files(paste(google_dir, 'Data', 'NutrientExperiment', 'Picoplankton', sep='/'))
PicoFiles<-PicoFiles[grep('.xls', PicoFiles)]

pico_list<-read_excel_allsheets(paste(google_dir, 'Data', 'NutrientExperiment', 'Picoplankton', PicoFiles[1], sep='/'))

pico_cyano<-pico_list[[1]]
pico_bact<-pico_list[[2]]

Pico_df<-ldply(pico_list, data.frame)


pico_df2 <- Pico_df %>%
  dplyr::select('STATION', 'DATE', 'CATEGORY', 'DIVISION', 'TALLY', "DENSITY_CellsPerLiter", "TOTAL_BV_um3PerLiter") %>%
  filter(!is.na(DATE))

shortnames <- substr(pico_df2$STATION, 1, 5)
pico_df2$Station<- gsub(' #', '', shortnames)

pico_totals <- dplyr::select(pico_df2, DATE, Station, CATEGORY, TOTAL_BV_um3PerLiter, -STATION) %>%
  spread(key=CATEGORY, value='TOTAL_BV_um3PerLiter') %>%
  mutate(
    Station = factor(Station, sitetable$site1),
    Date = DATE,
    Total_picocyanobacteria = `PC - rich Picocyanobacteria` + `PE - rich Picocyanobacteria`,
    Total_bacteria = `Cocci shaped bacteria` + `Rod shaped bacteria`
  ) %>%
  dplyr::select(-DATE)


rm(pico_df2, shortnames, Pico_df, pico_cyano, pico_bact, pico_list, PicoFiles)


write.csv(pico_totals, file=paste(google_dir, 'DataOutputs', 'PicosCountsAll_NutExp1.csv', sep='/'), row.names=F)
saveRDS(pico_totals, file=paste0(dropbox_dir, '/Data/Rdata/Picos_FullRecord_NutExp1.rds'))



