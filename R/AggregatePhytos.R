
library(viridis)
library(ggplot2)
library(gridExtra)
library(plyr)
library(dplyr)
library(tidyr)
library(lubridate)
library(RColorBrewer)

# source('R/CompilePhytos.R')

# Project folder where outputs are stored
dropbox_dir<-'C:/Dropbox/USBR Delta Project'

#Where data come from
google_dir<-'C:/GoogleDrive/DeltaNutrientExperiment'


Phyto_FullRecord <- readRDS(file=paste0(dropbox_dir, '/Data/Rdata/Phyto_FullRecord.rds'))

#Need to update everything below


# ############################
# Since data are 'count' data, need to populate dataset with zeros for any genus not counted in a given sample
# ############################

#All phyto species recorded
GenusList<-unique(Phyto_FullRecord[c('GENUS', 'DIVISION')])

#Unique combinations of Date and Site
StationDates<-expand(Phyto_FullRecord, nesting(Station, Date))

#Create empty data.frame with every sample site/date with every phyto genus
StationDatesGenus<-data.frame(matrix(ncol=4, nrow=nrow(StationDates)*nrow(GenusList)))
names(StationDatesGenus)<-c("Station", "Date", "GENUS", "DIVISION")
StationDatesGenus$Station<-rep(StationDates$Station, each=nrow(GenusList))
StationDatesGenus$Date<-rep(StationDates$Date, each=nrow(GenusList))

StationDate<-1
for (StationDate in 1:nrow(StationDates)){
  StationDatesGenus[((StationDate-1)*nrow(GenusList)+1):(StationDate*nrow(GenusList)),]$GENUS<-GenusList$GENUS
  StationDatesGenus[((StationDate-1)*nrow(GenusList)+1):(StationDate*nrow(GenusList)),]$DIVISION<-GenusList$DIVISION
}

#Join empty data.frame with observations
Phyto_CompleteList<-full_join(StationDatesGenus, Phyto_FullRecord)

#Replace NAs with zeros (not observed)
Phyto_CompleteList$TALLY[which(is.na(Phyto_CompleteList$SAMPLE))]<-0
Phyto_CompleteList$DENSITY[which(is.na(Phyto_CompleteList$SAMPLE))]<-0
Phyto_CompleteList$TOTAL.BV[which(is.na(Phyto_CompleteList$SAMPLE))]<-0


# Modify columns structure to summarize and improve plotting
Phyto_CompleteList <- Phyto_CompleteList %>%
  dplyr::select(-'SAMPLE') %>%
  mutate(Month=month(Date), 
         Station=factor(Station, c('16', '34','44', 'Pro', '56','62', '64','66', '70','74', '76','84', 'WSP')))


#Save complete recrod with NAs
write.csv(Phyto_CompleteList, file=paste(google_dir, 'DataOutputs', 'Phyto_CompleteList', sep='/'), row.names=F)
saveRDS(Phyto_CompleteList , file=paste0(dropbox_dir, '/Data/Rdata/Phyto_CompleteList.rds'))


# ##################################################
# Summarize data by division then by calendar month
# ##################################################


Phyto_summary<- Phyto_CompleteList %>%
  dplyr::select(Station,Date, DIVISION, TOTAL.BV, DENSITY, Month) %>%
  group_by(Station,Date, DIVISION, Month) %>%
  summarize(Total_BioVolume=sum(TOTAL.BV)/(10^9), Density=sum(DENSITY)) %>%
  drop_na(Station)

Phyto_summary_select<- Phyto_summary %>%
  filter(DIVISION %in% c("Bacillariophyta", "Chlorophyta", "Cryptophyta", "Chrysophyta", "Cyanobacteria") & Station != '64')

#Export long summary table for ggplotting
write.csv(Phyto_summary_select, file=paste(google_dir, 'DataOutputs', 'PhytoSummaryDivisionLongTable.csv', sep='/'), row.names=F)
saveRDS(Phyto_summary_select , file=paste0(dropbox_dir, '/Data/Rdata/Phyto_summary_select.rds'))

# Make a wide table for merging with other datasets
Phyto_summary_spread <- Phyto_summary_select %>%
  dplyr::select(Station, Date, DIVISION, Total_BioVolume) %>% 
  spread(key = DIVISION, value= Total_BioVolume)

#Export summary by division table to be merged with other datasets (Nutrients, Zoops, etc.)
write.csv(Phyto_summary_spread, file=paste(google_dir, 'DataOutputs', 'PhytoSummaryDivision.csv', sep='/'), row.names=F)
saveRDS(Phyto_summary_spread , file=paste0(dropbox_dir, '/Data/Rdata/Phyto_summary_spread.rds'))


#Summarize by month to look at seasonal patterns
Phyto_monthly <- Phyto_summary_select %>% 
  group_by(Station, DIVISION, Month) %>%
  summarize(Mean_BioVolume=mean(Total_BioVolume, na.rm=T), Median_BioVolume=median(Total_BioVolume, na.rm=T), Mean_Density=mean(Density, na.rm=T))

Phyto_monthly$Mean_BioVolume_log<-Phyto_monthly$Mean_BioVolume
Phyto_monthly$Mean_BioVolume_log[which(Phyto_monthly$Mean_BioVolume_log==0)]<-1

Phyto_monthly$Median_BioVolume_log<-Phyto_monthly$Median_BioVolume
Phyto_monthly$Median_BioVolume_log[which(Phyto_monthly$Median_BioVolume_log==0)]<-1000

#Calculate total phyto biomass and summarize
Phyto_total <- Phyto_summary %>% 
  group_by(Station, Date) %>%
  summarize(Total_BioVolume=sum(Total_BioVolume, na.rm=T), Total_Density=sum(Density, na.rm=T), Month=median(Month)) %>%
  filter(Station != '64')

Phyto_total_monthly <-Phyto_total  %>% 
  group_by(Station, Month) %>%
  summarize(Mean_BioVolume=mean(Total_BioVolume, na.rm=T), Median_BioVolume=median(Total_BioVolume, na.rm=T), Mean_Density=mean(Total_Density, na.rm=T))


#Export summary by division table to be merged with other datasets (Nutrients, Zoops, etc.)
write.csv(Phyto_total, file=paste(google_dir, 'DataOutputs', 'PhytoTotalBiovolumes.csv', sep='/'), row.names=F)
saveRDS(Phyto_total , file=paste0(dropbox_dir, '/Data/Rdata/Phyto_total.rds'))


rm(StationDate, GenusList, StationDates, StationDatesGenus)


