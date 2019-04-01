
library(viridis)
library(ggplot2)
library(gridExtra)
library(plyr)
library(dplyr)
library(tidyr)
library(lubridate)
library(RColorBrewer)

# source('R/CompileZoos.R')

# # Project folder where outputs are stored
# dropbox_dir<-'C:/Dropbox/USBR Delta Project'
# 
# #Where data come from
# google_dir<-'C:/GoogleDrive/DeltaNutrientExperiment'

Zoo_FullRecord <- readRDS(file=paste0(dropbox_dir, '/Data/Rdata/Zoo_FullRecord.rds'))

#Need to update everything below


# names(Zoo_FullRecord)[match(c("X.individuals.counted", "X....L", "species.biomass..µg.d.w..L."), names(Zoo_FullRecord))]<-c("Tally", "NumberPerLiter", "SpeciesBiomass_ugdwL")

# ############################
# Since data are 'count' data, need to populate dataset with zeros for any genus not counted in a given sample
# ############################

#All Zoo species recorded
GenusList<-unique(Zoo_FullRecord[c('genus', 'species', 'division')])

#Unique combinations of Date and Site
StationDates<-expand(Zoo_FullRecord, nesting(Station, Date))

#Create empty data.frame with every sample site/date with every Zoo genus
StationDatesGenus<-data.frame(matrix(ncol=5, nrow=nrow(StationDates)*nrow(GenusList)))
names(StationDatesGenus)<-c("Station", "Date", "genus", "species", "division")
StationDatesGenus$Station<-rep(StationDates$Station, each=nrow(GenusList))
StationDatesGenus$Date<-rep(StationDates$Date, each=nrow(GenusList))

StationDate<-1
for (StationDate in 1:nrow(StationDates)){
  StationDatesGenus[((StationDate-1)*nrow(GenusList)+1):(StationDate*nrow(GenusList)),]$genus<-GenusList$genus
  StationDatesGenus[((StationDate-1)*nrow(GenusList)+1):(StationDate*nrow(GenusList)),]$species<-GenusList$species
    StationDatesGenus[((StationDate-1)*nrow(GenusList)+1):(StationDate*nrow(GenusList)),]$division<-GenusList$division
}

#Join empty data.frame with observations
Zoo_CompleteList<-full_join(StationDatesGenus, Zoo_FullRecord)


#Replace NAs with zeros (not observed)
Zoo_CompleteList$NumberPerLiter[which(is.na(Zoo_CompleteList$NumberIndividualsCounted))]<-0
Zoo_CompleteList$SpeciesBiomass_ugdwL[which(is.na(Zoo_CompleteList$NumberIndividualsCounted))]<-0
Zoo_CompleteList$NumberIndividualsCounted[which(is.na(Zoo_CompleteList$NumberIndividualsCounted))]<-0

# Modify columns structure to summarize and improve plotting
Zoo_CompleteList$Month<-month(Zoo_CompleteList$Date)

#Save complete recrod with NAs
write.csv(Zoo_CompleteList, file=paste(google_dir, 'DataOutputs', 'Zoo_CompleteList.csv', sep='/'), row.names=F)
saveRDS(Zoo_CompleteList , file=paste0(dropbox_dir, '/Data/Rdata/Zoo_CompleteList.rds'))


# ##################################################
# Summarize data by division then by calendar month
# ##################################################

Zoo_summary<- Zoo_CompleteList %>%
  dplyr::select(Station, Date, division, genus, SpeciesBiomass_ugdwL, NumberPerLiter, Month) %>%
  drop_na(Station) %>%
  group_by(Station, Date, division, Month) %>%
  dplyr::summarize(Total_SpeciesBiomass_ugdwL=sum(SpeciesBiomass_ugdwL), Total_NumberPerLiter=sum(NumberPerLiter))

Zoo_summary_select<- Zoo_summary %>%
  filter(division %in% c("Bivalvia", "Cladocera", "Copepoda", "Gastropoda", "Ostracoda", "Rotifera") & Station != '64')

#Export summary by division table to ggplot
write.csv(Zoo_summary_select, file=paste(google_dir, 'DataOutputs', 'ZooSummaryDivisionLongTable.csv', sep='/'), row.names=F)
saveRDS(Zoo_summary_select , file=paste0(dropbox_dir, '/Data/Rdata/Zoo_summary_select.rds'))


Zoo_summary_spread <- Zoo_summary_select %>%
  dplyr::select(Station, Date, division, Total_SpeciesBiomass_ugdwL) %>% 
  spread(key = division, value= Total_SpeciesBiomass_ugdwL)

#Export summary by division wide table to be merged with other datasets (Nutrients, Zoops, etc.)
write.csv(Zoo_summary_spread, file=paste(google_dir, 'DataOutputs', 'ZooSummaryDivision.csv', sep='/'), row.names=F)
saveRDS(Zoo_summary_spread , file=paste0(dropbox_dir, '/Data/Rdata/Zoo_summary_spread.rds'))


#Summarize by month to look at seasonal patterns
Zoo_monthly <- Zoo_summary_select %>% 
  group_by(Station, division, Month) %>%
  dplyr::summarize(Mean_SpeciesBiomass_ugdwL=mean(Total_SpeciesBiomass_ugdwL, na.rm=T), Median_SpeciesBiomass_ugdwL=median(Total_SpeciesBiomass_ugdwL, na.rm=T), Mean_NumberPerLiter=mean(Total_NumberPerLiter, na.rm=T))

Zoo_monthly$Mean_SpeciesBiomass_ugdwL_log<-Zoo_monthly$Mean_SpeciesBiomass_ugdwL
Zoo_monthly$Mean_SpeciesBiomass_ugdwL_log[which(Zoo_monthly$Mean_SpeciesBiomass_ugdwL_log==0)]<-1

Zoo_monthly$Median_SpeciesBiomass_ugdwL_log<-Zoo_monthly$Median_SpeciesBiomass_ugdwL
Zoo_monthly$Median_SpeciesBiomass_ugdwL_log[which(Zoo_monthly$Median_SpeciesBiomass_ugdwL_log==0)]<-1000

#Calculate total Zoo biomass and summarize
Zoo_total <- Zoo_summary %>% 
  group_by(Station, Date) %>%
  dplyr::summarize(Total_SpeciesBiomass_ugdwL=sum(Total_SpeciesBiomass_ugdwL, na.rm=T), Total_NumberPerLiter=sum(Total_NumberPerLiter, na.rm=T), Month=median(Month)) %>%
  filter(Station != '64')

Zoo_total_monthly <-Zoo_total  %>% 
  group_by(Station, Month) %>%
  dplyr::summarize(Mean_SpeciesBiomass_ugdwL=mean(Total_SpeciesBiomass_ugdwL, na.rm=T), Median_SpeciesBiomass_ugdwL=median(Total_SpeciesBiomass_ugdwL, na.rm=T), Mean_NumberPerLiter=mean(Total_NumberPerLiter, na.rm=T))


#Export summary by division wide table to be merged with other datasets (Nutrients, Zoops, etc.)
write.csv(Zoo_total, file=paste(google_dir, 'DataOutputs', 'ZooTotalBiomass.csv', sep='/'), row.names=F)
saveRDS(Zoo_total , file=paste0(dropbox_dir, '/Data/Rdata/Zoo_total.rds'))


