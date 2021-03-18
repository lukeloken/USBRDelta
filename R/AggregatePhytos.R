
library(viridis)
library(ggplot2)
library(gridExtra)
library(plyr)
library(dplyr)
library(tidyr)
library(lubridate)
library(RColorBrewer)

# source('R/CompilePhytos.R')

# # Project folder where outputs are stored
# dropbox_dir<-'C:/Dropbox/USBR Delta Project'
# 
# #Where data come from
# google_dir<-'C:/GoogleDrive/DeltaNutrientExperiment'



Phyto_FullRecord <- readRDS(file = file.path(onedrive_dir, "RData", "MonthlyCruises", 
                                             "Phyto_FullRecord.rds"))


Phyto_FullRecord_clean <- Phyto_FullRecord %>%
  rename(Date = DATE) %>%
  filter(!is.na(Station), !grepl(" PM", bottle.ID)) %>%
  group_by(bottle.ID, 
           GENUS,
           DIVISION,
           Date, 
           Station) %>%
  summarize(across(c("TALLY", "DENSITY", "TOTAL.BV"), ~sum(.x)), 
            NOTES = paste(NOTES, collapse = ",")) %>%
  ungroup() %>%
  select(-bottle.ID) %>%
  distinct()
  
dim(Phyto_FullRecord_clean)

# ############################
# Since data are 'count' data, need to populate dataset with zeros for any genus not counted in a given sample
# ############################

#All phyto species recorded
GenusList<-unique(Phyto_FullRecord_clean[c('GENUS', 'DIVISION')])

#Unique combinations of Date and Site
StationDates<-expand(Phyto_FullRecord_clean, nesting(Station, Date))

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

StationDatesGenus <- distinct(StationDatesGenus)


#Join empty data.frame with observations
Phyto_CompleteList<-full_join(StationDatesGenus, Phyto_FullRecord_clean)



#Replace NAs with zeros (not observed)
Phyto_CompleteList$TALLY[which(is.na(Phyto_CompleteList$TALLY))]<-0
Phyto_CompleteList$DENSITY[which(is.na(Phyto_CompleteList$DENSITY))]<-0
Phyto_CompleteList$TOTAL.BV[which(is.na(Phyto_CompleteList$TOTAL.BV))]<-0


# Modify columns structure to summarize and improve plotting
Phyto_CompleteList <- Phyto_CompleteList %>%
  # dplyr::select(-'SAMPLE') %>%
  mutate(Month=month(Date), 
         Station=factor(Station, c('16', '34','44', 'Pro', '56','62', '64','66', '70','74', '76','84', 'WSP')))


#Save complete recrod with NAs
write.csv(Phyto_CompleteList, file=paste(google_dir, 'DataOutputs', 'Phyto_CompleteList.csv', sep='/'), row.names=F)
saveRDS(Phyto_CompleteList , file=paste0(dropbox_dir, '/Data/Rdata/Phyto_CompleteList.rds'))


saveRDS(Phyto_CompleteList , file = file.path(onedrive_dir, "RData", "MonthlyCruises", "Phyto_CompleteList.rds"))
write.csv(Phyto_CompleteList, file = file.path(onedrive_dir, "OutputData", "MonthlyCruises", 'Phyto_CompleteList.csv'), row.names = FALSE)


# ##################################################
# Summarize data by division then by calendar month
# ##################################################


Phyto_summary<- Phyto_CompleteList %>%
  dplyr::select(Station, Date, DIVISION, TOTAL.BV, DENSITY, Month) %>%
  group_by(Station,Date, DIVISION, Month) %>%
  dplyr::summarize(Total_BioVolume=sum(TOTAL.BV)/(10^9), Density=sum(DENSITY)) %>%
  drop_na(Station)

#Export long summary table for ggplotting
write.csv(Phyto_summary, file=paste(google_dir, 'DataOutputs', 'PhytoSummaryAllDivisionLongTable.csv', sep='/'), row.names=F)
saveRDS(Phyto_summary , file=paste0(dropbox_dir, '/Data/Rdata/Phyto_summary.rds'))

saveRDS(Phyto_summary , file = file.path(onedrive_dir, "RData", "MonthlyCruises", "Phyto_summary.rds"))
write.csv(Phyto_summary, file = file.path(onedrive_dir, "OutputData", "MonthlyCruises", 'PhytoSummaryAllDivisionLongTable.csv'), row.names = FALSE)


# Phyto_summary_select<- Phyto_summary %>%
  # filter(DIVISION %in% c("Bacillariophyta", "Chlorophyta", "Cryptophyta", "Chrysophyta", "Cyanobacteria") & Station != '64')

Phyto_summary_select<- Phyto_summary %>%
  filter(DIVISION %in% c("Bacillariophyta", "Chlorophyta", "Cryptophyta", "Chrysophyta", "Cyanobacteria"))

#Export long summary table for ggplotting
write.csv(Phyto_summary_select, file=paste(google_dir, 'DataOutputs', 'PhytoSummaryMainDivisionsLongTable.csv', sep='/'), row.names=F)
saveRDS(Phyto_summary_select , file=paste0(dropbox_dir, '/Data/Rdata/Phyto_summary_select.rds'))

saveRDS(Phyto_summary_select , file = file.path(onedrive_dir, "RData", "MonthlyCruises", "Phyto_summary_select.rds"))
write.csv(Phyto_summary_select, file = file.path(onedrive_dir, "OutputData", "MonthlyCruises", 'PhytoSummaryMainDivisionsLongTable.csv'), row.names = FALSE)


# Make a wide table for merging with other datasets
Phyto_summary_spread <- Phyto_summary_select %>%
  dplyr::select(Station, Date, DIVISION, Total_BioVolume) %>% 
  spread(key = DIVISION, value= Total_BioVolume)

Phyto_summary_spread$Phyto_total <- rowSums(Phyto_summary_spread[,c("Bacillariophyta", "Chlorophyta", "Cryptophyta", "Chrysophyta", "Cyanobacteria")])

#Export summary by division table to be merged with other datasets (Nutrients, Zoops, etc.)
write.csv(Phyto_summary_spread, file=paste(google_dir, 'DataOutputs', 'PhytoSummaryDivision.csv', sep='/'), row.names=F)
saveRDS(Phyto_summary_spread , file=paste0(dropbox_dir, '/Data/Rdata/Phyto_summary_spread.rds'))

saveRDS(Phyto_summary_spread , file = file.path(onedrive_dir, "RData", "MonthlyCruises", "Phyto_summary_spread.rds"))
write.csv(Phyto_summary_spread, file = file.path(onedrive_dir, "OutputData", "MonthlyCruises", 'PhytoSummaryDivision.csv'), row.names = FALSE)



#Summarize by month to look at seasonal patterns
Phyto_monthly <- Phyto_summary_select %>% 
  filter(Station !='64') %>%
  group_by(Station, DIVISION, Month) %>%
  dplyr::summarize(Mean_BioVolume=mean(Total_BioVolume, na.rm=T), Median_BioVolume=median(Total_BioVolume, na.rm=T), Mean_Density=mean(Density, na.rm=T))

Phyto_monthly$Mean_BioVolume_log<-Phyto_monthly$Mean_BioVolume
Phyto_monthly$Mean_BioVolume_log[which(Phyto_monthly$Mean_BioVolume_log==0)]<-1

Phyto_monthly$Median_BioVolume_log<-Phyto_monthly$Median_BioVolume
Phyto_monthly$Median_BioVolume_log[which(Phyto_monthly$Median_BioVolume_log==0)]<-1000

#Calculate total phyto biomass and summarize
Phyto_total <- Phyto_summary_select %>% 
  filter(Station !='64') %>%
  group_by(Station, Date) %>%
  dplyr::summarize(Total_BioVolume=sum(Total_BioVolume, na.rm=T), Total_Density=sum(Density, na.rm=T), Month=median(Month))

Phyto_total_monthly <-Phyto_total  %>% 
  group_by(Station, Month) %>%
  dplyr::summarize(Mean_BioVolume=mean(Total_BioVolume, na.rm=T), Median_BioVolume=median(Total_BioVolume, na.rm=T), Mean_Density=mean(Total_Density, na.rm=T))


#Export summary by division table to be merged with other datasets (Nutrients, Zoops, etc.)
write.csv(Phyto_total, file=paste(google_dir, 'DataOutputs', 'PhytoTotalBiovolumes.csv', sep='/'), row.names=F)
saveRDS(Phyto_total , file=paste0(dropbox_dir, '/Data/Rdata/Phyto_total.rds'))

saveRDS(Phyto_total , file = file.path(onedrive_dir, "RData", "MonthlyCruises", "Phyto_total.rds"))
write.csv(Phyto_total, file = file.path(onedrive_dir, "OutputData", "MonthlyCruises", 'PhytoTotalBiovolumes.csv'), row.names = FALSE)


rm(StationDate, GenusList, StationDates, StationDatesGenus)

plot(Phyto_total$Date, Phyto_total$Total_BioVolume)
# points(SSC_joined_data$Date, SSC_joined_data$Phyto_total, col = "red")

