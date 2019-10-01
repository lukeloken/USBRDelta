


#Code to download meta and field sheet data

library(readxl)
library(plyr)
library(viridis)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(grid)
library(stringr)

source('R/read_excel_allsheets.R')
source('R/g_legend.R')

# # Project folder where outputs are stored
# dropbox_dir<-'C:/Dropbox/USBR Delta Project'
# 
# #Where data come from
# google_dir<-'C:/GoogleDrive/DeltaNutrientExperiment'
# 
# box_dir <- "C:/Users/lcloken/Box/SadroLab/Luke/SSCN2"

field_list<-read_excel_allsheets(paste0(box_dir, "/Data/SSCN2_FieldNotes.xlsx"))

# field_df<-read_excel(paste0(box_dir, "/Data/SSCN2_FieldNotes.xlsx"))

site_df<-field_list[[1]]
event_df<-field_list[[2]]

# #####################
# Clean site field notes
# #####################

#Clean Times
site_df$Date<-as.Date(site_df$Date)

site_df$DateTime_start<-as.POSIXct(paste0(site_df$Date, str_pad(site_df$TimeStart, 4, pad='0')), format="%Y-%m-%d%H%M", tz='America/Los_Angeles')

site_df$DateTime_end<-as.POSIXct(paste0(site_df$Date, str_pad(site_df$TimeEnd, 4, pad='0')), format="%Y-%m-%d%H%M", tz='America/Los_Angeles')


#Add site names
sitetable<-data.frame(site1=c('NL70', 'Site2','Site3','NL74','Site5','Site6','NL76'), site3=str_pad(1:7, 2, pad='0'))

site_df$Site<-factor(site_df$LocationName, sitetable$site1)

head(as.data.frame(site_df))


saveRDS(site_df , file=paste0(dropbox_dir, '/Data/Rdata_SSCN2/SSCN2_FieldData.rds'))
write.table(site_df, file=paste0(google_dir, '/SSCN2_DataOutputs/SSCN2_FieldData.csv'), row.names=F, sep=',')

# #################
#Clean event notes
# #################

#Clean times
event_df$Date<-as.Date(event_df$Date)

event_df$Long_AM_StartTime<-as.POSIXct(paste0(event_df$Date, str_pad(event_df$Long_AM_StartTime, 4, pad='0')), format="%Y-%m-%d%H%M", tz='America/Los_Angeles')

event_df$Long_AM_EndTime<-as.POSIXct(paste0(event_df$Date, str_pad(event_df$Long_AM_EndTime, 4, pad='0')), format="%Y-%m-%d%H%M", tz='America/Los_Angeles')

event_df$Long_PM_StartTime<-as.POSIXct(paste0(event_df$Date, str_pad(event_df$Long_PM_StartTime, 4, pad='0')), format="%Y-%m-%d%H%M", tz='America/Los_Angeles')

event_df$Long_PM_EndTime<-as.POSIXct(paste0(event_df$Date, str_pad(event_df$Long_PM_EndTime, 4, pad='0')), format="%Y-%m-%d%H%M", tz='America/Los_Angeles')


saveRDS(event_df , file=paste0(dropbox_dir, '/Data/Rdata_SSCN2/SSCN2_EventData.rds'))
write.table(event_df, file=paste0(google_dir, '/SSCN2_DataOutputs/SSCN2_EventData.csv'), row.names=F, sep=',')

rm(field_list)

