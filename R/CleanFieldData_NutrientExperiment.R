


#Code to download meta and field sheet data

library(readxl)
library(plyr)
library(viridis)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(stringr)

source('R/read_excel_allsheets.R')
source('R/g_legend.R')

# Project folder where outputs are stored
dropbox_dir<-'C:/Dropbox/USBR Delta Project'

#Where data come from
google_dir<-'C:/GoogleDrive/DeltaNutrientExperiment'


field_df<-read_excel(paste0(google_dir, "/Data/NutrientExperiment/DataSheets/SSCN_FieldNotes.xlsx"))

#Clean Times
field_df$Date<-as.Date(field_df$Date)

field_df$DateTime_start<-as.POSIXct(paste0(field_df$Date, str_pad(field_df$TimeStart, 4, pad='0')), format="%Y-%m-%d%H%M", tz='America/Los_Angeles')

field_df$DateTime_end<-as.POSIXct(paste0(field_df$Date, str_pad(field_df$TimeEnd, 4, pad='0')), format="%Y-%m-%d%H%M", tz='America/Los_Angeles')

#Add site names
sitetable<-data.frame(site1=c('NL70', 'EC2','EC3','EC4','EC5','EC6','EC7','EC8','NL76'), site2=c( "SSCN01_NV70", "SSCN02", "SSCN03", "SSCN04", "SSCN05", "SSCN06", "SSCN07", "SSCN08", "SSCN09 NL76"), site3=str_pad(1:9, 2, pad='0'))

field_df$Site<-sitetable$site1[match(field_df$Location, sitetable$site3)]

head(as.data.frame(field_df))

write.table(field_df, file=paste0(dropbox_dir, '/Data/NutrientExperiment/FieldData.csv'), row.names=F, sep=',')
