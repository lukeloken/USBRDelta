


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

field_df$Date<-as.Date(field_df$Date)

field_df$DateTime_start<-as.POSIXct(paste0(field_df$Date, str_pad(field_df$TimeStart, 4, pad='0')), format="%Y-%m-%d%H%M", tz='America/Los_Angeles')

field_df$DateTime_end<-as.POSIXct(paste0(field_df$Date, str_pad(field_df$TimeEnd, 4, pad='0')), format="%Y-%m-%d%H%M", tz='America/Los_Angeles')

                                    
write.table(field_df, file=paste0(dropbox_dir, '/Data/NutrientExperiment/FieldData.csv'), row.names=F, sep=',')
