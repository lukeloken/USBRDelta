

#download meta and field sheet data for nutrient experiment 2


field_list<-read_excel_allsheets(file.path(onedrive_dir, "RawData", "NutrientExperiment2", "SSCN2_FieldNotes.xlsx"))

#Old location on Loken UCD location
# field_df<-read_excel(paste0(box_dir, "/Data/SSCN2_FieldNotes.xlsx"))

#Excel sheet contains two sheets. One for site data, the other for event data
site_df<-field_list[[1]]
event_df<-field_list[[2]]

# ######################
# Clean site data
# ######################

#Clean Times
site_df$Date<-as.Date(site_df$Date)

site_df$DateTime_start<-as.POSIXct(paste0(site_df$Date, str_pad(site_df$TimeStart, 4, pad='0')), format="%Y-%m-%d%H%M", tz='America/Los_Angeles')

site_df$DateTime_end<-as.POSIXct(paste0(site_df$Date, str_pad(site_df$TimeEnd, 4, pad='0')), format="%Y-%m-%d%H%M", tz='America/Los_Angeles')


#Add site names
sitetable<-data.frame(site1=c('NL70', 'Site2','Site3','NL74','Site5','Site6','NL76'), site3=str_pad(1:7, 2, pad='0'))
site_df$Site<-factor(site_df$LocationName, sitetable$site1)

#Old location
# saveRDS(site_df , file=paste0(onddrive_dir, '/Data/Rdata_SSCN2/SSCN2_FieldData.rds'))
# write.table(site_df, file=paste0(onddrive_dir, '/SSCN2_DataOutputs/SSCN2_FieldData.csv'), row.names=F, sep=',')

saveRDS(site_df , file=file.path(onedrive_dir, 'RData', 'NutrientExperiment2', 'SSCN2_FieldData.rds'))
write.table(site_df, file=file.path(onedrive_dir, 'OutputData', 'NutrientExperiment2', 'SSCN2_FieldData.csv'), row.names=F, sep=',')


# #################
# Clean event data
# #################

#Clean times
event_df$Date<-as.Date(event_df$Date)

event_df$Long_AM_StartTime<-as.POSIXct(paste0(event_df$Date, str_pad(event_df$Long_AM_StartTime, 4, pad='0')), format="%Y-%m-%d%H%M", tz='America/Los_Angeles')

event_df$Long_AM_EndTime<-as.POSIXct(paste0(event_df$Date, str_pad(event_df$Long_AM_EndTime, 4, pad='0')), format="%Y-%m-%d%H%M", tz='America/Los_Angeles')

event_df$Long_PM_StartTime<-as.POSIXct(paste0(event_df$Date, str_pad(event_df$Long_PM_StartTime, 4, pad='0')), format="%Y-%m-%d%H%M", tz='America/Los_Angeles')

event_df$Long_PM_EndTime<-as.POSIXct(paste0(event_df$Date, str_pad(event_df$Long_PM_EndTime, 4, pad='0')), format="%Y-%m-%d%H%M", tz='America/Los_Angeles')

#Old location
# saveRDS(event_df , file=paste0(dropbox_dir, '/Data/Rdata_SSCN2/SSCN2_EventData.rds'))
# write.table(event_df, file=paste0(google_dir, '/SSCN2_DataOutputs/SSCN2_EventData.csv'), row.names=F, sep=',')


saveRDS(event_df , file=file.path(onedrive_dir, 'RData', 'NutrientExperiment2', 'SSCN2_EventData.rds'))
write.table(event_df, file=file.path(onedrive_dir, 'OutputData', 'NutrientExperiment2', 'SSCN2_EventData.csv'), row.names=F, sep=',')


rm(field_list)

