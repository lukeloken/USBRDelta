
library(dataRetrieval)
library(zoo)
library(lubridate)

# parameterCd <- c("00060", "00010", "00065", "00095", "00300", "00400", "32316", "63680", "72254")
parameterCd <- c("00060", "00010", "00065")
startDate <- "2022-08-01"
endDate <- Sys.Date()

# deploydates <- as.POSIXct(c("2019-07-02 16:00:00", "2019-09-18 08:00:00"), tz = 'America/Los_Angeles')


# Get USGS gauge data for all SSCN stations.
# siteNumbers<-c("11455335", "11455142", "11455136", "11455095", "11447605") # NL54, 62, 66, 72, WestSac
siteNumbers <- c("11455095") #NL72


siteINFO <- readNWISsite(siteNumbers)

RawData <- readNWISuv(siteNumbers, parameterCd, startDate, endDate)
RawData <- renameNWISColumns(RawData)

attr(RawData$dateTime, "tzone") <- 'America/Los_Angeles'
# attr(RawData$dateTime, "tzone") <- 'tz="Etc/GMT+8'

RawData$site_no <- factor(RawData$site_no, siteNumbers)

#For one station use the Hydro dataset
RawData$DO_Inst[which(RawData$site_no=="11455142")]<-RawData$DWS.BOR...HYDRO.PROJECT._DO_Inst[which(RawData$site_no=="11455142")]

RawData$SpecCond_Inst[which(RawData$site_no=="11455142")]<-RawData$DWS.BOR...HYDRO.PROJECT._SpecCond_Inst[which(RawData$site_no=="11455142")]

ggplot(RawData[which(!is.na(RawData$GH_Inst)),], aes(x=dateTime, y=GH_Inst, group=site_no)) +
  # geom_point() +
  # geom_vline(xintercept=fert_posix, color='green', linetype=2, size=1) + 
  theme_bw() + 
  geom_line(col='black') +
  facet_wrap(~site_no, scales='free_y', ncol=1) +
  theme(legend.position = 'none', axis.title.x = element_blank()) +
  labs(y='Gage height (ft)')

waterlevel_ts <- ggplot(RawData[which(!is.na(RawData$GH_Inst) & RawData$site_no == "11455095"),], aes(x=dateTime, y=GH_Inst, group=site_no)) +
  # geom_point() +
  # geom_vline(xintercept=fert_posix, color='green', linetype=2, size=1) + 
  theme_bw() + 
  geom_line(col='black', linewidth=.4) +
  theme(legend.position = 'none', axis.title.x = element_blank()) +
  labs(y='Gage height (ft)') +
  ggtitle('Water level at USGS gage 11455095 (NL72)')

ggsave(file=file.path(box_dir, 'Figures', 'Waterlevel_TS.png'), 
       waterlevel_ts, width=10, height=3)


#Save to file
write.table(RawData, file=file.path(box_dir, 'DWSC_Data_Processed', 'USGSGageData.csv'), row.names=F, sep=',')

saveRDS(RawData, file=file.path(box_dir, 'DWSC_Data_Processed', 'USGSGageData.rds'))




