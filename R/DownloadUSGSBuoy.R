
library(dataRetrieval)
library(zoo)
library(lubridate)

parameterCd <- c("00060", "00010", "00065", "00095", "00300", "00400", "32316", "63680", "72254")
startDate <- "2018-09-20"
endDate <- "2018-10-31"

# Get USGS gauge data for all SSCN stations.
siteNumbers<-c("11455335", "11455142", "11455136", "11455095", "11447605") # NL54, 62, 66, 72, WestSac

siteINFO<-readNWISsite(siteNumbers)

RawData <- readNWISuv(siteNumbers, parameterCd, startDate, endDate)
RawData <- renameNWISColumns(RawData)

attr(RawData$dateTime, "tzone") <- 'America/Los_Angeles'

RawData$site_no <- factor(RawData$site_no, siteNumbers)

#For one station use the Hydro dataset
RawData$DO_Inst[which(RawData$site_no=="11455142")]<-RawData$DWS.BOR...HYDRO.PROJECT._DO_Inst[which(RawData$site_no=="11455142")]

# plot(RawData$dateTime, RawData$DO_Inst, type='l')
