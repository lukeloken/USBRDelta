
library(dataRetrieval)
library(zoo)
library(lubridate)

parameterCd <- c("00060", "00010", "00065", "00095", "00300", "00400", "32316", "63680", "72254")
startDate <- "2018-09-20"
endDate <- "2018-10-31"

# Get USGS gauge data for all SSCN stations.
siteNumbers<-c("11455095") # NL72

siteINFO<-readNWISsite(siteNumbers)

RawData <- readNWISuv(siteNumbers, parameterCd, startDate, endDate)
RawData <- renameNWISColumns(RawData)

attr(RawData$dateTime, "tzone") <- 'America/Los_Angeles'

plot(RawData$dateTime, RawData$DO_Inst, type='l')
