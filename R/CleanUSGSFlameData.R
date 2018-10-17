



library(lubridate)
library(viridis)
library(sp)
library(rgdal)
library(RgoogleMaps)

source('R/ImageScale.R')


# Project folder where outputs are stored
dropbox_dir<-'C:/Dropbox/USBR Delta Project'

#Where data come from
google_dir<-'C:/GoogleDrive/DeltaNutrientExperiment'

Date<-as.Date('2018-09-26')

#Load longitudinal data
rtmc<-read.table(paste0(google_dir, '/Data/NutrientExperiment/LongitudinalData/SSCN_Event1_RTMC.dat'), header=T, sep=',', skip=3, stringsAsFactors = F)

#extract headers
rtmc_headers<-read.table(paste0(google_dir, '/Data/NutrientExperiment/LongitudinalData/SSCN_Event1_RTMC.dat'), header=T, sep=',', skip=1)

names(rtmc)<-names(rtmc_headers)
rm(rtmc_headers)

#Make sure columns are numeric
cols = c(which(names(rtmc) %in% c("Latitude", "Longitude")), grep('SUNA', names(rtmc)), grep('EXO', names(rtmc)))   

rtmc[,cols] = apply(rtmc[,cols], 2, function(x) as.numeric(as.character(x)))

#Convert times
rtmc$TIMESTAMP<- as.POSIXct(rtmc$TIMESTAMP, format="%Y-%m-%d %H:%M:%S", tz='America/Los_Angeles')
rtmc$Date<- as.Date(rtmc$TIMESTAMP, format="%Y-%m-%d")
rtmc<-rtmc[which(rtmc$Date==Date),]

#Exclude weird data
rtmc$SUNANO3uM[which(rtmc$SUNANO3uM>200 | rtmc$SUNANO3uM<(-0.1))]<-NA
rtmc$EXOSpCond[which(rtmc$EXOSpCond<500)]<-NA


#Remove data with missing GPS
rtmc_geo<-rtmc[which(is.finite(rtmc$Longitude) & is.finite(rtmc$Latitude)),]
rtmc_geo<-rtmc_geo[which(abs(rtmc_geo$Longitude-mean(rtmc_geo$Longitude, na.rm=T)) < 2 ),]
rtmc_geo<-rtmc_geo[which(abs(rtmc_geo$Latitude-mean(rtmc_geo$Latitude, na.rm=T)) < 2 ),]

#Convert to spatial object
coordinates(rtmc_geo) <- ~Longitude+Latitude

proj4string(rtmc_geo) <- "+init=epsg:4326"

head(rtmc_geo)
str(rtmc_geo)

plot(rtmc_geo)

writeOGR(rtmc_geo, dsn=paste0(dropbox_dir, "/Data/NutrientExperiment/LongitudinalProfiles"), layer=paste0("LongitudinalProfile_", Date), overwrite_layer=T, verbose=F, driver='ESRI Shapefile')

spplot(rtmc_geo, z='SUNANO3uM')
hist(rtmc_geo$SUNANO3uM)

B=100
colors<-bpy.colors(n=B, cutoff.tails=0.1, alpha=1)

rtmc_geo$Col <- as.numeric(cut(rtmc_geo@data[,"EXOSpCond"],breaks = B))
rtmc_geo$Color<-colors[rtmc_geo$Col]

plot(rtmc_geo, col=rtmc_geo$Color, pch=16)
hist(rtmc_geo$EXOSpCond)
