
#Warning this script uses your API key. Do not use this a lot as you can get charged if you go over your monthly allotment. 


#Where spatial data are
Arc_dir <- 'C:/Dropbox/ArcGIS/Delta'

# Project folder where outputs are stored
dropbox_dir<-'C:/Dropbox/USBR Delta Project'

#Where data come from
google_dir<-'C:/GoogleDrive/DeltaNutrientExperiment'

#GoogleKey
GoogleAPIkey<-unlist(read.delim("C:/Users/lcloken/Documents/Google/LokenAPIKey2.txt", stringsAsFactor=F, check.names = FALSE, header=F))

library(rgdal)
# library(gtools)
library(sp)
library(RODBC)
library(RgoogleMaps)
library(plyr)
library(dplyr)


source('R/ImageScale.R')

#shapefile outline of north delta major water bodies
outline<-readOGR(Arc_dir, "NorthDeltaOutline_MajorWater")

# # Google background map 
map<-GetMap(center=c(38.5, -121.57), size=c(320,640), zoom=12, maptype=c("satellite"), GRAYSCALE=F, API_console_key=GoogleAPIkey)

# Google background map using ggmap
# register_google(key = "AbCdEfGhIjKlMnOpQrStUvWxYz")
# map<-get_map(location=c(lon= -121.66548, lat=38.28988), zoom=10, maptype=c("satellite"))

#Field notes
field_df<-read.csv(file=paste0(dropbox_dir, '/Data/NutrientExperiment/FieldData.csv'), sep=',', header=T, stringsAsFactors = F)
field_df$Date<-as.Date(field_df$Date)
field_df$Site<-sitetable$site1[match(field_df$Location, sitetable$site4)]
field_df$DateTime_start<-as.POSIXct(field_df$DateTime_start, tz='America/Los_Angeles', format='%Y-%m-%d %H:%M:%S')
field_df$DateTime_end<-as.POSIXct(field_df$DateTime_end, tz='America/Los_Angeles', format='%Y-%m-%d %H:%M:%S')


#List of spatial files in google drive folder
spatialfiles<-list.files(paste0(google_dir, "/Data/NutrientExperiment/LongitudinalData"))
SUNAfiles<-spatialfiles[grep('SUNA', spatialfiles)]
RTMCfiles<-spatialfiles[grep('RTMC', spatialfiles)]

# ##################
# Process SUNA data
# ##################

SUNAfileslong<-paste0(google_dir, "/Data/NutrientExperiment/LongitudinalData/", SUNAfiles)
SUNA_list<-lapply(SUNAfileslong, function (l) read.csv(l, header=F, skip=7, stringsAsFactors = F))

SUNA_df<-ldply(SUNA_list, data.frame)
SUNA_df<-SUNA_df[,c(1,2,3,4,5)]
names(SUNA_df)<-c('DateTime', 'Type', 'X', 'NO3_uM', "NO3_mgL")

SUNA_df$Date<-as.Date(SUNA_df$DateTime)

times_list<-strsplit(SUNA_df$DateTime, 'T')
times_vector<-sapply(times_list, function (l) l[2])

SUNA_df$TIMESTAMP<-as.POSIXct(paste0(SUNA_df$Date, times_vector), "America/Los_Angeles", format="%Y-%m-%d%H:%M:%S")

SUNA_df_light<-SUNA_df[SUNA_df$Type=='SATSLF0250',]
SUNA_df_good<-SUNA_df_light[is.finite(SUNA_df_light$NO3_uM) & SUNA_df_light$NO3_uM>0 & !is.na(SUNA_df_light$DateTime),]

# summary(SUNA_df_good)

# ##################
# Process RTMC data
# ##################

RTMCfileslong<-paste0(google_dir, "/Data/NutrientExperiment/LongitudinalData/", RTMCfiles)
RTMC_list<-lapply(RTMCfileslong, function (l) read.csv(l, header=F, skip=4, stringsAsFactors = F))

RTMC_names<-names(read.csv(RTMCfileslong[1], header=T, skip=1))

RTMC_df<-ldply(RTMC_list, data.frame)

names(RTMC_df)<-RTMC_names

RTMC_df<-RTMC_df[,c(1,4,5, 19:28)]

RTMC_df$TIMESTAMP<-as.POSIXct(RTMC_df$TIMESTAMP, "America/Los_Angeles", format="%Y-%m-%d %H:%M:%S")

badtimes<-which(RTMC_df$TIMESTAMP<=as.POSIXct("2018-09-29 00:00:01", tz='America/Los_Angeles') & RTMC_df$TIMESTAMP>=as.POSIXct("2018-09-28 13:38:28", tz='America/Los_Angeles'))

timedifference<-as.POSIXct(c("2018-10-01 11:39:45", "2018-09-28 17:48:16"), tz='America/Los_Angeles')

addtime<-difftime(timedifference[1],timedifference[2], units=c('secs'))-2

RTMC_df$TIMESTAMP[badtimes]<-RTMC_df$TIMESTAMP[badtimes]+addtime

#Drop fdom
RTMC_df<-RTMC_df[,-grep("FDOM", names(RTMC_df))]

#convert all data columns into numeric
RTMC_df[,2:ncol(RTMC_df)]<-sapply(RTMC_df[,2:ncol(RTMC_df)], as.numeric)

#Get rid of entire rows when any data are NA
RTMC_df_good<-RTMC_df[is.finite(rowMeans(RTMC_df[,2:ncol(RTMC_df)])),]

#Data limits
datalimits=data.frame(var=names(RTMC_df_good)[2:ncol(RTMC_df_good)], min=NA, max=NA)
datalimits$min=c(38.342,-121.645,15,400,6,-180, 6,40,-1, -1, -2)
datalimits$max=c(38.562,-121.55,30,1500,9,0, 15,150,150, 100, 10)
#Omit gps data that are

plotvars<-c(as.character(datalimits$var[-which(datalimits$var %in% c("Latitude", "Longitude", "EXOpHmV"))]), 'NO3_uM')

#Convert to NA if outside datalimits
var_no<-2
for (var_no in 1:nrow(datalimits)){
  var_name<-as.character(datalimits$var[var_no])
  min<-datalimits[var_no, c('min')]
  max<-datalimits[var_no, c('max')]
  RTMC_df_good[which(RTMC_df_good[,var_name]>max | RTMC_df_good[,var_name]<min),var_name]<-NA
}

#Get rid of entire rows when geo data are NA
RTMC_df_geo<-RTMC_df_good[is.finite(rowMeans(RTMC_df_good[,2:3])),]

summary(RTMC_df_geo)

#Omit dates outside sampling window
RTMC_df_gooddates<-RTMC_df_geo[as.Date(RTMC_df_geo$TIMESTAMP) %in% unique(field_df$Date),]


summary(RTMC_df_gooddates)

# ######################
# Merge EXO and SUNA and convert to spatial object
# ######################

SUNA_df_good$TIMESTAMP_round<-lubridate::round_date(SUNA_df_good$TIMESTAMP, "5 seconds")
RTMC_df_gooddates$TIMESTAMP_round<-lubridate::round_date(RTMC_df_gooddates$TIMESTAMP, "5 seconds")


merge_df<-left_join(RTMC_df_gooddates, SUNA_df_good[c('TIMESTAMP_round', 'NO3_uM')])

geo<-merge_df
coordinates(geo)<- ~Longitude + Latitude
proj4string(geo) <- proj4string(outline)

geo$Date<-as.Date(geo$TIMESTAMP_round)

geo_list<-apply(field_df[c('DateTime_start', 'DateTime_end')], 1, function (x) geo@data[geo$TIMESTAMP_round>x[1] & geo$TIMESTAMP_round<x[2],])

site_medians<-lapply(geo_list, summarize_all, .funs=median, na.rm=T) 

site_medians_df<-ldply(site_medians, data.frame)

field_df_withFlame<-cbind(field_df, site_medians_df)   

write.table(field_df_withFlame, file=paste0(dropbox_dir, '/Data/NutrientExperiment/FlameSiteData.csv'), row.names=F, sep=',')

#Plotting parameters
B<-100 #Number of color breaks
colors<-bpy.colors(n=B, cutoff.tails=0.1, alpha=1)

event_i<-4
for (event_i in 1:length(unique(field_df$Date))){
  date<-as.Date(unique(field_df$Date)[event_i])
  geo_i<-geo[geo$Date==date,]
  
  time_am<-min(field_df$DateTime_start[field_df$Date==date], na.rm=T)
  time_pm<-max(field_df$DateTime_end[field_df$Date==date], na.rm=T)
  
  geo_am<-geo_i[geo_i$TIMESTAMP<time_am,]
  geo_pm<-geo_i[geo_i$TIMESTAMP>time_pm,]
  
  
  #Save shapefile
  writeOGR(geo_i, dsn=paste0(dropbox_dir, "/Data/NutrientExperiment/LongitudinalProfiles"), layer=paste0("LongitudinalProfile_", date), overwrite_layer=T, verbose=F, driver='ESRI Shapefile')
  
  #Identify variables to plot  
  plotvars_i<-intersect(plotvars, names(geo_i))
  
  var_i=1
  #Loop through geo_i and plot each variable
  for (var_i in 1:length(plotvars_i)){
    name<-plotvars_i[var_i]
    if (is.numeric(geo_i@data[,name])==TRUE){
      a<-geo_i[!is.na(geo_i@data[,name]),]
      if (nrow(a)>0){
        a$Col <- as.numeric(cut(a@data[,name],breaks = B))
        a$Color<-colors[a$Col]
        
        png(paste0(dropbox_dir, "/Figures/NutrientExperiment/LongitudinalProfiles/", date, '_', name, ".png", sep=""), res=300, width=4,height=9, units="in")
        
        layout(matrix(c(1,2), nrow=2, ncol=1), widths=c(4), heights=c(8,1))
        
        breaks <- seq(min(a@data[name], na.rm = TRUE), max(a@data[name], na.rm = TRUE),length.out=100)
        par(mar=c(1,1,1,1))
        
        PlotOnStaticMap(map, lat=a$Latitude, lon=a$Longitude, col=a$Color, pch=16, FUN=points)
        
        legend('topleft', inset=0.01, date, box.lty= 0, bty='n', bg='white', date, text.col='white')
        
        #Add scale
        par(mar=c(4,1,0,1), bg=NA)
        image.scale((a@data), col=colors[1:(B-1)], breaks=breaks-1e-8,axis.pos=1)
        mtext((paste(name)), 1, 2.5, cex=2)
        #abline(v=levs)
        box()
        
        dev.off()
      }
    }
  }
  print(date)
}
