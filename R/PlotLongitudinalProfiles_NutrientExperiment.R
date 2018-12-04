
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
library(ggmap)
library(plyr)
library(dplyr)

library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(viridis)
library(stringr)


source('R/ImageScale.R')

#shapefile outline of north delta major water bodies
outline<-readOGR(Arc_dir, "NorthDeltaOutline_MajorWater")

# # Google background map 
map<-GetMap(center=c(38.5, -121.57), size=c(320,640), zoom=12, maptype=c("satellite"), GRAYSCALE=F, API_console_key=GoogleAPIkey)

# # Google background map 
map2<-GetMap(center=c(38.51, -121.57), size=c(240,480), zoom=12, maptype=c("satellite"), GRAYSCALE=F, API_console_key=GoogleAPIkey)


#Experiment with ggmap
map_test<-get_googlemap(center=c(-121.57,38.51), size=c(250, 500), zoom = 12, maptype = "satellite")
color.palette = colorRampPalette(c(viridis(6, begin=.1, end=.98), rev(magma(5, begin=.25, end=.98))), bias=1)
# colours = color.palette(12)


#Field notes
field_df<-read.csv(file=paste0(dropbox_dir, '/Data/NutrientExperiment/FieldData.csv'), sep=',', header=T, stringsAsFactors = F)
field_df$Date<-as.Date(field_df$Date)

sitetable<-data.frame(site1=c('NL70', 'EC2','EC3','EC4','EC5','EC6','EC7','EC8','NL76'), site2=c( "SSCN01_NV70", "SSCN02", "SSCN03", "SSCN04", "SSCN05", "SSCN06", "SSCN07", "SSCN08", "SSCN09 NL76"), site3=str_pad(1:9, 2, pad='0'))

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

badtimes<-which(RTMC_df$TIMESTAMP<=as.POSIXct("2018-09-29 00:00:01", tz='America/Los_Angeles') & RTMC_df$TIMESTAMP>=as.POSIXct("2018-09-28 13:48:28", tz='America/Los_Angeles'))

timedifference<-as.POSIXct(c("2018-10-01 11:39:45", "2018-09-28 17:48:16"), tz='America/Los_Angeles')

addtime<-difftime(timedifference[1],timedifference[2], units=c('secs'))-2

RTMC_df$TIMESTAMP[badtimes]<-RTMC_df$TIMESTAMP[badtimes]+addtime

#Drop fdom
RTMC_df<-RTMC_df[,-grep("FDOM", names(RTMC_df))]

#convert all data columns into numeric
RTMC_df[,2:ncol(RTMC_df)]<-sapply(RTMC_df[,2:ncol(RTMC_df)], as.numeric)

#Get rid of entire rows when any data are NA
RTMC_df_good<-RTMC_df[is.finite(rowMeans(RTMC_df[,2:ncol(RTMC_df)])),]

#Delete oxygen data from first day (cap was over sensor)
RTMC_df_good$EXODOmgL[which(RTMC_df_good$TIMESTAMP<=as.POSIXct("2018-09-27 00:00:01", tz='America/Los_Angeles'))]<-NA


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
  
  
  #Identify north bound and south bound transects, omit other data
  geo_am<-geo_i[geo_i$TIMESTAMP<time_am,]
  
  am_lat_rolldiff<-roll_mean(diff(geo_am$Latitude), n=241, fill=NA)
  am_long_rolldiff<-roll_mean(diff(geo_am$Longitude), n=241, fill=NA)
  
  # plot(am_lat_rolldiff)
  # abline(h=0, col='red')
  # abline(h=(-0.000002), col='blue')
  # 
  # plot(am_long_rolldiff)
  # abline(h=0, col='red')
  # abline(h=(-0.000002), col='blue')

  bad_am<-which(am_lat_rolldiff>=(-0.000002) & am_long_rolldiff>=(-0.000002))
  if (length(bad_am)>0){
    geo_am_clip<-geo_am[1:bad_am[1],]
  } else {
    geo_am_clip<-geo_am
  }
  
  geo_pm<-geo_i[geo_i$TIMESTAMP>time_pm,]
  
  pm_lat_rolldiff<-roll_mean(diff(geo_pm$Latitude), n=181, fill=NA)
  pm_long_rolldiff<-roll_mean(diff(geo_pm$Longitude), n=181, fill=NA)

  # plot(pm_lat_rolldiff)
  # abline(h=0, col='red')
  # abline(h=(0.000005), col='blue')
  # 
  # plot(pm_long_rolldiff)
  # abline(h=0, col='red')
  # abline(h=(0.000005), col='blue')

  bad_pm<-which(pm_lat_rolldiff<=(0.000002) & pm_long_rolldiff<=(0.000002))
  if (length(bad_pm)>0){
    geo_pm_clip<-geo_pm[bad_pm[length(bad_pm)]:nrow(geo_pm@data),]
  } else {
    geo_pm_clip<-geo_pm
  }
  
  
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
      
      am<-geo_am_clip[!is.na(geo_am_clip@data[,name]),]
      pm<-geo_pm_clip[!is.na(geo_pm_clip@data[,name]),]
      
      
      if (nrow(a)>0){
        a$Col <- as.numeric(cut(a@data[,name],breaks = B))
        a$Color<-colors[a$Col]
        
        col_values<-as.numeric(cut(c(am@data[,name], pm@data[,name]), breaks =B))
        col_colors<-colors[col_values]
        
        am$Color<-col_colors[1:nrow(am)]
        pm$Color<-col_colors[(nrow(am)+1):length(col_colors)]
        
        
        #Plot single image of all data
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
        
        #Side by Side plots for AM and PM longitudinal profiles
        png(paste0(dropbox_dir, "/Figures/NutrientExperiment/LongitudinalProfilesTwoPanels/", date, '_', name, ".png", sep=""), res=300, width=8,height=9.25, units="in")
        
        # layout(matrix(c(1,2,3,3), nrow=2, ncol=2, byrow=TRUE), widths=c(4,4), heights=c(10,1))
        
        layout(matrix(c(1,2,3,3), 2, 2, byrow=T), widths=c(4,4), heights=c(8,1.25))
        
        breaks <- seq(min(a@data[name], na.rm = TRUE), max(a@data[name], na.rm = TRUE),length.out=100)
        par(mar=c(1,1,1,1))
        
        PlotOnStaticMap(map2, lat=am$Latitude, lon=am$Longitude, col=am$Color, pch=16, FUN=points, cex=3)
        
        legend('topleft', inset=0.01, legend=c('AM'), box.lty= 0, bty='n', bg='white', text.col='white', cex=2)
        
        box(which='plot', lwd=2)
        
        PlotOnStaticMap(map2, lat=pm$Latitude, lon=pm$Longitude, col=pm$Color, pch=16, FUN=points, cex=3)
        box(which='plot', lwd=2)
        
        legend('topleft', inset=0.01, legend=c('PM'), box.lty= 0, bty='n', bg='white', text.col='white', cex=2)
        
        #Add scale
        par(mar=c(4.5,3,0,3), bg='white')
        image.scale((a@data), col=colors[1:(B-1)], breaks=breaks-1e-8,axis.pos=1, las=1, cex.axis=2)
        mtext((paste(name)), 1, 3, cex=2)
        #abline(v=levs)
        box()
        
        dev.off()
        
        #GGMAP side by side, better quality
        png(paste0(dropbox_dir, "/Figures/NutrientExperiment/LongitudinalProfilesTwoPanels_ggmap/", date, '_', name, ".png", sep=""), res=300, width=8,height=8, units="in")
        
        commonTheme_map<-list(
          theme(axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(), axis.ticks=element_blank(), plot.margin = unit(c(0, 0, 0, 0), "cm")), 
          scale_colour_gradientn(colours = color.palette(n=B), limits=range(c(am@data[,name], pm@data[,name]), na.rm=T)),
          theme(legend.position = c(.98, .04), legend.justification = c(1,0), legend.background = element_rect(fill = 'white', colour='black'),
                legend.text=element_text(size=10),legend.title=element_text(size=12), legend.key.height = unit(.5, "cm"), 
                legend.key.width = unit(1, "cm"), panel.border=element_rect(fill=NA, colour='black'), legend.direction="horizontal"),
          guides(colour=guide_colorbar(title.position = 'bottom', title.hjust=0.5, title=name, ticks.colour = "black", ticks.linewidth = 1)) 
        )
        
        
        map_am<-  ggmap(map_test) + 
          geom_text(aes(x = (-121.61), y = (38.575), vjust=1, hjust=0, label = 'AM'), size = 10, color='white') +
          geom_point(aes_string(x = am$Longitude, y = am$Latitude, colour = as.character(name)), data = am@data, alpha = .2, size=4) + 
          commonTheme_map + 
          theme(legend.position = 'none') 
        
        
        map_pm<-ggmap(map_test) + 
          geom_text(aes(x = (-121.61), y = (38.575), vjust=1, hjust=0, label = 'PM'), size = 10, color='white') +
          geom_point(aes_string(x = pm$Longitude, y = pm$Latitude, colour = as.character(name)), data = pm@data, alpha = .2, size=4) + 
          commonTheme_map
        
        grid.arrange(map_am, map_pm, top=textGrob(as.character(date), gp=gpar(fontsize=18)), ncol=2)
        
        dev.off()  
        
        
      }
    }
  }
  print(date)
}





