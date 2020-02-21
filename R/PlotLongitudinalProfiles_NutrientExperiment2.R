
#Warning this script uses your API key. Do not use this a lot as you can get charged if you go over your monthly allotment. 

#Load spatial libraries
library(rgdal)
# library(gtools)
library(sp)
library(RODBC)
library(RgoogleMaps)
library(ggmap)
library(riverdist)



#Where spatial data are
# Arc_dir <- 'C:/Dropbox/ArcGIS/Delta'
# 
# # Project folder where outputs are stored
# dropbox_dir<-'C:/Dropbox/USBR Delta Project'
# 
# #Where data come from
# google_dir<-'C:/GoogleDrive/DeltaNutrientExperiment'
# 
# box_dir <- "C:/Users/lcloken/Box/SadroLab/Luke/SSCN2"

# ##################################################################################################
# Google maps
# Commented code will load API key on Loken-UC-Davis laptop
# Download 3 maps from google server
# As of Oct 2018, this costs money
# Instead, I saved these maps to the dropbox spatial folder and load them individually
# If needed you can uncomment and redownload maps, but shouldn't need to do so with ship channel
# ##################################################################################################

#GoogleKey
# GoogleAPIkey<-unlist(read.delim("H:/Private/Google/LokenAPIKey2.txt", stringsAsFactor=F, check.names = FALSE, header=F))
# 
# register_google(key = as.character(GoogleAPIkey))

# Google background map 
# map<-GetMap(center=c(38.5, -121.57), size=c(320,640), zoom=12, maptype=c("satellite"), GRAYSCALE=F, API_console_key=GoogleAPIkey)

# Google background map 
# map2<-GetMap(center=c(38.51, -121.57), size=c(240,480), zoom=12, maptype=c("satellite"), GRAYSCALE=F, API_console_key=GoogleAPIkey)

# ggmap
# map_test<-get_googlemap(center=c(-121.57,38.51), size=c(250, 500), zoom = 12, scale=2, maptype = "satellite", key=GoogleAPIkey )


#stamenmap
# map_stamen<-get_stamenmap(bbox=c(left=-121.61, right=-121.53, bottom=38.45, top=38.57), zoom = 12, scale=2, maptype = "terrain-background")
# ggmap(map_stamen)

#save maps
# saveRDS(map, file=paste0(dropbox_dir, '/Data/SpatialData/UpperShipChannel_map1.rds'))
# saveRDS(map2, file=paste0(dropbox_dir, '/Data/SpatialData/UpperShipChannel_map2.rds'))
# saveRDS(map_test, file=paste0(dropbox_dir, '/Data/SpatialData/UpperShipChannel_ggmap.rds'))

# As of Nov 2019, load maps from file instead
#load maps
map <- readRDS(file=file.path(onedrive_dir, 'SpatialData', 'UpperShipChannel_map1.rds'))
map2<- readRDS(file=file.path(onedrive_dir, 'SpatialData', 'UpperShipChannel_map2.rds'))
map_test <- readRDS(file=file.path(onedrive_dir, 'SpatialData', 'UpperShipChannel_ggmap.rds'))

#In Feb 2020 this map has an error. Two images plot side by side. 
ggmap(map_test)



#Load other spatial data
#shapefile outline of north delta major water bodies
outline<-readOGR(file.path(onedrive_dir, "SpatialData"), "NorthDeltaOutline_MajorWater")

SSCNetwork_clean <- readRDS(file.path(onedrive_dir, "SpatialData", 'ShipChannelNetwork.rds'))

SSCSites <- readRDS(file.path(onedrive_dir, "SpatialData", 'SSCSites.rds'))
CloseSites<-SSCSites[SSCSites$Station %in% c('NL 70', 'NL 74', 'NL 76'),]

#UTM zone 10 for linear reference
projection = "+init=epsg:26910"

color.palette = colorRampPalette(c(viridis(6, begin=.1, end=.98), rev(magma(5, begin=.25, end=.98))), bias=1)
colours = color.palette(12)



#Field notes (event)
event_df <- readRDS(file=file.path(onedrive_dir, 'Rdata', 'NutrientExperiment2', 'SSCN2_EventData.rds'))
fls_dates<-unique(event_df$Date[!is.na(event_df$Date)])

#Site notes (site)
site_df<-readRDS(file=file.path(onedrive_dir, 'Rdata', 'NutrientExperiment2', 'SSCN2_FieldData.rds'))


#List of spatial files in google drive folder
spatialfiles<-list.files(file.path(onedrive_dir, 'RawData', 'NutrientExperiment2', 'LongitudinalProfiles'))
# SUNAfiles<-spatialfiles[grep('SUNA', spatialfiles)]
RTMCfiles<-spatialfiles[grep('raw', spatialfiles)]


#Process RTMC (FLAME) data
#Make one big file with everything. 
RTMCfileslong<-file.path(file.path(onedrive_dir, 'RawData', 'NutrientExperiment2', 'LongitudinalProfiles'), RTMCfiles)
RTMC_list<-lapply(RTMCfileslong, function (l) read.csv(l, header=F, skip=4, stringsAsFactors = F))

RTMC_names<-names(read.csv(RTMCfileslong[1], header=T, skip=1))

RTMC_df<-ldply(RTMC_list, data.frame)

names(RTMC_df)<-RTMC_names

#Subset data
RTMC_df<-RTMC_df[,c(1,4,5, 9:10, 17:33)]
RTMC_df[,2:ncol(RTMC_df)]<-sapply(RTMC_df[,2:ncol(RTMC_df)], as.numeric)

RTMC_df$Date<-as.Date(RTMC_df$TIMESTAMP)
RTMC_df$TIMESTAMP<-as.POSIXct(RTMC_df$TIMESTAMP, "Etc/GMT+8", format="%Y-%m-%d %H:%M:%S")

goodtimes<-which(RTMC_df$TIMESTAMP>=as.POSIXct("2019-07-08 00:00:01", tz='Etc/GMT+8') & RTMC_df$TIMESTAMP<=as.POSIXct("2019-09-15 23:59:59", tz='Etc/GMT+8'))

RTMC_df<- RTMC_df[goodtimes,]


#Data limits
# datalimits=data.frame(var=names(RTMC_df_good)[2:ncol(RTMC_df_good)], min=NA, max=NA)
# datalimits$min=c(38.342,-121.645,15,400,6,-180, 6,40,-1, -1, -2)
# datalimits$max=c(38.562,-121.55,30,1500,9,0, 15,150,150, 100, 10)
#Omit gps data that are

plotvars<-names(RTMC_df)[-which(names(RTMC_df) %in% c("TIMESTAMP", "Latitude", "Longitude", "EXOpHmV"))]

#Convert to NA if outside datalimits
# var_no<-2
# for (var_no in 1:nrow(datalimits)){
#   var_name<-as.character(datalimits$var[var_no])
#   min<-datalimits[var_no, c('min')]
#   max<-datalimits[var_no, c('max')]
#   RTMC_df_good[which(RTMC_df_good[,var_name]>max | RTMC_df_good[,var_name]<min),var_name]<-NA
# }

#Get rid of entire rows when geo data are NA
RTMC_df_geo<-RTMC_df[is.finite(rowMeans(RTMC_df[,2:3])),]

summary(RTMC_df_geo)

#convert dataframe into a spatial object
geo<-RTMC_df_geo
coordinates(geo)<- ~Longitude + Latitude
proj4string(geo) <- proj4string(outline)


#Linear Reference (This takes a lot of computation!)
geo_UTM<-spTransform(geo, CRS(projection))
geo_snapped<-xy2segvert(x=coordinates(geo_UTM)[,1], y=coordinates(geo_UTM)[,2], rivers=SSCNetwork_clean)
geo$LinearDist<-unlist(SSCNetwork_clean$cumuldist)[geo_snapped$vert]
geo$LinearDist_km<-geo$LinearDist/1000
attributes(geo$TIMESTAMP)$tzone <- 'America/Los_Angeles'

#Plot two variables to visualize linear reference
plot(geo$LinearDist, geo$EXOSpCn, type='l')
plot(geo$LinearDist_km, geo$NO3_uM, pch=16)

row=15
median_list <- list()
geo_points <- list()
geo_datacoords <- bind_cols (geo@data, data.frame(geo@coords))
for (row in 1:nrow(site_df)){
  
  median_list[[row]] <- filter(geo_datacoords, TIMESTAMP > site_df$DateTime_start[row] &
                    TIMESTAMP < site_df$DateTime_end[row]) %>%
    summarize_all(.funs=median, na.rm=T)
  
}

site_medians_df<-ldply(median_list, data.frame) %>%
  dplyr::select(-Date, -Latitude, -Longitude) 
names(site_medians_df)<-paste0("FLAMe_", names(site_medians_df))


site_points_df <- ldply(median_list, data.frame) %>%
  dplyr::select(Latitude, Longitude) 

site_df_withFlame<-bind_cols(site_df, site_points_df) %>%
  bind_cols(site_medians_df)   

SiteLocations<- site_df_withFlame %>% 
  group_by(Site) %>%
  summarize(LinearDist = median(FLAMe_LinearDist, na.rm=T),
            Latitude = median(Latitude, na.rm=T),
            Longitude = median(Longitude, na.rm=T))

#Save
write.table(site_df_withFlame, file=file.path(onedrive_dir, 'OutputData', 'NutrientExperiment2', 'FlameSiteData.csv'), row.names=F, sep=',')
saveRDS(site_df_withFlame , file=file.path(onedrive_dir, 'RData', 'NutrientExperiment2', 'FlameSiteData.rds'))

write.table(SiteLocations, file =file.path(onedrive_dir, 'OutputData', 'NutrientExperiment2', 'SiteLocations.csv'), row.names=F, sep=',')
saveRDS(SiteLocations , file=file.path(onedrive_dir, 'RData', 'NutrientExperiment2', 'SiteLocations.rds'))



#Plotting parameters
B<-100 #Number of color breaks
colors<-bpy.colors(n=B, cutoff.tails=0.1, alpha=1)

dates<-unique(geo$Date)
# dates<-dates[9]

# dates<-dates[length(dates)] #Just use last date if processing newest file
event_i<-1
plot=TRUE

dir.create(file.path(onedrive_dir, "Figures", "NutrientExperiment2", "LongitudinalProfiles"), showWarnings = F)
dir.create(file.path(onedrive_dir, "Figures", "NutrientExperiment2", "LongitudinalProfilesTwoPanels"), showWarnings = F)
dir.create(file.path(onedrive_dir, "Figures", "NutrientExperiment2", "LongitudinalProfilesTwoPanels_ggmap"), showWarnings = F)

dir.create(file.path(onedrive_dir, "Figures", "NutrientExperiment2", "LinearDistance"), showWarnings = F)

dir.create(file.path(onedrive_dir, "Rdata", "NutrientExperiment2", "LongitudinalProfiles"), showWarnings = F)

dir.create(file.path(onedrive_dir, "OutputData", "NutrientExperiment2", "LongitudinalProfiles"), showWarnings = F)

dir.create(file.path(onedrive_dir, "OutputData", "NutrientExperiment2", "LongitudinalProfilesCSV"), showWarnings = F)


for (event_i in 1:length(dates)){
  # date<-as.Date(unique(field_df$Date)[event_i])
  date = dates[event_i]
  
  if(date %in% fls_dates){
  
  geo_i<-geo[geo$Date==date,]

  onoff_times<-event_df[match(date, event_df$Date),5:8]
  # onoff_times[,4]<-as.POSIXct("2019-07-25 12:43:00")
  # event10am<-as.POSIXct(c("2019-07-26 08:02", "2019-07-26 08:21"))
  #Identify north bound and south bound transects, omit other data
  geo_am<-geo_i[geo_i$TIMESTAMP<onoff_times[,2] & geo_i$TIMESTAMP>onoff_times[,1],]
  
  plot(geo_am)
  plot(geo_am$Latitude)
  plot(geo_am$Longitude)
  
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
  
  geo_pm<-geo_i[geo_i$TIMESTAMP<onoff_times[,4] & geo_i$TIMESTAMP>onoff_times[,3],]
  
  if (nrow(geo_pm)>0) {
  plot(geo_pm)
  plot(geo_pm$Latitude)
  plot(geo_pm$Longitude)
  }
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
  saveRDS(geo_i, file.path(onedrive_dir, "Rdata", "NutrientExperiment2", "LongitudinalProfiles", paste0("LongitudinalProfile_", date, ".rds")))
  
  writeOGR(geo_am_clip, dsn=file.path(onedrive_dir, "OutputData", "NutrientExperiment2", "LongitudinalProfiles"), layer=paste0("LongitudinalProfile_", date, "_am"), overwrite_layer=T, verbose=F, driver='ESRI Shapefile')
  
  writeOGR(geo_pm_clip, dsn=file.path(onedrive_dir, "OutputData", "NutrientExperiment2", "LongitudinalProfiles"), layer=paste0("LongitudinalProfile_", date, "_pm"), overwrite_layer=T, verbose=F, driver='ESRI Shapefile')
  
  df_am_clip<-data.frame(coordinates(geo_am_clip), geo_am_clip@data)
  df_pm_clip<-data.frame(coordinates(geo_pm_clip), geo_pm_clip@data)
  
  write.csv(df_am_clip, file=file.path(onedrive_dir, "OutputData", "NutrientExperiment2", "LongitudinalProfilesCSV", paste0("LongituidnalProfile_", date, "_am.csv")), row.names = F)
  
  write.csv(df_pm_clip, file=file.path(onedrive_dir, "OutputData", "NutrientExperiment2", "LongitudinalProfilesCSV", paste0("LongituidnalProfile_", date, "_pm.csv")), row.names = F)
  

  
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
        
        if(nrow(am)>0){
        am$Color<-col_colors[1:nrow(am)]
        }
        if (nrow(pm)>0){
        pm$Color<-col_colors[(nrow(am)+1):length(col_colors)]
        }
        
        # am$col_values<-col_values[1:nrow(am)]
        # am$color2<-color.palette(n=B)[am$col_values]

        if (plot==TRUE){

        #Plot single image of all data
        png(file.path(onedrive_dir, "Figures", "NutrientExperiment2", "LongitudinalProfiles", paste(date, '_', name, ".png", sep="")), res=300, width=4,height=9, units="in")

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
        png(file.path(onedrive_dir, "Figures", "NutrientExperiment2", "LongitudinalProfilesTwoPanels", paste(date, '_', name, ".png", sep="")), res=300, width=8,height=9.25, units="in")

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
        png(file.path(onedrive_dir, "Figures", "NutrientExperiment2", "LongitudinalProfilesTwoPanels_ggmap", paste(date, '_', name, ".png", sep="")), res=300, width=8,height=8, units="in")

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

        #Linear Distance
        png(file.path(onedrive_dir, "Figures", "NutrientExperiment2", "LinearDistance", paste(date, '_', name, ".png", sep="")), res=300, width=6,height=4, units="in")

        layout(matrix(c(1), 1, 1, byrow=T))
        par(mar=c(4,4,1,1))
        par(mgp=c(2,0.5,0), las=0,tck=-0.02, pch=16)
        color_lines<-c("#4daf4a", "#984ea3")
        ylim=c(min(c(geo_am_clip@data[,name], geo_pm_clip@data[,name]), na.rm=T), extendrange(c(geo_am_clip@data[,name], geo_pm_clip@data[,name]), f=0.07)[2])
        xlim=range(geo_i$LinearDist)/1000
        plot((geo_am_clip$LinearDist/1000), geo_am_clip@data[,name], xlab='Linear distance (km)', ylab='', ylim=ylim, xlim=xlim, las=1, type='n')

        abline(v=CloseSites$Dist/1000, lty=3, col='darkgrey')
        text(CloseSites$Dist/1000, par('usr')[4], CloseSites$Station, pos=1, cex=0.8, col='darkgrey')

        points((geo_am_clip$LinearDist/1000), geo_am_clip@data[,name], col=color_lines[1], cex=0.5)
        points(geo_pm_clip$LinearDist/1000, geo_pm_clip@data[,name], col=color_lines[2], cex=0.5)
        mtext(name, 2, 2.5)
        legend('topleft', inset=0.0, c('AM', 'PM'), lty=0, pch=NA, text.col=color_lines, bty='n', cex=.8, ncol=2)

        box(which='plot')

        dev.off()
        # 
        # #Stadler style figure
        # #GGMAP side by side, better quality
        # png(paste0(dropbox_dir, "/Figures/NutrientExperiment/LongitudinalProfilesStadlerStyle/", date, '_', name, ".png", sep=""), res=300, width=6,height=8, units="in")
        # 
        # image1<-ggmap(map_test) +
        #   geom_point(aes_string(x = am$Longitude, y = am$Latitude, colour = as.character(name)), data = am@data, alpha = .2, size=4) +
        #   commonTheme_map
        # 
        # image2<-ggplot(aes_string(y = "LinearDist_km", x = name, fill = "col_values"), data =am@data ) + 
        #   labs(x=name, y="Distance (km)") + 
        #   theme_classic2() + 
        #   theme(plot.margin=unit(c(0,.1,.1,.05), "in")) +
        #   theme(panel.border=element_rect(fill=NA, colour='black')) + 
        #   # theme(panel.background = element_rect(fill = "white", colour = "grey50")) + 
        #   scale_y_reverse(position = 'right', limits=c(16,2.8)) +
        #   theme(legend.position='none') + 
        #   geom_point(color=am@data$color2, size=2)
        #   # scale_colour_manual(values = color.palette(n=B))
        # # scale_colour_gradientn(colours = color.palette(n=B), limits=range(c(am@data[,name], pm@data[,name]), na.rm=T))
        #   
        # grid.arrange(image1, image2, ncol=2, widths=c(2,1))
        # 
        # dev.off()

      }
      }
    }
  }
  
  print(date)
}

rm(a, am, CloseSites, commonTheme_map,geo, geo_am, geo_am_clip, geo_i, geo_list, geo_pm, geo_pm_clip,geo_UTM, map, map2, onoff_times, outline, pm, map_am, map_pm, map_test, RTMC_df, RTMC_df_geo, RTMC_list, site_medians, site_medians_df, SSCNetwork_clean, SSCSites, df_am_clip, df_pm_clip, geo_points, geo_snapped, site_points, site_points_df)



