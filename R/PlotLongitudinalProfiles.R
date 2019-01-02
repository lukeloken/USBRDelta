
#Warning this script uses your API key. Do not use this a lot as you can get charged if you go over your monthly allotment. 

#Where spatial data are
Arc_dir <- 'C:/Dropbox/ArcGIS/Delta'

# Project folder where outputs are stored
dropbox_dir<-'C:/Dropbox/USBR Delta Project'

#Where data come from
google_dir<-'C:/GoogleDrive/DeltaNutrientExperiment'

#GoogleKey
GoogleAPIkey<-unlist(read.delim("C:/Users/lcloken/Documents/Google/LokenAPIKey2.txt", stringsAsFactor=F, check.names = FALSE, header=F))

#Max east boundary. Used to identify start/end of north/south bound transects
EastBound<-c(-121.5330)

library(rgdal)
# library(gtools)
library(sp)
library(RODBC)
library(RgoogleMaps)
library(ggmap)
library(ggpubr)
library(grid)

source('R/ImageScale.R')
source('R/SensorCleanLongitudinal.R')
ruletable<-read.csv('SensorQCRulesLongitudinal.csv', header=T, stringsAsFactors = F)

#shapefile outline of north delta major water bodies
outline<-readOGR(Arc_dir, "NorthDeltaOutline_MajorWater")

# # Google background map 
map<-GetMap(center=c(38.28988, -121.66548), size=c(320,640), zoom=10, maptype=c("satellite"), GRAYSCALE=F, API_console_key=GoogleAPIkey)

#ggmap
map_test<-get_googlemap(center=c(-121.665,38.289), size=c(300, 600), zoom = 10, maptype = "satellite", key=GoogleAPIkey )
color.palette2 = colorRampPalette(c(viridis(6, begin=.1, end=.98), rev(magma(5, begin=.25, end=.98))), bias=1)

# Google background map using ggmap
# register_google(key = "AbCdEfGhIjKlMnOpQrStUvWxYz")
# map<-get_map(location=c(lon= -121.66548, lat=38.28988), zoom=10, maptype=c("satellite"))

#List of moped files in google drive folder
mopedfiles<-list.files(paste0(google_dir, "/Data/LongitudinalProfiles"))
mopedfiles<-mopedfiles[grep('.mdb', mopedfiles)]

mopedfiles<-mopedfiles[-which(mopedfiles %in% c("moped_10_27_2016.mdb", "moped_10_9_2012.mdb", "moped_6_30_2016.mdb", "moped_6_7_2016.mdb"))]

# Or you can indicate a single file
# mopedfiles<-c("moped_9_19_2018.mdb")
# mopedfiles<-mopedfiles[47]

#Plotting parameters
B<-100 #Number of color breaks
colors<-bpy.colors(n=B, cutoff.tails=0.1, alpha=1)

file_nu<-1
for (file_nu in 1:length(mopedfiles)){
  conn <- odbcConnectAccess2007(path.expand(paste0(google_dir, "/Data/LongitudinalProfiles/", mopedfiles[file_nu]))) 
  # subset(sqlTables(conn), TABLE_TYPE == "TABLE") 
  df <- sqlFetch(conn, "Horizontal") 
  allvars <- sqlFetch(conn, "Constituent") 
  vars<-as.character(allvars$column_name[allvars$Parent_table == 'Horizontal'])
  vars<-vars[!is.na(vars)]
  
  gps<-sqlFetch(conn, "GPS")
  gps<-gps[c('Current_Date', 'GPS_Time', 'Depth', 'Cvt_Lat',  'Cvt_Long')]
  
  close(conn) 
  
  #Clean datatable

  df$DateTime<-as.POSIXct(paste(df$Current_Date, strftime(df$Current_Time, format="%H:%M:%S")))
  
  df2<-subset(df, select=-c(Data_ID, Extension, Cruise_ID, Cruise_Instance_ID, Current_Time, Current_Date, RTM_Number))
  
  Date<-as.Date(median(df2$DateTime, na.rm=T))
  
  #Clean gps datatable
  gps$DateTime<-as.POSIXct(paste(as.Date(gps$Current_Date), strftime(gps$GPS_Time, format="%H:%M:%S")))
  
  gps2<-gps[which(as.Date(gps$DateTime) == Date),]
  gps2$Depth[which(gps2$Depth==0)]<-NA
  
  gps3<-subset(gps2, select=c(DateTime, Cvt_Lat, Cvt_Long))
  
  #Merge tables
  join_df<-left_join(gps3, df2, by='DateTime')
  
  geo<-join_df[!is.na(join_df$Latitude) & !is.na(join_df$Longitude),]
  geo2<-subset(geo, select=-c(Cvt_Lat, Cvt_Long))
  
  geo3<-geo2[order(geo2$DateTime),]
  
  
  # Use sensorclean script to identify outliers and convert to NA
  # Uses SensorQC and a ruletable loaded at start of script
  # Save timeseries image for assessing outlier detection
  
  png(paste0(dropbox_dir, "/Figures/LongitudinalProfilesSensorClean/", Date, '_SensorQC.png', sep=""), res=300, width=8,height=8, units="in")
  
  #par( oma = c( 0,2,1,0 ) )
  layout(matrix(c(1,1), nrow=1, ncol=1))
  #layout.show(3)
  par(mfrow=c(4,2), mgp=c(3,1,0), mar=c(4,4,.5,.5), oma=c(0,0,2,0))
  
  geo_clean<-sensorclean(geo3, ruletable)
  
  mtext(Date, 3, 0, outer=T)
  
  dev.off()
  
  #Omit any NA lat/long and convert to spatial object
  geo_clean<-geo_clean[!is.na(geo_clean$Latitude) & !is.na(geo_clean$Longitude),]

  coordinates(geo_clean) <- ~Longitude + Latitude
  proj4string(geo_clean) <- proj4string(outline)
  

  writeOGR(geo_clean, dsn=paste0(dropbox_dir, "/Data/LongitudinalProfiles"), layer=paste0("LongitudinalProfile_", Date), overwrite_layer=T, verbose=F, driver='ESRI Shapefile')
  
  
  #Plotting
  plotvars<-intersect(vars, names(geo_clean))

  i=1
  #Loop through geo and plot each variable
  #Outputs are currently several map types
  for (i in 1:length(plotvars)){
    name<-plotvars[i]
    if (is.numeric(geo_clean@data[,name])==TRUE){
      a<-geo_clean[!is.na(geo_clean@data[,name]),]
      if (nrow(a)>0){
        a$Col <- as.numeric(cut(a@data[,name],breaks = B))
        a$Color<-colors[a$Col]
        
        
        png(paste0(dropbox_dir, "/Figures/LongitudinalProfiles/", Date, '_', name, ".png", sep=""), res=300, width=4,height=9, units="in")
        
        #par( oma = c( 0,2,1,0 ) )
        layout(matrix(c(1,2), nrow=2, ncol=1), widths=c(4), heights=c(8,1))
        #layout.show(3)
        
        breaks <- seq(min(a@data[name], na.rm = TRUE), max(a@data[name], na.rm = TRUE),length.out=100)
        par(mar=c(1,1,1,1))
        
        PlotOnStaticMap(map, lat=a$Latitude, lon=a$Longitude, col=a$Color, pch=16, FUN=points)
        
        legend('top', inset=0.01, c(Date), box.lty= 0, bty='n', bg='white', Date, text.col='white')
        
        #Add scale
        par(mar=c(4,1,0,1), bg=NA)
        image.scale((a@data), col=colors[1:(B-1)], breaks=breaks-1e-8,axis.pos=1)
        mtext((paste(name)), 1, 2.5, cex=2)
        #abline(v=levs)
        box()
        
        dev.off()
        
        #Split map into north bound/south bound transects
        endofnorth<-which(a$Longitude>=EastBound)[1]
        startofsouth<-tail(which(a$Longitude>=EastBound), 1)
        north<-a[1:endofnorth,]
        south<-a[startofsouth:length(a),]
        
        
        png(paste0(dropbox_dir, "/Figures/LongitudinalProfiles_SouthNorth/", Date, '_', name, ".png", sep=""), res=300, width=8,height=9, units="in")
        
        
        # layout(matrix(c(1,2,3,3), nrow=2, ncol=2, byrow=TRUE), widths=c(4,4), heights=c(10,1))
        
        layout(matrix(c(1,2,3,3), 2, 2, byrow=T), widths=c(4,4), heights=c(8,1.25))
        
        breaks <- seq(min(a@data[name], na.rm = TRUE), max(a@data[name], na.rm = TRUE),length.out=100)
        par(mar=c(1,1,1,1))
        
        PlotOnStaticMap(map, lat=north$Latitude, lon=north$Longitude, col=north$Color, pch=16, FUN=points, cex=2)
        
        legend('topleft', inset=0.01, legend=c('Northbound'), box.lty= 0, bty='n', bg='white', text.col='white', cex=2)
        
        box(which='plot', lwd=2)
        
        PlotOnStaticMap(map, lat=south$Latitude, lon=south$Longitude, col=south$Color, pch=16, FUN=points, cex=2)
        box(which='plot', lwd=2)
        
        legend('topleft', inset=0.01, legend=c('Southbound'), box.lty= 0, bty='n', bg='white', text.col='white', cex=2)
        
        #Add scale
        par(mar=c(4.5,3,0,3), bg='white')
        image.scale((a@data), col=colors[1:(B-1)], breaks=breaks-1e-8,axis.pos=1, las=1, cex.axis=2)
        mtext((paste(name)), 1, 3, cex=2)
        #abline(v=levs)
        box()
        
        dev.off()
        
        
        #GGMAP side by side, better quality
        png(paste0(dropbox_dir, "/Figures/LongitudinalProfiles_SouthNorth_ggmap/", Date, '_', name, ".png", sep=""), res=300, width=8,height=8, units="in")
        
        commonTheme_map<-list(
          theme(axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(), axis.ticks=element_blank(), plot.margin = unit(c(0, 0, 0, 0), "cm")),
          scale_colour_gradientn(colours = color.palette2(n=B), limits=range(c(a@data[,name]), na.rm=T)),
          theme(legend.position = c(.98, .04), legend.justification = c(1,0), legend.background = element_rect(fill = 'white', colour='black'),
                legend.text=element_text(size=10),legend.title=element_text(size=12), legend.key.height = unit(.5, "cm"),
                legend.key.width = unit(1, "cm"), panel.border=element_rect(fill=NA, colour='black'), legend.direction="horizontal"),
          guides(colour=guide_colorbar(title.position = 'bottom', title.hjust=0.5, title=name, ticks.colour = "black", ticks.linewidth = 1))
        )
        
        
        map_am<-  ggmap(map_test) +
          geom_text(aes(x =  (-121.85), y = (38.59), vjust=1, hjust=0, label = 'Northbound'), size = 8, color='white') +
          geom_point(aes_string(x = north$Longitude, y = north$Latitude, colour = as.character(name)), data = north@data, alpha = .2, size=4) +
          commonTheme_map +
          theme(legend.position = 'none')
        
        
        map_pm<-ggmap(map_test) +
          geom_text(aes(x = (-121.85), y = (38.59), vjust=1, hjust=0, label = 'Southbound'), size = 8, color='white') +
          geom_point(aes_string(x = south$Longitude, y = south$Latitude, colour = as.character(name)), data = south@data, alpha = .2, size=4) +
          commonTheme_map
        
        grid.arrange(map_am, map_pm, top=textGrob(as.character(Date), gp=gpar(fontsize=18)), ncol=2)
        
        dev.off()
        
      }
    }
  }
  print(Date)
}
