

#Where spatial data are
Arc_dir <- 'C:/Dropbox/ArcGIS/Delta'

# Project folder where outputs are stored
dropbox_dir<-'C:/Dropbox/USBR Delta Project'

#Where data come from
google_dir<-'C:/GoogleDrive/DeltaNutrientExperiment'

library(rgdal)
# library(gtools)
library(sp)
library(RODBC)
library(RgoogleMaps)

source('R/ImageScale.R')

#shapefile outline of north delta major water bodies
outline<-readOGR(Arc_dir, "NorthDeltaOutline_MajorWater")


# Google background map 
map<-GetMap(center=c(38.28988, -121.66548), size=c(320,640), zoom=10, maptype=c("satellite"), GRAYSCALE=F)

#List of moped files in google drive folder
mopedfiles<-list.files(paste0(google_dir, "/Data/LongitudinalProfiles"))
mopedfiles<-mopedfiles[grep('.mdb', mopedfiles)]

# Or you can indicate a single file
mopedfiles<-c("moped_9_19_2018.mdb")

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
  close(conn) 
  
  head(df)
  geo<-df[!is.na(df$Latitude) & !is.na(df$Longitude),]
  Date<-as.Date(median(df$Current_Date, na.rm=T))
  coordinates(geo) <- ~Longitude + Latitude
  proj4string(geo) <- proj4string(outline)
  
  writeOGR(geo, dsn=paste0(dropbox_dir, "/Data/LongitudinalProfiles"), layer=paste0("LongitudinalProfile_", Date), overwrite_layer=T, verbose=F, driver='ESRI Shapefile')
  
  plotvars<-intersect(vars, names(geo))
  

  i=1
  #Loop through geo and plot each variable
  for (i in 1:length(plotvars)){
    name<-plotvars[i]
    if (is.numeric(geo@data[,name])==TRUE){
      a<-geo[!is.na(geo@data[,name]),]
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
      }
    }
  }
  print(Date)
}
