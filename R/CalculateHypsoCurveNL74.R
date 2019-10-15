

library(rgdal)
library(sp)
library(raster)
library(dplyr)

#Calculate hyposgraphic curve for NL74.
#use csv file from Aug 2018

dropbox_dir<-'C:/Dropbox/USBR Delta Project'


elevations<-(read.csv(paste(dropbox_dir, 'Data', 'Bathy', 'Elevations_hypsocalc.csv', sep='/'), header=F))
elevations_vec<-round(as.numeric(unlist(elevations)), 2)*(-1)
elevations_noNA<-elevations_vec[which(!is.na(elevations_vec))]
elevations_ordered<-elevations_noNA[order(elevations_noNA)]

x<-elevations[,3]
length(which(!is.na(x)))

width <- apply(elevations, 2, function (x) 2*length(which(!is.na(x))))
summary(width)

unique_depths<-unique(elevations_ordered)

hypso_df<-data.frame(depth=unique_depths, count=NA)

depth_i<-1
for(depth_i in 1:length(unique_depths)){
  count = length(which(elevations_ordered>=unique_depths[depth_i]))
  hypso_df[depth_i,2]<-count
}

seq_depths<-data.frame(depth=seq(0, max(unique_depths), 0.01))

hypso_alldepths<-left_join(seq_depths, hypso_df)

hypso_alldepths$count_approx<-approx(x=hypso_df$depth, y=hypso_df$count , xout=hypso_alldepths$depth, method='linear', rule=2)$y

hypso_alldepths$Xarea_m2=round(hypso_alldepths$count_approx*0.01, 2)*2/ncol(elevations)

hypso_alldepths$Xwidth_m<-hypso_alldepths$Xarea_m2*100

hypso_alldepths <- hypso_alldepths[order(hypso_alldepths$depth, decreasing=T ),]
  
hypso_alldepths$CumArea_m2<-cumsum(hypso_alldepths$Xarea_m2)


# head(hypso_alldepths,100)
# 
# 
plot(hypso_alldepths$CumArea_m2, hypso_alldepths$depth*(-1))


write.table(hypso_alldepths, file=paste(dropbox_dir, 'Data', 'Bathy', 'Hypso_NL74.csv', sep='/'), row.names=F, sep=',')

outline_data<-readOGR(paste(dropbox_dir, 'Data', 'SpatialData', sep='/'), "ShipChannel_4km_outline")
outline_area<-area(outline_data)

sp_data<-readOGR(paste(dropbox_dir, 'Data', 'SpatialData', sep='/'), "DWSC_Bathy_NL74_4km")
summary(sp_data@data)

sp_data$Depth_m_v2 <- as.numeric(gsub("Level ", "", sp_data$Name))

sp_data$Depth_m <- as.numeric(as.character(sp_data$Depth_m))
sp_data$Area_m2 <- area(sp_data)
summary(sp_data@data)


Centroids <- getSpPPolygonsLabptSlots(sp_data)

sp_data2<-sp_data[which(Centroids[,1]>=-121.587),]

WaterArea<- sp_data2[which(sp_data2$Name =='Level -11'),]
plot(WaterArea)
Area_Total<-sum(WaterArea$Area_m2)


Depth_names<-unique(sp_data2$Name)

i=1
Depth_df<-data.frame(Depth_name=Depth_names, Depth_m=NA, Area_m2=NA)
for (i in 1:length(Depth_names)){
  Area_i <- sp_data2[which(sp_data2$Name ==Depth_names[i]),]
  Area_sum <-  sum(Area_i$Area_m2, na.rm=T)
  Depth_df$Area_m2[i] <- (Area_Total - Area_sum)
  if (Depth_df$Area_m2[i] <0 ){ Depth_df$Area_m2[i] = 0}
  # plot(FullArea, col='black', border=NA)
  # plot(Area_i, col='green', border=NA, add=T)
  }



Depth_df$Depth_m= (as.numeric(gsub('Level ', '', Depth_names)))*(-1)+0.5

shallow_df<-Depth_df[1,]
shallow_df$Depth_name='Level +0.5'
shallow_df$Depth_m=0
shallow_df$Area_m2<-outline_area
Depth_df<-bind_rows(Depth_df, shallow_df)

Depth_df$Volume_m3 <- Depth_df$Area_m2*.5

Depth_df$Area_percent<-Depth_df$Area_m2/outline_area
Depth_df$Volume_percent<-Depth_df$Volume_m3/sum(Depth_df$Volume_m3)

plot(Depth_df$Area_m2, Depth_df$Depth_m, ylim=c(12,0), type='o', pch=16, xlab='Area (m2)', ylab='Depth (m)')
plot(Depth_df$Volume_m3, Depth_df$Depth_m, ylim=c(12,0))

sum(Depth_df$Volume_m3)/9400*400

write.csv(Depth_df, paste(google_dir, "/SSCN2_DataOutputs/HypsoCurveNL74.csv", sep=''), row.names = F)
saveRDS(Depth_df, file=paste0(dropbox_dir, '/Data/Rdata_SSCN2/HypsoCurveNL74.rds'))
