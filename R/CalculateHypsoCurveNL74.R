

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

hypso_alldepths$CumArea_m2<-cumsum(hypso_alldepths$Xarea_m2)


# head(hypso_alldepths,100)
# 
# 
# plot(hypso_alldepths$depth, hypso_alldepths$CumArea_m2)


write.table(hypso_alldepths, file=paste(dropbox_dir, 'Data', 'Bathy', 'Hypso_NL74.csv', sep='/'), row.names=F, sep=',')
     