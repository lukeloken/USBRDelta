

#load shapefile
# date<-ymd("2015-10-03")
# geo_i<-readOGR(paste0(dropbox_dir, "/Data/NutrientExperiment/LongitudinalProfiles"), layer=paste0("LongitudinalProfile_", date))
# 
# 
# plot(geo_i)


SUNANO3<-read.csv(paste0(dropbox_dir, "/Data/NutrientExperiment/SurfaceChemistry/SUNANO3PostFertilization.csv"), header=T, stringsAsFactors = F)
startingNO3<-mean(SUNANO3$NO3_uM[which(SUNANO3$NO3_uM<=7)])

NL74<-10806.774/1000

poly_x<-c(rep(startingNO3,2), rep(28,2), rep(startingNO3,2))
poly_y<-c(2.8,NL74-.2,NL74-.2,NL74+.2,NL74+.2,16)


#GGMAP side by side, better quality
png(paste0(dropbox_dir, "/Figures/NutrientExperiment/FertilizerExpectation.png", sep=""), res=300, width=8,height=8, units="in")

image3<-ggplot(aes_string(y = "LinearDist_km", x = name, fill = "col_values"), data =am@data ) + 
  labs(x=name, y="Distance (km)") + 
  theme_classic2() + 
  theme(plot.margin=unit(c(0,.1,.1,.05), "in")) +
  theme(panel.border=element_rect(fill=NA, colour='black')) + 
  # theme(panel.background = element_rect(fill = "white", colour = "grey50")) + 
  scale_y_reverse(position = 'right', limits=c(16,2.8)) +
  theme(legend.position='none') + 
  geom_point(color=am@data$color2, size=2) + 
  geom_vline(xintercept=startingNO3, color='#636363', linetype=2, size=1.5) + 
  coord_cartesian(xlim=c(2.8, 5))

# scale_colour_manual(values = color.palette(n=B))
# scale_colour_gradientn(colours = color.palette(n=B), limits=range(c(am@data[,name], pm@data[,name]), na.rm=T))


image4<- image3 + 
  geom_path(aes(x=poly_x, y=poly_y), data=data.frame(poly_x, poly_y), inherit.aes=F, size=2)
  
image5 <- image4 + 
  coord_cartesian(xlim=c(2.8, 29))

grid.arrange(image2, image3, image4, image5, ncol=4)

dev.off()