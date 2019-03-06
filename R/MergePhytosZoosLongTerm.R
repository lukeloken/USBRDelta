
library(readxl)
library(plyr)
library(viridis)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(data.table)
library(dplyr)
library(tidyr)

source('R/read_excel_allsheets.R')
source('R/g_legend.R')

# Project folder where outputs are stored
dropbox_dir<-'C:/Dropbox/USBR Delta Project'

#Where data come from
google_dir<-'C:/GoogleDrive/DeltaNutrientExperiment'


BioVolExp<-expression(paste("Phytoplankton biovolume (1 x 10"^"9", " ", mu, "m"^"3", " L"^"-1", ")  ", sep=''))
BiomassExp<-expression(paste("Zooplankton biomass (", mu, "g d.w. L"^"-1", ")  ", sep=''))


SSCNetwork_clean <- readRDS(file=paste0(dropbox_dir, '/Data/SpatialData/ShipChannelNetwork.rds'))

SSCSites <- readRDS(file=paste0(dropbox_dir, '/Data/SpatialData/SSCSites.rds'))
CloseSites<-SSCSites[SSCSites$Station %in% c('NL 70', 'NL 74', 'NL 76'),]

SSCSites$STATIONclean<-c('16', '34', '44', '56', '62', '64', '66', '70', '74', '76', '84', 'WSP', 'Pro')
unique(Zoo_total$STATIONclean)


Phyto_total<-read.csv(file=paste(dropbox_dir, 'Data', 'Phyto', 'PhytoTotals.csv', sep='/'))
Phyto_total$date<-as.Date(Phyto_total$DATE)
Phyto_total<-Phyto_total[,which(names(Phyto_total) != c('DATE'))]
names(Phyto_total)<-gsub('Total', 'Phyto', names(Phyto_total))

Zoo_total<-read.csv(file=paste(dropbox_dir, 'Data', 'Zoops', 'ZooTotals.csv', sep='/'))
Zoo_total$date<-as.Date(Zoo_total$date)
names(Zoo_total)<-gsub('Total', 'Zoo', names(Zoo_total))


PhyZoo<-full_join(Phyto_total, Zoo_total)
head(PhyZoo)
PhyZoo$Dist<-SSCSites$Dist[match(PhyZoo$STATIONclean, SSCSites$STATIONclean)]

heat<-PhyZoo[PhyZoo$STATIONclean != 'Pro',]
heat$Dist<-round(heat$Dist, -1)
plot(PhyZoo$Dist, PhyZoo$Phyto_BioVolume)
points(heat$Dist, heat$Phyto_BioVolume, col='red')




#Make data.table to plot heatmaps (x=time, y=station, z=value)



str(heat)

sampledates<-unique(heat$DATE)
sampledates<-sampledates[order(sampledates)]

#Zoo

Zoo_df<- heat %>%
  select(Dist, date, Zoo_BiomassPerLiter) %>%
  group_by(date, Dist) %>%
  drop_na(Zoo_BiomassPerLiter) %>%
  dplyr::summarize(Zoo_BiomassPerLiter=mean(Zoo_BiomassPerLiter, na.rm=T)) %>%
  spread(key=Dist, value=Zoo_BiomassPerLiter)




library(akima)
library(ggplot2)

range<-range(as.numeric(names(Zoo_df)[2:ncol(Zoo_df)]))
distances<-seq(range[1], range[2], by=100)

dates<-seq(min(heat$date), max(heat$date), 5)

interped <- with(heat[is.finite(heat$Zoo_BiomassPerLiter),], interp(date, Dist, Zoo_BiomassPerLiter, duplicate='mean', xo=dates, yo=distances ))
Zoo_data_interp <- with(interped, data.frame(date=as.Date(rep(dates, length.out=length(z))), Dist=rep(y, each=length(x)), Zoo=as.vector(z))
)

max_zoo<-300
Zoo_data_interp$Zoo[which(Zoo_data_interp$Zoo>max_zoo)]<-max_zoo


interped_phy<-with(heat[is.finite(heat$Phyto_BioVolume),], interp(date, Dist,Phyto_BioVolume, duplicate='mean', xo=dates, yo=distances ))
Phy_data_interp <- with(interped_phy, data.frame(date=as.Date(rep(dates, length.out=length(z))), Dist=rep(y, each=length(x)), Phy=as.vector(z))
)

max_phyto<-20
Phy_data_interp$Phy[which(Phy_data_interp$Phy>max_phyto)]<-max_phyto



#colors
color.palette = colorRampPalette(c(viridis(6, begin=.2, end=.98), rev(magma(5, begin=.35, end=.98))), bias=1)
colors<-color.palette(20)

# rasterplots<-list()
# 
# for (day_nu in 1:length(sampledates)){
#   
  # rasterplots[[day_nu]]<-
    
    # ggplot(Zoo_gather, aes(x=date, y=Dist, z=Zoo, fill=Zoo)) + 
    viz <- ggplot(Zoo_data_interp, aes(date, Dist/1000))
    heatplot<- viz + geom_tile(aes(fill = (Zoo))) + 
    labs(y='Distance (km)', x='Date') + 
    # stat_contour() + 
    # geom_raster(aes(fill=Zoo), interpolate=TRUE) + 
      # aes(x = x, y = y, z = z, fill = z) + 
    # geom_tile() + 
    scale_fill_gradientn(colours=color.palette(20), breaks=seq(0,300, 100), labels=c(0, 100, 200, ">300")) +
    # scale_fill_gradientn(colours=c(color.palette(15),rep(color.palette(15)[15], 30)), breaks=c(seq(0,500, 100), seq(750, 1250, 250))) + 
    # scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 100) + 
    
                         # floor(min(Zoo_gather$Zoo)), ceiling(max(Zoo_gather$Zoo)))) +
    scale_y_reverse(expand = c(0, 0)) + 
    scale_x_date(expand=c(0,0)) + 
    geom_vline(xintercept=as.Date(paste(2010:2019, "-01-01", sep="")), linetype=2) + 
    theme_classic(base_size = 16) +
    # ggtitle(sampledates[day_nu]) + 
    theme(plot.title = element_text(size=10, hjust = 0.5), axis.title.x=element_blank()) + 
    # theme(legend.position='right')
    theme(legend.position='bottom') + 
    guides(fill=guide_colourbar(title=BiomassExp, barwidth=20)) 
  # scale_colour_manual(values = colors)

    print(heatplot)
    


png(paste0(dropbox_dir, '/Figures/Heatmaps_Zoo.png'), width=12, height=8, units='in', res=200)

print(heatplot)

dev.off()



color.palette = colorRampPalette(c(viridis(6, begin=.2, end=.98), rev(magma(5, begin=.35, end=.98))), bias=1)
colors<-color.palette(20)


# ggplot(Zoo_gather, aes(x=date, y=Dist, z=Zoo, fill=Zoo)) + 
viz <- ggplot(Phy_data_interp, aes(date, Dist/1000))
heatplot<- viz + geom_tile(aes(fill = (Phy))) + 
  labs(y='Distance (km)', x='Date') + 
  # stat_contour() + 
  # geom_raster(aes(fill=Zoo), interpolate=TRUE) + 
  # aes(x = x, y = y, z = z, fill = z) + 
  # geom_tile() + 
  scale_fill_gradientn(colours=color.palette(20), breaks=seq(0,20, 5), labels=c(0, 5,10,15,">20"), na.value='white') +
  # floor(min(Zoo_gather$Zoo)), ceiling(max(Zoo_gather$Zoo)))) +
  scale_y_reverse(expand = c(0, 0)) + 
  scale_x_date(expand=c(0,0)) + 
  geom_vline(xintercept=as.Date(paste(2010:2019, "-01-01", sep="")), linetype=2) + 
  theme_classic(base_size = 16) +
  # ggtitle(sampledates[day_nu]) + 
  theme(plot.title = element_text(size=10, hjust = 0.5), axis.title.x=element_blank()) + 
  # theme(legend.position='right')
  theme(legend.position='bottom') + 
  guides(fill=guide_colourbar(title=BioVolExp, barwidth=20))
# scale_colour_manual(values = colors)

print(heatplot)



png(paste0(dropbox_dir, '/Figures/Heatmaps_Phy.png'), width=12, height=8, units='in', res=200)

print(heatplot)

dev.off()


