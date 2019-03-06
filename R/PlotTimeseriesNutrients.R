

library(lubridate)
library(viridis)

# Project folder where outputs are stored
dropbox_dir<-'C:/Dropbox/USBR Delta Project'

#Where data come from
google_dir<-'C:/GoogleDrive/DeltaNutrientExperiment'


# setwd("C:/Users/Luke/Dropbox/USBR Delta Project")
# 
# source("R/ReadInMasterData.R")
source('R/g_legend.R')

merge_df <- read.csv(file=paste0(dropbox_dir, '/Data/SurfaceChemistry/YSIChemSurface.csv'), stringsAsFactors = F)
merge_df$Date<-as.Date(merge_df$Date)
merge_df$Month<-as.character(lubridate::month(merge_df$Date, label = TRUE))
merge_df$Month<-factor(merge_df$Month, month.abb[1:12])

merge_df$Zone<-NA
merge_df$Zone[merge_df$Station %in% c('WSP', '84')]<-'5'
merge_df$Zone[merge_df$Station %in% c('70', '74', '76')]<-'4'
merge_df$Zone[merge_df$Station %in% c('62', '64', '66')]<-'3'
merge_df$Zone[merge_df$Station %in% c('44', '56', 'Pro')]<-'2'
merge_df$Zone[merge_df$Station %in% c('16', '34')]<-'1'


merge_df$Station<-factor(merge_df$Station, c("16", "34", "44", "Pro", "56", "62", "64", "66" ,"70" ,"74" ,"76" ,"84" ,"WSP"))

#loop through sites and plot NO3
#colors
color.palette = colorRampPalette(c(viridis(6, begin=.2, end=.98), rev(magma(5, begin=.35, end=.98))), bias=1)
colors<-color.palette(length(unique(merge_df$Station)))

variables<-c('NO3.Nppm', 'NH4.Nppm', 'PO4.Pppm', 'Chloro.appb')
# variables<-c('NO3.Nppm', 'NH4.Nppm', 'PO4.Pppm')
stations<-c('66', '70', '74', '76', '84')


# stations<-c( '74', '70')


#All Stations
plot_list<-list()

var<-1
for (var in 1:length(variables)){
  
plot_list[[var]]<-ggplot(merge_df, aes_string('Date', variables[var], group='Station')) + 
  scale_shape_manual(values=rep(21:25, 5))  + 
  scale_fill_manual(values = color.palette(length(unique(merge_df$Station)))) + 
  scale_colour_manual(values = color.palette(length(unique(merge_df$Station)))) +
  geom_line(size=.5, aes(colour=Station,  group=Station)) +    
  geom_point(size=1, aes(fill=Station, shape=Station)) + 
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))  +
  theme(legend.position='none') + 
  theme(axis.title.x=element_blank())

if (variables[var]=="Chloro.appb"){
  plot_list[[var]] <- plot_list[[var]] + 
    scale_y_log10()
}

}

# add legeng to first plot and then extract it
p1<-plot_list[[1]] + 
  theme(legend.position='bottom') +
  guides(shape = guide_legend(nrow = 2, title.position='top', title.hjust=0.5))
mylegend<-g_legend(p1)

# arrange plots without legend
p2<-grid.arrange(grobs=plot_list, ncol=1, as.table=F)


png(paste0(dropbox_dir, '/Figures/Timeseries/InorganicNutrients_AllStations_TimeSeries.png'), units='in', width=7, height=7, res=400, bg='white')

grid.arrange(p2, mylegend, nrow=2, heights=c(10, 1.2))

dev.off()


#Upper stations
plot_list<-list()

var<-1
for (var in 1:length(variables)){
  
  plot_list[[var]]<-ggplot(merge_df[merge_df$Station %in% stations,], aes_string('Date', variables[var], group='Station')) + 
    scale_shape_manual(values=rep(21:25, 5))  + 
    scale_fill_manual(values = color.palette(length(stations))) + 
    scale_colour_manual(values = color.palette(length(stations))) +
    geom_line(size=.5, aes(colour=Station,  group=Station)) +    
    geom_point(size=2, aes(fill=Station, shape=Station)) + 
    theme_bw() +
    theme(plot.title = element_text(hjust=0.5))  +
    theme(legend.position='none') + 
    theme(axis.title.x=element_blank())
  
  if (variables[var]=="Chloro.appb"){
    plot_list[[var]] <- plot_list[[var]] + 
      scale_y_log10()
  }
  
}

# add legeng to first plot and then extract it
p1<-plot_list[[1]] + 
  theme(legend.position='bottom')
mylegend<-g_legend(p1)

# arrange plots without legend
p2<-grid.arrange(grobs=plot_list, ncol=1, as.table=F)


png(paste0(dropbox_dir, '/Figures/Timeseries/InorganicNutrients_UpperStations_TimeSeries.png'), units='in', width=7, height=7, res=400, bg='white')

grid.arrange(p2, mylegend, nrow=2, heights=c(10, 0.5))

dev.off()



#Boxplots by zone
colors<-color.palette(length(unique(merge_df$Zone)))


#Common theme for all boxplots
commonTheme_boxplot<-list(
  scale_fill_manual(values = colors),
  scale_colour_manual(values = colors),
  theme_bw(),
  theme(plot.title = element_text(hjust=0.5), legend.position="none", axis.title.x=element_blank()),
  geom_boxplot(outlier.size=0.5)
)

# Loop through metrics and make a gg object
box_list<-list()
plot_nu<-1
for (plot_nu in 1:length(variables)){
  # Pick data
  metric<-variables[plot_nu]
  #Plot
  box_list[[plot_nu]] <- ggplot(aes_string(y = metric, x = 'Month', fill = 'Zone'), data = merge_df) + 
    labs(x='Month', y=metric) +
    commonTheme_boxplot
  
  if (variables[plot_nu ]=="Chloro.appb"){
    box_list[[plot_nu]]<-   box_list[[plot_nu]] + 
      scale_y_log10()
  }
}



#Add and extract legend from first plot
box_withlegend <- box_list[[1]] + 
  theme(legend.position='bottom') 

mylegend_box<-g_legend(box_withlegend)


# arrange plots without legend
p2_box<-grid.arrange(grobs=box_list, ncol=2, as.table=F)


#Add legend to bottom of figure and save
png(paste0(dropbox_dir, '/Figures/Timeseries/ChemistryByZoneByMonth.png'), width=8, height=5, units='in', res=200)

grid.arrange(p2_box, mylegend_box, nrow=2,heights=c(10, 1))

dev.off()





#Boxplots by station
colors<-color.palette(length(unique(merge_df$Station)))


#Common theme for all boxplots
commonTheme_boxplot<-list(
  scale_fill_manual(values = colors),
  scale_colour_manual(values = colors),
  theme_bw(),
  theme(plot.title = element_text(hjust=0.5), legend.position="none"),
  geom_boxplot(outlier.size=0.5, na.rm=T)
)

# Loop through metrics and make a gg object
box_list<-list()
plot_nu<-1
for (plot_nu in 1:length(variables)){
  # Pick data
  metric<-variables[plot_nu]
  #Plot
  box_list[[plot_nu]] <- ggplot(aes_string(y = metric, x = 'Station', fill = 'Station'), data = merge_df) + 
    labs(x='Station', y=metric) +
    commonTheme_boxplot
  
  if (variables[plot_nu ]=="Chloro.appb"){
    box_list[[plot_nu]]<-   box_list[[plot_nu]] + 
      scale_y_log10()
  }
}



#Add and extract legend from first plot
box_withlegend <- box_list[[1]] + 
  theme(legend.position='bottom') 

mylegend_box<-g_legend(box_withlegend)


# arrange plots without legend
p2_box<-grid.arrange(grobs=box_list, ncol=2, as.table=F)


#Add legend to bottom of figure and save
png(paste0(dropbox_dir, '/Figures/Timeseries/ChemistryByStationAllMonths.png'), width=8, height=5, units='in', res=200)

grid.arrange(p2_box, mylegend_box, nrow=2,heights=c(10, 2),  top='All months')

dev.off()


#Summer only
#Boxplots by station
colors<-color.palette(length(unique(merge_df$Station)))


#Common theme for all boxplots
commonTheme_boxplot<-list(
  scale_fill_manual(values = colors),
  scale_colour_manual(values = colors),
  theme_bw(),
  theme(plot.title = element_text(hjust=0.5), legend.position="none"),
  geom_boxplot(outlier.size=0.5, na.rm=T)
)

# Loop through metrics and make a gg object
box_list<-list()
plot_nu<-1
for (plot_nu in 1:length(variables)){
  # Pick data
  metric<-variables[plot_nu]
  #Plot
  box_list[[plot_nu]] <- ggplot(aes_string(y = metric, x = 'Station', fill = 'Station'), data = merge_df[merge_df$Month %in% month.abb[6:9],]) + 
    labs(x='Station', y=metric) +
    commonTheme_boxplot
  
  if (variables[plot_nu ]=="Chloro.appb"){
    box_list[[plot_nu]]<-   box_list[[plot_nu]] + 
      scale_y_log10()
  }
}



#Add and extract legend from first plot
box_withlegend <- box_list[[1]] + 
  theme(legend.position='bottom') 

mylegend_box<-g_legend(box_withlegend)


# arrange plots without legend
p2_box<-grid.arrange(grobs=box_list, ncol=2, as.table=F)


#Add legend to bottom of figure and save
png(paste0(dropbox_dir, '/Figures/Timeseries/ChemistryByStationSummerMonths.png'), width=8, height=5, units='in', res=200)

grid.arrange(p2_box, mylegend_box, nrow=2,heights=c(10, 2), top='June through Sept')

dev.off()




#Heatmaps

library(akima)
library(ggplot2)


SSCSites <- readRDS(file=paste0(dropbox_dir, '/Data/SpatialData/SSCSites.rds'))

SSCSites$STATIONclean<-c('16', '34', '44', '56', '62', '64', '66', '70', '74', '76', '84', 'WSP', 'Pro')
unique(merge_df$Station)


merge_df$Dist<-SSCSites$Dist[match(merge_df$Station, SSCSites$STATIONclean)]

heat<-merge_df[-which(merge_df$Station %in% c('Pro', '64')),]
heat$Dist<-round(heat$Dist, -1)
plot(merge_df$Dist, merge_df$Chloro.appb)
points(heat$Dist, heat$Chloro.appb, col='red')





range<-range(heat$Dist, na.rm=T)
distances<-seq(range[1], range[2], by=100)

dates<-seq(min(heat$Date), max(heat$Date), 5)

interped <- with(heat[is.finite(heat$Chloro.appb),], interp(Date, Dist, Chloro.appb, duplicate='mean', xo=dates, yo=distances ))
ChlA_data_interp <- with(interped, data.frame(date=as.Date(rep(dates, length.out=length(z))), Dist=rep(y, each=length(x)), ChlA=as.vector(z))
)

max_ChlA<-30
ChlA_data_interp$ChlA[which(ChlA_data_interp$ChlA>max_ChlA)]<-max_ChlA



#colors
color.palette = colorRampPalette(c(viridis(6, begin=.2, end=.98), rev(magma(5, begin=.35, end=.98))), bias=1)
colors<-color.palette(20)

ChlAExp<-expression(paste("Chlorophyll a (", mu, "g L"^"-1", ")  ", sep=''))


# rasterplots<-list()
# 
# for (day_nu in 1:length(sampledates)){
#   
# rasterplots[[day_nu]]<-

# ggplot(Zoo_gather, aes(x=date, y=Dist, z=Zoo, fill=Zoo)) + 
viz <- ggplot(ChlA_data_interp, aes(date, Dist/1000))
heatplot<- viz + geom_tile(aes(fill = (ChlA))) + 
  labs(y='Distance (km)', x='Date') + 
  # stat_contour() + 
  # geom_raster(aes(fill=Zoo), interpolate=TRUE) + 
  # aes(x = x, y = y, z = z, fill = z) + 
  # geom_tile() + 
  scale_fill_gradientn(colours=color.palette(20), breaks=seq(0,30, 10), labels=c(0,10,20,">30"), na.value='white') +
  # floor(min(Zoo_gather$Zoo)), ceiling(max(Zoo_gather$Zoo)))) +
  scale_y_reverse(expand = c(0, 0)) + 
  scale_x_date(expand=c(0,0)) + 
  geom_vline(xintercept=as.Date(paste(2010:2019, "-01-01", sep="")), linetype=2) + 
  theme_classic(base_size = 16) +
  # ggtitle(sampledates[day_nu]) + 
  theme(plot.title = element_text(size=10, hjust = 0.5), axis.title.x=element_blank()) + 
  # theme(legend.position='right')
  theme(legend.position='bottom') + 
  guides(fill=guide_colourbar(title=ChlAExp, barwidth=20)) 
# scale_colour_manual(values = colors)

print(heatplot)



png(paste0(dropbox_dir, '/Figures/Heatmaps_ChlA.png'), width=12, height=8, units='in', res=200)

print(heatplot)

dev.off()



#Temperature
interped <- with(heat[is.finite(heat$Temp.C),], interp(Date, Dist, Temp.C, duplicate='mean', xo=dates, yo=distances ))
Temp_data_interp <- with(interped, data.frame(date=as.Date(rep(dates, length.out=length(z))), Dist=rep(y, each=length(x)), Temp=as.vector(z))
)

# max_ChlA<-30
# ChlA_data_interp$ChlA[which(ChlA_data_interp$ChlA>max_ChlA)]<-max_ChlA



#colors
color.palette = colorRampPalette(c(viridis(6, begin=.2, end=.98), rev(magma(5, begin=.35, end=.98))), bias=1)
colors<-color.palette(20)

TempExp<-expression(paste("Temperature (", degree, "C", ")  ", sep=''))


# ggplot(Zoo_gather, aes(x=date, y=Dist, z=Zoo, fill=Zoo)) + 
viz <- ggplot(Temp_data_interp, aes(date, Dist/1000))
heatplot<- viz + geom_tile(aes(fill = (Temp))) + 
  labs(y='Distance (km)', x='Date') + 
  # stat_contour() + 
  # geom_raster(aes(fill=Zoo), interpolate=TRUE) + 
  # aes(x = x, y = y, z = z, fill = z) + 
  # geom_tile() + 
  scale_fill_gradientn(colours=color.palette(20), na.value='white') +
  # scale_fill_gradientn(colours=color.palette(20), breaks=seq(0,30, 10), labels=c(0,10,20,">30"), na.value='white') +
  # floor(min(Zoo_gather$Zoo)), ceiling(max(Zoo_gather$Zoo)))) +
  scale_y_reverse(expand = c(0, 0)) + 
  scale_x_date(expand=c(0,0)) + 
  geom_vline(xintercept=as.Date(paste(2010:2019, "-01-01", sep="")), linetype=2) + 
  theme_classic(base_size = 16) +
  # ggtitle(sampledates[day_nu]) + 
  theme(plot.title = element_text(size=10, hjust = 0.5), axis.title.x=element_blank()) + 
  # theme(legend.position='right')
  theme(legend.position='bottom') + 
  guides(fill=guide_colourbar(title=TempExp, barwidth=20)) 
# scale_colour_manual(values = colors)

print(heatplot)

png(paste0(dropbox_dir, '/Figures/Heatmaps_Temp.png'), width=12, height=8, units='in', res=200)

print(heatplot)

dev.off()



#Conductivity
interped <- with(heat[is.finite(heat$SpCond.uS),], interp(Date, Dist, SpCond.uS, duplicate='mean', xo=dates, yo=distances ))
SPC_data_interp <- with(interped, data.frame(date=as.Date(rep(dates, length.out=length(z))), Dist=rep(y, each=length(x)), SPC=as.vector(z))
)

max_SPC<-1200
SPC_data_interp$SPC[which(SPC_data_interp$SPC>max_SPC)]<-max_SPC



#colors
color.palette = colorRampPalette(c(viridis(6, begin=.2, end=.98), rev(magma(5, begin=.35, end=.98))), bias=1)
colors<-color.palette(20)

SPCExp<-expression(paste("Specific conductivity (", mu, "S cm"^"-1", ")  ", sep=''))


# ggplot(Zoo_gather, aes(x=date, y=Dist, z=Zoo, fill=Zoo)) + 
viz <- ggplot(SPC_data_interp, aes(date, Dist/1000))
heatplot<- viz + geom_tile(aes(fill = (SPC))) + 
  labs(y='Distance (km)', x='Date') + 
  # stat_contour() + 
  # geom_raster(aes(fill=Zoo), interpolate=TRUE) + 
  # aes(x = x, y = y, z = z, fill = z) + 
  # geom_tile() + 
  scale_fill_gradientn(colours=color.palette(20), breaks=seq(0,max_SPC, 200), labels=c(0,200,400,600,800,1000,">1200"), na.value='white') +
  # floor(min(Zoo_gather$Zoo)), ceiling(max(Zoo_gather$Zoo)))) +
  scale_y_reverse(expand = c(0, 0)) + 
  scale_x_date(expand=c(0,0)) + 
  geom_vline(xintercept=as.Date(paste(2010:2019, "-01-01", sep="")), linetype=2) + 
  theme_classic(base_size = 16) +
  # ggtitle(sampledates[day_nu]) + 
  theme(plot.title = element_text(size=10, hjust = 0.5), axis.title.x=element_blank()) + 
  # theme(legend.position='right')
  theme(legend.position='bottom') + 
  guides(fill=guide_colourbar(title=SPCExp, barwidth=20)) 
# scale_colour_manual(values = colors)

print(heatplot)

png(paste0(dropbox_dir, '/Figures/Heatmaps_SPC.png'), width=12, height=8, units='in', res=200)

print(heatplot)

dev.off()




#Nitrate
interped <- with(heat[is.finite(heat$NO3.Nppm),], interp(Date, Dist, NO3.Nppm, duplicate='mean', xo=dates, yo=distances ))
NO3_data_interp <- with(interped, data.frame(date=as.Date(rep(dates, length.out=length(z))), Dist=rep(y, each=length(x)), NO3=as.vector(z))
)

max_NO3<-2
NO3_data_interp$NO3[which(NO3_data_interp$NO3>max_NO3)]<-max_NO3



#colors
color.palette = colorRampPalette(c(viridis(6, begin=.2, end=.98), rev(magma(5, begin=.35, end=.98))), bias=1)
colors<-color.palette(20)

NO3Exp<-expression(paste("Nitrate (", "mg N L"^"-1", ")  ", sep=''))


# ggplot(Zoo_gather, aes(x=date, y=Dist, z=Zoo, fill=Zoo)) + 
viz <- ggplot(NO3_data_interp, aes(date, Dist/1000))
heatplot<- viz + geom_tile(aes(fill = (NO3))) + 
  labs(y='Distance (km)', x='Date') + 
  # stat_contour(aes(z=NO3), breaks=0.1, colour='black', linetype=1, size=1.5) + 
  # geom_raster(aes(fill=Zoo), interpolate=TRUE) + 
  # aes(x = x, y = y, z = z, fill = z) + 
  # geom_tile() + 
  # scale_fill_gradientn(colours=color.palette(20), breaks=seq(0,max_NO3, 0.5), labels=c(0,0.5, 1, 1.5 ,">2"), na.value='white') +
  scale_fill_gradientn(colours=color.palette(20), na.value='white') +
  # floor(min(Zoo_gather$Zoo)), ceiling(max(Zoo_gather$Zoo)))) +
  scale_y_reverse(expand = c(0, 0)) + 
  scale_x_date(expand=c(0,0)) + 
  geom_vline(xintercept=as.Date(paste(2010:2019, "-01-01", sep="")), linetype=2) + 
  theme_classic(base_size = 16) +
  # ggtitle(sampledates[day_nu]) + 
  theme(plot.title = element_text(size=10, hjust = 0.5), axis.title.x=element_blank()) + 
  # theme(legend.position='right')
  theme(legend.position='bottom') + 
  guides(fill=guide_colourbar(title=NO3Exp, barwidth=20)) 
# scale_colour_manual(values = colors)

print(heatplot)

png(paste0(dropbox_dir, '/Figures/Heatmaps_NO3.png'), width=12, height=8, units='in', res=200)

print(heatplot)

dev.off()


#withcontour
heatplot<- viz + geom_tile(aes(fill = (NO3))) + 
  labs(y='Distance (km)', x='Date') + 
  stat_contour(aes(z=NO3), breaks=0.05, colour='black', linetype=1, size=1.5) + 
  # geom_raster(aes(fill=Zoo), interpolate=TRUE) + 
  # aes(x = x, y = y, z = z, fill = z) + 
  # geom_tile() + 
  # scale_fill_gradientn(colours=color.palette(20), breaks=seq(0,max_NO3, 0.5), labels=c(0,0.5, 1, 1.5 ,">2"), na.value='white') +
  scale_fill_gradientn(colours=color.palette(20), na.value='white') +
  # floor(min(Zoo_gather$Zoo)), ceiling(max(Zoo_gather$Zoo)))) +
  scale_y_reverse(expand = c(0, 0)) + 
  scale_x_date(expand=c(0,0)) + 
  geom_vline(xintercept=as.Date(paste(2010:2019, "-01-01", sep="")), linetype=2) + 
  theme_classic(base_size = 16) +
  # ggtitle(sampledates[day_nu]) + 
  theme(plot.title = element_text(size=10, hjust = 0.5), axis.title.x=element_blank()) + 
  # theme(legend.position='right')
  theme(legend.position='bottom') + 
  guides(fill=guide_colourbar(title=NO3Exp, barwidth=20)) 
# scale_colour_manual(values = colors)

print(heatplot)

png(paste0(dropbox_dir, '/Figures/Heatmaps_withContour_NO3.png'), width=12, height=8, units='in', res=200)

print(heatplot)

dev.off()


