
# Merge nutrient, ysi, phytoplannkton, and zooplankton data
# Can add more as necessary


# plot phytoplankton community

library(viridis)
library(ggplot2)
library(gridExtra)
library(plyr)
library(dplyr)
library(tidyr)
library(lubridate)
library(RColorBrewer)
library(MASS)



# # Project folder where outputs are stored
# dropbox_dir<-'C:/Dropbox/USBR Delta Project'
# 
# #Where data come from
# google_dir<-'C:/GoogleDrive/DeltaNutrientExperiment'


stationfactors<-c("16", "34", "44", "Pro", "56", "62", "64", "66" ,"70" ,"74" ,"76" ,"84" ,"WSP")


#Input Data
#nutrients
WQ_stations<-readRDS(file=paste0(dropbox_dir, '/Data/Rdata/WQ_stations'))
WQ_stations$Station<-factor(WQ_stations$Station, stationfactors)
WQ_stations <- dplyr::select(WQ_stations, -NH4_20_ftppm, -NO3_20_ftppm, -PO4_20_ft)

#YSI
YSI_ThreeDepths<-readRDS(file=paste0(dropbox_dir, '/Data/Rdata/YSI_ThreeDepths.rds'))
YSI_surf<-dplyr::filter(YSI_ThreeDepths, DepthStrata=='lessthan3')

#Phyto
Phyto_summary_spread <- readRDS(file=paste0(dropbox_dir, '/Data/Rdata/Phyto_summary_spread.rds'))

#Zoops
Zoo_summary_spread <- readRDS(file=paste0(dropbox_dir, '/Data/Rdata/Zoo_summary_spread.rds'))

# others...


SSC_joined_data<-full_join(WQ_stations, YSI_surf) %>%
  full_join(Phyto_summary_spread) %>%
  full_join(Zoo_summary_spread)

write.table(SSC_joined_data, file=paste0(google_dir, '/DataOutputs/SSC_JoinedSurfaceData.csv'), row.names=F, sep=',')
saveRDS(SSC_joined_data , file=paste0(dropbox_dir, '/Data/Rdata/SSC_joined_data'))





#Omit below, this will likely get pushed to a plotting script. 

#Plotting factors
SSC_joined_data$Month<-as.character(month(SSC_joined_data$Date, label = TRUE))
SSC_joined_data$Month<-factor(SSC_joined_data$Month, month.abb[1:12])

SSC_joined_data$Zone<-NA
SSC_joined_data$Zone[SSC_joined_data$Station %in% c('WSP', '84')]<-'5'
SSC_joined_data$Zone[SSC_joined_data$Station %in% c('70', '74', '76')]<-'4'
SSC_joined_data$Zone[SSC_joined_data$Station %in% c('62', '64', '66')]<-'3'
SSC_joined_data$Zone[SSC_joined_data$Station %in% c('44', '56', 'Pro')]<-'2'
SSC_joined_data$Zone[SSC_joined_data$Station %in% c('16', '34')]<-'1'

# #############################################
# Plot phytoplankton and chlorlophyll patterns
# #############################################

#colors
color.palette = colorRampPalette(c(viridis(6, begin=.2, end=.98), rev(magma(5, begin=.35, end=.98))), bias=1)
colors<-color.palette(length(unique(SSC_joined_data$Station)))
colors_zone<-color.palette(length(unique(SSC_joined_data$Zone)))


variables<-c("Chloroappb", "Bacillariophyta", "Chlorophyta", "Cryptophyta", "Chrysophyta", "Cyanobacteria", "Phyto_total") 
upperstations<-c('66', '70', '74', '76', '84')


#All Stations
plot_list<-list()

var<-1
for (var in 1:length(variables)){
  
  plot_list[[var]]<-ggplot(SSC_joined_data, aes_string('Date', variables[var], group='Station')) + 
    scale_shape_manual(values=rep(21:25, 5))  + 
    scale_fill_manual(values = colors) + 
    scale_colour_manual(values = colors) +
    geom_line(size=.5, aes(colour=Station,  group=Station)) +    
    geom_point(size=1, aes(fill=Station, shape=Station)) + 
    theme_bw() +
    theme(plot.title = element_text(hjust=0.5))  +
    theme(legend.position='none') +
    scale_y_log10()
  
  # if (variables[var]=="Chloro.appb"){
  #   plot_list[[var]] <- plot_list[[var]] + 
  #     scale_y_log10()
  # }
  
}

# add legeng to first plot and then extract it
p1<-plot_list[[1]] + 
  theme(legend.position='bottom') +
  guides(shape = guide_legend(nrow = 2, title.position='top', title.hjust=0.5))
mylegend<-g_legend(p1)

# arrange plots without legend
p2<-grid.arrange(grobs=plot_list, ncol=1, as.table=F)


png(paste0(dropbox_dir, '/Figures/Phytos/AllGenus_ChlA_TimeSeries.png'), units='in', width=7, height=12, res=400, bg='white')

grid.arrange(p2, mylegend, nrow=2, heights=c(10, 1.2))

dev.off()



#Chla (lab extraction) vs phyto divisions
plot_list2<-list()
plot_list2_zone<-list()
variables<-c("Bacillariophyta", "Chlorophyta", "Cryptophyta", "Chrysophyta", "Cyanobacteria", "Phyto_total") 
var<-1
for (var in 1:length(variables)){
  
  plot_list2[[var]]<-ggplot(SSC_joined_data[which(SSC_joined_data$Station != '64'),], aes_string('Chloroappb', variables[var], group='Station', shape='Station', fill='Station')) + 
    labs(x='Chl a') +
    scale_shape_manual(values=rep(21:25, 5))  + 
    scale_fill_manual(values = colors) + 
    scale_colour_manual(values = colors) +
    # geom_line(size=.5, aes(colour=Station,  group=Station)) +    
    geom_point(size=1) + 
    theme_bw() +
    theme(plot.title = element_text(hjust=0.5))  +
    theme(legend.position='none') +
    scale_y_log10() + 
    scale_x_log10() +
    geom_smooth(method='lm', se=F, aes(colour=Station), size=1)
    # geom_smooth(se=F, aes(fill=Station, colour=Station))  
    # facet_grid(.~Station)
  
  plot_list2_zone[[var]]<-ggplot(SSC_joined_data[!is.na(SSC_joined_data$Zone),], aes_string('Chloroappb', variables[var], group='Zone', shape='Zone', fill='Zone')) + 
    labs(x='Chl a') +
    scale_shape_manual(values=rep(21:25, 5))  + 
    scale_fill_manual(values = colors_zone) + 
    scale_colour_manual(values = colors_zone) +
    # geom_line(size=.5, aes(colour=Station,  group=Station)) +    
    geom_point(size=1) + 
    theme_bw() +
    theme(plot.title = element_text(hjust=0.5))  +
    theme(legend.position='none') +
    scale_y_log10() + 
    scale_x_log10() +
    geom_smooth(method='lm', se=F, aes(colour=Zone), size=1)
  # geom_smooth(se=F, aes(fill=Station, colour=Station))  
  # facet_grid(.~Station)
  
  
}

# add legend to first plot and then extract it
p<-plot_list2[[1]] + 
  theme(legend.position='bottom') +
  guides(shape = guide_legend(nrow = 2, title.position='top', title.hjust=0.5))
mylegend2<-g_legend(p)

# arrange plots without legend
plot2<-grid.arrange(grobs=plot_list2, ncol=2, as.table=F)

# add legend to first plot and then extract it
p<-plot_list2_zone[[1]] + 
  theme(legend.position='bottom') +
  guides(shape = guide_legend(nrow = 1, title.position='top', title.hjust=0.5))
mylegend2_zone<-g_legend(p)
plot2_zone<-grid.arrange(grobs=plot_list2_zone, ncol=2, as.table=F)

# plot_list2[[length(variables)+1]]<-mylegend2

png(paste0(dropbox_dir, '/Figures/Phytos/AllGenus_ChlA_Scatterplots.png'), units='in', width=6, height=9, res=400, bg='white')

grid.arrange(plot2, mylegend2, nrow=2, heights=c(10, 1))

dev.off()

#Zone
png(paste0(dropbox_dir, '/Figures/Phytos/AllGenus_ChlA_Scatterplots_byZone.png'), units='in', width=6, height=9, res=400, bg='white')

grid.arrange(plot2_zone, mylegend2_zone, nrow=2, heights=c(10, 1))

dev.off()


#regression models

Phyto_ChlA_model<-lm(Chloroappb ~ Bacillariophyta+Chlorophyta+Cryptophyta, data=SSC_joined_data)
summary(Phyto_ChlA_model)
anova(Phyto_ChlA_model)

step_model<-stepAIC(Phyto_ChlA_model, direction = 'both', k=3)
summary(step_model)




#Chla +Pheo vs phyto divisions
plot_list2<-list()
variables<-c("Bacillariophyta", "Chlorophyta", "Cryptophyta", "Chrysophyta", "Cyanobacteria", "Phyto_total") 
var<-1
for (var in 1:length(variables)){
  
  plot_list2[[var]]<-ggplot(SSC_joined_data[which(SSC_joined_data$Station != '64'),], aes_string('ChloPheoppb', variables[var], group='Station')) + 
    labs(x='Chl a + Pheo') +
    scale_shape_manual(values=rep(21:25, 5))  + 
    scale_fill_manual(values = colors) + 
    scale_colour_manual(values = colors) +
    # geom_line(size=.5, aes(colour=Station,  group=Station)) +    
    geom_point(size=1, aes(fill=Station, shape=Station)) + 
    theme_bw() +
    theme(plot.title = element_text(hjust=0.5))  +
    theme(legend.position='none') +
    scale_y_log10() + 
    scale_x_log10() +
    geom_smooth(method='lm', se=F, aes(fill=Station, colour=Station))
  # geom_smooth(se=F, aes(fill=Station, colour=Station))  
  # facet_grid(.~Station)
  
}

# add legend to first plot and then extract it
p<-plot_list2[[1]] + 
  theme(legend.position='bottom') +
  guides(shape = guide_legend(nrow = 2, title.position='top', title.hjust=0.5))
mylegend2<-g_legend(p)

# arrange plots without legend
plot2<-grid.arrange(grobs=plot_list2, ncol=2, as.table=F)


png(paste0(dropbox_dir, '/Figures/Phytos/AllGenus_ChlAPheo_Scatterplots.png'), units='in', width=6, height=9, res=400, bg='white')

grid.arrange(plot2, mylegend2, nrow=2, heights=c(10, 1))

dev.off()



#Chla (ysi) vs phyto divisions
plot_list2<-list()
variables<-c("Bacillariophyta", "Chlorophyta", "Cryptophyta", "Chrysophyta", "Cyanobacteria", "Phyto_total") 
var<-1
for (var in 1:length(variables)){
  
  plot_list2[[var]]<-ggplot(SSC_joined_data[which(SSC_joined_data$Station != '64'),], aes_string("Chl.RFU", variables[var], group='Station')) + 
    labs(x='Chl a (YSI)') +
    scale_shape_manual(values=rep(21:25, 5))  + 
    scale_fill_manual(values = colors) + 
    scale_colour_manual(values = colors) +
    # geom_line(size=.5, aes(colour=Station,  group=Station)) +    
    geom_point(size=1, aes(fill=Station, shape=Station)) + 
    theme_bw() +
    theme(plot.title = element_text(hjust=0.5))  +
    theme(legend.position='none') +
    scale_y_log10() + 
    scale_x_log10() +
    geom_smooth(method='lm', se=F, aes(fill=Station, colour=Station))
  # geom_smooth(se=F, aes(fill=Station, colour=Station))  
  # facet_grid(.~Station)
  
}

# add legend to first plot and then extract it
p<-plot_list2[[1]] + 
  theme(legend.position='bottom') +
  guides(shape = guide_legend(nrow = 2, title.position='top', title.hjust=0.5))
mylegend2<-g_legend(p)

# arrange plots without legend
plot2<-grid.arrange(grobs=plot_list2, ncol=2, as.table=F)


png(paste0(dropbox_dir, '/Figures/Phytos/AllGenus_ChlAYSI_Scatterplots.png'), units='in', width=6, height=9, res=400, bg='white')

grid.arrange(plot2, mylegend2, nrow=2, heights=c(10, 1))

dev.off()





#select only the dominant phytoplankton division

dom_phyto<- SSC_joined_data %>%
  dplyr::select("Bacillariophyta", "Chlorophyta", "Cryptophyta", "Chrysophyta", "Cyanobacteria", "Phyto_total", "Chloroappb", 'Station', 'Zone', 'Date') %>%
  drop_na(Phyto_total)

dom_phyto$max<-apply(dom_phyto[,1:5], 1, max) 
dom_phyto$species<-apply(dom_phyto, 1, function(x) names(x)[which(as.numeric(x[1:5]) == as.numeric(x[c('max')]))])

#Chla (lab extraction) vs phyto divisions
plot_list2<-list()
plot_list2_zone<-list()
# variables<-c("Bacillariophyta", "Chlorophyta", "Cryptophyta", "Chrysophyta", "Cyanobacteria") 

colorset<-'Accent'
colors_divisions<-brewer.pal(length(unique(dom_phyto$species)), colorset)
colors_divisions<-colors_divisions[c(2,1,3,4,5)]

  dominantspeciesplot<-ggplot(dom_phyto[which(is.finite(dom_phyto$Chloroappb)),], aes_string('Chloroappb', 'max', shape='species', fill='species', group='species')) + 
    labs(x='Total Chl a (lab extraction)', y='Volume of dominant phytoplankton division') +
    scale_shape_manual(values=rep(21:25, 5))  + 
    scale_fill_manual(values = colors_divisions) + 
    scale_colour_manual(values = colors_divisions) +
    # geom_line(size=.5, aes(colour=Station,  group=Station)) +    
    geom_smooth(method='lm', se=T,  size=1, aes(colour=species), color='black', fill='black') + 
    geom_point(size=2) + 
    theme_bw() +
    theme(plot.title = element_text(hjust=0.5))  +
    theme(legend.position='bottom') +
    scale_y_log10() + 
    scale_x_log10() +
    theme(legend.title = element_blank()) + 
    facet_wrap(~species)
  
png(paste0(dropbox_dir, '/Figures/Phytos/DominantGenus_ChlA_Scatterplots.png'), units='in', width=6, height=5, res=400, bg='white')

print(dominantspeciesplot)

dev.off()




