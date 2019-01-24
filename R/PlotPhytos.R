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


source('R/g_legend.R')
# source('R/CompilePhytos.R')

# Project folder where outputs are stored
dropbox_dir<-'C:/Dropbox/USBR Delta Project'

#Where data come from
google_dir<-'C:/GoogleDrive/DeltaNutrientExperiment'

stationfactors<-c("16", "34", "44", "Pro", "56", "62", "66" ,"70" ,"74" ,"76" ,"84" ,"WSP")

Phyto_summary_spread <- read.csv(file=paste(dropbox_dir, 'Data', 'Phyto', 'PhytoSummaryDivision.csv', sep='/'), stringsAsFactors = F)
Phyto_summary_spread$DATE<-as.Date(Phyto_summary_spread$DATE)
Phyto_summary_spread$STATIONclean<-factor(Phyto_summary_spread$STATIONclean, stationfactors)

Phyto_summary_spread$Total_BioVolume<-rowSums(Phyto_summary_spread[,c("Bacillariophyta", "Chlorophyta", "Cryptophyta", "Chrysophyta", "Cyanobacteria")])

names(Phyto_summary_spread)[which(names(Phyto_summary_spread)=='STATIONclean')]<-"Station"
names(Phyto_summary_spread)[which(names(Phyto_summary_spread)=='DATE')]<-"Date"



#nutrients and chla data.frame
merge_df <- read.csv(file=paste0(dropbox_dir, '/Data/SurfaceChemistry/YSIChemSurface.csv'), stringsAsFactors = F)
merge_df$Date<-as.Date(merge_df$Date)
merge_df$Month<-as.character(month(merge_df$Date, label = TRUE))
merge_df$Month<-factor(merge_df$Month, month.abb[1:12])

merge_df$Zone<-NA
merge_df$Zone[merge_df$Station %in% c('WSP', '84')]<-'5'
merge_df$Zone[merge_df$Station %in% c('70', '74', '76')]<-'4'
merge_df$Zone[merge_df$Station %in% c('62', '64', '66')]<-'3'
merge_df$Zone[merge_df$Station %in% c('44', '56', 'Pro')]<-'2'
merge_df$Zone[merge_df$Station %in% c('16', '34')]<-'1'


merge_df$Station<-factor(merge_df$Station, stationfactors)

merge_phyto<-full_join(merge_df, Phyto_summary_spread)
merge_phyto<-merge_phyto %>%
  filter(Station %in% stationfactors)

# str(merge_phyto)
# plot(merge_phyto$Date, merge_phyto$Bacillariophyta)

#loop through sites and plot NO3
#colors
color.palette = colorRampPalette(c(viridis(6, begin=.2, end=.98), rev(magma(5, begin=.35, end=.98))), bias=1)
colors<-color.palette(length(unique(merge_phyto$Station)))
colors_zone<-color.palette(length(unique(merge_phyto$Zone)))


variables<-c("Chloro.appb", "Bacillariophyta", "Chlorophyta", "Cryptophyta", "Chrysophyta", "Cyanobacteria", "Total_BioVolume") 
# variables<-c('NO3.Nppm', 'NH4.Nppm', 'PO4.Pppm')
stations<-c('66', '70', '74', '76', '84')


#All Stations
plot_list<-list()

var<-1
for (var in 1:length(variables)){
  
  plot_list[[var]]<-ggplot(merge_phyto, aes_string('Date', variables[var], group='Station')) + 
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
variables<-c("Bacillariophyta", "Chlorophyta", "Cryptophyta", "Chrysophyta", "Cyanobacteria", "Total_BioVolume") 
var<-1
for (var in 1:length(variables)){
  
  plot_list2[[var]]<-ggplot(merge_phyto, aes_string('Chloro.appb', variables[var], group='Station', shape='Station', fill='Station')) + 
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
  
  plot_list2_zone[[var]]<-ggplot(merge_phyto[!is.na(merge_phyto$Zone),], aes_string('Chloro.appb', variables[var], group='Zone', shape='Zone', fill='Zone')) + 
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

Phyto_ChlA_model<-lm(Chloro.appb ~ Bacillariophyta+Chlorophyta+Cryptophyta, data=merge_phyto)
summary(Phyto_ChlA_model)
anova(Phyto_ChlA_model)

step_model<-stepAIC(Phyto_ChlA_model, direction = 'both', k=3)
summary(step_model)




#Chla +Pheo vs phyto divisions
plot_list2<-list()
variables<-c("Bacillariophyta", "Chlorophyta", "Cryptophyta", "Chrysophyta", "Cyanobacteria", "Total_BioVolume") 
var<-1
for (var in 1:length(variables)){
  
  plot_list2[[var]]<-ggplot(merge_phyto, aes_string("Chlo.Pheoppb", variables[var], group='Station')) + 
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
variables<-c("Bacillariophyta", "Chlorophyta", "Cryptophyta", "Chrysophyta", "Cyanobacteria", "Total_BioVolume") 
var<-1
for (var in 1:length(variables)){
  
  plot_list2[[var]]<-ggplot(merge_phyto, aes_string("Chl.RFU", variables[var], group='Station')) + 
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

dom_phyto<- merge_phyto %>%
  dplyr::select("Bacillariophyta", "Chlorophyta", "Cryptophyta", "Chrysophyta", "Cyanobacteria", "Total_BioVolume", "Chloro.appb", 'Station', 'Zone', 'Date') %>%
  drop_na(Total_BioVolume)

dom_phyto$max<-apply(dom_phyto[,1:5], 1, max) 
dom_phyto$species<-apply(dom_phyto, 1, function(x) names(x)[which(as.numeric(x[1:5]) == as.numeric(x[c('max')]))])

#Chla (lab extraction) vs phyto divisions
plot_list2<-list()
plot_list2_zone<-list()
# variables<-c("Bacillariophyta", "Chlorophyta", "Cryptophyta", "Chrysophyta", "Cyanobacteria") 

colorset<-'Accent'
colors_divisions<-brewer.pal(length(unique(dom_phyto$species)), colorset)
colors_divisions<-colors_divisions[c(2,1,3,4,5)]

  dominantspeciesplot<-ggplot(dom_phyto[which(is.finite(dom_phyto$Chloro.appb)),], aes_string('Chloro.appb', 'max', shape='species', fill='species', group='species')) + 
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

#Zone
# png(paste0(dropbox_dir, '/Figures/Phytos/AllGenus_ChlA_Scatterplots_byZone.png'), units='in', width=6, height=9, res=400, bg='white')

grid.arrange(plot2_zone, mylegend2_zone, nrow=2, heights=c(10, 1))

dev.off()



