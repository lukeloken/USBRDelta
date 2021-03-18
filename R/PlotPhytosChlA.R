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


# source('R/g_legend.R')
# # source('R/CompilePhytos.R')
# 
# # Project folder where outputs are stored
# dropbox_dir<-'C:/Dropbox/USBR Delta Project'
# 
# #Where data come from
# google_dir<-'C:/GoogleDrive/DeltaNutrientExperiment'

# SSC_joined_data<-readRDS(file=paste0(dropbox_dir, '/Data/Rdata/SSC_joined_data'))

SSC_joined_data <- readRDS(file = file.path(onedrive_dir, 'RData', 'MonthlyCruises', 
                                            'SSC_joined_data.rds'))

# #############################################
# Plot phytoplankton and chlorlophyll patterns
# #############################################

variables<-c("Chloroappb", "Bacillariophyta", "Chlorophyta", "Cryptophyta", "Chrysophyta", "Cyanobacteria", "Phyto_total") 
upperstations<-c('66', '70', '74', '76', '84')


#All Stations
plot_list<-list()

var<-1
for (var in 1:length(variables)){
  
  plot_list[[var]]<-ggplot(SSC_joined_data, aes_string('Date', variables[var], group='Station')) + 
    scale_shape_manual(values=rep(21:25, 5))  + 
    scale_fill_manual(values = colors_stations) + 
    scale_colour_manual(values = colors_stations) +
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

# png(file.path(onedrive_dir, "Figures", "MonthlyCruises", "Phytos", '

png(file.path(onedrive_dir, "Figures", "MonthlyCruises", 
              "Phytos", 'AllGenus_ChlA_TimeSeries.png'), 
    units='in', width=7, height=12, res=400, bg='white')

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
    scale_fill_manual(values = colors_stations) + 
    scale_colour_manual(values = colors_stations) +
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

png(file.path(onedrive_dir, "Figures", "MonthlyCruises", 
              "Phytos", 'AllGenus_ChlA_Scatterplots.png'), 
    units='in', width=6, height=9, res=400, bg='white')

grid.arrange(plot2, mylegend2, nrow=2, heights=c(10, 1))

dev.off()

#Zone
png(file.path(onedrive_dir, "Figures", "MonthlyCruises", 
              "Phytos", 'AllGenus_ChlA_Scatterplots_byZone.png'), 
    units='in', width=6, height=9, res=400, bg='white')

grid.arrange(plot2_zone, mylegend2_zone, nrow=2, heights=c(10, 1))

dev.off()


#regression models

Phyto_ChlA_model<-lm(Chloroappb ~ Bacillariophyta+Chlorophyta+Cryptophyta, data=SSC_joined_data)
summary(Phyto_ChlA_model)
anova(Phyto_ChlA_model)

step_model<-stepAIC(Phyto_ChlA_model, direction = 'both', k=3)
print(summary(step_model))
print(anova(step_model))



#Chla +Pheo vs phyto divisions
plot_list2<-list()
variables<-c("Bacillariophyta", "Chlorophyta", "Cryptophyta", "Chrysophyta", "Cyanobacteria", "Phyto_total") 
var<-1
for (var in 1:length(variables)){
  
  plot_list2[[var]]<-ggplot(SSC_joined_data[which(SSC_joined_data$Station != '64'),], aes_string('ChloPheoppb', variables[var], group='Station')) + 
    labs(x='Chl a + Pheo') +
    scale_shape_manual(values=rep(21:25, 5))  + 
    scale_fill_manual(values = colors_stations) + 
    scale_colour_manual(values = colors_stations) +
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


png(file.path(onedrive_dir, "Figures", "MonthlyCruises", 
              "Phytos", 'AllGenus_ChlAPheo_Scatterplots.png'), 
    units='in', width=6, height=9, res=400, bg='white')

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
    scale_fill_manual(values = colors_stations) + 
    scale_colour_manual(values = colors_stations) +
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


png(file.path(onedrive_dir, "Figures", "MonthlyCruises", 
              "Phytos", 'AllGenus_ChlAYSI_Scatterplots.png'), 
    units='in', width=6, height=9, res=400, bg='white')

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

dominantspeciesplot<-ggplot(dom_phyto[which(is.finite(dom_phyto$Chloroappb)),], 
                            aes_string('Chloroappb', 'max', shape='species', 
                                       fill='species', group='species')) + 
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

png(file.path(onedrive_dir, "Figures", "MonthlyCruises", 
              "Phytos", 'DominantGenus_ChlA_Scatterplots.png'), 
    units='in', width=6, height=5, res=400, bg='white')

print(dominantspeciesplot)

dev.off()




