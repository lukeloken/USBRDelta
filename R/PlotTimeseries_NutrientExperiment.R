

library(lubridate)
library(viridis)
library(grid)
library(gridExtra)
library(ggplot2)
library(dplyr)

# Project folder where outputs are stored
dropbox_dir<-'C:/Dropbox/USBR Delta Project'

#Where data come from
google_dir<-'C:/GoogleDrive/DeltaNutrientExperiment'


source('R/read_excel_allsheets.R')
source('R/g_legend.R')

# setwd("C:/Users/Luke/Dropbox/USBR Delta Project")
# 
# source("R/ReadInMasterData.R")

#New data input (after O18 metabolism)
met.final<-read.csv(file = paste(dropbox_dir, "Data", "NutrientExperiment", "Oxygen18", "O18MetabolismEstimates.csv", sep='/'), stringsAsFactors = F)

met.final$Date<-as.Date(met.final$Date)
met.final$Site<-factor(met.final$Site, c('NL70', 'EC2','EC3','EC4','EC5','EC6','EC7','EC8','NL76'))

met.final$GPP<-met.final$NEP-met.final$ER

#Old data input (before oxygen 18 metabolism)
# merge_df <- read.csv(file=paste0(dropbox_dir, '/Data/NutrientExperiment/SurfaceChemistry/YSIMetabolismSurface.csv'), stringsAsFactors = F)
# merge_df$Date<-as.Date(merge_df$Date)
# merge_df$Site<-factor(merge_df$Site, c('NL70', 'EC2','EC3','EC4','EC5','EC6','EC7','EC8','NL76'))


metrics<-c('EXOSpCond', 'SpCond.uS', 'EXOTemp', 'Temp.C', 'EXOpH', 'pH', 'EXOChlugL', 'Chl.ug.L', 'EXOBGAPCugL', 'EXOODO', 'EXODOmgL', 'ODO.mg.L', 'EXOTurbFNU', 'Turbid..NTU', 'HachTurbidity', 'TSS', 'VSS', 'SecchiDepth', "d180_02.vs.VSMOW", "gppv", "rv", 'nepv', 'NEP', 'ER', 'SUNA_NO3_uM',"NO3.ppm", "NH4.ppm", "DIN.ppm", 'TN.ppm', 'TDN.ppm', "DON.ppm", "TPN.ppm", "PO4",   'TP.ppm', "DOC.ppm")



#Plotting parameters
jitterwidth=0.15
# colorset<-'Dark2'
# colors<-brewer.pal(3, colorset)[c(1,3,2)]

#colors
color.palette = colorRampPalette(c(viridis(6, begin=.2, end=.98), rev(magma(5, begin=.35, end=.98))), bias=1)
colors<-color.palette(length(unique(met.final$Site)))
shapes<-rep(21:25, 5)


#Common theme for all metabolism timeseries panels
commonTheme<-list(
  scale_colour_manual(values = colors),
  scale_fill_manual(values = colors),
  scale_shape_manual(values=rep(21:25, 5)),
  # geom_smooth(method='loess',  se=F),
  # geom_smooth(method='auto', se=T, alpha=.2),
  # geom_jitter(size=2, width=jitterwidth, height=0, aes(fill=Site, shape=Site)),
  geom_vline(xintercept=as.Date('2018-10-02'), linetype="solid", color = "black", size=1),
  theme_bw(),
  theme(plot.title = element_text(hjust=0.5), legend.position="none", axis.title.x=element_blank())
)

# Make a table to determine how many panels are needed
# Each panel is a site and a metric


uniquesites<-unique(met.final[c('Site')])

ranges<-sapply(metrics, function(x) extendrange(met.final[,x], f=0.05))

# Loop through metrics and sites and make a gg object
plot_list<-list()
plot_nu<-1
for (plot_nu in 1:length(metrics)){
  
  metric<-metrics[plot_nu]
  
  table<-met.final[,c('Site', 'Date', metric)]
  
  plot_list[[plot_nu]]<-ggplot(table, aes_string('Date', metric, group='Site')) + 
    ggtitle(metric) +
    geom_line(size=1, aes(colour=Site,  group=Site)) +    
    geom_point(size=2, aes(fill=Site, shape=Site)) + 
    commonTheme
  
  plot_print<-plot_list[[plot_nu]] + 
    theme(legend.position="bottom", legend.title= element_blank()) +
    guides(color = guide_legend(nrow = 2, title.position='top', title.hjust=0.5))
  
  png(paste0(dropbox_dir, '/Figures/NutrientExperiment/Timeseries/', metric, '.png'), width=5, height=3, units='in', res=200)
  
  print(plot_print)
  
  dev.off()
}
  
plot_withlegend <- plot_list[[1]] + 
  theme(legend.position="bottom") +
  guides(color = guide_legend(nrow = 3, title.position='top', title.hjust=0.5))

mylegend<-g_legend(plot_withlegend)


plot_list2<-plot_list
plot_list2[[length(plot_list)+1]]<-mylegend
# arrange plots with legend
# p2<-grid.arrange(grobs=plot_list2, nrow=ceiling(length(plot_list2)/2), as.table=F)
p2<-grid.arrange(grobs=plot_list2, ncol=3, as.table=F)


#Add legend to bottom of figure and save
png(paste0(dropbox_dir, '/Figures/NutrientExperiment/EcosystemResponseTimeSeries_Allmetrics.png'), width=12, height=16, units='in', res=200)

grid.arrange(p2)

dev.off()


for (plot_nu in 1:length(plot_list)){
  plot_withlegend <- plot_list[[plot_nu]] + 
    theme(legend.position="bottom") +
    guides(color = guide_legend(nrow = 3, title.position='top', title.hjust=0.5))
  
}

 # #############
#Scatterplots
# ##############
png(paste0(dropbox_dir, '/Figures/NutrientExperiment/Oxygen18vDO.png'), width=4, height=5, units='in', res=200)

ggplot(met.final, aes(x=EXOODO, y=d180_02.vs.VSMOW, group=Site))+
  labs(x='Dissolved oxygen (% sat)', y=expression(paste(delta^'18', "O-", O[2], " (", "\211", ")")))+
  geom_vline(xintercept=100, linetype="dashed", color = "grey", size=1) +
  geom_hline(yintercept=24.2, linetype="dashed", color = "grey", size=1) +
  geom_point(size=2, aes(fill=Site, shape=Site)) + 
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_shape_manual(values=rep(21:25, 5)) +
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5), legend.position="bottom") +
  guides(fill = guide_legend(nrow = 3, title.position='top', title.hjust=0.5))


dev.off()

png(paste0(dropbox_dir, '/Figures/NutrientExperiment/Oxygen18vO2Ar.png'), width=4, height=5, units='in', res=200)

ggplot(met.final, aes(x=O2.Ar, y=d180_02.vs.VSMOW, group=Site))+
  labs(x=expression(paste(O[2], ":Ar (ratio)")), y=expression(paste(delta^'18', "O-", O[2], " (", "\211", ")")))+
  # geom_vline(xintercept=100, linetype="dashed", color = "grey", size=1) +
  geom_hline(yintercept=24.2, linetype="dashed", color = "grey", size=1) +
  geom_point(size=2, aes(fill=Site, shape=Site)) + 
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_shape_manual(values=rep(21:25, 5)) +
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5), legend.position="bottom") +
  guides(fill = guide_legend(nrow = 3, title.position='top', title.hjust=0.5))


dev.off()

png(paste0(dropbox_dir, '/Figures/NutrientExperiment/Oxygen18vTurb.png'), width=4, height=5, units='in', res=200)

ggplot(met.final, aes(x=log10(EXOTurbFNU), y=d180_02.vs.VSMOW, group=Site))+
  labs(x=expression(paste(log[10], ' of Turbidity (FNU)')), y=expression(paste(delta^'18', "O-", O[2], " (", "\211", ")")))+
  # geom_vline(xintercept=100, linetype="dashed", color = "grey", size=1) +
  geom_hline(yintercept=24.2, linetype="dashed", color = "grey", size=1) +
  geom_point(size=2, aes(fill=Site, shape=Site)) + 
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_shape_manual(values=rep(21:25, 5)) +
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5), legend.position="bottom") +
  guides(fill = guide_legend(nrow = 3, title.position='top', title.hjust=0.5))


dev.off()



ggplot(met.final, aes(x=O2.Ar, y=d180_02.vs.air, group=Site))+
  labs(x=expression(paste(O[2], ":Ar (ratio)")), y=expression(paste(delta^'18', "O-", O[2], " (", "\211", ")")))+
  # geom_vline(xintercept=100, linetype="dashed", color = "grey", size=1) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey", size=1) +
  geom_point(size=2, aes(fill=Site, shape=Site)) + 
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_shape_manual(values=rep(21:25, 5)) +
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5), legend.position="bottom") +
  guides(fill = guide_legend(nrow = 3, title.position='top', title.hjust=0.5))


#End



# ###########################
# Plots for Paper
# ###########################



#Nitrate and Turbdity timeseries



#Common theme for all metabolism timeseries panels
commonThemePrint<-list(
  scale_colour_manual(values = colors),
  scale_fill_manual(values = colors),
  scale_shape_manual(values=c(23, 22,22,21,21,21,22,22,23)),
  # geom_smooth(method='loess',  se=F),
  # geom_smooth(method='auto', se=T, alpha=.2),
  # geom_jitter(size=2, width=jitterwidth, height=0, aes(fill=Site, shape=Site)),
  geom_vline(xintercept=(as.Date('2018-10-02')-0.7), linetype="solid", color = "black", size=1),
  geom_vline(xintercept=as.Date('2018-10-07'), linetype='dashed', color='grey', size=1),
  theme_bw(),
  theme(plot.title = element_text(hjust=0.5), legend.position="none", axis.title.x=element_blank())
)

NO3TS<-ggplot(met.final, aes_string('Date', 'NO3.ppm', group='Site')) + 
  commonThemePrint + 
  geom_line(size=1, aes(colour=Site,  group=Site)) +    
  geom_point(size=2, aes(fill=Site, shape=Site)) + 
  labs(y=expression(paste(NO[3], ' (mg N L'^'-1', ')')))

TurbTS<-ggplot(met.final, aes_string('Date', 'EXOTurbFNU', group='Site')) + 
  commonThemePrint + 
  # scale_y_log10() + 
  geom_line(size=1, aes(colour=Site,  group=Site)) +    
  geom_point(size=2, aes(fill=Site, shape=Site)) + 
  labs(y=expression(paste('Turbidity (FNU)')))


TurbTS_withLegened <- TurbTS + 
  theme(legend.position="bottom",  legend.title=element_blank()) +
  guides(color = guide_legend(nrow = 1, title.position='top', title.hjust=0.5))

# NO3legend<-g_legend(NO3TS_withLegened)
# 
# grid.arrange(grobs=list(NO3TS, TurbTS), ncol=1)

png(paste0(dropbox_dir, '/Figures/NutrientExperiment/Timeseries/NO3Turb.png'), width=5, height=5, units='in', res=200)

grid.newpage()

plots<-grid.draw(rbind(ggplotGrob(NO3TS), ggplotGrob(TurbTS_withLegened), size = "first"))

dev.off()



NH4TS<-ggplot(met.final, aes_string('Date', 'NH4.ppm', group='Site')) + 
  commonThemePrint + 
  geom_line(size=1, aes(colour=Site,  group=Site)) +    
  geom_point(size=2, aes(fill=Site, shape=Site)) + 
  labs(y=expression(paste(NH[4], ' (mg N L'^'-1', ')')))

SRPTS<-ggplot(met.final, aes_string('Date', 'PO4', group='Site')) + 
  commonThemePrint + 
  # scale_y_log10() + 
  geom_line(size=1, aes(colour=Site,  group=Site)) +    
  geom_point(size=2, aes(fill=Site, shape=Site)) + 
  labs(y=expression(paste('SRP (mg P L'^'-1', ')')))


SRPTS_withLegened <- SRPTS + 
  theme(legend.position="bottom",  legend.title=element_blank()) +
  guides(color = guide_legend(nrow = 1, title.position='top', title.hjust=0.5))

png(paste0(dropbox_dir, '/Figures/NutrientExperiment/Timeseries/NH4SRP.png'), width=5, height=5, units='in', res=200)

grid.newpage()

plots<-grid.draw(rbind(ggplotGrob(NH4TS), ggplotGrob(SRPTS_withLegened), size = "first"))

dev.off()
