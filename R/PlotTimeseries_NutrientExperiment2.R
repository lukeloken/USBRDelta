

library(lubridate)
library(viridis)
library(grid)
library(gridExtra)
library(ggplot2)
library(dplyr)

#New data input (after O18 metabolism)
merge_final<-readRDS(file=file.path(onedrive_dir, 'Rdata', 'NutrientExperiment2', 'SiteData_withGas_Merged.rds'))


# flame_metrics<-c('FLAMe_SUNANO3mgL', 'FLAMe_EXOSpCond', 'FLAMe_EXOpH', 'FLAMe_EXOTemp', 'FLAMe_EXODOmgL', 'FLAMe_EXODOSAT', 'FLAMe_EXOTurbFNU', 'FLAMe_EXOfDOMRFU', 'FLAMe_EXOfCHLAugL', 'FLAMe_EXOfBGAPCugL')
flame_metrics<-c('FLAMe_SUNANO3mgL', 'FLAMe_EXOfDOMRFU')
ysi_metrics<-c("YSI_Temp_C", "YSI_DO_perSat", "YSI_DO_mgL", "YSI_SPC_uScm", "YSI_pH", "YSI_Turb_FNU", "YSI_BGA_ugL", "YSI_ChlA_ugL"  )
chem_metrics<-c("PO4_ppm", "TDP_ppm", "TP_ppm", "NH4_ppm", "NO3_ppm", "TDN_ppm", "TN_ppm", "DOC_ppm", "DIN_ppm", "DON_ppm", "TPN_ppm", "TSS", "VSS", "chla_mean", "pheo_mean")
gas_metrics<-c("CO2uM", "CH4uM", "N2OuM")

other_metrics<-c("SecchiDepth_m")

# metricsMetabolism<-c("gppv",  'nepv', "rv", 'GPP', 'NEP', 'ER', 'GPP_buoy', 'NEP_buoy', 'ER_buoy', 'GPP_inc', 'NEP_inc', 'ER_inc')
metrics <- c(flame_metrics, ysi_metrics, chem_metrics, gas_metrics, other_metrics)


merge_surface <- merge_final %>%
  dplyr::filter(DepthCode == 'S')

names(merge_surface) <-gsub("-", "_", names(merge_surface))
  

#Plotting parameters
jitterwidth=0.15
# colorset<-'Dark2'
# colors<-brewer.pal(3, colorset)[c(1,3,2)]

#colors
color.palette = colorRampPalette(c(viridis(6, begin=.2, end=.98), rev(magma(5, begin=.35, end=.98))), bias=1)
colors<-color.palette(length(levels(merge_final$Site)))
shapes<-rep(21:25, 5)


#Common theme for all metabolism timeseries panels
commonTheme<-list(
  scale_colour_manual(values = colors),
  scale_fill_manual(values = colors),
  scale_shape_manual(values=rep(21:25, 5)),
  # geom_smooth(method='loess',  se=F),
  # geom_smooth(method='auto', se=T, alpha=.2),
  # geom_jitter(size=2, width=jitterwidth, height=0, aes(fill=Site, shape=Site)),
  geom_vline(xintercept=fert_dates, linetype="dashed", color = "green", size=0.5),
  theme_bw(),
  theme(plot.title = element_text(hjust=0.5), legend.position="none", axis.title.x=element_blank())
)

# Make a table to determine how many panels are needed
# Each panel is a site and a metric

uniquesites<-unique(merge_final[c('Site')])


# Loop through metrics and sites and make a gg object
plot_list<-list()
ranges<-sapply(metrics[1:28], function(x) extendrange(merge_surface[,x], f=0.05))

plot_nu<-1
for (plot_nu in 1:length(metrics)){
  
  metric<-metrics[plot_nu]
  
  table<-merge_surface[,c('Site', 'Date', metric)]
  
  plot_list[[plot_nu]]<-ggplot(table, aes_string('Date', y=metric, group='Site')) + 
    commonTheme + 
    ggtitle(metric) +
    geom_line(size=1, aes(colour=Site,  group=Site)) +    
    geom_point(size=2, aes(fill=Site, shape=Site))
  
  plot_print<-plot_list[[plot_nu]] + 
    theme(legend.position="bottom", legend.title= element_blank()) +
    guides(color = guide_legend(nrow = 1, title.position='top', title.hjust=0.5))
  
  png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'Timeseries', paste0(metric, '.png')), width=5, height=3, units='in', res=200)
  
  print(plot_print)
  
  dev.off()
}
  
plot_withlegend <- plot_list[[1]] + 
  theme(legend.position="bottom") +
  guides(color = guide_legend(nrow = 1, title.position='top', title.hjust=0.5))

mylegend<-g_legend(plot_withlegend)


plot_list2<-plot_list
# plot_list2[[length(plot_list)+1]]<-mylegend
# arrange plots with legend
# p2<-grid.arrange(grobs=plot_list2, nrow=ceiling(length(plot_list2)/2), as.table=F)
p2<-grid.arrange(grobs=plot_list2, ncol=4, as.table=F)


#Add legend to bottom of figure and save
png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'EcosystemResponseTimeSeries_AllMeasurements.png'), width=20, height=16, units='in', res=200)

grid.arrange(p2, mylegend, nrow=2, heights=c(15,1))

dev.off()


#End for now

# #Metabolism only timeseries
# 
# # Loop through metrics and sites and make a gg object
# plot_list<-list()
# ranges<-sapply(metricsMetabolism, function(x) extendrange(met.join[,x], f=0.05))
# 
# plot_nu<-1
# for (plot_nu in 1:length(metricsMetabolism)){
#   
#   metric<-metricsMetabolism[plot_nu]
#   
#   table<-met.join[,c('Site', 'Date', metric)]
#   
#   plot_list[[plot_nu]]<-ggplot(table, aes_string('Date', metric, group='Site')) + 
#     ggtitle(metric) +
#     geom_line(size=1, aes(colour=Site,  group=Site)) +    
#     geom_point(size=2, aes(fill=Site, shape=Site)) + 
#     commonTheme
#   
#   plot_print<-plot_list[[plot_nu]] + 
#     theme(legend.position="bottom", legend.title= element_blank()) +
#     guides(color = guide_legend(nrow = 2, title.position='top', title.hjust=0.5))
#   
#   png(paste0(dropbox_dir, '/Figures/NutrientExperiment/Timeseries/', metric, '.png'), width=5, height=3, units='in', res=200)
#   
#   print(plot_print)
#   
#   dev.off()
# }
# 
# plot_withlegend <- plot_list[[1]] + 
#   theme(legend.position="bottom") +
#   guides(color = guide_legend(nrow = 3, title.position='top', title.hjust=0.5))
# 
# mylegend<-g_legend(plot_withlegend)
# 
# 
# plot_list2<-plot_list
# # plot_list2[[length(plot_list)+1]]<-mylegend
# # arrange plots with legend
# # p2<-grid.arrange(grobs=plot_list2, nrow=ceiling(length(plot_list2)/2), as.table=F)
# p2<-grid.arrange(grobs=plot_list2, nrow=3, as.table=F)
# 
# 
# #Add legend to bottom of figure and save
# png(paste0(dropbox_dir, '/Figures/NutrientExperiment/EcosystemResponseTimeSeries_Metabolismmetrics.png'), width=12, height=8, units='in', res=200)
# 
# grid.arrange(p2, mylegend, nrow=2, heights=c(8,1))
# 
# dev.off()
# 
# 
# 
# 
# 
# # for (plot_nu in 1:length(plot_list)){
# #   plot_withlegend <- plot_list[[plot_nu]] + 
# #     theme(legend.position="bottom") +
# #     guides(color = guide_legend(nrow = 3, title.position='top', title.hjust=0.5))
# #   
# # }
# 
#  # #############
# #Scatterplots
# # ##############
# png(paste0(dropbox_dir, '/Figures/NutrientExperiment/Oxygen18vDO.png'), width=4, height=5, units='in', res=200)
# 
# print(
# ggplot(met.join, aes(x=EXOODO, y=d180_02.vs.VSMOW, group=Site))+
#   labs(x='Dissolved oxygen (% sat)', y=expression(paste(delta^'18', "O-", O[2], " (", "\211", ")")))+
#   geom_vline(xintercept=100, linetype="dashed", color = "grey", size=1) +
#   geom_hline(yintercept=24.2, linetype="dashed", color = "grey", size=1) +
#   geom_point(size=2, aes(fill=Site, shape=Site)) + 
#   scale_colour_manual(values = colors) +
#   scale_fill_manual(values = colors) +
#   scale_shape_manual(values=rep(21:25, 5)) +
#   theme_bw() + 
#   theme(plot.title = element_text(hjust=0.5), legend.position="bottom") +
#   guides(fill = guide_legend(nrow = 3, title.position='top', title.hjust=0.5))
# )
# 
# dev.off()
# 
# png(paste0(dropbox_dir, '/Figures/NutrientExperiment/Oxygen18vO2Ar.png'), width=4, height=5, units='in', res=200)
# 
# print(
# ggplot(met.join, aes(x=O2.Ar, y=d180_02.vs.VSMOW, group=Site))+
#   labs(x=expression(paste(O[2], ":Ar (ratio)")), y=expression(paste(delta^'18', "O-", O[2], " (", "\211", ")")))+
#   # geom_vline(xintercept=100, linetype="dashed", color = "grey", size=1) +
#   geom_hline(yintercept=24.2, linetype="dashed", color = "grey", size=1) +
#   geom_point(size=2, aes(fill=Site, shape=Site)) + 
#   scale_colour_manual(values = colors) +
#   scale_fill_manual(values = colors) +
#   scale_shape_manual(values=rep(21:25, 5)) +
#   theme_bw() + 
#   theme(plot.title = element_text(hjust=0.5), legend.position="bottom") +
#   guides(fill = guide_legend(nrow = 3, title.position='top', title.hjust=0.5))
# )
# 
# dev.off()
# 
# png(paste0(dropbox_dir, '/Figures/NutrientExperiment/Oxygen18vTurb.png'), width=4, height=5, units='in', res=200)
# 
# print(
# ggplot(met.join, aes(x=log10(EXOTurbFNU), y=d180_02.vs.VSMOW, group=Site))+
#   labs(x=expression(paste(log[10], ' of Turbidity (FNU)')), y=expression(paste(delta^'18', "O-", O[2], " (", "\211", ")")))+
#   # geom_vline(xintercept=100, linetype="dashed", color = "grey", size=1) +
#   geom_hline(yintercept=24.2, linetype="dashed", color = "grey", size=1) +
#   geom_point(size=2, aes(fill=Site, shape=Site)) + 
#   scale_colour_manual(values = colors) +
#   scale_fill_manual(values = colors) +
#   scale_shape_manual(values=rep(21:25, 5)) +
#   theme_bw() + 
#   theme(plot.title = element_text(hjust=0.5), legend.position="bottom") +
#   guides(fill = guide_legend(nrow = 3, title.position='top', title.hjust=0.5))
# )
# 
# dev.off()
# 
# 
# 
# 
# 
# # ###########################
# # Plots for Paper
# # ###########################
# 
# 
# 
# #Nitrate and Turbdity timeseries
# # "2018-10-07 14:00:00 PDT" shipdate based on water level at mooring
# # shipdate<-as.POSIXct(c("2018-10-07 14:00:00", "2018-10-06 11:20:00"), format='%Y-%m-%d %H:%M:%S', tz='America/Los_Angeles')
# 
# #colors
# color.palette = colorRampPalette(c(viridis(6, begin=.2, end=.98), rev(magma(5, begin=.35, end=.98))), bias=1)
# colors<-color.palette(length(unique(met.join$Site)))
# 
# 
# #Common theme for all metabolism timeseries panels
# commonThemePrint<-list(
#   scale_colour_manual(values = colors),
#   scale_fill_manual(values = colors),
#   scale_shape_manual(values=c(23, 22,22,21,21,21,22,22,23)),
#   # geom_smooth(method='loess',  se=F),
#   # geom_smooth(method='auto', se=T, alpha=.2),
#   # geom_jitter(size=2, width=jitterwidth, height=0, aes(fill=Site, shape=Site)),
#   geom_vline(xintercept=(as.Date('2018-10-02')-0.7), linetype="solid", color = "black", size=1),
#   geom_vline(xintercept=c(as.Date('2018-10-07')+.58, as.Date('2018-10-06')+.47), linetype='dashed', color='grey', size=1),
#   theme_bw(),
#   theme(plot.title = element_text(hjust=0.5), legend.position="none", axis.title.x=element_blank())
# )
# 
# NO3TS<-ggplot(met.join, aes_string('Date', 'NO3.ppm', group='Site')) + 
#   commonThemePrint + 
#   geom_line(size=1, aes(colour=Site,  group=Site)) +    
#   geom_point(size=2, aes(fill=Site, shape=Site)) + 
#   labs(y=expression(paste(NO[3], ' (mg N L'^'-1', ')')))
# 
# TurbTS<-ggplot(met.join, aes_string('Date', 'EXOTurbFNU', group='Site')) + 
#   commonThemePrint + 
#   # scale_y_log10() + 
#   geom_line(size=1, aes(colour=Site,  group=Site)) +    
#   geom_point(size=2, aes(fill=Site, shape=Site)) + 
#   labs(y=expression(paste('Turbidity (FNU)')))
# 
# TurbTS_withLegened <- TurbTS + 
#   theme(legend.position="bottom",  legend.title=element_blank()) +
#   guides(color = guide_legend(nrow = 1, title.position='top', title.hjust=0.5))
# 
# 
# png(paste0(dropbox_dir, '/Figures/NutrientExperiment/Timeseries/NO3Turb.png'), width=5, height=5, units='in', res=200)
# 
# grid.newpage()
# plots<-grid.draw(rbind(ggplotGrob(NO3TS), ggplotGrob(TurbTS_withLegened), size = "first"))
# 
# dev.off()
# 
# 
# # ammonium and srp
# NH4TS<-ggplot(met.join, aes_string('Date', 'NH4.ppm', group='Site')) + 
#   commonThemePrint + 
#   geom_line(size=1, aes(colour=Site,  group=Site)) +    
#   geom_point(size=2, aes(fill=Site, shape=Site)) + 
#   labs(y=expression(paste(NH[4], ' (mg N L'^'-1', ')')))
# 
# SRPTS<-ggplot(met.join, aes_string('Date', 'PO4', group='Site')) + 
#   commonThemePrint + 
#   # scale_y_log10() + 
#   geom_line(size=1, aes(colour=Site,  group=Site)) +    
#   geom_point(size=2, aes(fill=Site, shape=Site)) + 
#   labs(y=expression(paste('SRP (mg P L'^'-1', ')')))
# 
# 
# SRPTS_withLegened <- SRPTS + 
#   theme(legend.position="bottom",  legend.title=element_blank()) +
#   guides(color = guide_legend(nrow = 1, title.position='top', title.hjust=0.5))
# 
# png(paste0(dropbox_dir, '/Figures/NutrientExperiment/Timeseries/NH4SRP.png'), width=5, height=5, units='in', res=200)
# 
# grid.newpage()
# plots<-grid.draw(rbind(ggplotGrob(NH4TS), ggplotGrob(SRPTS_withLegened), size = "first"))
# 
# dev.off()
# 
# 
# 
# 
# # incubation metabolism
# NEPTS<-ggplot(met.join, aes(Date, NEP_inc, group=Site)) + 
#   commonThemePrint + 
#   geom_line(size=1, aes(colour=Site,  group=Site)) +    
#   geom_point(size=2, aes(fill=Site, shape=Site)) + 
#   labs(y=expression(paste('NEP (mg ', O[2], ' L'^'-1', ' d'^'-1', ')'))) +
#   theme(plot.margin = unit(c(0.5, .5, 0.5, 1.5), "lines")) + 
#   ggtitle('Incubation Metabolism')
# 
# 
# GPPTS<-ggplot(met.join, aes(Date, GPP_inc, group=Site)) + 
#   commonThemePrint + 
#   # scale_y_log10() + 
#   geom_line(size=1, aes(colour=Site,  group=Site)) +    
#   geom_point(size=2, aes(fill=Site, shape=Site)) + 
#   labs(y=expression(paste('GPP (mg ', O[2], ' L'^'-1', ' d'^'-1', ')')))
# 
# ERTS<-ggplot(met.join, aes(Date, ER_inc, group=Site)) + 
#   commonThemePrint + 
#   # scale_y_log10() + 
#   geom_line(size=1, aes(colour=Site,  group=Site)) +    
#   geom_point(size=2, aes(fill=Site, shape=Site)) + 
#   labs(y=expression(paste('ER (mg ', O[2], ' L'^'-1', ' d'^'-1', ')')))
# 
# ERTS_withLegened <- ERTS + 
#   theme(legend.position="bottom",  legend.title=element_blank()) +
#   guides(color = guide_legend(nrow = 1, title.position='top', title.hjust=0.5))
# 
# png(paste0(dropbox_dir, '/Figures/NutrientExperiment/Metabolism/Inc_GPPERNEP_print.png'), width=5, height=7, units='in', res=200)
# 
# grid.newpage()
# plots1<-grid.draw(rbind(ggplotGrob(NEPTS), ggplotGrob(GPPTS), ggplotGrob(ERTS_withLegened), size = "last"))
# 
# dev.off()
# 
# 
# 
# 
# 
# 
# # oxygen 18 metabolism
# NEPTS_O18<-ggplot(met.join, aes(Date, nepv, group=Site)) + 
#   commonThemePrint + 
#   geom_line(size=1, aes(colour=Site,  group=Site)) +    
#   geom_point(size=2, aes(fill=Site, shape=Site)) + 
#   labs(y=expression(paste('NEP (mg ', O[2], ' L'^'-1', ' d'^'-1', ')'))) +
#   theme(plot.margin = unit(c(0.5, .5, 0.5, 1.5), "lines")) + 
#   ggtitle(expression(paste(delta^'18', "O-", O[2], " Metabolism")))
# 
# GPPTS_O18<-ggplot(met.join, aes(Date, gppv, group=Site)) + 
#   commonThemePrint + 
#   # scale_y_log10() + 
#   geom_line(size=1, aes(colour=Site,  group=Site)) +    
#   geom_point(size=2, aes(fill=Site, shape=Site)) + 
#   labs(y=expression(paste('GPP (mg ', O[2], ' L'^'-1', ' d'^'-1', ')')))
# 
# ERTS_O18<-ggplot(met.join, aes(Date, rv, group=Site)) + 
#   commonThemePrint + 
#   # scale_y_log10() + 
#   geom_line(size=1, aes(colour=Site,  group=Site)) +    
#   geom_point(size=2, aes(fill=Site, shape=Site)) + 
#   labs(y=expression(paste('ER (mg ', O[2], ' L'^'-1', ' d'^'-1', ')')))
# 
# ERTS_O18_withLegened <- ERTS_O18 + 
#   theme(legend.position="bottom",  legend.title=element_blank()) +
#   guides(color = guide_legend(nrow = 1, title.position='top', title.hjust=0.5))
# 
# png(paste0(dropbox_dir, '/Figures/NutrientExperiment/Metabolism/O18_GPPERNEP_print.png'), width=5, height=7, units='in', res=200)
# 
# grid.newpage()
# plots2<-grid.draw(rbind(ggplotGrob(NEPTS_O18), ggplotGrob(GPPTS_O18), ggplotGrob(ERTS_O18_withLegened), size = "last"))
# 
# dev.off()
# 
# 
# 
# 
# 
# 
# # buoy metabolism
# NEPTS_buoy<-ggplot(met.join, aes(Date, NEP_buoy, group=Site)) + 
#   commonThemePrint + 
#   geom_line(size=1, aes(colour=Site,  group=Site)) +    
#   geom_point(size=2, aes(fill=Site, shape=Site)) + 
#   labs(y=expression(paste('NEP (mg ', O[2], ' L'^'-1', ' d'^'-1', ')'))) +
#   theme(plot.margin = unit(c(0.5, .5, 0.5, 1.5), "lines")) +
#   ggtitle("Buoy Metabolism")
# 
# GPPTS_buoy<-ggplot(met.join, aes(Date, GPP_buoy, group=Site)) + 
#   commonThemePrint + 
#   # scale_y_log10() + 
#   geom_line(size=1, aes(colour=Site,  group=Site)) +    
#   geom_point(size=2, aes(fill=Site, shape=Site)) + 
#   labs(y=expression(paste('GPP (mg ', O[2], ' L'^'-1', ' d'^'-1', ')')))
# 
# ERTS_buoy<-ggplot(met.join, aes(Date, ER_buoy, group=Site)) + 
#   commonThemePrint + 
#   # scale_y_log10() + 
#   geom_line(size=1, aes(colour=Site,  group=Site)) +    
#   geom_point(size=2, aes(fill=Site, shape=Site)) + 
#   labs(y=expression(paste('ER (mg ', O[2], ' L'^'-1', ' d'^'-1', ')')))
# 
# ERTS_buoy_withLegened <- ERTS_buoy + 
#   theme(legend.position="bottom",  legend.title=element_blank()) +
#   guides(color = guide_legend(nrow = 1, title.position='top', title.hjust=0.5))
# 
# png(paste0(dropbox_dir, '/Figures/NutrientExperiment/Metabolism/Buoy_GPPERNEP_print.png'), width=5, height=7, units='in', res=200)
# 
# grid.newpage()
# plots3<-grid.draw(rbind(ggplotGrob(NEPTS_buoy), ggplotGrob(GPPTS_buoy), ggplotGrob(ERTS_buoy_withLegened), size = "last"))
# 
# dev.off()
# 
# 
# 
# mylegend<-g_legend(ERTS_buoy_withLegened)
# 
# 
# png(paste0(dropbox_dir, '/Figures/NutrientExperiment/Metabolism/ThreeMethods_GPPERNEP_print.png'), width=10, height=7, units='in', res=200)
# 
# panel9<-grid.arrange((NEPTS_buoy+theme(plot.title=element_blank())), (GPPTS_buoy), (ERTS_buoy),
#              (NEPTS_O18+theme(plot.title=element_blank())), (GPPTS_O18), (ERTS_O18),
#              (NEPTS+theme(plot.title=element_blank())), (GPPTS), (ERTS), nrow=3)
# 
# grid.arrange(panel9, mylegend, nrow=2, heights=c(15,1))
# 
# dev.off()
# 
# 
# png(paste0(dropbox_dir, '/Figures/NutrientExperiment/Metabolism/ThreeMethods_GPPERNEP_print_ggarrange.png'), width=12, height=7, units='in', res=200)
# 
# 
# panel9_v2<-ggarrange((NEPTS_buoy+theme(plot.title=element_blank())), (GPPTS_buoy), (ERTS_buoy),
#           (NEPTS_O18+theme(plot.title=element_blank())), (GPPTS_O18), (ERTS_O18),
#           (NEPTS+theme(plot.title=element_blank())), (GPPTS), (ERTS),
#           heights = c(1,1,1),
#           ncol = 3, nrow = 3, align = "hv")
# 
# 
# grid.arrange(panel9_v2, mylegend, nrow=2, heights=c(15,1))
# 
# 
# dev.off()
# 
# 
# 
# grid.newpage()
# plots<-grid.draw(rbind(ggplotGrob(NEPTS_buoy), ggplotGrob(GPPTS_buoy), ggplotGrob(ERTS_buoy),
#                        ggplotGrob(NEPTS_O18), ggplotGrob(GPPTS_O18), ggplotGrob(ERTS_O18),
#                        ggplotGrob(NEPTS), ggplotGrob(GPPTS), ggplotGrob(ERTS),
#                        size = "last"))
# 
