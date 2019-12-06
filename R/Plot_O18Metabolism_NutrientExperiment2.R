
library(gridExtra)
library(grid)

source('R/g_legend.R')


#Plot O18 metabolism data
met.final<-readRDS(file=paste0(dropbox_dir, '/Data/Rdata_SSCN2/merge_df_O18.rds'))

met.final<-drop_na(met.final, d180_02.vs.VSMOW)


#colors
color.palette = colorRampPalette(c(viridis(6, begin=.2, end=.98), rev(magma(5, begin=.35, end=.98))), bias=1)
colors<-color.palette(length(unique(met.final$Site)))

# shipdate<-as.Date(c("2018-10-07", "2018-10-06"), format='%Y-%m-%d')
# fertdate<-as.Date("2018-10-02", format='%Y-%m-%d')


#Common theme for all metabolism timeseries panels
commonThemePrint<-list(
  scale_colour_manual(values = colors),
  scale_fill_manual(values = colors),
  scale_shape_manual(values=c(23, 22, 21,21,21, 22, 23)),
  # geom_smooth(method='loess',  se=F),
  # geom_smooth(method='auto', se=T, alpha=.2),
  # geom_jitter(size=2, width=jitterwidth, height=0, aes(fill=Site, shape=Site)),
  geom_vline(xintercept=(fert_dates), linetype="dashed", color = "green", size=1),
  geom_hline(yintercept=0, color='lightgrey', linetype=1.5, size=1), 
  theme_bw(),
  theme(plot.title = element_text(hjust=0.5), legend.position="none", axis.title.x=element_blank())
)

NEP_O18<-ggplot(met.final, aes(x=Date, y=nepv, fill=Site)) + 
  commonThemePrint + 
  geom_line(size=1, aes(colour=Site,  group=Site)) +    
  geom_point(size=2, aes(fill=Site, shape=Site)) + 
  labs(x='Date', y=expression(paste('NEP (mg ', O[2], ' L'^'-1', ' d'^'-1', ')'))) + 
  ggtitle(expression(paste(delta^'18', "O-", O[2], ' Metabolism')))

GPP_O18<-ggplot(met.final, aes(x=Date, y=gppv, fill=Site)) + 
  commonThemePrint + 
  geom_line(size=1, aes(colour=Site,  group=Site)) +    
  geom_point(size=2, aes(fill=Site, shape=Site)) + 
  labs(x='Date', y=expression(paste('GPP (mg ', O[2], ' L'^'-1', ' d'^'-1', ')')))

ER_O18<-ggplot(met.final, aes(x=Date, y=(rv), fill=Site)) + 
  commonThemePrint + 
  geom_line(size=1, aes(colour=Site,  group=Site)) +    
  geom_point(size=2, aes(fill=Site, shape=Site)) + 
  labs(x='Date', y=expression(paste('ER (mg ', O[2], ' L'^'-1', ' d'^'-1', ')')))


ER_O18_withLegened <- ER_O18 + 
  theme(legend.position="bottom",  legend.title=element_blank()) +
  guides(color = guide_legend(nrow = 1, title.position='top', title.hjust=0.5))


png(paste0(dropbox_dir, '/Figures/NutrientExperiment2/O18Metabolism/NEPGPPER_TS.png'), width=5, height=7, units='in', res=200)

grid.newpage()
plots<-grid.draw(rbind(ggplotGrob(NEP_O18), ggplotGrob(GPP_O18), ggplotGrob(ER_O18_withLegened), size = "first"))

dev.off()

#area
NEP_O18_area<-ggplot(met.final, aes(x=Date, y=nepa, fill=Site)) + 
  commonThemePrint + 
  geom_line(size=1, aes(colour=Site,  group=Site)) +    
  geom_point(size=2, aes(fill=Site, shape=Site)) + 
  labs(x='Date', y=expression(paste('NEP (g ', O[2], ' m'^'-2', ' d'^'-1', ')'))) + 
  ggtitle(expression(paste(delta^'18', "O-", O[2], ' Metabolism')))

GPP_O18_area<-ggplot(met.final, aes(x=Date, y=gppa, fill=Site)) + 
  commonThemePrint + 
  geom_line(size=1, aes(colour=Site,  group=Site)) +    
  geom_point(size=2, aes(fill=Site, shape=Site)) + 
  labs(x='Date', y=expression(paste('GPP (g ', O[2], ' m'^'-2', ' d'^'-1', ')')))

ER_O18_area<-ggplot(met.final, aes(x=Date, y=(ra), fill=Site)) + 
  commonThemePrint + 
  geom_line(size=1, aes(colour=Site,  group=Site)) +    
  geom_point(size=2, aes(fill=Site, shape=Site)) + 
  labs(x='Date', y=expression(paste('ER (g ', O[2], ' m'^'-2', ' d'^'-1', ')')))


ER_O18area_withLegened <- ER_O18_area + 
  theme(legend.position="bottom",  legend.title=element_blank()) +
  guides(color = guide_legend(nrow = 1, title.position='top', title.hjust=0.5))


png(paste0(dropbox_dir, '/Figures/NutrientExperiment2/O18Metabolism/NEPGPPER_area_TS.png'), width=5, height=7, units='in', res=200)

grid.newpage()
plots<-grid.draw(rbind(ggplotGrob(NEP_O18_area), ggplotGrob(GPP_O18_area), ggplotGrob(ER_O18area_withLegened), size = "first"))

dev.off()



plot_list<-list()
plot_list[[1]]<-ggplot(met.final, aes(x=Date, y=gppv, fill=Site)) + 
  labs(x='Date', y=expression(paste('GPP (mg ', O[2], ' L'^'-1', ' d'^'-1', ')'))) + 
  scale_shape_manual(values=rep(21:25, 5))  + 
  scale_fill_manual(values = color.palette(length(unique(met.final$Site)))) + 
  scale_colour_manual(values = color.palette(length(unique(met.final$Site)))) +
  geom_vline(xintercept=fert_dates, color='#2ca25f', linetype=2, size=1) +
  geom_line(size=.5, aes(colour=Site,  group=Site)) +    
  geom_point(size=2, aes(fill=Site, shape=Site)) + 
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))  +
  theme(legend.position='none') + 
  theme(axis.title.x=element_blank())

plot_list[[2]]<-ggplot(met.final, aes(x=Date, y=rv, fill=Site)) + 
  labs(x='Date', y=expression(paste('ER (mg ', O[2], ' L'^'-1', ' d'^'-1', ')'))) + 
  scale_shape_manual(values=rep(21:25, 5))  + 
  scale_fill_manual(values = color.palette(length(unique(met.final$Site)))) + 
  scale_colour_manual(values = color.palette(length(unique(met.final$Site)))) +
  geom_vline(xintercept=fert_dates, color='#2ca25f', linetype=2, size=1) +
  geom_line(size=.5, aes(colour=Site,  group=Site)) +    
  geom_point(size=2, aes(fill=Site, shape=Site)) + 
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))  +
  theme(legend.position='none') + 
  theme(axis.title.x=element_blank())

plot_list[[3]]<-ggplot(met.final, aes(x=Date, y=nepv, fill=Site)) + 
  labs(x='Date', y=expression(paste('NEP (mg ', O[2], ' L'^'-1', ' d'^'-1', ')'))) + 
  scale_shape_manual(values=rep(21:25, 5))  + 
  scale_fill_manual(values = color.palette(length(unique(met.final$Site)))) + 
  scale_colour_manual(values = color.palette(length(unique(met.final$Site)))) +
  geom_vline(xintercept=fert_dates, color='#2ca25f', linetype=2, size=1) +
  geom_line(size=.5, aes(colour=Site,  group=Site)) +    
  geom_point(size=2, aes(fill=Site, shape=Site)) + 
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))  +
  theme(legend.position='none') + 
  theme(axis.title.x=element_blank())


plot_withlegend <- plot_list[[1]] + 
  theme(legend.position="bottom") +
  guides(shape = guide_legend(nrow = 1, title.position='top', title.hjust=0.5))

mylegend<-g_legend(plot_withlegend)


# arrange plots without legend
p2<-grid.arrange(grobs=plot_list, ncol=1, as.table=F)

# arrange multi plot with legend below and save to project folder
png(paste0(dropbox_dir, '/Figures/NutrientExperiment2/O18Metabolism/GPP_ER_NEP_O18.png'), width=5, height=7, units='in', res=200)

grid.arrange(p2, mylegend, nrow=2,heights=c(10, 1))

dev.off()




png(paste0(dropbox_dir, '/Figures/NutrientExperiment2/O18Metabolism/GPP_ER_NEP_IncubationVersusO18.png'), width=12, height=4, units='in', res=200)

GPPplot<-ggplot(met.final, aes(x=GPP_Total, y=gppv, fill=Site)) + 
  labs(x=expression(paste('Incubation GPP (mg ', O[2], ' L'^'-1', ' d'^'-1', ')')), y=expression(paste(delta^'18', "O-", O[2], ' GPP (mg ', O[2], ' L'^'-1', ' d'^'-1', ')'))) + 
  scale_shape_manual(values=rep(21:25, 5))  + 
  scale_fill_manual(values = color.palette(length(unique(met.final$Site)))) + 
  scale_colour_manual(values = color.palette(length(unique(met.final$Site)))) +
  geom_point(size=2, aes(fill=Site, shape=Site)) + 
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))  +
  theme(legend.position='bottom')

ERplot<-ggplot(met.final, aes(x=ER_Total, y=rv, fill=Site)) + 
  labs(x=expression(paste('Incubation ER (mg ', O[2], ' L'^'-1', ' d'^'-1', ')')), y=expression(paste(delta^'18', "O-", O[2], ' ER (mg ', O[2], ' L'^'-1', ' d'^'-1', ')'))) + 
  scale_shape_manual(values=rep(21:25, 5))  + 
  scale_fill_manual(values = color.palette(length(unique(met.final$Site)))) + 
  scale_colour_manual(values = color.palette(length(unique(met.final$Site)))) +
  geom_point(size=2, aes(fill=Site, shape=Site)) + 
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))  +
  theme(legend.position='bottom')

NEPplot<-ggplot(met.final, aes(x=NEP_Total, y=nepv, fill=Site)) + 
  labs(x=expression(paste('Incubation NEP (mg ', O[2], ' L'^'-1', ' d'^'-1', ')')), y=expression(paste(delta^'18', "O-", O[2], ' NEP (mg ', O[2], ' L'^'-1', ' d'^'-1', ')'))) + 
  scale_shape_manual(values=rep(21:25, 5))  + 
  scale_fill_manual(values = color.palette(length(unique(met.final$Site)))) + 
  scale_colour_manual(values = color.palette(length(unique(met.final$Site)))) +
  geom_point(size=2, aes(fill=Site, shape=Site)) + 
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))  +
  theme(legend.position='bottom')

grid.arrange(GPPplot, ERplot, NEPplot, nrow=1)

dev.off()


png(paste0(dropbox_dir, '/Figures/NutrientExperiment2/O18Metabolism/GPP_VersusTurb.png'), width=4, height=4, units='in', res=200)

print(
ggplot(met.final, aes(x=YSI_Turb_FNU, y=gppv, fill=Site)) +
  labs(x=expression(paste('Turbidity (FNU)')), y=expression(paste(delta^'18', "O-", O[2], ' GPP (mg ', O[2], ' L'^'-1', ' d'^'-1', ')'))) +
  # scale_x_log10() +
  scale_shape_manual(values=rep(21:25, 5))  +
  scale_fill_manual(values = color.palette(length(unique(met.final$Site)))) +
  scale_colour_manual(values = color.palette(length(unique(met.final$Site)))) +
  geom_point(size=2, aes(fill=Site, shape=Site)) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))  +
  theme(legend.position='bottom')
)

dev.off()

# ggplot(met.final, aes(x=EXOTurbFNU, y=rv, fill=Site)) + 
#   labs(x=expression(paste('Turbidity (FNU)')), y=expression(paste(delta^'18', "O-", O[2], ' ER (mg ', O[2], ' L'^'-1', ' d'^'-1', ')'))) + 
#   # scale_x_log10() + 
#   scale_shape_manual(values=rep(21:25, 5))  + 
#   scale_fill_manual(values = color.palette(length(unique(met.final$Site)))) + 
#   scale_colour_manual(values = color.palette(length(unique(met.final$Site)))) +
#   geom_point(size=2, aes(fill=Site, shape=Site)) + 
#   theme_bw() +
#   theme(plot.title = element_text(hjust=0.5))  +
#   theme(legend.position='bottom')
# 
# ggplot(met.final, aes(x=EXOTurbFNU, y=nepv, fill=Site)) + 
#   labs(x=expression(paste('Turbidity (FNU)')), y=expression(paste(delta^'18', "O-", O[2], ' NEP (mg ', O[2], ' L'^'-1', ' d'^'-1', ')'))) + 
#   # scale_x_log10() + 
#   scale_shape_manual(values=rep(21:25, 5))  + 
#   scale_fill_manual(values = color.palette(length(unique(met.final$Site)))) + 
#   scale_colour_manual(values = color.palette(length(unique(met.final$Site)))) +
#   geom_point(size=2, aes(fill=Site, shape=Site)) + 
#   theme_bw() +
#   theme(plot.title = element_text(hjust=0.5))  +
#   theme(legend.position='bottom')



png(paste0(dropbox_dir, '/Figures/NutrientExperiment2/O18Metabolism/GPP_VersusChlA.png'), width=4, height=4, units='in', res=200)

print(
ggplot(met.final, aes(x=YSI_ChlA_ugL, y=gppv, fill=Site)) +
  labs(x=expression(paste('Chl a (', mu, 'g L'^'-1', ')')), y=expression(paste(delta^'18', "O-", O[2], ' GPP (mg ', O[2], ' L'^'-1', ' d'^'-1', ')'))) +
  # scale_x_log10() +
  scale_shape_manual(values=rep(21:25, 5))  +
  scale_fill_manual(values = color.palette(length(unique(met.final$Site)))) +
  scale_colour_manual(values = color.palette(length(unique(met.final$Site)))) +
  geom_point(size=2, aes(fill=Site, shape=Site)) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))  +
  theme(legend.position='bottom')
)

dev.off()


png(paste0(dropbox_dir, '/Figures/NutrientExperiment2/O18Metabolism/GPP_VersusNO3.png'), width=4, height=4, units='in', res=200)

print(
ggplot(met.final, aes(x=`NO3-ppm`, y=gppv, fill=Site)) +
  labs(x=expression(paste(NO[3], ' (mg N L'^'-1', ')')), y=expression(paste(delta^'18', "O-", O[2], ' GPP (mg ', O[2], ' L'^'-1', ' d'^'-1', ')'))) +
  # scale_x_log10() +
  scale_shape_manual(values=rep(21:25, 5))  +
  scale_fill_manual(values = color.palette(length(unique(met.final$Site)))) +
  scale_colour_manual(values = color.palette(length(unique(met.final$Site)))) +
  geom_point(size=2, aes(fill=Site, shape=Site)) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))  +
  theme(legend.position='bottom')
)

dev.off()

