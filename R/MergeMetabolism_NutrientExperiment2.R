
library(tidyr)
#Combine buoy metabolism with bigger dataset


#load water chem with incubation and O18 results
met.final<-readRDS(file=file.path(onedrive_dir, 'Rdata', 'NutrientExperiment2', 'merge_df_O18.rds'))


#Buoy metabolism
metab.df<- readRDS(file=file.path(onedrive_dir, 'Rdata', 'NutrientExperiment2', 'BuoyMetabolism.rds')) %>%
  dplyr::select(Date, Site, GPP_roll, ER_roll, NEP_roll) %>%
  dplyr::rename(GPP_buoy_area = GPP_roll,
                ER_buoy_area = ER_roll,
                NEP_buoy_area = NEP_roll)


merge_df_all <- full_join(met.final, metab.df)
merge_df_allmetab <- merge_df_all %>%
  # drop_na(GPP_Total_area, GPP_buoy_area) %>%
  dplyr::rename(GPP_Inc_area = GPP_Total_area,
                ER_Inc_area = ER_Total_area,
                NEP_Inc_area = NEP_Total_area,
                GPP_O18_area = gppa,
                ER_O18_area =  ra,
                NEP_O18_area = nepa)

head(merge_df_allmetab)
head(merge_df_all)


metab_summary<-merge_df_allmetab %>%
  group_by(Date) %>%
  summarize_all(mean, na.rm=T)

#Axis titles
GPPbuoyexp<-expression(paste('Buoy-GPP (g ', O[2], ' m'^'-2', ' d'^'-1', ')'))
GPPIncexp<-expression(paste('Incubation-GPP (g ', O[2], ' m'^'-2', ' d'^'-1', ')'))
GPPO18exp<-expression(paste(delta^'18', "O-", O[2], '-GPP (g ', O[2], ' m'^'-2', ' d'^'-1', ')'))

ERbuoyexp<-expression(paste('Buoy-ER (g ', O[2], ' m'^'-2', ' d'^'-1', ')'))
ERIncexp<-expression(paste('Incubation-ER (g ', O[2], ' m'^'-2', ' d'^'-1', ')'))
ERO18exp<-expression(paste(delta^'18', "O-", O[2], '-ER (g ', O[2], ' m'^'-2', ' d'^'-1', ')'))

NEPbuoyexp<-expression(paste('Buoy-NEP (g ', O[2], ' m'^'-2', ' d'^'-1', ')'))
NEPIncexp<-expression(paste('Incubation-NEP (g ', O[2], ' m'^'-2', ' d'^'-1', ')'))
NEPO18exp<-expression(paste(delta^'18', "O-", O[2], '-NEP (g ', O[2], ' m'^'-2', ' d'^'-1', ')'))


color.palette = colorRampPalette(c(viridis(6, begin=.2, end=.98), rev(magma(5, begin=.35, end=.98))), bias=1)
colors<-color.palette(length(levels(merge_df_allmetab$Site)))
shapes<-rep(21:25, 5)



# #######################
# Comparison of 3 methods
# ######################

GPP_Buoy_Inc <- ggplot(merge_df_allmetab, aes(x=GPP_buoy_area, y=GPP_Inc_area, group=Site, fill=Site, shape=Site)) +
  geom_abline(linetype='dashed') +
  geom_point(size=2) + 
  theme_bw() +
  # lims(x=c(0, max(c(merge_df_allmetab$GPP_buoy_area, merge_df_allmetab$GPP_Inc_area))), 
       # y= c(0, max(c(merge_df_allmetab$GPP_buoy_area, merge_df_allmetab$GPP_Inc_area)))) + 
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_shape_manual(values=rep(21:25, 5)) +
  labs(x=GPPbuoyexp, y=GPPIncexp) +
  theme(legend.position='none')

GPP_Buoy_O18 <- ggplot(merge_df_allmetab, aes(x=GPP_buoy_area, y=GPP_O18_area, group=Site, fill=Site, shape=Site)) +
  geom_abline(linetype='dashed') +
  geom_point(size=2) + 
  theme_bw() +
  # lims(x=c(0, max(c(merge_df_allmetab$GPP_buoy_area, merge_df_allmetab$GPP_Inc_area))), 
  # y= c(0, max(c(merge_df_allmetab$GPP_buoy_area, merge_df_allmetab$GPP_Inc_area)))) + 
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_shape_manual(values=rep(21:25, 5)) +
  labs(x=GPPbuoyexp,  y=GPPO18exp) +
  theme(legend.position='none')

GPP_Inc_O18 <- ggplot(merge_df_allmetab, aes(x=GPP_Inc_area, y=GPP_O18_area, group=Site, fill=Site, shape=Site)) +
  geom_abline(linetype='dashed') +
  geom_point(size=2) + 
  theme_bw() +
  # lims(x=c(0, max(c(merge_df_allmetab$GPP_buoy_area, merge_df_allmetab$GPP_Inc_area))), 
  # y= c(0, max(c(merge_df_allmetab$GPP_buoy_area, merge_df_allmetab$GPP_Inc_area)))) + 
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_shape_manual(values=rep(21:25, 5)) +
  labs(x=GPPIncexp,  y=GPPO18exp) +
  theme(legend.position='none')


plot_withlegend <- GPP_Inc_O18 + 
  theme(legend.position="bottom", legend.title = element_blank()) +
  guides(fill = guide_legend(nrow = 1, title.hjust=0.5))

mylegend<-g_legend(plot_withlegend)

GPPCompare <- grid.arrange(grobs=list(GPP_Buoy_Inc, GPP_Buoy_O18, GPP_Inc_O18), ncol=3)


#Add legend to bottom of figure and save
png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'GPPCompare_3methods.png'), width=12, height=4.25, units='in', res=200)

grid.arrange(GPPCompare, mylegend, nrow=2, heights=c(4,.25))

dev.off()



#ER
ER_Buoy_Inc <- ggplot(merge_df_allmetab, aes(x=ER_buoy_area, y=ER_Inc_area, group=Site, fill=Site, shape=Site)) +
  geom_abline(linetype='dashed') +
  geom_point(size=2) + 
  theme_bw() +
  # lims(x=c(0, max(c(merge_df_allmetab$ER_buoy_area, merge_df_allmetab$ER_Inc_area))), 
  # y= c(0, max(c(merge_df_allmetab$ER_buoy_area, merge_df_allmetab$ER_Inc_area)))) + 
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_shape_manual(values=rep(21:25, 5)) +
  labs(x=ERbuoyexp, y=ERIncexp) +
  theme(legend.position='none')

ER_Buoy_O18 <- ggplot(merge_df_allmetab, aes(x=ER_buoy_area, y=ER_O18_area, group=Site, fill=Site, shape=Site)) +
  geom_abline(linetype='dashed') +
  geom_point(size=2) + 
  theme_bw() +
  # lims(x=c(0, max(c(merge_df_allmetab$ER_buoy_area, merge_df_allmetab$ER_Inc_area))), 
  # y= c(0, max(c(merge_df_allmetab$ER_buoy_area, merge_df_allmetab$ER_Inc_area)))) + 
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_shape_manual(values=rep(21:25, 5)) +
  labs(x=ERbuoyexp,  y=ERO18exp) +
  theme(legend.position='none')

ER_Inc_O18 <- ggplot(merge_df_allmetab, aes(x=ER_Inc_area, y=ER_O18_area, group=Site, fill=Site, shape=Site)) +
  geom_abline(linetype='dashed') +
  geom_point(size=2) + 
  theme_bw() +
  # lims(x=c(0, max(c(merge_df_allmetab$ER_buoy_area, merge_df_allmetab$ER_Inc_area))), 
  # y= c(0, max(c(merge_df_allmetab$ER_buoy_area, merge_df_allmetab$ER_Inc_area)))) + 
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_shape_manual(values=rep(21:25, 5)) +
  labs(x=ERIncexp,  y=ERO18exp) +
  theme(legend.position='none')


ERCompare <- grid.arrange(grobs=list(ER_Buoy_Inc, ER_Buoy_O18, ER_Inc_O18), ncol=3)


#Add legend to bottom of figure and save
png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'ERCompare_3methods.png'), width=12, height=4.25, units='in', res=200)

grid.arrange(ERCompare, mylegend, nrow=2, heights=c(4,.25))

dev.off()




#NEP
ggplot(merge_df_allmetab, aes(x=NEP_buoy_area, y=NEP_Inc_area, group=Site, fill=Site, shape=Site)) +
  geom_abline() + 
  geom_point(size=2) + 
  theme_bw() +
  lims(x=c(0, max(c(merge_df_allmetab$NEP_buoy_area, merge_df_allmetab$NEP_Inc_area))), 
       y= c(0, max(c(merge_df_allmetab$NEP_buoy_area, merge_df_allmetab$NEP_Inc_area)))) + 
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_shape_manual(values=rep(21:25, 5)) 


NEP_Buoy_Inc <- ggplot(merge_df_allmetab, aes(x=NEP_buoy_area, y=NEP_Inc_area, group=Site, fill=Site, shape=Site)) +
  geom_abline(linetype='dashed') +
  geom_point(size=2) + 
  theme_bw() +
  # lims(x=c(0, max(c(merge_df_allmetab$NEP_buoy_area, merge_df_allmetab$NEP_Inc_area))), 
  # y= c(0, max(c(merge_df_allmetab$NEP_buoy_area, merge_df_allmetab$NEP_Inc_area)))) + 
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_shape_manual(values=rep(21:25, 5)) +
  labs(x=NEPbuoyexp, y=NEPIncexp) +
  theme(legend.position='none')

NEP_Buoy_O18 <- ggplot(merge_df_allmetab, aes(x=NEP_buoy_area, y=NEP_O18_area, group=Site, fill=Site, shape=Site)) +
  geom_abline(linetype='dashed') +
  geom_point(size=2) + 
  theme_bw() +
  # lims(x=c(0, max(c(merge_df_allmetab$NEP_buoy_area, merge_df_allmetab$NEP_Inc_area))), 
  # y= c(0, max(c(merge_df_allmetab$NEP_buoy_area, merge_df_allmetab$NEP_Inc_area)))) + 
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_shape_manual(values=rep(21:25, 5)) +
  labs(x=NEPbuoyexp,  y=NEPO18exp) +
  theme(legend.position='none')

NEP_Inc_O18 <- ggplot(merge_df_allmetab, aes(x=NEP_Inc_area, y=NEP_O18_area, group=Site, fill=Site, shape=Site)) +
  geom_abline(linetype='dashed') +
  geom_point(size=2) + 
  theme_bw() +
  # lims(x=c(0, max(c(merge_df_allmetab$NEP_buoy_area, merge_df_allmetab$NEP_Inc_area))), 
  # y= c(0, max(c(merge_df_allmetab$NEP_buoy_area, merge_df_allmetab$NEP_Inc_area)))) + 
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_shape_manual(values=rep(21:25, 5)) +
  labs(x=NEPIncexp,  y=NEPO18exp) +
  theme(legend.position='none')


NEPCompare <- grid.arrange(grobs=list(NEP_Buoy_Inc, NEP_Buoy_O18, NEP_Inc_O18), ncol=3)


#Add legend to bottom of figure and save
png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'NEPCompare_3methods.png'), width=12, height=4.25, units='in', res=200)

grid.arrange(NEPCompare, mylegend, nrow=2, heights=c(4,.25))

dev.off()





# ####################
# Plot daily summarizes scatterplots
# ####################

GPP_Buoy_Inc_sum <- ggplot(metab_summary, aes(x=GPP_buoy_area, y=GPP_Inc_area)) +
  # geom_abline(linetype='dashed') +
  geom_point(size=3) + 
  theme_bw() +
  # lims(x=c(0, max(c(merge_df_allmetab$GPP_buoy_area, merge_df_allmetab$GPP_Inc_area))), 
  # y= c(0, max(c(merge_df_allmetab$GPP_buoy_area, merge_df_allmetab$GPP_Inc_area)))) + 
  # scale_colour_manual(values = colors) +
  # scale_fill_manual(values = colors) +
  # scale_shape_manual(values=rep(21:25, 5)) +
  labs(x=GPPbuoyexp, y=GPPIncexp) +
  theme(legend.position='none')

GPP_Buoy_O18_sum <- ggplot(metab_summary, aes(x=GPP_buoy_area, y=GPP_O18_area)) +
  # geom_abline(linetype='dashed') +
  geom_point(size=3) + 
  theme_bw() +
  # lims(x=c(0, max(c(merge_df_allmetab$GPP_buoy_area, merge_df_allmetab$GPP_Inc_area))), 
  # y= c(0, max(c(merge_df_allmetab$GPP_buoy_area, merge_df_allmetab$GPP_Inc_area)))) + 
  # scale_colour_manual(values = colors) +
  # scale_fill_manual(values = colors) +
  # scale_shape_manual(values=rep(21:25, 5)) +
  labs(x=GPPbuoyexp,  y=GPPO18exp) +
  theme(legend.position='none')

GPP_Inc_O18_sum <- ggplot(metab_summary, aes(x=GPP_Inc_area, y=GPP_O18_area)) +
  # geom_abline(linetype='dashed') +
  geom_point(size=3) + 
  theme_bw() +
  # lims(x=c(0, max(c(merge_df_allmetab$GPP_buoy_area, merge_df_allmetab$GPP_Inc_area))), 
  # y= c(0, max(c(merge_df_allmetab$GPP_buoy_area, merge_df_allmetab$GPP_Inc_area)))) + 
  # scale_colour_manual(values = colors) +
  # scale_fill_manual(values = colors) +
  # scale_shape_manual(values=rep(21:25, 5)) +
  labs(x=GPPIncexp,  y=GPPO18exp) +
  theme(legend.position='none')



png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'GPPCompare_3methods_sum.png'), width=12, height=4.25, units='in', res=200)

grid.arrange(grobs=list(GPP_Buoy_Inc_sum, GPP_Buoy_O18_sum, GPP_Inc_O18_sum), ncol=3)

dev.off()

#ER
ER_Buoy_Inc_sum <- ggplot(metab_summary, aes(x=ER_buoy_area, y=ER_Inc_area)) +
  # geom_abline(linetype='dashed') +
  geom_point(size=3) + 
  theme_bw() +
  # lims(x=c(0, max(c(merge_df_allmetab$ER_buoy_area, merge_df_allmetab$ER_Inc_area))), 
  # y= c(0, max(c(merge_df_allmetab$ER_buoy_area, merge_df_allmetab$ER_Inc_area)))) + 
  # scale_colour_manual(values = colors) +
  # scale_fill_manual(values = colors) +
  # scale_shape_manual(values=rep(21:25, 5)) +
  labs(x=ERbuoyexp, y=ERIncexp) +
  theme(legend.position='none')

ER_Buoy_O18_sum <- ggplot(metab_summary, aes(x=ER_buoy_area, y=ER_O18_area)) +
  # geom_abline(linetype='dashed') +
  geom_point(size=3) + 
  theme_bw() +
  # lims(x=c(0, max(c(merge_df_allmetab$ER_buoy_area, merge_df_allmetab$ER_Inc_area))), 
  # y= c(0, max(c(merge_df_allmetab$ER_buoy_area, merge_df_allmetab$ER_Inc_area)))) + 
  # scale_colour_manual(values = colors) +
  # scale_fill_manual(values = colors) +
  # scale_shape_manual(values=rep(21:25, 5)) +
  labs(x=ERbuoyexp,  y=ERO18exp) +
  theme(legend.position='none')

ER_Inc_O18_sum <- ggplot(metab_summary, aes(x=ER_Inc_area, y=ER_O18_area)) +
  # geom_abline(linetype='dashed') +
  geom_point(size=3) + 
  theme_bw() +
  # lims(x=c(0, max(c(merge_df_allmetab$ER_buoy_area, merge_df_allmetab$ER_Inc_area))), 
  # y= c(0, max(c(merge_df_allmetab$ER_buoy_area, merge_df_allmetab$ER_Inc_area)))) + 
  # scale_colour_manual(values = colors) +
  # scale_fill_manual(values = colors) +
  # scale_shape_manual(values=rep(21:25, 5)) +
  labs(x=ERIncexp,  y=ERO18exp) +
  theme(legend.position='none')



png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'ERCompare_3methods_sum.png'), width=12, height=4.25, units='in', res=200)

grid.arrange(grobs=list(ER_Buoy_Inc_sum, ER_Buoy_O18_sum, ER_Inc_O18_sum), ncol=3)

dev.off()

#NEP
NEP_Buoy_Inc_sum <- ggplot(metab_summary, aes(x=NEP_buoy_area, y=NEP_Inc_area)) +
  # geom_abline(linetype='dashed') +
  geom_point(size=3) + 
  theme_bw() +
  # lims(x=c(0, max(c(merge_df_allmetab$NEP_buoy_area, merge_df_allmetab$NEP_Inc_area))), 
  # y= c(0, max(c(merge_df_allmetab$NEP_buoy_area, merge_df_allmetab$NEP_Inc_area)))) + 
  # scale_colour_manual(values = colors) +
  # scale_fill_manual(values = colors) +
  # scale_shape_manual(values=rep(21:25, 5)) +
  labs(x=NEPbuoyexp, y=NEPIncexp) +
  theme(legend.position='none')

NEP_Buoy_O18_sum <- ggplot(metab_summary, aes(x=NEP_buoy_area, y=NEP_O18_area)) +
  # geom_abline(linetype='dashed') +
  geom_point(size=3) + 
  theme_bw() +
  # lims(x=c(0, max(c(merge_df_allmetab$NEP_buoy_area, merge_df_allmetab$NEP_Inc_area))), 
  # y= c(0, max(c(merge_df_allmetab$NEP_buoy_area, merge_df_allmetab$NEP_Inc_area)))) + 
  # scale_colour_manual(values = colors) +
  # scale_fill_manual(values = colors) +
  # scale_shape_manual(values=rep(21:25, 5)) +
  labs(x=NEPbuoyexp,  y=NEPO18exp) +
  theme(legend.position='none')

NEP_Inc_O18_sum <- ggplot(metab_summary, aes(x=NEP_Inc_area, y=NEP_O18_area)) +
  # geom_abline(linetype='dashed') +
  geom_point(size=3) + 
  theme_bw() +
  # lims(x=c(0, max(c(merge_df_allmetab$NEP_buoy_area, merge_df_allmetab$NEP_Inc_area))), 
  # y= c(0, max(c(merge_df_allmetab$NEP_buoy_area, merge_df_allmetab$NEP_Inc_area)))) + 
  # scale_colour_manual(values = colors) +
  # scale_fill_manual(values = colors) +
  # scale_shape_manual(values=rep(21:25, 5)) +
  labs(x=NEPIncexp,  y=NEPO18exp) +
  theme(legend.position='none')



png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'NEPCompare_3methods_sum.png'), width=12, height=4.25, units='in', res=200)

grid.arrange(grobs=list(NEP_Buoy_Inc_sum, NEP_Buoy_O18_sum, NEP_Inc_O18_sum), ncol=3)

dev.off()




# ###################
# Timeseries boxplots
# ###################

commonBox<-list(
  geom_vline(xintercept=fert_dates, color='green', linetype=2, size=1),
  geom_hline(yintercept=0, color='lightgrey', linetype=1.5, size=1), 
  theme_bw(),
  theme(plot.title = element_text(hjust=0.5), legend.position="none", axis.title.x=element_blank()), 
  scale_x_date(limits=xlim, date_minor_breaks= "weeks", date_breaks = "2 weeks", date_labels="%b %d"), 
  guides(color = guide_legend(nrow = 1, title.position='top', title.hjust=0.5)) 
)

#Incubation metabolism 
GPPbox_Inc <- ggplot(merge_df_allmetab, aes(x=Date, group=Date, y=GPP_Inc_area)) + 
  commonBox +
  labs(x='Date', y=GPPIncexp) +
  geom_boxplot(fill='darkgreen', outlier.size=0.5) 

ERbox_Inc <- ggplot(merge_df_allmetab, aes(x=Date, group=Date, y=ER_Inc_area)) + 
  commonBox +
  labs(x='Date', y=ERIncexp) +
  geom_boxplot(fill='sienna4', outlier.size=0.5) 

NEPbox_Inc <- ggplot(merge_df_allmetab, aes(x=Date, group=Date, y=NEP_Inc_area)) + 
  commonBox +
  labs(x='Date', y=NEPIncexp) +
  geom_boxplot(fill='grey30', outlier.size=0.5) 


#Buoy
GPPbox_buoy <- ggplot(merge_df_allmetab, aes(x=Date, group=Date, y=GPP_buoy_area)) + 
  commonBox +
  labs(x='Date', y=GPPbuoyexp) +
  geom_boxplot(fill='darkgreen', outlier.size=0.5) 

ERbox_buoy <- ggplot(merge_df_allmetab, aes(x=Date, group=Date, y=ER_buoy_area)) + 
  commonBox +
  labs(x='Date', y=ERbuoyexp) +
  geom_boxplot(fill='sienna4', outlier.size=0.5) 

NEPbox_buoy <- ggplot(merge_df_allmetab, aes(x=Date, group=Date, y=NEP_buoy_area)) + 
  commonBox +
  labs(x='Date', y=NEPbuoyexp) +
  geom_boxplot(fill='grey30', outlier.size=0.5) 

#O18
GPPbox_O18 <- ggplot(merge_df_allmetab, aes(x=Date, group=Date, y=GPP_O18_area)) + 
  commonBox +
  labs(x='Date', y=GPPO18exp) +
  geom_boxplot(fill='darkgreen', outlier.size=0.5) 

ERbox_O18 <- ggplot(merge_df_allmetab, aes(x=Date, group=Date, y=ER_O18_area)) + 
  commonBox +
  labs(x='Date', y=ERO18exp) +
  geom_boxplot(fill='sienna4', outlier.size=0.5) 

NEPbox_O18 <- ggplot(merge_df_allmetab, aes(x=Date, group=Date, y=NEP_O18_area)) + 
  commonBox +
  labs(x='Date', y=NEPO18exp) +
  geom_boxplot(fill='grey30', outlier.size=0.5) 



png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'GPP_Boxplot_TS.png'), width=5, height=7, units='in', res=200)
grid.arrange(grobs=list(GPPbox_buoy, GPPbox_Inc, GPPbox_O18), ncol=1)
dev.off()

png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'ER_Boxplot_TS.png'), width=5, height=7, units='in', res=200)
grid.arrange(grobs=list(ERbox_buoy, ERbox_Inc, ERbox_O18), ncol=1)
dev.off()

png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'NEP_Boxplot_TS.png'), width=5, height=7, units='in', res=200)
grid.arrange(grobs=list(NEPbox_buoy, NEPbox_Inc, NEPbox_O18), ncol=1)
dev.off()





# ############
# Other plots
# ############




ggplot(metab_summary, aes(x=GPP_buoy_area, y=GPP_Inc_area)) +
  # geom_abline() + 
  geom_point(size=2) + 
  theme_bw() +
  # lims(x=range(c(metab_summary$GPP_buoy_area, metab_summary$GPP_Inc_area)), 
  # y= range(c(metab_summary$GPP_buoy_area, metab_summary$GPP_Inc_area))) + 
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_shape_manual(values=rep(21:25, 5)) 

ggplot(metab_summary, aes(x=ER_buoy_area, y=ER_Inc_area)) +
  # geom_abline() + 
  geom_point(size=2) + 
  theme_bw() +
  # lims(x=c(0,min(c(metab_summary$ER_buoy_area, metab_summary$ER_Inc_area))), 
  # y=c(0,min(c(metab_summary$ER_buoy_area, metab_summary$ER_Inc_area)))) + 
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_shape_manual(values=rep(21:25, 5)) 

ggplot(metab_summary, aes(x=NEP_buoy_area, y=NEP_Inc_area)) +
  # geom_abline() + 
  geom_point(size=2) + 
  theme_bw() +
  # lims(x=range(c(metab_summary$NEP_buoy_area, metab_summary$NEP_Inc_area)), 
  # y= range(c(metab_summary$NEP_buoy_area, metab_summary$NEP_Inc_area))) + 
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_shape_manual(values=rep(21:25, 5)) 




# #######################
# Other figures
# ######################




png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'BuoyVsInc_GPP.png'), width=5, height=4, units='in', res=200)

print(
ggplot(merge_df_allmetab, aes(x=GPP_buoy_area, y=GPP_Inc_area, group=Site, fill=Site, shape=Site)) +
  # geom_abline() + 
  geom_point(size=2) + 
  theme_bw() +
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_shape_manual(values=rep(21:25, 5)) 
)

dev.off()


ggplot(merge_df_allmetab, aes(x=ER_buoy_area, y=ER_Inc_area, group=Site, fill=Site, shape=Site)) +
  geom_abline() + 
  geom_point(size=2) + 
  theme_bw() +
  lims(x=c(min(c(merge_df_allmetab$ER_buoy_area, merge_df_allmetab$ER_Inc_area)),0), 
       y= c(min(c(merge_df_allmetab$ER_buoy_area, merge_df_allmetab$ER_Inc_area)),0)) + 
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_shape_manual(values=rep(21:25, 5)) 


png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'BuoyVsInc_ER.png'), width=5, height=4, units='in', res=200)

print(
ggplot(merge_df_allmetab, aes(x=ER_buoy_area, y=ER_Inc_area, group=Site, fill=Site, shape=Site)) +
  # geom_abline() + 
  geom_point(size=2) + 
  theme_bw() +
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_shape_manual(values=rep(21:25, 5)) 
)

dev.off()

ggplot(merge_df_allmetab, aes(x=NEP_buoy_area, y=NEP_Inc_area, group=Site, fill=Site, shape=Site)) +
  geom_abline() + 
  geom_point(size=2) + 
  theme_bw() +
  lims(x=range(c(merge_df_allmetab$NEP_buoy_area, merge_df_allmetab$NEP_Inc_area)), 
       y= range(c(merge_df_allmetab$NEP_buoy_area, merge_df_allmetab$NEP_Inc_area))) + 
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_shape_manual(values=rep(21:25, 5)) 




png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'BuoyVsInc_NEP.png'), width=5, height=4, units='in', res=200)

print(
ggplot(merge_df_allmetab, aes(x=NEP_buoy_area, y=NEP_Inc_area, group=Site, fill=Site, shape=Site)) +
  # geom_abline() + 
  geom_point(size=2) + 
  theme_bw() +
  # lims(x=c(0, max(c(merge_df_allmetab$NEP_buoy_area, merge_df_allmetab$NEP_Inc_area))), 
  # y= c(0, max(c(merge_df_allmetab$NEP_buoy_area, merge_df_allmetab$NEP_Inc_area)))) + 
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_shape_manual(values=rep(21:25, 5)) 
)
dev.off()







