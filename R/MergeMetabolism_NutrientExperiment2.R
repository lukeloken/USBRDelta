
library(randomForest)
library(bestglm)
library(tidyr)
#Combine buoy metabolism with bigger dataset


#load water chem with incubation and O18 results
met.final<-readRDS(file=file.path(onedrive_dir, 'Rdata', 'NutrientExperiment2', 'merge_df_O18.rds'))

# Gage Data water level
RawData <- readRDS(file=file.path(onedrive_dir, 'RData', 'NutrientExperiment2', 'USGSGageData.rds'))

DailyGage <- RawData %>%
  select(dateTime, GH_Inst) %>%
  mutate(Date = as.Date(dateTime, tz='America/Los_Angeles')) %>%
  group_by(Date) %>%
  summarize(GH_max = max(GH_Inst, na.rm=T),
            GH_min = min(GH_Inst, na.rm=T),
            n = n()) %>%
  mutate(GH_range = GH_max - GH_min) %>%
  filter(n>460)

# plot(DailyGage$Date, DailyGage$GH_range, type='l')

#Stratification data from Paul
duration <- read.csv(file.path(onedrive_dir, 'OutputData', 'NutrientExperiment2', 'DWSC_CM74_Duration_Stratification.csv'))
strength <- read.csv(file.path(onedrive_dir, 'OutputData', 'NutrientExperiment2', 'DWSC_CM74_Strength_Stratification.csv'))

duration <- duration %>%
  mutate(Date = mdy(Timestamp..PST.)) %>%
  rename(StratificationDuration = Duration.of.Stratification..hours.) %>%
  select(Date, StratificationDuration)

strength <- strength %>%
  mutate(DateTime = as.POSIXct(Timestamp..PST., format='%m/%d/%Y %H:%M', tz="Etc/GMT+8")) %>%
  rename(StratificationStrength = Strength.of.Stratification..deg.C.m.) %>%
  select(DateTime, StratificationStrength) %>%
  mutate(Date = as.Date(DateTime, tz="Etc/GMT+8"))

stratification <- strength %>%
  group_by(Date) %>%
  dplyr::summarize(Strength_mean = mean(StratificationStrength, na.rm=T),
                   Strength_median = median(StratificationStrength, na.rm=T),
                   Strength_min = min(StratificationStrength, na.rm=T),
                   Strength_max = max(StratificationStrength, na.rm=T),
                   n=n()) %>%
  filter(n>20) %>%
  select(-n) %>%
  full_join(duration)

#SchmidtStability
strat_df <- readRDS(file=file.path(onedrive_dir, 'RData', 'NutrientExperiment2', 'Buoy', 'Schmidt_Stability.rds'))

strat_df_daily <- strat_df %>%
  mutate(Date = as.Date(Datetime_PDT_round, tz='America/Los_Angeles')) %>%
  group_by(Site, Date) %>%
  summarize_at("Schmidt", .funs=c(min, mean, max, median))

names(strat_df_daily)[3:6] <- paste0('Schmidt_', c('min', 'mean', 'max', 'median'))


# ###########################################
#Buoy metabolism
# ############################################
metab.df<- readRDS(file=file.path(onedrive_dir, 'Rdata', 'NutrientExperiment2', 'BuoyMetabolism.rds')) %>%
  dplyr::select(Date, Site, GPP_roll, ER_roll, NEP_roll) %>%
  dplyr::rename(GPP_buoy_area = GPP_roll,
                ER_buoy_area = ER_roll,
                NEP_buoy_area = NEP_roll)


metab.df <- metab.df %>%
  left_join(stratification) %>%
  left_join(strat_df_daily) %>%
  mutate(Site = factor(Site, sitetable$site1))

metab.buoy.dailymean <- metab.df %>%
  group_by(Date) %>%
  summarize_at(vars(GPP_buoy_area:Schmidt_median), .funs=mean, na.rm=T)


merge_df_all <- full_join(met.final, metab.df)
# merge_df_allmetab <- merge_df_all %>%
#   # drop_na(GPP_Total_area, GPP_buoy_area) %>%
#   dplyr::rename(GPP_Inc_area = GPP_Total_area,
#                 ER_Inc_area = ER_Total_area,
#                 NEP_Inc_area = NEP_Total_area,
#                 GPP_O18_area = gppa,
#                 ER_O18_area =  ra,
#                 NEP_O18_area = nepa)

merge_df_allmetab <- merge_df_all %>%
  # drop_na(GPP_Total_area, GPP_buoy_area) %>%
  dplyr::rename(GPP_O18_area = gppa,
                ER_O18_area =  ra,
                NEP_O18_area = nepa)

head(merge_df_allmetab)
head(merge_df_all)


metab_summary<-merge_df_allmetab %>%
  group_by(Date) %>%
  summarize_all(mean, na.rm=T)

# plot(merge_df_allmetab$GPP_Inc_area, merge_df_allmetab$GPP_Total_area)
# abline(0,1)
# 
# plot(merge_df_allmetab$ER_Inc_area, merge_df_allmetab$ER_Total_area)
# abline(0,1)
# 
# plot(merge_df_allmetab$NEP_Inc_area, merge_df_allmetab$NEP_Total_area)
# abline(0,1)


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

xlim<-range(metab.df$Date[which(is.finite(metab.df$NEP))], na.rm=T)


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
GPPbox_buoy <- ggplot(merge_df_allmetab, aes(x=Date, y=GPP_buoy_area)) + 
  commonBox +
  labs(x='Date', y=GPPbuoyexp) +
  geom_boxplot(aes(group=Date), fill='darkgreen', outlier.size=0.5)  

GPP_box_buoy_withWL <- GPPbox_buoy + 
  geom_point(data=DailyGage, aes(x=Date, y=2*(GH_range-mean(GH_range))), col='darkblue', size=2)

GPP_box_buoy_withSchmidt <- GPPbox_buoy + 
  geom_path(data=metab.buoy.dailymean, aes(x=Date, y=Schmidt_mean), col='darkblue', size=2, alpha=.5)

# GPP_box_buoy_withSchmidt

ERbox_buoy <- ggplot(merge_df_allmetab, aes(x=Date, group=Date, y=ER_buoy_area)) + 
  commonBox +
  labs(x='Date', y=ERbuoyexp) +
  geom_boxplot(fill='sienna4', outlier.size=0.5) 

NEPbox_buoy <- ggplot(merge_df_allmetab, aes(x=Date, y=NEP_buoy_area)) + 
  commonBox +
  labs(x='Date', y=NEPbuoyexp) +
  geom_boxplot(aes(group=Date), fill='grey30', outlier.size=0.5) 

NEP_box_buoy_withSchmidt <- NEPbox_buoy + 
  geom_path(data=metab.buoy.dailymean, aes(x=Date, y=Schmidt_mean/4), col='darkblue', size=2, alpha=.5)

NEP_box_buoy_withSchmidt

#O18
GPPbox_O18 <- ggplot(merge_df_allmetab, aes(x=Date, group=Date, y=GPP_O18_area)) + 
  commonBox +
  labs(x='Date', y=GPPO18exp) +
  geom_boxplot(fill='darkgreen', outlier.size=0.5) 

ERbox_O18 <- ggplot(merge_df_allmetab, aes(x=Date, group=Date, y=ER_O18_area)) + 
  commonBox +
  labs(x='Date', y=ERO18exp) +
  geom_boxplot(fill='sienna4', outlier.size=0.5) 

NEPbox_O18 <- ggplot(merge_df_allmetab, aes(x=Date, y=NEP_O18_area)) + 
  commonBox +
  labs(x='Date', y=NEPO18exp) +
  geom_boxplot(aes(group=Date), fill='grey30', outlier.size=0.5) 

NEPbox_O18_withSchmidt <- NEPbox_O18 + 
  geom_path(data=metab.buoy.dailymean, aes(x=Date, y=Schmidt_mean/4), col='darkblue', size=2, alpha=.5)

NEPbox_O18_withSchmidt


png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'GPP_Boxplot_TS.png'), width=5, height=7, units='in', res=200)
# grid.arrange(grobs=list(GPPbox_buoy, GPPbox_Inc, GPPbox_O18), ncol=1)
grid.newpage()
plots<-grid.draw(rbind(ggplotGrob(GPPbox_buoy), ggplotGrob(GPPbox_Inc), ggplotGrob(GPPbox_O18), size = "first"))

dev.off()

png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'GPP_Boxplot_withWL_TS.png'), width=5, height=7, units='in', res=200)
grid.arrange(grobs=list(GPP_box_buoy_withWL, GPPbox_Inc, GPPbox_O18), ncol=1)
dev.off()

png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'GPP_Boxplot_withSchmidt_TS.png'), width=5, height=7, units='in', res=200)
grid.arrange(grobs=list(GPP_box_buoy_withSchmidt, GPPbox_Inc, GPPbox_O18), ncol=1)
dev.off()


png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'ER_Boxplot_TS.png'), width=5, height=7, units='in', res=200)
# grid.arrange(grobs=list(ERbox_buoy, ERbox_Inc, ERbox_O18), ncol=1)
grid.newpage()
plots<-grid.draw(rbind(ggplotGrob(ERbox_buoy), ggplotGrob(ERbox_Inc), ggplotGrob(ERbox_O18), size = "first"))
dev.off()

png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'NEP_Boxplot_TS.png'), width=5, height=7, units='in', res=200)
# grid.arrange(grobs=list(NEPbox_buoy, NEPbox_Inc, NEPbox_O18), ncol=1)
grid.newpage()
plots<-grid.draw(rbind(ggplotGrob(NEPbox_buoy), ggplotGrob(NEPbox_Inc), ggplotGrob(NEPbox_O18), size = "first"))
dev.off()

png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'NEP_Boxplot_withSchmidt_TS.png'), width=5, height=7, units='in', res=200)
grid.arrange(grobs=list(NEP_box_buoy_withSchmidt, NEPbox_Inc, NEPbox_O18), ncol=1)
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






#Plot against drivers

png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'O18Metabolism', 'GPP_VersusTurb.png'), width=3.5, height=3.5, units='in', res=200)

print(
  ggplot(merge_df_allmetab[which(!is.na(merge_df_allmetab$GPP_O18_area)),], aes(x=YSI_Turb_FNU, y=GPP_O18_area)) +
    labs(x=expression(paste('Turbidity (FNU)')), y=expression(paste(delta^'18', "O-", O[2], ' GPP (mg ', O[2], ' m'^'-2', ' d'^'-1', ')'))) +
    # geom_smooth(method='lm', formula= y~x) + 
    scale_shape_manual(values=shapes)  +
    scale_fill_manual(values = color.palette(length(unique(merge_df_allmetab$Site)))) +
    scale_colour_manual(values = color.palette(length(unique(merge_df_allmetab$Site)))) +
    geom_point(size=2, aes(fill=Site, shape=Site)) +
    theme_bw() +
    theme(plot.title = element_text(hjust=0.5))  +
    theme(legend.position='none')
)

dev.off()

# ggplot(merge_df_allmetab, aes(x=EXOTurbFNU, y=ER_O18_area, fill=Site)) + 
#   labs(x=expression(paste('Turbidity (FNU)')), y=expression(paste(delta^'18', "O-", O[2], ' ER (mg ', O[2], ' L'^'-1', ' d'^'-1', ')'))) + 
#   # scale_x_log10() + 
#   scale_shape_manual(values=rep(21:25, 5))  + 
#   scale_fill_manual(values = color.palette(length(unique(merge_df_allmetab$Site)))) + 
#   scale_colour_manual(values = color.palette(length(unique(merge_df_allmetab$Site)))) +
#   geom_point(size=2, aes(fill=Site, shape=Site)) + 
#   theme_bw() +
#   theme(plot.title = element_text(hjust=0.5))  +
#   theme(legend.position='bottom')
# 
# ggplot(merge_df_allmetab, aes(x=EXOTurbFNU, y=nepv, fill=Site)) + 
#   labs(x=expression(paste('Turbidity (FNU)')), y=expression(paste(delta^'18', "O-", O[2], ' NEP (mg ', O[2], ' L'^'-1', ' d'^'-1', ')'))) + 
#   # scale_x_log10() + 
#   scale_shape_manual(values=rep(21:25, 5))  + 
#   scale_fill_manual(values = color.palette(length(unique(merge_df_allmetab$Site)))) + 
#   scale_colour_manual(values = color.palette(length(unique(merge_df_allmetab$Site)))) +
#   geom_point(size=2, aes(fill=Site, shape=Site)) + 
#   theme_bw() +
#   theme(plot.title = element_text(hjust=0.5))  +
#   theme(legend.position='bottom')



png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'O18Metabolism', 'GPP_VersusChlA.png'), width=4, height=4, units='in', res=200)

print(
  ggplot(merge_df_allmetab[which(!is.na(merge_df_allmetab$GPP_O18_area)),], aes(x=YSI_ChlA_ugL, y=GPP_O18_area, fill=Site)) +
    labs(x=expression(paste('Chl a (', mu, 'g L'^'-1', ')')), y=expression(paste(delta^'18', "O-", O[2], ' GPP (mg ', O[2], ' m'^'-2', ' d'^'-1', ')'))) +
    # scale_x_log10() +
    scale_shape_manual(values=shapes)  +
    scale_fill_manual(values = color.palette(length(unique(merge_df_allmetab$Site)))) +
    scale_colour_manual(values = color.palette(length(unique(merge_df_allmetab$Site)))) +
    geom_point(size=2, aes(fill=Site, shape=Site)) +
    theme_bw() +
    theme(plot.title = element_text(hjust=0.5))  +
    theme(legend.position='bottom')
)

dev.off()


png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'O18Metabolism', 'GPP_VersusNO3.png'), width=4, height=4, units='in', res=200)

print(
  ggplot(merge_df_allmetab[which(!is.na(merge_df_allmetab$GPP_O18_area)),], aes(x=`NO3-ppm`, y=GPP_O18_area, fill=Site)) +
    labs(x=expression(paste(NO[3], ' (mg N L'^'-1', ')')), y=expression(paste(delta^'18', "O-", O[2], ' GPP (mg ', O[2], ' m'^'-2', ' d'^'-1', ')'))) +
    # scale_x_log10() +
    scale_shape_manual(values=shapes)  +
    scale_fill_manual(values = color.palette(length(unique(merge_df_allmetab$Site)))) +
    scale_colour_manual(values = color.palette(length(unique(merge_df_allmetab$Site)))) +
    geom_point(size=2, aes(fill=Site, shape=Site)) +
    theme_bw() +
    theme(plot.title = element_text(hjust=0.5))  +
    theme(legend.position='bottom')
)

dev.off()


png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'O18Metabolism', 'GPP_VersusStratification.png'), width=4, height=4, units='in', res=200)

print(
  ggplot(merge_df_allmetab[which(!is.na(merge_df_allmetab$GPP_O18_area)),], aes(x=StratificationDuration, y=GPP_O18_area, fill=Site)) +
    labs(x=expression(paste('Stratification duration')), y=expression(paste(delta^'18', "O-", O[2], ' GPP (mg ', O[2], ' m'^'-2', ' d'^'-1', ')'))) +
    # scale_x_log10() +
    scale_shape_manual(values=shapes)  +
    scale_fill_manual(values = color.palette(length(unique(merge_df_allmetab$Site)))) +
    scale_colour_manual(values = color.palette(length(unique(merge_df_allmetab$Site)))) +
    geom_point(size=2, aes(fill=Site, shape=Site)) +
    theme_bw() +
    theme(plot.title = element_text(hjust=0.5))  +
    theme(legend.position='bottom')
)

dev.off()


# png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'O18Metabolism', 'BuoyGPP_VersusStratification.png'), width=4, height=4, units='in', res=200)

print(
  ggplot(merge_df_allmetab, aes(x=Schmidt_mean, y=NEP_buoy_area, fill=Site)) +
    labs(x=expression(paste('Schmidt max (unit)')), y=expression(paste('Buoy NEP (mg ', O[2], ' m'^'-2', ' d'^'-1', ')'))) +
    # scale_x_log10() +
    geom_smooth(aes(colour=Site), method='lm', se=F) +
    scale_shape_manual(values=shapes)  +
    scale_fill_manual(values = color.palette(length(unique(merge_df_allmetab$Site)))) +
    scale_colour_manual(values = color.palette(length(unique(merge_df_allmetab$Site)))) +
    geom_point(size=2, aes(fill=Site, shape=Site)) +
    theme_bw() +
    theme(plot.title = element_text(hjust=0.5))  +
    theme(legend.position='bottom')
)

print(
  ggplot(merge_df_allmetab, aes(x=Strength_max, y=GPP_buoy_area, fill=Site)) +
    labs(x=expression(paste('Stratification max')), y=expression(paste('Buoy GPP (mg ', O[2], ' m'^'-2', ' d'^'-1', ')'))) +
    # scale_x_log10() +
    scale_shape_manual(values=shapes)  +
    scale_fill_manual(values = color.palette(length(unique(merge_df_allmetab$Site)))) +
    scale_colour_manual(values = color.palette(length(unique(merge_df_allmetab$Site)))) +
    scale_linetype(guide=F) + 
    # geom_smooth(aes(colour=Site), method='lm', se=F) +
    geom_point(size=2, aes(fill=Site, shape=Site), col='black') +
    theme_bw() +
    theme(plot.title = element_text(hjust=0.5))  +
    theme(legend.position='bottom') +
    guides(colour = guide_legend(override.aes = list(colour = NULL)))
)

print(
  ggplot(merge_df_allmetab, aes(x=Strength_max, y=NEP_buoy_area, fill=Site)) +
    labs(x=expression(paste('Stratification max')), y=expression(paste('Buoy NEP (mg ', O[2], ' m'^'-2', ' d'^'-1', ')'))) +
    # scale_x_log10() +
    scale_shape_manual(values=shapes)  +
    scale_fill_manual(values = color.palette(length(unique(merge_df_allmetab$Site)))) +
    scale_colour_manual(values = color.palette(length(unique(merge_df_allmetab$Site)))) +
    scale_linetype(guide=F) + 
    # geom_smooth(aes(colour=Site), method='lm', se=F) +
    geom_point(size=2, aes(fill=Site, shape=Site), col='black') +
    theme_bw() +
    theme(plot.title = element_text(hjust=0.5))  +
    theme(legend.position='bottom') +
    guides(colour = guide_legend(override.aes = list(colour = NULL)))
)

print(
  ggplot(merge_df_allmetab, aes(x=Strength_mean, y=NEP_buoy_area, fill=Site)) +
    labs(x=expression(paste('Stratification mean')), y=expression(paste('Buoy NEP (mg ', O[2], ' m'^'-2', ' d'^'-1', ')'))) +
    # scale_x_log10() +
    scale_shape_manual(values=shapes)  +
    scale_fill_manual(values = color.palette(length(unique(merge_df_allmetab$Site)))) +
    scale_colour_manual(values = color.palette(length(unique(merge_df_allmetab$Site)))) +
    scale_linetype(guide=F) + 
    # geom_smooth(aes(colour=Site), method='lm', se=F) +
    geom_point(size=2, aes(fill=Site, shape=Site), col='black') +
    theme_bw() +
    theme(plot.title = element_text(hjust=0.5))  +
    theme(legend.position='bottom') +
    guides(colour = guide_legend(override.aes = list(colour = NULL)))
)

print(
  ggplot(merge_df_allmetab, aes(x=Strength_max, y=ER_buoy_area, fill=Site)) +
    labs(x=expression(paste('Stratification max')), y=expression(paste('Buoy ER (mg ', O[2], ' m'^'-2', ' d'^'-1', ')'))) +
    # scale_x_log10() +
    scale_shape_manual(values=shapes)  +
    scale_fill_manual(values = color.palette(length(unique(merge_df_allmetab$Site)))) +
    scale_colour_manual(values = color.palette(length(unique(merge_df_allmetab$Site)))) +
    scale_linetype(guide=F) + 
    # geom_smooth(aes(colour=Site), method='lm', se=F) +
    geom_point(size=2, aes(fill=Site, shape=Site), col='black') +
    theme_bw() +
    theme(plot.title = element_text(hjust=0.5))  +
    theme(legend.position='bottom') +
    guides(colour = guide_legend(override.aes = list(colour = NULL)))
)

print(
  ggplot(merge_df_allmetab, aes(x=Strength_mean, y=ER_buoy_area, fill=Site)) +
    labs(x=expression(paste('Stratification mean')), y=expression(paste('Buoy ER (mg ', O[2], ' m'^'-2', ' d'^'-1', ')'))) +
    # scale_x_log10() +
    scale_shape_manual(values=shapes)  +
    scale_fill_manual(values = color.palette(length(unique(merge_df_allmetab$Site)))) +
    scale_colour_manual(values = color.palette(length(unique(merge_df_allmetab$Site)))) +
    scale_linetype(guide=F) + 
    # geom_smooth(aes(colour=Site), method='lm', se=F) +
    geom_point(size=2, aes(fill=Site, shape=Site), col='black') +
    theme_bw() +
    theme(plot.title = element_text(hjust=0.5))  +
    theme(legend.position='bottom') +
    guides(colour = guide_legend(override.aes = list(colour = NULL)))
)

#Spatial averages
#GPP
Daily_GPP_StratMax <- ggplot(metab.buoy.dailymean, aes(x=Strength_max, y=GPP_buoy_area)) +
    labs(x=expression(paste('Max daily stratification strength (', degree, 'C m'^'-1', ')')), 
         y=expression(paste('Buoy GPP (mg ', O[2], ' m'^'-2', ' d'^'-1', ')'))) +
    geom_point(size=2, col='darkgreen') +
    theme_bw() +
    theme(plot.title = element_text(hjust=0.5))


Daily_GPP_StratMean <- ggplot(metab.buoy.dailymean, aes(x=Strength_mean, y=GPP_buoy_area)) +
    labs(x=expression(paste('Mean daily stratification strength (', degree, 'C m'^'-1', ')')), 
         y=expression(paste('Buoy GPP (mg ', O[2], ' m'^'-2', ' d'^'-1', ')'))) +
    geom_point(size=2, col='darkgreen') +
    theme_bw() +
    theme(plot.title = element_text(hjust=0.5))


Daily_GPP_StratDur <- ggplot(metab.buoy.dailymean, aes(x=StratificationDuration, y=GPP_buoy_area)) +
    labs(x=expression(paste('Daily stratification duration (hr)')), 
         y=expression(paste('Buoy GPP (mg ', O[2], ' m'^'-2', ' d'^'-1', ')'))) +
    geom_point(size=2, col='darkgreen') +
    theme_bw() +
    theme(plot.title = element_text(hjust=0.5))


GPP_startgrid <- grid.arrange(grobs=list(Daily_GPP_StratMax, Daily_GPP_StratMean, Daily_GPP_StratDur), ncol=1)

ggsave(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'Buoys', 'DailyGPP_Versus_Stratification.png'), plot=GPP_startgrid, height=10, width=4, dpi=300, units='in')

#ER
Daily_ER_StratMax <- ggplot(metab.buoy.dailymean, aes(x=Strength_max, y=ER_buoy_area)) +
  labs(x=expression(paste('Max daily stratification strength (', degree, 'C m'^'-1', ')')), 
       y=expression(paste('Buoy ER (mg ', O[2], ' m'^'-2', ' d'^'-1', ')'))) +
  geom_point(size=2, col='sienna4') +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))


Daily_ER_StratMean <- ggplot(metab.buoy.dailymean, aes(x=Strength_mean, y=ER_buoy_area)) +
  labs(x=expression(paste('Mean daily stratification strength (', degree, 'C m'^'-1', ')')), 
       y=expression(paste('Buoy ER (mg ', O[2], ' m'^'-2', ' d'^'-1', ')'))) +
  geom_point(size=2, col='sienna4') +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))


Daily_ER_StratDur <- ggplot(metab.buoy.dailymean, aes(x=StratificationDuration, y=ER_buoy_area)) +
  labs(x=expression(paste('Daily stratification duration (hr)')), 
       y=expression(paste('Buoy ER (mg ', O[2], ' m'^'-2', ' d'^'-1', ')'))) +
  geom_point(size=2, col='sienna4') +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))


ER_startgrid <- grid.arrange(grobs=list(Daily_ER_StratMax, Daily_ER_StratMean, Daily_ER_StratDur), ncol=1)

ggsave(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'Buoys', 'DailyER_Versus_Stratification.png'), plot=ER_startgrid, height=10, width=4, dpi=300, units='in')




#NEP
Daily_NEP_StratMax <- ggplot(metab.buoy.dailymean, aes(x=Strength_max, y=NEP_buoy_area)) +
  labs(x=expression(paste('Max daily stratification strength (', degree, 'C m'^'-1', ')')), 
       y=expression(paste('Buoy NEP (mg ', O[2], ' m'^'-2', ' d'^'-1', ')'))) +
  geom_point(size=2, col='grey30') +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))


Daily_NEP_StratMean <- ggplot(metab.buoy.dailymean, aes(x=Strength_mean, y=NEP_buoy_area)) +
  labs(x=expression(paste('Mean daily stratification strength (', degree, 'C m'^'-1', ')')), 
       y=expression(paste('Buoy NEP (mg ', O[2], ' m'^'-2', ' d'^'-1', ')'))) +
  geom_point(size=2, col='grey30') +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))


Daily_NEP_StratDur <- ggplot(metab.buoy.dailymean, aes(x=StratificationDuration, y=NEP_buoy_area)) +
  labs(x=expression(paste('Daily stratification duration (hr)')), 
       y=expression(paste('Buoy NEP (mg ', O[2], ' m'^'-2', ' d'^'-1', ')'))) +
  geom_point(size=2, col='grey30') +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))


NEP_startgrid <- grid.arrange(grobs=list(Daily_NEP_StratMax, Daily_NEP_StratMean, Daily_NEP_StratDur), ncol=1)

ggsave(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'Buoys', 'DailyNEP_Versus_Stratification.png'), plot=NEP_startgrid, height=10, width=4, dpi=300, units='in')



#NEP
Daily_NEP_SchmidtMax <- ggplot(metab.buoy.dailymean, aes(x=Schmidt_max, y=NEP_buoy_area)) +
  labs(x=expression(paste('Max daily Schmidt stability (unit)')), 
       y=expression(paste('Buoy NEP (mg ', O[2], ' m'^'-2', ' d'^'-1', ')'))) +
  geom_point(size=2, col='grey30') +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))


Daily_NEP_SchmidtMean <- ggplot(metab.buoy.dailymean, aes(x=Schmidt_mean, y=NEP_buoy_area)) +
  labs(x=expression(paste('Mean daily Schmidt stability (unit)')), 
       y=expression(paste('Buoy NEP (mg ', O[2], ' m'^'-2', ' d'^'-1', ')'))) +
  geom_point(size=2, col='grey30') +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))



NEP_schmidtgrid <- grid.arrange(grobs=list(Daily_NEP_SchmidtMax, Daily_NEP_SchmidtMean), ncol=1)

ggsave(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'Buoys', 'DailyNEP_Versus_Schmidt.png'), plot=NEP_schmidtgrid, height=7, width=4, dpi=300, units='in')



print(
  ggplot(merge_df_allmetab[which(!is.na(merge_df_allmetab$GPP_O18_area)),], aes(x=Schmidt_mean, y=YSI_ChlA_ugL, fill=Site)) +
    labs(y=expression(paste('Chl a (', mu, 'g L'^'-1', ')')), x=expression(paste('Schmidt mean'))) +
    # scale_x_log10() +
    scale_shape_manual(values=shapes)  +
    scale_fill_manual(values = color.palette(length(unique(merge_df_allmetab$Site)))) +
    scale_colour_manual(values = color.palette(length(unique(merge_df_allmetab$Site)))) +
    geom_point(size=2, aes(fill=Site, shape=Site)) +
    theme_bw() +
    theme(plot.title = element_text(hjust=0.5))  +
    theme(legend.position='bottom')
)



predict_vars <- c("SecchiDepth_m", "YSI_SPC_uScm", "YSI_Turb_FNU", 
                  "PO4-ppm", "TDP-ppm", "TP-ppm", "NH4-ppm", "NO3-ppm", "TDN-ppm",
                  "TN-ppm", "DOC-ppm", "DIN-ppm", "DON-ppm", "TPN-ppm", "TSS", 
                  "chla_mean", "pheo_mean", "Strength_mean", "Strength_median",
                  "Strength_min", "Strength_max", "StratificationDuration", "Schmidt_min",
                  "Schmidt_mean", "Schmidt_max", "Schmidt_median" )


GPP_buoy_data <- merge_df_allmetab %>%
  select(predict_vars, GPP_buoy_area) 

names(GPP_buoy_data) <- gsub('-', '', names(GPP_buoy_data) )
predict_vars2 <- gsub('-', '', predict_vars)

GPP_buoy_data_complete <- GPP_buoy_data[complete.cases(GPP_buoy_data),]


GPP_glm <- bestglm(GPP_buoy_data_complete,  family=gaussian, IC='BIC', nvmax=5)
summary(GPP_glm)
GPP_glm



mod.equation <- as.formula(paste('GPP_buoy_area', paste(predict_vars2[which(predict_vars2 !='chla_mean')], collapse = " + "), sep = " ~ "))

mod <- randomForest(mod.equation, data = GPP_buoy_data, importance = T, na.action = na.omit, ntree = 1000)

summary(mod)

print(randomForest::varImpPlot(mod))


ggplot(data=merge_df_allmetab, aes(x=Site, y=Schmidt_max)) +
  geom_boxplot(aes(fill=Site))
