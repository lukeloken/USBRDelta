
#Load libraies

library(plyr)
library(dplyr)
library(tidyr)

library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(viridis)

library(lubridate)
library(LakeMetabolizer)



#upload merged data
merge_df_gascals <- readRDS(file=file.path(onedrive_dir, 'RData', 'NutrientExperiment2', 'SiteData_withGas_Merged.rds'))

#Calculate photic depth (1%) from kd
# merge_df_gascals$PhoticDepth_m=log(1/100)/(merge_df_gascals$kd_meters*(-1))
# #Calculate 10 and 30 percent light depths
# merge_df_gascals$PhoticDepth_30per=log(30/100)/(merge_df_gascals$kd_meters*(-1))
# merge_df_gascals$PhoticDepth_10per=log(10/100)/(merge_df_gascals$kd_meters*(-1))

#Add photic depth and light extinction
kd_alldates<-readRDS(file=file.path(onedrive_dir, 'RData', 'NutrientExperiment2', 'SSCN2_Kd.rds'))
merge_df_gascals<-full_join(merge_df_gascals, kd_alldates)


#Model light extinction and photic depth using Secchi Depth or Turbidity
Turb<-merge_df_gascals$YSI_Turb_FNU
Kd<-merge_df_gascals$kd_meters
PD<-merge_df_gascals$PhoticDepth_m
SD<-merge_df_gascals$SecchiDepth_m

PD_SD_model<-lm(PD~SD)
PD_Turb_model<-lm(PD~log10(Turb))

InverseKd_SD_model <- lm(1/Kd ~ SD)
InverseKd_Turb_model<-lm(1/Kd~log10(Turb))

#Best Kd predictor
plot(1/Kd ~ SD)
abline(InverseKd_SD_model)

plot(1/Kd ~ log10(Turb))
abline(InverseKd_Turb_model)

#Best Photic Depth predictor
plot(PD ~ SD)
abline(PD_SD_model)

plot(PD ~ log10(Turb))
abline(PD_Turb_model)


merge_df_gascals$PD_est<-predict(PD_SD_model, newdata=data.frame(SD))
merge_df_gascals$PD_est[which(is.na(SD) &  merge_df_gascals$DepthCode == 'S')]<- predict(PD_Turb_model, newdata=data.frame(log10(Turb)))[which(is.na(SD) &  merge_df_gascals$DepthCode == 'S')]

merge_df_gascals$Kd_est<-1/predict(InverseKd_SD_model, newdata=data.frame(SD))
merge_df_gascals$Kd_est[which(is.na(SD) &  merge_df_gascals$DepthCode == 'S')]<- 1/predict(InverseKd_Turb_model, newdata=data.frame(log10(Turb)))[which(is.na(SD) &  merge_df_gascals$DepthCode == 'S')]

#Estimate 10 and 30 percent light depths using estimated kd
# merge_df_gascals$PD30_est = log(30/100)/(merge_df_gascals$Kd_est*(-1))
# merge_df_gascals$PD10_est = log(10/100)/(merge_df_gascals$Kd_est*(-1))
merge_df_gascals$PD01_est = log(1/100)/(merge_df_gascals$Kd_est*(-1))


rm(Turb, PD, SD, Kd, PD_SD_model, PD_Turb_model, InverseKd_SD_model, InverseKd_Turb_model)


# #######################################################################
#Inputs for loop below. These are the numbers used for each water sample.
# ####################################################################### 


#Estimate day length
sunrise<-sun.rise.set(as.POSIXct(merge_df_gascals$Date), lat=38.5064)[,1]
sunset<-sun.rise.set(as.POSIXct(merge_df_gascals$Date), lat=38.5064)[,2]
daylength<- as.numeric(sunset-sunrise, unit='hours')



#upload hypso data
Depth_df <- readRDS(file=file.path(onedrive_dir, 'RData', 'HypsoCurveNL74.rds'))
plot(Depth_df$Area_m2, Depth_df$Depth_m, ylim=c(12,0), type='o', pch=16)

#Approximate volume by depth
# Depth_df
Depth_pred <- data.frame(approx(x=Depth_df$Depth_m, y=Depth_df$Volume_m3, xo=seq(0, max(Depth_df$Depth_m), by=0.01))) %>%
  mutate(y= y/50)
 
Total_volume = sum(Depth_df$Volume_m3, na.rm=T)
Surface_area = Depth_df$Area_m2[which(Depth_df$Depth_m==0)]
Mean_depth <- Total_volume/Surface_area


#Calculate depth to 65%, 20%, and 1% Light. These are the boundaries for the rate measurments
d1 <- log(65/100)/(merge_df_gascals$Kd_est*(-1))
d2 <- log(20/100)/(merge_df_gascals$Kd_est*(-1))
d3 <- log(1/100)/(merge_df_gascals$Kd_est*(-1))

V1 = sapply(d1, function(x) sum(Depth_pred$y[which(Depth_pred$x < x/2)]))
V2 = sapply(d2, function(x) sum(Depth_pred$y[which(Depth_pred$x < x/2)])) - V1
V3 = sapply(d3, function(x) sum(Depth_pred$y[which(Depth_pred$x < x/2)])) - V2
V4 = sapply(d3, function(x) sum(Depth_pred$y[which(Depth_pred$x > x/2)])) 



#Load incubation results
sitetable$site2<-paste0("site", 1:7)

resultsfiles<-list.files(file.path(onedrive_dir, "RData", "NutrientExperiment2", "IncubationMetabolism"))

results_list<-lapply(file.path(onedrive_dir, "RData", "NutrientExperiment2", "IncubationMetabolism", resultsfiles), readRDS)

results_df<-ldply(results_list, data.frame) %>%
  group_by(SampleDate, Site, Metric, Treatment) %>%
  dplyr::summarize(MeanValue = mean(Value, na.rm=T), 
            SDValue = sd(Value, na.rm=T)) %>%
  drop_na(Site) %>%
  dplyr::rename(Date = SampleDate)

results_df$Site <- sitetable$site1[match(results_df$Site, sitetable$site2)]
results_df$Site <-factor(results_df$Site, sitetable$site1)

results_df2<-merge_df_gascals %>%
  filter(DepthCode=="S") %>%
  dplyr::select(Site, Date, chla_mean, `NO3-ppm`) %>%
  group_by(Site, Date) %>%
  right_join(results_df) %>%
  dplyr::rename(ChlAJar = chla_mean,
         NO3Jar = `NO3-ppm`) %>%
  mutate(Metric = factor(Metric, c('GPP', 'ER', 'NEP')))

#Spread tables
GPPTable<- results_df %>%
  dplyr::select(Date, Site, Metric, MeanValue, Treatment) %>%
  dplyr::filter(Metric == 'GPP') %>%
  tidyr::spread(key=Treatment, value = MeanValue)

ERTable<- results_df %>%
  dplyr::select(Date, Site, Metric, MeanValue, Treatment) %>%
  dplyr::filter(Metric == 'ER') %>%
  tidyr::spread(key=Treatment, value = MeanValue)





#Loop through tables and sum daily metabolism for each depth layer
i=1
GPP_out <- c()
ER_out <-c()
for (i in 1:nrow(GPPTable)){
  Date_i <- GPPTable$Date[i]
  Site_i <- GPPTable$Site[i]
  
  row_i<-which(merge_df_gascals$Date == Date_i & merge_df_gascals$Site == Site_i & merge_df_gascals$DepthCode=="S")
  
  V1_i <- V1[row_i]
  V2_i <- V2[row_i]
  V3_i <- V3[row_i]
  V4_i <- V4[row_i]
  
  daylength_i <- daylength[row_i]
  
  GPP_Total_i = daylength_i/24*((GPPTable$`100`[i]* V1_i) + 
                                  (GPPTable$`30`[i]* V2_i) + 
                                  (GPPTable$`10`[i]* V3_i)) / (Total_volume)
  
  ER_Total_i = ((ERTable$`100`[i]* V1_i) + 
                  (ERTable$`30`[i]* V2_i) + 
                  (ERTable$`10`[i]* (V3_i + V4_i))) / (Total_volume)
  
  GPP_out[i] <- GPP_Total_i
  ER_out[i] <- ER_Total_i
  
}

GPPTable$GPP_Total <- GPP_out
ERTable$ER_Total <- ER_out

# Metabolism is in mg O2 L-1 hr-1
IncMetabDaily <- full_join(GPPTable, ERTable, by = c("Date", "Site")) %>%
  group_by() %>%
  dplyr::select(Date, Site, GPP_Total, ER_Total) %>%
  mutate(Site = factor(Site, levels(merge_df_gascals$Site)),
         NEP_Total = GPP_Total + ER_Total) %>%
  #Convert to areal rates
  # g O2 per m2 per d
  mutate(GPP_Total_area = GPP_Total*Mean_depth*24,
         ER_Total_area = ER_Total*Mean_depth*24,
         NEP_Total_area = NEP_Total*Mean_depth*24,
         DepthCode = "S")

merge_df_IncMetab<-left_join(merge_df_gascals, IncMetabDaily)


#Save incubation results
write.csv(merge_df_IncMetab,  file=file.path(onedrive_dir, 'OutputData', 'NutrientExperiment2', 'SiteData_withIncMetab_Merged.csv'))
saveRDS(merge_df_IncMetab, file=file.path(onedrive_dir, 'Rdata', 'NutrientExperiment2', 'SiteData_withIncMetab_Merged.rds'))

write.csv(results_df, file=file.path(onedrive_dir, 'OutputData', 'NutrientExperiment2', 'IncubationMetabolismSummary.csv'))
saveRDS(results_df, file=file.path(onedrive_dir, 'Rdata', 'NutrientExperiment2', 'IncubationMetabolismSummary.rds'))





# ########################################
# Plotting
# 1) Timeseries of each metric/site
# 2) Boxplot of metric by site
# #######################################


color.palette = colorRampPalette(c(viridis(6, begin=.2, end=.98), rev(magma(5, begin=.35, end=.98))), bias=1)
colors<-color.palette(length(levels(merge_df_IncMetab$Site)))
shapes<-rep(21:25, 5)


#Common theme for all metabolism timeseries panels
commonTheme_metab<-list(
  scale_colour_manual(values = colors),
  scale_fill_manual(values = colors),
  scale_shape_manual(values=rep(21:25, 5)),
  # geom_smooth(method='loess',  se=F),
  # geom_smooth(method='auto', se=T, alpha=.2),
  # geom_jitter(size=2, width=jitterwidth, height=0, aes(fill=Site, shape=Site)),
  # geom_vline(xintercept=fert_dates, linetype="dashed", color = "green", size=0.5),
  geom_vline(xintercept=(fert_dates), linetype="dashed", color = "green", size=1),
  geom_hline(yintercept=0, color='lightgrey', linetype=1.5, size=1), 
  theme_bw(),
  theme(plot.title = element_text(hjust=0.5), legend.position="bottom", axis.title.x=element_blank())
)



#Timeseries volumetric rate
p1<-ggplot(aes(x=Date, y=GPP_Total, color=Site, group=Site, shape=Site, fill=Site), data=merge_df_IncMetab[which(!is.na(merge_df_IncMetab$GPP_Total)),]) + 
  commonTheme_metab + 
  # geom_hline(yintercept=0) + 
  geom_point(colour='black', size=2) + 
  geom_path() + 
  theme(legend.position='none') + 
  labs(y=expression(paste('Inc GPP (mg ', O[2], ' L'^'-1', ' hr'^'-1', ')')))

p2<-ggplot(aes(x=Date, y=ER_Total, color=Site, group=Site, shape=Site, fill=Site), data=merge_df_IncMetab[which(!is.na(merge_df_IncMetab$GPP_Total)),]) + 
  commonTheme_metab + 
  # geom_hline(yintercept=0) + 
  geom_point(colour='black', size=2) + 
  geom_path() + 

  theme(legend.position='none')+ 
  labs(y=expression(paste('Inc ER (mg ', O[2], ' L'^'-1', ' hr'^'-1', ')')))

p3<-ggplot(aes(x=Date, y=NEP_Total, color=Site, group=Site, shape=Site, fill=Site), data=merge_df_IncMetab[which(!is.na(merge_df_IncMetab$GPP_Total)),]) + 
  commonTheme_metab + 
  # geom_hline(yintercept=0) + 
  geom_point(colour='black', size=2) + 
  geom_path() + 

  theme(legend.position='none')+ 
  labs(y=expression(paste('Inc NEP (mg ', O[2], ' L'^'-1', ' hr'^'-1', ')')))


plot_withlegend <- p1 + 
  theme(legend.position="bottom", legend.title=element_blank()) +
  guides(color = guide_legend(nrow = 1))

mylegend<-g_legend(plot_withlegend)


plot3<-grid.arrange(grobs=list(p1, p2, p3), ncol=1, as.table=F)


#Add legend to bottom of figure and save
png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'IncubationMetabolismTimeseries.png'), width=10, height=8, units='in', res=200)

grid.arrange(plot3, mylegend, nrow=2, heights=c(15,1))

dev.off()



#Timeseries areal rate
p1<-ggplot(aes(x=Date, y=GPP_Total_area, color=Site, group=Site, shape=Site, fill=Site), data=merge_df_IncMetab[which(!is.na(merge_df_IncMetab$GPP_Total_area)),]) + 
  commonTheme_metab + 
  # geom_hline(yintercept=0) + 
  geom_point(colour='black', size=2) + 
  # geom_path() + 
  geom_line(size=1, aes(colour=Site,  group=Site)) +    
  theme(legend.position='none') + 
  labs(y=expression(paste('Inc GPP (g ', O[2], ' m'^'-2', ' d'^'-1', ')')))

p2<-ggplot(aes(x=Date, y=ER_Total_area, color=Site, group=Site, shape=Site, fill=Site), data=merge_df_IncMetab[which(!is.na(merge_df_IncMetab$GPP_Total_area)),]) + 
  commonTheme_metab + 
  # geom_hline(yintercept=0) + 
  geom_point(colour='black', size=2) + 
  # geom_path() + 
  geom_line(size=1, aes(colour=Site,  group=Site)) +    
  theme(legend.position='none')+ 
  labs(y=expression(paste('Inc ER (g ', O[2], ' m'^'-2', ' d'^'-1', ')')))

p3<-ggplot(aes(x=Date, y=NEP_Total_area, color=Site, group=Site, shape=Site, fill=Site), data=merge_df_IncMetab[which(!is.na(merge_df_IncMetab$GPP_Total_area)),]) + 
  commonTheme_metab + 
  # geom_hline(yintercept=0) + 
  geom_point(colour='black', size=2) + 
  geom_line(size=1, aes(colour=Site,  group=Site)) +    
  # geom_path() + 
  theme(legend.position='none')+ 
  labs(y=expression(paste('Inc NEP (g ', O[2], ' m'^'-2', ' d'^'-1', ')')))


plot_withlegend <- p2 + 
  theme(legend.position="bottom", legend.title=element_blank()) +
  guides(color = guide_legend(nrow = 2))

mylegend<-g_legend(plot_withlegend)


plot3<-grid.arrange(grobs=list(p3, p1, p2), ncol=1, as.table=F)


#Add legend to bottom of figure and save
png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'IncubationMetabolism_Area_Timeseries.png'), width=5, height=7, units='in', res=200)

grid.newpage()
plots<-grid.draw(rbind(ggplotGrob(p3), ggplotGrob(p1), ggplotGrob(plot_withlegend), size = "first"))

# grid.arrange(plot3, mylegend, nrow=2, heights=c(15,1))

dev.off()


#Areal rates boxplot
commonBox<-list(
  geom_vline(xintercept=fert_dates, color='green', linetype=2, size=1),
  geom_hline(yintercept=0, color='lightgrey', linetype=1.5, size=1), 
  theme_bw(),
  theme(plot.title = element_text(hjust=0.5), legend.position="none", axis.title.x=element_blank()), 
  scale_x_date(date_minor_breaks= "weeks", date_breaks = "2 weeks", date_labels="%b %d"), 
  guides(color = guide_legend(nrow = 1, title.position='top', title.hjust=0.5)) 
)



GPPbox<-ggplot(aes(x=Date, group=Date, y=GPP_Total_area), data=merge_df_IncMetab[which(!is.na(merge_df_IncMetab$GPP_Total_area)),]) + 
  commonBox +
  labs(x='Date', y=expression(paste('Daily GPP (g ', O[2], ' m'^'-2', ' d'^'-1', ')'))) +
  geom_boxplot(fill='darkgreen', outlier.size=0.5) 

ERbox<-ggplot(aes(x=Date, group=Date, y=ER_Total_area), data=merge_df_IncMetab[which(!is.na(merge_df_IncMetab$GPP_Total_area)),]) + 
  commonBox +
  labs(x='Date', y=expression(paste('Daily ER (g ', O[2], ' m'^'-2', ' d'^'-1', ')'))) +
  geom_boxplot(fill='sienna4', outlier.size=0.5) 

NEPbox<-ggplot(aes(x=Date, group=Date, y=NEP_Total_area), data=merge_df_IncMetab[which(!is.na(merge_df_IncMetab$GPP_Total_area)),]) + 
  commonBox +
  labs(x='Date', y=expression(paste('Daily NEP (g ', O[2], ' m'^'-2', ' d'^'-1', ')'))) +
  geom_boxplot(fill='grey30', outlier.size=0.5) 


png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'IncubationMetabolism_Boxplot_TS.png'), width=5, height=7, units='in', res=200)


grid.newpage()
boxes<-grid.draw(rbind(ggplotGrob(GPPbox), ggplotGrob(ERbox),  ggplotGrob(NEPbox), size = "first"))

dev.off()


#Scatterplot ER vs GPP
png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'IncubationMetabolismScatterplotGPPER.png'), width=4.5, height=4, units='in', res=200)

print(
  ggplot(aes(y=GPP_Total, x=ER_Total*(-1), fill=Site, shape=Site),data=merge_df_IncMetab) +
  geom_abline() + 
  geom_point() + 
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) + 
  scale_shape_manual(values=rep(21:25, 5)) + 
  theme_bw() +
  scale_x_continuous(limits=c(0,max(c(GPP_out, (ER_out*(-1)))))) +
  scale_y_continuous(limits=c(0,max(c(GPP_out, (ER_out*(-1)))))) + 
  labs(x = expression(paste("Inc ER (mg ", O[2], ' L'^'-1', ' hr'^'-1', ')')),
       y = expression(paste("Inc GPP (mg ", O[2], ' L'^'-1', ' hr'^'-1', ')')))+
  theme(legend.position = 'right', legend.title = element_blank())
)

dev.off()



#Scatterplot of other drivers (chla, turb, etc versus metabolism)

p1 <- ggplot(aes(y=GPP_Total, x=chla_mean, fill=Site, shape=Site),data=merge_df_IncMetab) +
  geom_point(size=2) + 
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) + 
  scale_shape_manual(values=rep(21:25, 5)) + 
  theme_bw() +
  labs(x = expression(paste("Chl a (", mu, 'g L'^'-1', ')')),
       y = expression(paste("Inc GPP (mg ", O[2], ' L'^'-1', ' hr'^'-1', ')'))) + 
  theme(legend.position='none')

p2 <- ggplot(aes(y=GPP_Total, x=YSI_Turb_FNU, fill=Site, shape=Site),data=merge_df_IncMetab) +
  geom_point(size=2) + 
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) + 
  scale_shape_manual(values=rep(21:25, 5)) + 
  theme_bw() +
  labs(x = expression(paste("Turbidity (FNU)")),
       y = expression(paste("Inc GPP (mg ", O[2], ' L'^'-1', ' hr'^'-1', ')'))) + 
  theme(legend.position='none')

p3 <- ggplot(aes(y=GPP_Total, x=`NO3-ppm`, fill=Site, shape=Site),data=merge_df_IncMetab[-which(is.na(merge_df_IncMetab$GPP_Total)),]) +
  geom_point(size=2) + 
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) + 
  scale_shape_manual(values=rep(21:25, 5)) + 
  theme_bw() +
  labs(x = expression(paste(NO[3], " mg N L"^"-1", ")")),
       y = expression(paste("Inc GPP (mg ", O[2], ' L'^'-1', ' hr'^'-1', ')'))) + 
  theme(legend.position='none')


p4 <- ggplot(aes(y=ER_Total, x=chla_mean, fill=Site, shape=Site),data=merge_df_IncMetab) +
  geom_point(size=2) + 
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) + 
  scale_shape_manual(values=rep(21:25, 5)) + 
  theme_bw() +
  labs(x = expression(paste("Chl a (", mu, 'g L'^'-1', ')')),
       y = expression(paste("Inc ER (mg ", O[2], ' L'^'-1', ' hr'^'-1', ')'))) + 
  theme(legend.position='none')

p5 <- ggplot(aes(y=ER_Total, x=YSI_Turb_FNU, fill=Site, shape=Site),data=merge_df_IncMetab) +
  geom_point(size=2) + 
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) + 
  scale_shape_manual(values=rep(21:25, 5)) + 
  theme_bw() +
  labs(x = expression(paste("Turbidity (FNU)")),
       y = expression(paste("Inc ER (mg ", O[2], ' L'^'-1', ' hr'^'-1', ')'))) + 
  theme(legend.position='none')

p6 <- ggplot(aes(y=ER_Total, x=`NO3-ppm`, fill=Site, shape=Site),data=merge_df_IncMetab[-which(is.na(merge_df_IncMetab$ER_Total)),]) +
  geom_point(size=2) + 
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) + 
  scale_shape_manual(values=rep(21:25, 5)) + 
  theme_bw() +
  labs(x = expression(paste(NO[3], " mg N L"^"-1", ")")),
       y = expression(paste("Inc ER (mg ", O[2], ' L'^'-1', ' hr'^'-1', ')')))+ 
  theme(legend.position='none')

p7 <- ggplot(aes(y=NEP_Total, x=chla_mean, fill=Site, shape=Site),data=merge_df_IncMetab) +
  geom_point(size=2) + 
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) + 
  scale_shape_manual(values=rep(21:25, 5)) + 
  theme_bw() +
  labs(x = expression(paste("Chl a (", mu, 'g L'^'-1', ')')),
       y = expression(paste("Inc NEP (mg ", O[2], ' L'^'-1', ' hr'^'-1', ')')))+ 
  theme(legend.position='none')

p8 <- ggplot(aes(y=NEP_Total, x=YSI_Turb_FNU, fill=Site, shape=Site),data=merge_df_IncMetab) +
  geom_point(size=2) + 
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) + 
  scale_shape_manual(values=rep(21:25, 5)) + 
  theme_bw() +
  labs(x = expression(paste("Turbidity (FNU)")),
       y = expression(paste("Inc NEP (mg ", O[2], ' L'^'-1', ' hr'^'-1', ')'))) + 
  theme(legend.position='none')

p9 <-ggplot(aes(y=NEP_Total, x=`NO3-ppm`, fill=Site, shape=Site),data=merge_df_IncMetab[-which(is.na(merge_df_IncMetab$NEP_Total)),]) +
  geom_point(size=2) + 
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) + 
  scale_shape_manual(values=rep(21:25, 5)) + 
  theme_bw() +
  labs(x = expression(paste(NO[3], " mg N L"^"-1", ")")),
       y = expression(paste("Inc NEP (mg ", O[2], ' L'^'-1', ' hr'^'-1', ')'))) + 
  theme(legend.position='none')


plot6<-grid.arrange(grobs=list(p1, p2, p3, p4, p5, p6, p7, p8, p9), ncol=3, as.table=T)

plot_withlegend <- p1 + 
  theme(legend.position="bottom", legend.title=element_blank()) +
  guides(fill = guide_legend(nrow = 1))

mylegend<-g_legend(plot_withlegend)


png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'IncubationMetabolismScatterplotDrivers.png'), width=11, height=8.5, units='in', res=200)

grid.arrange(plot6, mylegend, nrow=2, heights=c(15,1))

dev.off()




#boxplots of metabolism by site

p1 <- ggplot(aes(y=GPP_Total, x=Site, fill=Site),data=merge_df_IncMetab) +
  geom_hline(yintercept=0) + 
  geom_boxplot() + 
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) + 
  # scale_shape_manual(values=rep(21:25, 5)) + 
  theme_bw() +
  labs(x = expression(paste("Site")),
       y = expression(paste("Inc GPP (mg ", O[2], ' L'^'-1', ' hr'^'-1', ')'))) + 
  theme(legend.position='none', axis.title.x=element_blank())


p2<- ggplot(aes(y=ER_Total, x=Site, fill=Site),data=merge_df_IncMetab) +
  geom_hline(yintercept=0) + 
  geom_boxplot() + 
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) + 
  # scale_shape_manual(values=rep(21:25, 5)) + 
  theme_bw() +
  labs(x = expression(paste("Site")),
       y = expression(paste("Inc ER (mg ", O[2], ' L'^'-1', ' hr'^'-1', ')'))) + 
  theme(legend.position='none', axis.title.x=element_blank())

p3<-ggplot(aes(y=NEP_Total, x=Site, fill=Site),data=merge_df_IncMetab) +
  geom_hline(yintercept=0) + 
  geom_boxplot() + 
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) + 
  # scale_shape_manual(values=rep(21:25, 5)) + 
  theme_bw() +
  labs( y = expression(paste("Inc NEP (mg ", O[2], ' L'^'-1', ' hr'^'-1', ')'))) + 
  theme(legend.position='none', axis.title.x=element_blank())


png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'IncubationMetabolismBoxplotBySite.png'), width=4, height=8.5, units='in', res=200)

box3<-grid.arrange(grobs=list(p1, p2, p3), ncol=1, as.table=T)

dev.off()





png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'IncubationMetabolismByChlAJar.png'), width=8.5, height=8, units='in', res=200)

ggplot(aes(y=MeanValue, x=ChlAJar), data=results_df2) + 
  geom_hline(yintercept=0) + 
  geom_smooth(method='lm', formula=y~x, fill=NA, linetype='dashed' , color='black') + 
  geom_point(size=2, aes(fill=Site, shape=Site)) + 
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) + 
  scale_shape_manual(values=rep(21:25, 5)) + 
  facet_grid(Metric ~Treatment, scales='free_y') + 
  theme_bw() + 
  labs(y=expression(paste('(mg ', O[2], ' L'^'-1', ' hr'^'-1', ')')), 
       x=expression(paste('Jar Chl a (', mu, 'g L'^'-1', ')'))) + 
  ggtitle('Jar metabolism by light level and jar chlorophyll')

dev.off()


png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'IncubationMetabolismByNO3Jar.png'), width=8.5, height=8, units='in', res=200)

ggplot(aes(y=MeanValue, x=NO3Jar), data=results_df2) + 
  geom_hline(yintercept=0) + 
  geom_smooth(method='lm', formula=y~x, fill=NA, linetype='dashed' , color='black') + 
  geom_point(size=2, aes(fill=Site, shape=Site)) + 
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) + 
  scale_shape_manual(values=rep(21:25, 5)) + 
  facet_grid(Metric ~Treatment, scales='free_y') + 
  theme_bw() + 
  labs(y=expression(paste('(mg ', O[2], ' L'^'-1', ' hr'^'-1', ')')), 
       x=expression(paste('Jar ', NO[3], ' (mg N L'^'-1', ')'))) + 
  ggtitle('Jar metabolism by light level and jar nitrate')

dev.off()


jarmodel_GPP<-lm(MeanValue ~ ChlAJar*NO3Jar + Treatment, data=results_df2[results_df2$Metric=='GPP',])
jarmodel_ER<-lm(MeanValue ~ ChlAJar*NO3Jar + Treatment, data=results_df2[results_df2$Metric=='ER',])
jarmodel_NEP<-lm(MeanValue ~ ChlAJar*NO3Jar + Treatment, data=results_df2[results_df2$Metric=='NEP',])

summary(jarmodel_GPP)
anova(jarmodel_GPP)

summary(jarmodel_ER)
anova(jarmodel_ER)

summary(jarmodel_NEP)
anova(jarmodel_NEP)


jarmodel_P<-lm(MeanValue ~ ChlAJar + as.numeric(as.character(Treatment)), data=results_df2[results_df2$Metric=='GPP',])
summary(jarmodel_P)

