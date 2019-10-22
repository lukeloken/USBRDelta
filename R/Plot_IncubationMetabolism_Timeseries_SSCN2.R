
#Load libraies

library(plyr)
library(dplyr)
library(tidyr)

library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(viridis)

library(lubridate)

#choose starting directory
# setwd("C:/Users/lcloken/Box/SadroLab/Incubation_Experiments")

#Load custom functions
source('R/g_legend.R')

# # Project folder where outputs are stored
# results_dir<-c("Results")
# 
# #Where data come from
# data_dir<-c("Delta_NutrientExperiment")

#Dropbox directory
dropbox_dir<-'C:/Dropbox/USBR Delta Project'


#upload merged data
merge_df_gascals <- readRDS(file=paste0(dropbox_dir, '/Data/Rdata_SSCN2/SiteData_withGas_Merged.rds'))

#Calculate photic depth (1%) from kd
merge_df_gascals$PhoticDepth_m=log(1/100)/(merge_df_gascals$kd_meters*(-1))
#Calculate 10 and 30 percent light depths
merge_df_gascals$PhoticDepth_30per=log(30/100)/(merge_df_gascals$kd_meters*(-1))
merge_df_gascals$PhoticDepth_10per=log(10/100)/(merge_df_gascals$kd_meters*(-1))


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


#Inputs for loop below. These are the numbers used for each water sample. 
#Calculate depth to 65%, 20%, and 1% Light. These are the boundaries for the rate measurments
d1 <- log(65/100)/(merge_df_gascals$Kd_est*(-1))
d2 <- log(20/100)/(merge_df_gascals$Kd_est*(-1))
d3 <- log(1/100)/(merge_df_gascals$Kd_est*(-1))

V1 = sapply(d1, function(x) sum(Depth_pred$y[which(Depth_pred$x < x/2)]))
V2 = sapply(d2, function(x) sum(Depth_pred$y[which(Depth_pred$x < x/2)])) - V1
V3 = sapply(d3, function(x) sum(Depth_pred$y[which(Depth_pred$x < x/2)])) - V2
V4 = sapply(d3, function(x) sum(Depth_pred$y[which(Depth_pred$x > x/2)])) 

#Estimate day length
sunrise<-sun.rise.set(as.POSIXct(merge_df_gascals$Date), lat=38.5064)[,1]
sunset<-sun.rise.set(as.POSIXct(merge_df_gascals$Date), lat=38.5064)[,2]
daylength<- as.numeric(sunset-sunrise, unit='hours')



#upload hypso data
Depth_df <- readRDS(file=paste0(dropbox_dir, '/Data/Rdata_SSCN2/HypsoCurveNL74.rds'))
plot(Depth_df$Area_m2, Depth_df$Depth_m, ylim=c(12,0), type='o', pch=16)

#Approximate volume by depth
Depth_df
Depth_pred <- data.frame(approx(x=Depth_df$Depth_m, y=Depth_df$Volume_m3, xo=seq(0, max(Depth_df$Depth_m), by=0.01)))
Total_volume = sum(Depth_pred$y, na.rm=T)



#Load incubation results
sitetable$site2<-paste0("site", 1:7)

resultsfiles<-list.files(paste0(dropbox_dir, "/Data/Rdata_SSCN2/IncubationMetabolism"))

results_list<-lapply(paste0(dropbox_dir, "/Data/Rdata_SSCN2/IncubationMetabolism/", resultsfiles), readRDS)

results_df<-ldply(results_list, data.frame) %>%
  group_by(SampleDate, Site, Metric, Treatment) %>%
  summarize(MeanValue = mean(Value, na.rm=T), 
            SDValue = sd(Value, na.rm=T)) %>%
  drop_na(Site)

results_df$Site <- sitetable$site1[match(results_df$Site, sitetable$site2)]

#Spread tables
GPPTable<- results_df %>%
  dplyr::select(SampleDate, Site, Metric, MeanValue, Treatment) %>%
  dplyr::filter(Metric == 'GPP') %>%
  tidyr::spread(key=Treatment, value = MeanValue)

ERTable<- results_df %>%
  dplyr::select(SampleDate, Site, Metric, MeanValue, Treatment) %>%
  dplyr::filter(Metric == 'ER') %>%
  tidyr::spread(key=Treatment, value = MeanValue)





#Loop through tables and sum daily metabolism for each depth layer
i=1
GPP_out <- c()
ER_out <-c()
for (i in 1:nrow(GPPTable)){
  Date_i <- GPPTable$SampleDate[i]
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


IncMetabDaily <- full_join(GPPTable, ERTable, by = c("SampleDate", "Site")) %>%
  group_by() %>%
  dplyr::select(SampleDate, Site, GPP_Total, ER_Total) %>%
  mutate(Site = factor(Site, levels(merge_df_gascals$Site)),
         NEP_Total = GPP_Total + ER_Total,
         DepthCode = "S") %>%
  rename(Date = SampleDate)


merge_df_IncMetab<-left_join(merge_df_gascals, IncMetabDaily)


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
  geom_vline(xintercept=fert_dates, linetype="dashed", color = "green", size=0.5),
  theme_bw(),
  theme(plot.title = element_text(hjust=0.5), legend.position="bottom", axis.title.x=element_blank())
)

ggplot(aes(x=Date, y=GPP_Total, color=Site, group=Site), data=merge_df_IncMetab[which(!is.na(merge_df_IncMetab$GPP_Total)),]) + 
  geom_point() + 
  geom_path() + 
  commonTheme_metab

ggplot(aes(x=Date, y=ER_Total, color=Site, group=Site), data=merge_df_IncMetab[which(!is.na(merge_df_IncMetab$GPP_Total)),]) + 
  geom_point() + 
  geom_path() + 
  commonTheme_metab

ggplot(aes(x=Date, y=NEP_Total, color=Site, group=Site), data=merge_df_IncMetab[which(!is.na(merge_df_IncMetab$GPP_Total)),]) + 
  geom_point() + 
  geom_path() + 
  commonTheme_metab


ggplot(aes(y=GPP_Total, x=ER_Total*(-1), colour=Site),data=merge_df_IncMetab) +
  geom_abline() + 
  geom_point() + 
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) + 
  scale_shape_manual(values=rep(21:25, 5)) + 
  theme_bw() +
  scale_x_continuous(limits=c(0,max(c(GPP_out, (ER_out*(-1)))))) +
  scale_y_continuous(limits=c(0,max(c(GPP_out, (ER_out*(-1)))))) + 
  labs(x = expression(paste("Inc ER (mg ", O[2], ' L'^'-1', ' hr'^'-1', ')')),
       y = expression(paste("Inc ER (mg ", O[2], ' L'^'-1', ' hr'^'-1', ')')))

  


#Still working below


write.csv(results_df, file=paste0(google_dir, '/SSCN2_DataOutputs/IncubationMetabolismSummary.csv'))

saveRDS(results_df, file=paste0(dropbox_dir, '/Data/Rdata_SSCN2/IncubationMetabolismSummary.rds'))



results


# ########################################
# Plotting
# 1) Timeseries of each metric/site
# 2) Boxplot of within measurement sd
# 3) Boxplot of metric/site calculations
# #######################################


# ################################################
# 1) Multi panel of metabolism estimates over time 
# Each panel is a site/metric
# X is day, y is value, color is treatment
# ################################################


#Plotting parameters
jitterwidth=0.15
# colorset<-'Dark2'
# colors<-brewer.pal(3, colorset)[c(1,3,2)]

#colors
color.palette = colorRampPalette(c(viridis(6, begin=.2, end=.98), rev(magma(5, begin=.35, end=.98))), bias=1)
colors<-color.palette(length(unique(results_df$Site)))
shapes<-rep(21:25, 5)


#Common theme for all metabolism timeseries panels
commonTheme<-list(
  # scale_colour_manual(values = colors),
  # scale_fill_manual(values = colors),
  # geom_smooth(method='loess',  se=F),
  # geom_smooth(method='auto', se=T, alpha=.2),
  # geom_jitter(size=2, width=jitterwidth, height=0),
  theme_bw(),
  theme(plot.title = element_text(hjust=0.5), legend.position="bottom", axis.title.x=element_blank())
)

# Make a table to determine how many panels are needed
# Each panel is a site and a metric
uniquetable<-unique(results_df[c('Metric', 'Site')])
uniquetable$Metric<-factor(uniquetable$Metric, c('NEP', 'ER', 'GPP'))
uniquetable<-uniquetable[order(uniquetable$Site),]
uniquetable<-uniquetable[order(uniquetable$Metric),]

ranges<-sapply(uniquetable$Metric, function(x) extendrange(results_df$MeanValue[results_df$Metric ==x], f=0.05))

# Loop through metrics and sites and make a gg object
plot_list<-list()
plot_nu<-1
for (plot_nu in 1:nrow(uniquetable)){
  
  site<-uniquetable$Site[plot_nu]
  metric<-uniquetable$Metric[plot_nu]
  col<-c(colors,colors)[plot_nu]
  shape<-rep(shapes[1:9],2)[plot_nu]
  
  table<-results_df[results_df$Metric==metric & 
                                  results_df$Site==site,]
  
  plot_list[[plot_nu]] <- ggplot(table, aes(SampleDate, MeanValue, fill=Treatment)) + 
    labs(x='Date', y=metric) +
    # ggtitle(site) +
    geom_path(aes(color=Treatment), size=1.5) + 
    geom_point(aes(color=Treatment, shape=Treatment), col='black', size=3) +
    # ylim(ranges[1,plot_nu], ranges[2,plot_nu]) +
    # scale_y_continuous(limits=ranges[,plot_nu]) +
    coord_cartesian(ylim=ranges[,plot_nu]) + 
    commonTheme 
  
}


#Old code below. Do not run

#Add and extract legend from first plot
# NEP <- ggplot(summary_df[which(summary_df$Metric=='NEP'),], aes(Date, mean, color=Site, fill=Site)) + 
#   labs(x='Date', y=expression(paste('NEP (mg ', O[2], ' L'^'-1', ' hr'^'-1', ')'))) +
#   scale_fill_manual(values = colors) +
#   scale_colour_manual(values = colors) +
#   scale_shape_manual(values=rep(21:25, 5))  + 
#   ggtitle('NEP') +
#   geom_path(size=1.5) + 
#   geom_point(size=3, col='black', aes(fill=Site, shape=Site)) +
#   # ylim(ranges[1,plot_nu], ranges[2,plot_nu]) +
#   # scale_y_continuous(limits=ranges[,plot_nu]) +
#   coord_cartesian(ylim=ranges[,1]) + 
#   theme_bw() + 
#   theme(plot.title = element_text(hjust=0.5), legend.position="none")
# 
# ER <- ggplot(summary_df[which(summary_df$Metric=='ER'),], aes(Date, mean, color=Site, fill=Site)) + 
#   labs(x='Date', y=expression(paste('ER (mg ', O[2], ' L'^'-1', ' hr'^'-1', ')'))) +
#   scale_fill_manual(values = colors) +
#   scale_colour_manual(values = colors) +
#   scale_shape_manual(values=rep(21:25, 5))  + 
#   ggtitle('ER') +
#   geom_path(size=1.5) + 
#   geom_point(size=3, col='black', aes(fill=Site, shape=Site)) +
#   # ylim(ranges[1,plot_nu], ranges[2,plot_nu]) +
#   # scale_y_continuous(limits=ranges[,plot_nu]) +
#   coord_cartesian(ylim=ranges[,10]) + 
#   theme_bw() + 
#   theme(plot.title = element_text(hjust=0.5), legend.position="none")
# 
# 
# 
# plot_withlegend <- NEP + 
#   theme(plot.title = element_text(hjust=0.5), legend.position="bottom")
# 
#   
# 
# 
# mylegend<-g_legend(plot_withlegend)
# 
# # p_cols1<-grid.arrange(grobs=plot_list[c(1,4,7,11)], ncol=1, as.table=F, top = "GPP")
# 
# # arrange plots without legend
# p2<-grid.arrange(grobs=plot_list, ncol=length(unique(uniquetable$Metric)), as.table=F)
# 
# #Add legend to bottom of figure and save
# png(paste0(dropbox_dir, '/Figures/NutrientExperiment/IncubationMetabolism/MetabolismTimeseries_bySite.png'), width=6, height=12, units='in', res=200)
# 
# grid.arrange(p2, mylegend, nrow=2,heights=c(10, length(unique(uniquetable$Site))/16))
# 
# dev.off()
# 
# 
# #Add legend to bottom of figure and save
# png(paste0(dropbox_dir, '/Figures/NutrientExperiment/IncubationMetabolism/MetabolismTimeseries.png'), width=6, height=6, units='in', res=200)
# 
# grid.arrange(NEP, ER, mylegend, nrow=3, heights=c(5,5,1))
# 
# dev.off()
