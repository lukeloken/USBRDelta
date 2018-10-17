
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

resultsfiles<-list.files(paste0(dropbox_dir, "/Data/NutrientExperiment/IncubationMetabolism/MetabolismCalculations"))

summaryfiles<-resultsfiles[grep('SummaryMetabolism', resultsfiles)]

summary_list<-lapply(paste0(dropbox_dir, "/Data/NutrientExperiment/IncubationMetabolism/MetabolismCalculations/", summaryfiles), read.csv)

Dates<-gsub("[^0-9]", "", summaryfiles) 
names(summary_list)<-Dates

summary_df<-ldply(summary_list, data.frame)

summary_df$Date<-as.Date(summary_df$'.id', format='%Y%m%d')

summary_df$Site<-factor(summary_df$Site, c('NL70', 'EC2', 'EC3', 'EC4', 'EC5', 'EC6', 'EC7', 'EC8', 'NL76'))

write.csv(summary_df, file=paste0(dropbox_dir, '/Data/NutrientExperiment/IncubationMetabolism/', 'LightDarkMetabolism.csv'))

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
colors<-color.palette(length(unique(summary_df$Site)))
shapes<-rep(21:25, 5)


#Common theme for all metabolism timeseries panels
commonTheme<-list(
  # scale_colour_manual(values = colors),
  # scale_fill_manual(values = colors),
  # geom_smooth(method='loess',  se=F),
  # geom_smooth(method='auto', se=T, alpha=.2),
  # geom_jitter(size=2, width=jitterwidth, height=0),
  theme_bw(),
  theme(plot.title = element_text(hjust=0.5), legend.position="none", axis.title.x=element_blank())
)

# Make a table to determine how many panels are needed
# Each panel is a site and a metric
uniquetable<-unique(summary_df[c('Metric', 'Site')])
uniquetable$Metric<-factor(uniquetable$Metric, c('NEP', 'ER'))
uniquetable<-uniquetable[order(uniquetable$Site),]
uniquetable<-uniquetable[order(uniquetable$Metric),]

ranges<-sapply(uniquetable$Metric, function(x) extendrange(summary_df$mean[summary_df$Metric ==x], f=0.05))

# Loop through metrics and sites and make a gg object
plot_list<-list()
plot_nu<-10
for (plot_nu in 1:nrow(uniquetable)){
  
  site<-uniquetable$Site[plot_nu]
  metric<-uniquetable$Metric[plot_nu]
  col<-c(colors,colors)[plot_nu]
  shape<-rep(shapes[1:9],2)[plot_nu]
  
  table<-summary_df[summary_df$Metric==metric & 
                                  summary_df$Site==site,]
  
  plot_list[[plot_nu]] <- ggplot(table, aes(Date, mean)) + 
    labs(x='Date', y=metric) +
    # ggtitle(site) +
    geom_path(color=col, size=1.5) + 
    geom_point(color='black', fill=col, size=3, shape=shape) +
    # ylim(ranges[1,plot_nu], ranges[2,plot_nu]) +
    # scale_y_continuous(limits=ranges[,plot_nu]) +
    coord_cartesian(ylim=ranges[,plot_nu]) + 
    commonTheme 
  
}

#Add and extract legend from first plot
NEP <- ggplot(summary_df[which(summary_df$Metric=='NEP'),], aes(Date, mean, color=Site, fill=Site)) + 
  labs(x='Date', y=expression(paste('NEP (mg ', O[2], ' L'^'-1', ' hr'^'-1', ')'))) +
  scale_fill_manual(values = colors) +
  scale_colour_manual(values = colors) +
  scale_shape_manual(values=rep(21:25, 5))  + 
  ggtitle('NEP') +
  geom_path(size=1.5) + 
  geom_point(size=3, col='black', aes(fill=Site, shape=Site)) +
  # ylim(ranges[1,plot_nu], ranges[2,plot_nu]) +
  # scale_y_continuous(limits=ranges[,plot_nu]) +
  coord_cartesian(ylim=ranges[,1]) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5), legend.position="none")

ER <- ggplot(summary_df[which(summary_df$Metric=='ER'),], aes(Date, mean, color=Site, fill=Site)) + 
  labs(x='Date', y=expression(paste('ER (mg ', O[2], ' L'^'-1', ' hr'^'-1', ')'))) +
  scale_fill_manual(values = colors) +
  scale_colour_manual(values = colors) +
  scale_shape_manual(values=rep(21:25, 5))  + 
  ggtitle('ER') +
  geom_path(size=1.5) + 
  geom_point(size=3, col='black', aes(fill=Site, shape=Site)) +
  # ylim(ranges[1,plot_nu], ranges[2,plot_nu]) +
  # scale_y_continuous(limits=ranges[,plot_nu]) +
  coord_cartesian(ylim=ranges[,10]) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5), legend.position="none")



plot_withlegend <- NEP + 
  theme(plot.title = element_text(hjust=0.5), legend.position="bottom")

  


mylegend<-g_legend(plot_withlegend)

# p_cols1<-grid.arrange(grobs=plot_list[c(1,4,7,11)], ncol=1, as.table=F, top = "GPP")

# arrange plots without legend
p2<-grid.arrange(grobs=plot_list, ncol=length(unique(uniquetable$Metric)), as.table=F)

#Add legend to bottom of figure and save
png(paste0(dropbox_dir, '/Figures/NutrientExperiment/IncubationMetabolism/MetabolismTimeseries_bySite.png'), width=6, height=12, units='in', res=200)

grid.arrange(p2, mylegend, nrow=2,heights=c(10, length(unique(uniquetable$Site))/16))

dev.off()


#Add legend to bottom of figure and save
png(paste0(dropbox_dir, '/Figures/NutrientExperiment/IncubationMetabolism/MetabolismTimeseries.png'), width=6, height=6, units='in', res=200)

grid.arrange(NEP, ER, mylegend, nrow=3, heights=c(5,5,1))

dev.off()
