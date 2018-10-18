
#Load libraies

library(readxl)
library(openxlsx)

library(plyr)
library(dplyr)
library(tidyr)

library(ggplot2)
library(gridExtra)
library(RColorBrewer)

library(lubridate)

source('R/CalculateIncubationMetabolism.R')
source('R/read_excel_allsheets.R')
source('R/g_legend.R')

# Project folder where outputs are stored
dropbox_dir<-'C:/Dropbox/USBR Delta Project'

#Where data come from
google_dir<-'C:/GoogleDrive/DeltaNutrientExperiment'

# Set date %m%d%y
# This should match the folder name within the dropbox direcetory listed above. 

# Date<-'081618'
# Date<-'071218'
Date<-'101618'

# ##############################################################################################
# Code looks into the dropbox directory/incubation data and loads the correct file using 'Date'
# Shouldn't need to change anything below
# ##############################################################################################

#Find the correct file and load it
files<-list.files(paste0(dropbox_dir, '/Data/Incubations/', Date))

goodfile<-files[startsWith(files, 'Delta')]

mysheets <- read_excel_allsheets(paste0(dropbox_dir, '/Data/Incubations/', Date, '/', goodfile))

#Calculate metabolims
results<-CalculateIncubationMetabolism(mysheets)


DOrate_mean_long_table<-results[[1]]
DOsd<-results[[2]]

if ('Dark' %in% unique(DOrate_mean_long_table$Treatment)){
  DOrate_mean_long_table<-DOrate_mean_long_table[which(DOrate_mean_long_table$Treatment=='Light'),]

  
  stations<-c("NL70", "EC2", "EC3", "EC4", "EC5", "EC6", "EC7", "EC8", "NL76")
  if (length(DOrate_mean_long_table$Site %in% stations)>1){
  DOrate_mean_long_table$Site<-factor(DOrate_mean_long_table$Site, stations)
  }

# vector of metrics. This is what the loop will run through
uniquemetrics<-unique(DOrate_mean_long_table[c('Metric')])
uniquemetrics<-factor(uniquemetrics[,1], c('GPP', 'ER', 'NEP'))
uniquemetrics<-uniquemetrics[order(uniquemetrics)]


#Common theme for all boxplots
commonTheme_boxplot<-list(
  scale_fill_manual(values = colors),
  scale_colour_manual(values = colors),
  theme_bw(),
  theme(plot.title = element_text(hjust=0.5), legend.position="none"),
  geom_boxplot(outlier.size=0.5)
)

# Loop through metrics and make a gg object
box_list<-list()
plot_nu<-1
for (plot_nu in 1:length(uniquemetrics)){
  # Pick data
  metric<-uniquemetrics[plot_nu]
  box_table<-DOrate_mean_long_table[DOrate_mean_long_table$Metric==metric,]
  #Plot
  box_list[[plot_nu]] <- ggplot(aes(y = Value, x = Site, fill = Treatment), data = box_table) + 
    labs(x='Site', y=metric) +
    commonTheme_boxplot
}


#Add and extract legend from first plot
box_withlegend <- box_list[[1]] + 
  theme(legend.position='bottom') 

mylegend_box<-g_legend(box_withlegend)


# arrange plots without legend
p2_box<-grid.arrange(grobs=box_list, ncol=3, as.table=F)


#Add legend to bottom of figure and save
png(paste0(dropbox_dir, '/Figures/Incubations/', Date, 'Metabolism_Boxplot.png'), width=8, height=3, units='in', res=200)

grid.arrange(p2_box)

dev.off()


} else {


# Save to directory listed at the start
# Metabolism estimates are in mg O2 per liter per hour
write.csv(DOrate_mean_long_table, file=paste0(dropbox_dir, '/Data/Incubations/MetabolismCalculations/JarMetabolism_', Date, '.csv'), row.names=F)


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
colorset<-'Dark2'
colors<-brewer.pal(3, colorset)[c(1,3,2)]

#Common theme for all metabolism timeseries panels
commonTheme<-list(
  scale_colour_manual(values = colors),
  scale_fill_manual(values = colors),
  # geom_smooth(method='loess',  se=F),
  geom_smooth(method='auto', se=T, alpha=.2),
  geom_jitter(size=2, width=jitterwidth, height=0),
  theme_bw(),
  theme(plot.title = element_text(hjust=0.5), legend.position="none")
)

# Make a table to determine how many panels are needed
# Each panel is a site and a metric
uniquetable<-unique(DOrate_mean_long_table[c('Metric', 'Site')])
uniquetable$Metric<-factor(uniquetable$Metric, c('GPP', 'ER', 'NEP'))
uniquetable<-uniquetable[order(uniquetable$Site),]
uniquetable<-uniquetable[order(uniquetable$Metric),]

ranges<-sapply(uniquetable$Metric, function(x) extendrange(DOrate_mean_long_table$Value[DOrate_mean_long_table$Metric ==x], f=0.05))

# Loop through metrics and sites and make a gg object
plot_list<-list()
plot_nu<-1
for (plot_nu in 1:nrow(uniquetable)){
  
  site<-uniquetable$Site[plot_nu]
  metric<-uniquetable$Metric[plot_nu]
  
  table<-DOrate_mean_long_table[DOrate_mean_long_table$Metric==metric & 
                                  DOrate_mean_long_table$Site==site,]
  
  plot_list[[plot_nu]] <- ggplot(table, aes(Day, Value, colour=Treatment, fill=Treatment)) + 
    labs(x='Day', y=metric) +
    ggtitle(site) +
    # ylim(ranges[1,plot_nu], ranges[2,plot_nu]) +
    # scale_y_continuous(limits=ranges[,plot_nu]) +
    coord_cartesian(ylim=ranges[,plot_nu]) + 
    commonTheme 
  
}

#Add and extract legend from first plot
plot_withlegend <- plot_list[[1]] + 
  theme(legend.position='bottom')

mylegend<-g_legend(plot_withlegend)

# p_cols1<-grid.arrange(grobs=plot_list[c(1,4,7,11)], ncol=1, as.table=F, top = "GPP")

# arrange plots without legend
p2<-grid.arrange(grobs=plot_list, ncol=length(unique(uniquetable$Metric)), as.table=F)

#Add legend to bottom of figure and save
png(paste0(dropbox_dir, '/Figures/Incubations/', Date, 'Metabolism_Timeseries.png'), width=8, height=12, units='in', res=200)

grid.arrange(p2, mylegend, nrow=2,heights=c(10, length(unique(uniquetable$Site))/16))

dev.off()



# ##############################################
# 2) Boxplot of within jar standard deviations
# One panel is a site/metric
# Each box is a timepoint, y is the within-jar standard deviation, color is AM/PM
# #############################################


png(paste0(dropbox_dir, '/Figures/Incubations/', Date, 'Metabolism_WithinSampleSD_boxplot.png'), width=5, height=4, units='in', res=200)
par(mar=c(2.5,3,0.5,0.5))
par(mgp=c(3,0.5,0))
xlabels<-paste0('T', 1:(ncol(DOsd)-1))

boxplot(DOsd[2:ncol(DOsd)], names=xlabels, col=c(rep(c('grey50', 'darkgreen'),4), 'grey50'), boxwex=0.5, cex=0.8)
# axis(1, at=1:9, line=1, labels=c(rep(c('AM', 'PM'),4), 'AM'), tick=F, lty=0)
mtext(expression(paste('Within-measurement SD (mg  ', O[2], ' L'^'-1', ')', sep='')), 2, 1.5)
mtext('Timepoint', 1, 1.5)
legend('topleft', inset=0.02, bty='n', pt.bg=c('grey50', 'darkgreen'), c('AM', 'PM'), pt.cex=3, pch=22, cex=1, y.intersp=2)

dev.off()



# ##############################################
# 3) Boxplot of treatment effects
# three panels, each are a metric
# X is the Site/Treatment, Y is the Value (includes replicate jars and through time)
# It may be a good idea to pull out a single timepoint (e.g., Day 2 or 3)
# Uses the same color scheme as plot 1
# #############################################

# vector of metrics. This is what the loop will run through
uniquemetrics<-unique(DOrate_mean_long_table[c('Metric')])
uniquemetrics<-factor(uniquemetrics[,1], c('GPP', 'ER', 'NEP'))
uniquemetrics<-uniquemetrics[order(uniquemetrics)]


#Common theme for all boxplots
commonTheme_boxplot<-list(
  scale_fill_manual(values = colors),
  scale_colour_manual(values = colors),
  theme_bw(),
  theme(plot.title = element_text(hjust=0.5), legend.position="none"),
  geom_boxplot(outlier.size=0.5)
)

# Loop through metrics and make a gg object
box_list<-list()
plot_nu<-1
for (plot_nu in 1:length(uniquemetrics)){
  # Pick data
  metric<-uniquemetrics[plot_nu]
  box_table<-DOrate_mean_long_table[DOrate_mean_long_table$Metric==metric,]
  #Plot
  box_list[[plot_nu]] <- ggplot(aes(y = Value, x = Site, fill = Treatment), data = box_table) + 
    labs(x='Site', y=metric) +
    commonTheme_boxplot
}


#Add and extract legend from first plot
box_withlegend <- box_list[[1]] + 
  theme(legend.position='bottom') 

mylegend_box<-g_legend(box_withlegend)


# arrange plots without legend
p2_box<-grid.arrange(grobs=box_list, ncol=3, as.table=F)


#Add legend to bottom of figure and save
png(paste0(dropbox_dir, '/Figures/Incubations/', Date, 'Metabolism_Boxplot.png'), width=8, height=3, units='in', res=200)

grid.arrange(p2_box, mylegend_box, nrow=2,heights=c(10, 1))

dev.off()

}