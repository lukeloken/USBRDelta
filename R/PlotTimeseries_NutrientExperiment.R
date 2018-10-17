

library(lubridate)
library(viridis)

# Project folder where outputs are stored
dropbox_dir<-'C:/Dropbox/USBR Delta Project'

#Where data come from
google_dir<-'C:/GoogleDrive/DeltaNutrientExperiment'


source('R/read_excel_allsheets.R')
source('R/g_legend.R')

# setwd("C:/Users/Luke/Dropbox/USBR Delta Project")
# 
# source("R/ReadInMasterData.R")


merge_df <- read.csv(file=paste0(dropbox_dir, '/Data/NutrientExperiment/SurfaceChemistry/YSIMetabolismSurface.csv'), stringsAsFactors = F)
merge_df$Date<-as.Date(merge_df$Date)
merge_df$Site<-factor(merge_df$Site, c('NL70', 'EC2','EC3','EC4','EC5','EC6','EC7','EC8','NL76'))




#Plotting parameters
jitterwidth=0.15
# colorset<-'Dark2'
# colors<-brewer.pal(3, colorset)[c(1,3,2)]

#colors
color.palette = colorRampPalette(c(viridis(6, begin=.2, end=.98), rev(magma(5, begin=.35, end=.98))), bias=1)
colors<-color.palette(length(unique(merge_df$Site)))
shapes<-rep(21:25, 5)


#Common theme for all metabolism timeseries panels
commonTheme<-list(
  scale_colour_manual(values = colors),
  scale_fill_manual(values = colors),
  scale_shape_manual(values=rep(21:25, 5)),
  # geom_smooth(method='loess',  se=F),
  # geom_smooth(method='auto', se=T, alpha=.2),
  # geom_jitter(size=2, width=jitterwidth, height=0, aes(fill=Site, shape=Site)),
  theme_bw(),
  theme(plot.title = element_text(hjust=0.5), legend.position="none", axis.title.x=element_blank())
)

# Make a table to determine how many panels are needed
# Each panel is a site and a metric

metrics<-c('NEP', 'ER', 'Temp.C', 'pH', 'Chl.ug.L', 'ODO.mg.L', 'Turbid..NTU')

uniquesites<-unique(merge_df[c('Site')])

ranges<-sapply(metrics, function(x) extendrange(merge_df[,x], f=0.05))

# Loop through metrics and sites and make a gg object
plot_list<-list()
plot_nu<-1
for (plot_nu in 1:length(metrics)){
  
  metric<-metrics[plot_nu]
  
  table<-merge_df[,c('Site', 'Date', metric)]
  
  plot_list[[plot_nu]]<-ggplot(table, aes_string('Date', metric, group='Site')) + 
    ggtitle(metric) +
    geom_line(size=1, aes(colour=Site,  group=Site)) +    
    geom_point(size=2, aes(fill=Site, shape=Site)) + 
    commonTheme
}
  
  

plot_withlegend <- plot_list[[1]] + 
  theme(plot.title = element_text(hjust=0.5), legend.position="bottom")


mylegend<-g_legend(plot_withlegend)


plot_list2<-plot_list
plot_list2[[length(plot_list)+1]]<-mylegend
# arrange plots with legend
p2<-grid.arrange(grobs=plot_list2, nrow=ceiling(length(plot_list2)/2), as.table=F)

#Add legend to bottom of figure and save
png(paste0(dropbox_dir, '/Figures/NutrientExperiment/EcosystemResponseTimeSeries_Allmetrics.png'), width=10, height=12, units='in', res=200)

grid.arrange(p2)

dev.off()



