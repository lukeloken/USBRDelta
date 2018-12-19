

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


merge_df <- read.csv(file=paste0(dropbox_dir, '/Data/NutrientExperiment/SurfaceChemistry/YSIMetabolismSurface.csv'), stringsAsFactors = F)
merge_df$Date<-as.Date(merge_df$Date)
merge_df$Site<-factor(merge_df$Site, c('NL70', 'EC2','EC3','EC4','EC5','EC6','EC7','EC8','NL76'))


metrics<-c('EXOSpCond', 'SpCond.uS', 'EXOTemp', 'Temp.C', 'EXOpH', 'pH', 'EXOChlugL', 'Chl.ug.L', 'EXOBGAPCugL', 'EXOODO', 'EXODOmgL', 'ODO.mg.L', 'EXOTurbFNU', 'Turbid..NTU', 'HachTurbidity', 'TSS', 'VSS', 'SecchiDepth',  'NEP', 'ER', 'SUNA_NO3_uM',"NO3.ppm", "NH4.ppm", "PO4",  "DOC.ppm", 'TN.ppm', 'TDN.ppm', 'TP.ppm', "d180_02.vs.VSMOW")



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
  geom_vline(xintercept=as.Date('2018-10-02'), linetype="solid", color = "black", size=1),
  theme_bw(),
  theme(plot.title = element_text(hjust=0.5), legend.position="none", axis.title.x=element_blank())
)

# Make a table to determine how many panels are needed
# Each panel is a site and a metric


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

png(paste0(dropbox_dir, '/Figures/NutrientExperiment/Oxygen18vDO.png'), width=4, height=5, units='in', res=200)

ggplot(merge_df, aes(x=EXOODO, y=d180_02.vs.VSMOW, group=Site))+
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


png(paste0(dropbox_dir, '/Figures/NutrientExperiment/Oxygen18vTurb.png'), width=4, height=5, units='in', res=200)

ggplot(merge_df, aes(x=log10(EXOTurbFNU), y=d180_02.vs.VSMOW, group=Site))+
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

#End


# 
# #Testing new grid alignment
# 
# plot_list<-list()
# plot_nu<-1
# for (plot_nu in 1:length(metrics)){
#   
#   metric<-metrics[plot_nu]
#   
#   table<-merge_df[,c('Site', 'Date', metric)]
#   
#   plot_list[[plot_nu]]<-ggplot(table, aes_string('Date', metric, group='Site')) + 
#     ggtitle(metric) +
#     geom_line(size=1, aes(colour=Site,  group=Site)) +    
#     geom_point(size=2, aes(fill=Site, shape=Site)) + 
#     commonTheme
#   
#   if (plot_nu==1){
#     gtable1<-ggplotGrob(plot_list[[plot_nu]])
#   } else if (plot_nu>1){
#     gtable1 <-rbind(gtable1, ggplotGrob(plot_list[[plot_nu]]), size='last')
#   }
# }
# 
# # grob_list<-lapply(plot_list, function (l) ggplotGrob(l))
# # 
# # 
# # 
# # gtable_test<-gtable_rbind(grob_list[[1]],grob_list[[2]], grob_list[[3]], grob_list[[4]], size = "last")
# 
# grid.newpage()
# grid.draw(gtable1)
# 
# 
# gtable_test2<-gtable_rbind(expression(paste("grob_list[[", 1:2, "]],")), size='last')
# 
# 
# grid.newpage()
# grid.draw(gtable_test2)
# 
# 
# gtable_test2<-rbindlist(grob_list)
# , size = "last")
# 
# 
# 
# # rbind(plot_list[[1]], plot_list[[2]])
# 
# grid.draw
# 
# grob_list<-lapply(plot_list, function (l) ggplotGrob(l))
# 
# grid.newpage()
# grid.draw(rbind(grob_list, size='last'))
# 
