
#Merge buoy metabolism with dataset and compare methods



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

#New data input (after O18 metabolism and incubation scaling)
#O18 metabolism data
met.final<-read.csv(file = paste(dropbox_dir, "Data", "NutrientExperiment", "MergedData1.csv", sep='/'), stringsAsFactors = F)

met.final$Date<-as.Date(met.final$Date)
met.final$Site<-factor(met.final$Site, c('NL70', 'EC2','EC3','EC4','EC5','EC6','EC7','EC8','NL76'))

zonetostation<-data.frame(Site=c('NL70', 'EC2', 'EC5', 'EC8', 'NL76'), zone=c('NL70', 'lower', 'middle', 'upper', 'NL76'))

met.final$zone<-zonetostation$zone[match(met.final$Site, zonetostation$Site)]

metab.summary <- read.csv(file=paste0(dropbox_dir, '/Data/NutrientExperiment/Buoys/MetabolismSummaries_v1.csv'))

metab.summary2<- metab.summary %>%
  mutate(
    Date = as.Date(date)
    
  ) %>%
  dplyr::select(-date) %>%
  rename(NEP_buoy=NEP, GPP_buoy=GPP, ER_buoy=R)

met.join<-left_join(met.final, metab.summary2)

write.csv(met.join, file=paste0(dropbox_dir, '/Data/NutrientExperiment/MergedData2.csv'), row.names=F)



#Plotting parameters
jitterwidth=0.15
# colorset<-'Dark2'
# colors<-brewer.pal(3, colorset)[c(1,3,2)]

#colors
color.palette = colorRampPalette(c(viridis(6, begin=.2, end=.98), rev(magma(5, begin=.35, end=.98))), bias=1)
colors<-color.palette(length(unique(met.join$Site)))
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


uniquesites<-unique(met.join[c('Site')])




#GPP compare

gppplot1<-ggplot(met.join, aes(x=GPP_inc, y=gppv, group=Site))+
  labs(x=expression(paste('Incubation GPP')), y=expression(paste(delta^'18', "O-", O[2], " GPP")))+
  geom_vline(xintercept=0, linetype="dashed", color = "grey", size=1) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey", size=1) +
  geom_abline(intercept=0,slope=1) + 
  geom_point(size=2, aes(fill=Site, shape=Site)) + 
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_shape_manual(values=rep(21:25, 5)) +
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5), legend.position="none") +
  guides(fill = guide_legend(nrow = 3, title.position='top', title.hjust=0.5))

gppplot2<-ggplot(met.join, aes(x=GPP_inc, y=GPP_buoy, group=Site))+
  labs(x=expression(paste('Incubation GPP')), y="Buoy GPP")+
  geom_vline(xintercept=0, linetype="dashed", color = "grey", size=1) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey", size=1) +
  geom_abline(intercept=0,slope=1) + 
  geom_point(size=2, aes(fill=Site, shape=Site)) + 
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_shape_manual(values=rep(21:25, 5)) +
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5), legend.position="none") +
  guides(fill = guide_legend(nrow = 3, title.position='top', title.hjust=0.5))

gppplot3<-ggplot(met.join, aes(x=gppv, y=GPP_buoy, group=Site))+
  labs(x=expression(paste(delta^'18', "O-", O[2], " GPP")), y="Buoy GPP")+
  geom_vline(xintercept=0, linetype="dashed", color = "grey", size=1) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey", size=1) +
  geom_abline(intercept=0,slope=1) + 
  geom_point(size=2, aes(fill=Site, shape=Site)) + 
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_shape_manual(values=rep(21:25, 5)) +
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5), legend.position="bottom") +
  guides(fill = guide_legend(nrow = 3, title.position='top', title.hjust=0.5))


png(paste0(dropbox_dir, '/Figures/NutrientExperiment/GPPCompare.png'), width=4, height=10, units='in', res=200)

grid.newpage()
plots<-grid.draw(rbind(ggplotGrob(gppplot1), ggplotGrob(gppplot2), ggplotGrob(gppplot3), size = "first"))

dev.off()


#NEP Compare

nepplot1<-ggplot(met.join, aes(x=NEP_inc, y=nepv, group=Site))+
  labs(x=expression(paste('Incubation NEP')), y=expression(paste(delta^'18', "O-", O[2], " NEP")))+
  geom_vline(xintercept=0, linetype="dashed", color = "grey", size=1) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey", size=1) +
  geom_abline(intercept=0,slope=1) + 
  geom_point(size=2, aes(fill=Site, shape=Site)) + 
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_shape_manual(values=rep(21:25, 5)) +
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5), legend.position="none") +
  guides(fill = guide_legend(nrow = 3, title.position='top', title.hjust=0.5))

nepplot2<-ggplot(met.join, aes(x=NEP_inc, y=NEP_buoy, group=Site))+
  labs(x=expression(paste('Incubation NEP')), y="Buoy NEP")+
  geom_vline(xintercept=0, linetype="dashed", color = "grey", size=1) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey", size=1) +
  geom_abline(intercept=0,slope=1) + 
  geom_point(size=2, aes(fill=Site, shape=Site)) + 
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_shape_manual(values=rep(21:25, 5)) +
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5), legend.position="none") +
  guides(fill = guide_legend(nrow = 3, title.position='top', title.hjust=0.5))

nepplot3<-ggplot(met.join, aes(x=nepv, y=NEP_buoy, group=Site))+
  labs(x=expression(paste(delta^'18', "O-", O[2], " NEP")), y="Buoy NEP")+
  geom_vline(xintercept=0, linetype="dashed", color = "grey", size=1) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey", size=1) +
  geom_abline(intercept=0,slope=1) + 
  geom_point(size=2, aes(fill=Site, shape=Site)) + 
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_shape_manual(values=rep(21:25, 5)) +
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5), legend.position="bottom") +
  guides(fill = guide_legend(nrow = 3, title.position='top', title.hjust=0.5))

png(paste0(dropbox_dir, '/Figures/NutrientExperiment/NEPCompare.png'), width=4, height=10, units='in', res=200)

grid.newpage()
plots<-grid.draw(rbind(ggplotGrob(nepplot1), ggplotGrob(nepplot2), ggplotGrob(nepplot3), size = "first"))

dev.off()

#ER compare

erplot1<-ggplot(met.join, aes(x=ER_inc, y=rv, group=Site))+
  labs(x=expression(paste('Incubation ER')), y=expression(paste(delta^'18', "O-", O[2], " ER")))+
  geom_vline(xintercept=0, linetype="dashed", color = "grey", size=1) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey", size=1) +
  geom_abline(intercept=0,slope=1) + 
  geom_point(size=2, aes(fill=Site, shape=Site)) + 
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_shape_manual(values=rep(21:25, 5)) +
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5), legend.position="none") +
  guides(fill = guide_legend(nrow = 3, title.position='top', title.hjust=0.5)) 

erplot2<-ggplot(met.join, aes(x=ER_inc, y=ER_buoy, group=Site))+
  labs(x=expression(paste('Incubation ER')), y="Buoy ER")+
  geom_vline(xintercept=0, linetype="dashed", color = "grey", size=1) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey", size=1) +
  geom_abline(intercept=0,slope=1) + 
  geom_point(size=2, aes(fill=Site, shape=Site)) + 
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_shape_manual(values=rep(21:25, 5)) +
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5), legend.position="none") +
  guides(fill = guide_legend(nrow = 3, title.position='top', title.hjust=0.5))

erplot3<-ggplot(met.join, aes(x=rv, y=ER_buoy, group=Site))+
  labs(x=expression(paste(delta^'18', "O-", O[2], " ER")), y="Buoy ER")+
  geom_vline(xintercept=0, linetype="dashed", color = "grey", size=1) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey", size=1) +
  geom_abline(intercept=0,slope=1) + 
  geom_point(size=2, aes(fill=Site, shape=Site)) + 
  scale_colour_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_shape_manual(values=rep(21:25, 5)) +
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5), legend.position="bottom") +
  guides(fill = guide_legend(nrow = 3, title.position='top', title.hjust=0.5))


png(paste0(dropbox_dir, '/Figures/NutrientExperiment/ERCompare.png'), width=4, height=10, units='in', res=200)

grid.newpage()
plots<-grid.draw(rbind(ggplotGrob(erplot1), ggplotGrob(erplot2), ggplotGrob(erplot3), size = "first"))

dev.off()


#End






