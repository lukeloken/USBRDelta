# plot zooplankton community
# #Need to run AggregateZoops.R first

library(viridis)
library(ggplot2)
library(gridExtra)
library(plyr)
library(dplyr)
library(tidyr)
library(lubridate)
library(RColorBrewer)
library(MASS)


# # Project folder where outputs are stored
# dropbox_dir<-'C:/Dropbox/USBR Delta Project'
# 
# #Where data come from
# google_dir<-'C:/GoogleDrive/DeltaNutrientExperiment'


Zoo_summary_select<-readRDS(file=paste0(dropbox_dir, '/Data/Rdata/Zoo_summary_select.rds'))
Zoo_summary_no64<-filter(Zoo_summary_select, Station!='64')

# Also need Zoo_monthly and Zoo_total_monthly

# ##############################
# Zoo plots
# ##############################

BiomassExp<-expression(paste("Zooplankton biomass ( ", mu, "g d.w. L"^"-1", ")", sep=''))

Median_BiomassExp<-expression(paste("Median Zooplankton bioimass ( ", mu, "g d.w. L"^"-1", ")", sep=''))

#Colors for boxplots by division
colorset<-'Accent'
colors_zoops<-brewer.pal(7, colorset)[c(4,1,2,3,6,5,7)]


#Common theme for boxplot by division
commonTheme_boxplot<-list(
  scale_fill_manual(values = colors_zoops),
  scale_colour_manual(values = colors_zoops),
  theme_bw(),
  theme(plot.title = element_text(hjust=0.5), legend.position="bottom")
)


# colors for boxplots by month/sites
color.palette = colorRampPalette(c(viridis(6, begin=.2, end=.98), rev(magma(5, begin=.35, end=.98))), bias=1)
colors_month<-color.palette(length(unique(Zoo_summary_no64$Month)))
shapes_month<-rep(21:25, 5)


#Common theme for monthly boxplots
commonTheme_boxplot_monthly<-list(
  scale_fill_manual(values = colors_month),
  scale_colour_manual(values = colors_month),
  theme_bw(),
  theme(plot.title = element_text(hjust=0.5), legend.position="bottom")
)



png(paste0(dropbox_dir, "/Figures/Zoops/BoxplotGenusbyStation.png", sep=""), res=300, width=8,height=4, units="in")

print(
ggplot(Zoo_summary_no64, aes(x=Station, y=Total_SpeciesBiomass_ugdwL, fill=division)) + 
  labs(x='Station', y=BiomassExp) +
  commonTheme_boxplot + 
  # scale_y_log10() +
  scale_y_continuous(limits=c(0, 150)) +
  guides(fill=guide_legend(title="")) + 
  geom_boxplot(outlier.size=0.5, na.rm=T, outlier.shape=NA)
)

dev.off()

png(paste0(dropbox_dir, "/Figures/Zoops/BoxplotGenusbyStation_log.png", sep=""), res=300, width=8,height=4, units="in")

print(
ggplot(Zoo_summary_no64, aes(x=Station, y=Total_SpeciesBiomass_ugdwL, fill=division)) + 
  labs(x='Station', y=BiomassExp) +
  commonTheme_boxplot + 
  scale_y_log10() +
  # scale_y_continuous(limits=c(0, 150)) +
  guides(fill=guide_legend(title="")) + 
  geom_boxplot(outlier.size=0.5, na.rm=T, outlier.shape=NA)
)

dev.off()

png(paste0(dropbox_dir, "/Figures/Zoops/DensityBoxplotGenusbyStation.png", sep=""), res=300, width=8,height=4, units="in")

print(
ggplot(Zoo_summary_no64, aes(x=Station, y=Total_NumberPerLiter, fill=division)) + 
  labs(x='Station', y='Density') +
  commonTheme_boxplot + 
  scale_y_log10() +
  # scale_y_continuous(limits=c(0, 100000000)) +
  guides(fill=guide_legend(title="")) + 
  geom_boxplot(outlier.size=0.5, na.rm=T, outlier.shape=NA)
)

dev.off()


# Boxplots by month
png(paste0(dropbox_dir, "/Figures/Zoops/BoxplotGenusbyMonth.png", sep=""), res=300, width=8,height=4, units="in")

print(
ggplot(Zoo_summary_no64, aes(x=as.factor(Month), y=Total_SpeciesBiomass_ugdwL, fill=division)) + 
  labs(x='Month', y=BiomassExp) +
  commonTheme_boxplot + 
  # scale_y_log10() + 
  scale_y_continuous(limits=c(0,200)) +
  geom_boxplot(outlier.size=0.5, na.rm=T, outlier.shape=NA) + 
  guides(fill=guide_legend(title="")) 
)

dev.off()


#Boxplots by station by month by species
png(paste0(dropbox_dir, "/Figures/Zoops/BoxplotGenusbyMonthbyStation.png", sep=""), res=300, width=12,height=12, units="in")

print(
ggplot(Zoo_summary_no64, aes(x= Station, y=Total_SpeciesBiomass_ugdwL, color=as.factor(Month))) + 
  labs(x='Station', y=BiomassExp) +
  commonTheme_boxplot_monthly + 
  # scale_y_continuous(limits=c(0, 50)) +
  scale_y_log10() +
  geom_boxplot(outlier.size=0.5, na.rm=T, size=1, outlier.shape=NA) + 
  # geom_point(na.rm=T) + 
  facet_grid( division ~ .) + 
  guides(color=guide_legend(title="Month"))
)

dev.off()



#Boxplots by month by station by species


png(paste0(dropbox_dir, "/Figures/Zoops/BoxplotGenusbyStationbyMonth.png", sep=""), res=300, width=12,height=12, units="in")

print(
ggplot(Zoo_summary_no64, aes(x= as.factor(Month) , y=Total_SpeciesBiomass_ugdwL, color=Station)) + 
  labs(x='Month', y=BiomassExp) +
  commonTheme_boxplot_monthly + 
  scale_y_log10() + 
  # scale_y_continuous(limits=c(0, 50)) +
  geom_boxplot(outlier.size=0.5, na.rm=T, size=1, outlier.shape=NA) + 
  # geom_point(na.rm=T) + 
  facet_grid( division ~ .) + 
  guides(color=guide_legend(title="Station"))
)

dev.off()


#Boxplots by month by station by species geom_area
png(paste0(dropbox_dir, "/Figures/Zoops/StatckedPlotdivisionbyStationbyDate.png", sep=""), res=300, width=6,height=12, units="in")

print(
ggplot(Zoo_summary_no64, aes(x= Date , y=Total_SpeciesBiomass_ugdwL, fill=division)) + 
  labs(x='Month', y=BiomassExp) +
  commonTheme_boxplot + 
  # scale_y_log10(limits=c(10000, 10000000000)) + 
  geom_area(position='stack') + 
  # geom_path()
  # geom_boxplot(outlier.size=0.5, na.rm=T, size=1, outlier.shape=NA) + 
  # geom_point(na.rm=T) + 
  facet_grid( Station ~ .)
)

dev.off()




png(paste0(dropbox_dir, "/Figures/Zoops/StatckedPlotdivisionbyStationbyMonth.png", sep=""), res=300, width=6,height=12, units="in")

print(
ggplot(Zoo_monthly, aes(x= Month , y=Median_SpeciesBiomass_ugdwL, fill=division)) + 
  labs(x='Month', y=Median_BiomassExp) +
  commonTheme_boxplot + 
  scale_y_continuous() +
  geom_area(position='stack') + 
  # geom_path()
  # geom_boxplot(outlier.size=0.5, na.rm=T, size=1, outlier.shape=NA) + 
  # geom_point(na.rm=T) + 
  scale_x_continuous(breaks=1:12) + 
  guides(fill=guide_legend(title="")) + 
  facet_grid( Station ~ .)
)

dev.off()



# lines by month/station/division

png(paste0(dropbox_dir, "/Figures/Zoops/LinePlotdivisionbyStationbyMonth.png", sep=""), res=300, width=6,height=12, units="in")

print(
ggplot(Zoo_monthly, aes(x= Month , y=Median_SpeciesBiomass_ugdwL, color=division)) + 
  labs(x='Month', y=Median_BiomassExp) +
  commonTheme_boxplot + 
  # scale_y_log10() +
  # geom_jitter(size=3, width=0.1) + 
  geom_path(size=2) + 
  # geom_boxplot(outlier.size=0.5, na.rm=T, size=1, outlier.shape=NA) + 
  # geom_point(na.rm=T) + 
  scale_x_continuous(breaks=1:12) + 
  facet_grid( Station ~ .) + 
  guides(color=guide_legend(title="")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
)

dev.off()



# Total Zooplankton biomass

png(paste0(dropbox_dir, "/Figures/Zoops/TotalZoo_LinePlotdivisionbyStationbyMonth.png", sep=""), res=300, width=6,height=12, units="in")

print(
ggplot(Zoo_total_monthly, aes(x= Month , y=Median_SpeciesBiomass_ugdwL)) + 
  labs(x='Month', y=Median_BiomassExp) +
  commonTheme_boxplot + 
  # scale_y_log10() +
  # geom_jitter(size=3, width=0.1) + 
  geom_path(size=2) + 
  # geom_boxplot(outlier.size=0.5, na.rm=T, size=1, outlier.shape=NA) + 
  # geom_point(na.rm=T) + 
  scale_x_continuous(breaks=1:12) + 
  facet_grid( Station ~ .) + 
  guides(color=guide_legend(title="")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
)

dev.off()



