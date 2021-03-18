# plot phytoplankton community
# #Need to run AggregatePhytos.R first

library(viridis)
library(ggplot2)
library(gridExtra)
library(plyr)
library(dplyr)
library(tidyr)
library(lubridate)
library(RColorBrewer)
# library(MASS)


# # Project folder where outputs are stored
# dropbox_dir<-'C:/Dropbox/USBR Delta Project'
# 
# #Where data come from
# google_dir<-'C:/GoogleDrive/DeltaNutrientExperiment'

# stationfactors<-c("16", "34", "44", "Pro", "56", "62", "64", "66" ,"70" ,"74" ,"76" ,"84" ,"WSP")

# Phyto_summary_select<-readRDS(file=paste0(dropbox_dir, '/Data/Rdata/Phyto_summary_select.rds'))

Phyto_summary_select <- readRDS(file = file.path(onedrive_dir, 
                                               "RData", 
                                               "MonthlyCruises", 
                                               "Phyto_summary_select.rds"))

Phyto_summary_no64<-filter(Phyto_summary_select, Station!='64')

#Also need Phyto_total_monthly and Phyto_monthly

# ##############################
# Phyto plots
# ##############################

BioVolExp<-expression(paste("Phytoplankton biovolume (1 x 10"^"9", " ", mu, "m"^"3", " L"^"-1", ")", sep=''))

Median_BioVolExp<-expression(paste("Median phytoplankton biovolume (1 x 10"^"9", " ", mu, "m"^"3", " L"^"-1", ")", sep=''))

#Colors for phytoplankton divisions
colorset<-'Accent'
colors_phyto<-brewer.pal(length(unique(Phyto_summary_no64$DIVISION)), colorset)[c(2,1,3,4,5)]

#colors for months
color.palette = colorRampPalette(c(viridis(6, begin=.2, end=.98), rev(magma(5, begin=.35, end=.98))), bias=1)
colors_month<-color.palette(length(unique(Phyto_summary_no64$Month)))
shapes_month<-rep(21:25, 5)


#Common theme for division boxplots
commonTheme_boxplot<-list(
  scale_fill_manual(values = colors_phyto),
  scale_colour_manual(values = colors_phyto),
  theme_bw(),
  theme(plot.title = element_text(hjust=0.5), legend.position="bottom")
)

#Common theme for monthly boxplots
commonTheme_boxplot_monthly<-list(
  scale_fill_manual(values = colors_month),
  scale_colour_manual(values = colors_month),
  theme_bw(),
  theme(plot.title = element_text(hjust=0.5), legend.position="bottom")
)



#Boxplots biovolume by station
png(file = file.path(onedrive_dir, 
                     "Figures",  
                     "MonthlyCruises", 
                     "Phytos",           
                     "BoxplotGenusbyStation.png"),
    res=300, width=8,height=4, units="in")

print(
  ggplot(Phyto_summary_no64, aes(x=Station, y=Total_BioVolume, fill=DIVISION)) + 
  labs(x='Station', y=BioVolExp) +
  commonTheme_boxplot + 
  # scale_y_log10(limits=c(1000, 100000000000)) +
  scale_y_continuous(limits=c(0, 5)) +
  guides(fill=guide_legend(title="")) + 
  geom_boxplot(outlier.size=0.5, na.rm=T, outlier.shape=NA)
)

dev.off()

#Boxplots density by station
png(file = file.path(onedrive_dir,    
                     "Figures",     
                     "MonthlyCruises",   
                     "Phytos",          
                     "DensityBoxplotGenusbyStation.png"), 
    res=300, width=8,height=4, units="in")

print(
ggplot(Phyto_summary_no64, aes(x=Station, y=Density, fill=DIVISION)) + 
  labs(x='Station', y='Density') +
  commonTheme_boxplot + 
  scale_y_log10(limits=c(1, 100000000000)) +
  # scale_y_continuous(limits=c(0, 100000000)) +
  guides(fill=guide_legend(title="")) + 
  geom_boxplot(outlier.size=0.5, na.rm=T, outlier.shape=NA)
)

dev.off()


# Boxplots by month
png(file = file.path(onedrive_dir,   
                     "Figures",     
                     "MonthlyCruises",    
                     "Phytos",            
                     "BoxplotGenusbyMonth.png"), 
    res=300, width=8,height=4, units="in")

print(
ggplot(Phyto_summary_no64, aes(x=as.factor(Month), y=Total_BioVolume, fill=DIVISION)) + 
  labs(x='Month', y=BioVolExp) +
  commonTheme_boxplot + 
  # scale_y_log10() + 
  scale_y_continuous(limits=c(0, 5)) +
  geom_boxplot(outlier.size=0.5, na.rm=T, outlier.shape=NA) + 
  guides(fill=guide_legend(title="")) 
)

dev.off()


#Boxplots by station by month by species
png(file = file.path(onedrive_dir,         
                     "Figures",      
                     "MonthlyCruises",   
                     "Phytos",          
                     "BoxplotGenusbyMonthbyStation.png"),
    res=300, width=12,height=12, units="in")

print(
ggplot(Phyto_summary_no64, aes(x= Station, y=Total_BioVolume, color=as.factor(Month))) + 
  labs(x='Station', y=BioVolExp) +
  commonTheme_boxplot_monthly + 
  # scale_y_continuous(limits=c(0, 50)) +
  scale_y_log10(limits=c(0.00001, 50)) +
  geom_boxplot(outlier.size=0.5, na.rm=T, size=1, outlier.shape=NA) + 
  # geom_point(na.rm=T) + 
  facet_grid( DIVISION ~ .) + 
  guides(color=guide_legend(title="Month"))
)

dev.off()



#Boxplots by month by station by species
png(file = file.path(onedrive_dir,  
                     "Figures",    
                     "MonthlyCruises", 
                     "Phytos",          
                     "BoxplotGenusbyStationbyMonth.png"),
    res=300, width=12,height=12, units="in")

print(
ggplot(Phyto_summary_no64, aes(x= as.factor(Month) , y=Total_BioVolume, color=Station)) + 
  labs(x='Month', y=BioVolExp) +
  commonTheme_boxplot_monthly + 
  scale_y_log10(limits=c(.00001, 50)) + 
  # scale_y_continuous(limits=c(0, 50)) +
  geom_boxplot(outlier.size=0.5, na.rm=T, size=1, outlier.shape=NA) + 
  # geom_point(na.rm=T) + 
  facet_grid( DIVISION ~ .) + 
  guides(color=guide_legend(title="Station"))
)

dev.off()




#Boxplots by month by station by species geom_area
png(file = file.path(onedrive_dir,                
                     "Figures",           
                     "MonthlyCruises",    
                     "Phytos",            
                     "StatckedPlotDivisionbyStationbyDate.png"), 
    res=300, width=6,height=12, units="in")

print(
ggplot(Phyto_summary_no64, aes(x= Date , y=Total_BioVolume, fill=DIVISION)) + 
  labs(x='Month', y=BioVolExp) +
  commonTheme_boxplot + 
  # scale_y_log10(limits=c(10000, 10000000000)) + 
  geom_area(position='stack') + 
  # geom_path()
  # geom_boxplot(outlier.size=0.5, na.rm=T, size=1, outlier.shape=NA) + 
  # geom_point(na.rm=T) + 
  facet_grid( Station ~ .)
)

dev.off()


# Stacked Area by month
png(file = file.path(onedrive_dir,     
                     "Figures",     
                     "MonthlyCruises",  
                     "Phytos",            
                     "StatckedPlotDivisionbyStationbyMonth.png"), 
    res=300, width=6,height=12, units="in")

print(
ggplot(Phyto_monthly, aes(x= Month , y=Median_BioVolume, fill=DIVISION)) + 
  labs(x='Month', y=Median_BioVolExp) +
  commonTheme_boxplot + 
  scale_y_continuous(limits=c(0, 7)) +
  geom_area(position='stack') + 
  # geom_path()
  # geom_boxplot(outlier.size=0.5, na.rm=T, size=1, outlier.shape=NA) + 
  # geom_point(na.rm=T) + 
  scale_x_continuous(breaks=1:12) + 
  guides(fill=guide_legend(title="")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  facet_grid( Station ~ .)
)

dev.off()



# Line plot by month
png(file = file.path(onedrive_dir,       
                     "Figures",     
                     "MonthlyCruises", 
                     "Phytos",             
                     "LinePlotDivisionbyStationbyMonth.png"),
    res=300, width=6,height=12, units="in")

print(
ggplot(Phyto_monthly, aes(x= Month , y=Median_BioVolume, color=DIVISION)) + 
  labs(x='Month', y=Median_BioVolExp) +
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



# Total biovolume line plot by month
png(file = file.path(onedrive_dir,      
                     "Figures",         
                     "MonthlyCruises",    
                     "Phytos",            
                     "TotalPhyto_LinePlotDivisionbyStationbyMonth.png"), 
    res=300, width=6,height=12, units="in")

print(
ggplot(Phyto_total_monthly, aes(x= Month , y=Median_BioVolume)) + 
  labs(x='Month', y=Median_BioVolExp) +
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



#Each day gets a barplot

Phyto_dates<-unique(Phyto_total$Date)

Phyto_i= 1
for (Phyto_i in 1:length(Phyto_dates)){
  Phyto_date<-Phyto_dates[Phyto_i]
  
  data_phyto <- Phyto_total[which(Phyto_total$Date==Phyto_date),]
  phyto_barplot<- ggplot(data_phyto, aes(x=Station, y=Total_BioVolume)) + 
    geom_bar(stat='identity', fill='seagreen4') + 
    theme_bw() + 
    labs(x='Station', y=BioVolExp)+ 
    ggtitle(Phyto_date)
  
  png(file = file.path(onedrive_dir,  
                       "Figures",     
                       "MonthlyCruises",    
                       "Phytos",
                       "MonthlyBarplots", 
                       paste0("PhytoTotals_", Phyto_date, ".png", sep="")),
      res=300, width=6,height=4, units="in")
  
  print(phyto_barplot)
  
  dev.off()
  
}



