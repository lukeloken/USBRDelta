
library(viridis)
library(ggplot2)
library(gridExtra)
library(plyr)
library(dplyr)
library(tidyr)
library(lubridate)
library(RColorBrewer)

# source('R/CompilePhytos.R')

# Project folder where outputs are stored
dropbox_dir<-'C:/Dropbox/USBR Delta Project'

#Where data come from
google_dir<-'C:/GoogleDrive/DeltaNutrientExperiment'



Zoo_df <- read.csv(file=paste(dropbox_dir, 'Data', 'Zoops', 'ZoopsCountsAll.csv', sep='/'), stringsAsFactors = F)
Zoo_df$Month<-month(Zoo_df$date)
Zoo_df$date<-as.Date(Zoo_df$date)



Phyto_df<-read.csv(file=paste(dropbox_dir, 'Data', 'Phyto', 'PhytoCountsAll.csv', sep='/'), stringsAsFactors = F)

Phyto_df$DATE<-as.Date(Phyto_df$DATE)

GenusList<-unique(Phyto_df[c('GENUS', 'DIVISION')])


StationDates<-expand(Phyto_df, nesting(STATIONclean, DATE))

StationDatesGenus<-data.frame(matrix(ncol=4, nrow=nrow(StationDates)*nrow(GenusList)))
names(StationDatesGenus)<-c("STATIONclean", "DATE", "GENUS", "DIVISION")

StationDatesGenus$STATIONclean<-rep(StationDates$STATIONclean, each=nrow(GenusList))
StationDatesGenus$DATE<-rep(StationDates$DATE, each=nrow(GenusList))

StationDate<-1
for (StationDate in 1:nrow(StationDates)){
  StationDatesGenus[((StationDate-1)*nrow(GenusList)+1):(StationDate*nrow(GenusList)),]$GENUS<-GenusList$GENUS
  StationDatesGenus[((StationDate-1)*nrow(GenusList)+1):(StationDate*nrow(GenusList)),]$DIVISION<-GenusList$DIVISION
}

Phyto_CompleteList<-full_join(StationDatesGenus, Phyto_df)
Phyto_CompleteList$TALLY[which(is.na(Phyto_CompleteList$SAMPLE))]<-0
Phyto_CompleteList$DENSITY[which(is.na(Phyto_CompleteList$SAMPLE))]<-0
Phyto_CompleteList$TOTAL.BV[which(is.na(Phyto_CompleteList$SAMPLE))]<-0

Phyto_CompleteList$Month<-month(Phyto_CompleteList$DATE)

Phyto_CompleteList$STATIONclean=factor(Phyto_CompleteList$STATIONclean, c('16', '34','44', 'Pro', '56','62', '64','66', '70','74', '76','84', 'WSP'))

# 
# 
# Phyto_CompleteList<-complete(expand(Phyto_df, nesting(STATIONclean, DATE)), GENUS, fill=list(TALLY=0, DENSITY=0, TOTAL.BV=0))
# 
# Phyto_CompleteList<-complete(Phyto_df, STATIONclean, DATE, GENUS, fill=list(TALLY=0, DENSITY=0, TOTAL.BV=0))
# 
# 
# Phyto_CompleteList$DIVISION<-GenusList$DIVISION[match(Phyto_CompleteList$GENUS, GenusList$GENUS)]
# 
# head(data.frame(Phyto_CompleteList), 30)


# names(Phyto_CompleteList)
# FullFactors<-expand.grid(DATE=unique(Phyto_df$DATE), GENUS=unique(Phyto_df$GENUS), STATIONclean=unique(Phyto_df$STATIONclean))
# FullFactors$DIVISION
# 
# 
# Expand_Phyto_df<-full_join(FullFactors, Phyto_df)
# 
# GenusList<-complete(Phyto_df, STATIONclean, DATE, GENUS)
# 
# Divisions<-data.frame(GENUS=unique(Phyto_df$GENUS))
# Divisions$DIVISION<-Phyto_df$DIVISION[match(Divisions$GENUS, Phyto_df$GENUS) ]
# 
# Expand_Phyto_df$DIVISION<-Divisions$DIVISION[match(Expand_Phyto_df$GENUS, Divisions$GENUS)]
# 
# Expand_Phyto_df<-complete(Expand_Phyto_df, DATE, GENUS, STATIONclean, fill=list(TALLY=0, DENSITY=0, TOTAL.BV=0))


Phyto_summary<- Phyto_CompleteList %>%
  select(STATIONclean,DATE, DIVISION, TOTAL.BV, DENSITY, Month) %>%
  group_by(STATIONclean,DATE, DIVISION, Month) %>%
  summarize(Total_BioVolume=sum(TOTAL.BV)/(10^9), Density=sum(DENSITY)) %>%
  drop_na(STATIONclean)

Phyto_summary_select<- Phyto_summary %>%
  filter(DIVISION %in% c("Bacillariophyta", "Chlorophyta", "Cryptophyta", "Chrysophyta", "Cyanobacteria") & STATIONclean != '64')

Phyto_monthly <- Phyto_summary_select %>% 
  group_by(STATIONclean, DIVISION, Month) %>%
  summarize(Mean_BioVolume=mean(Total_BioVolume, na.rm=T), Median_BioVolume=median(Total_BioVolume, na.rm=T), Mean_Density=mean(Density, na.rm=T))

Phyto_monthly$Mean_BioVolume_log<-Phyto_monthly$Mean_BioVolume
Phyto_monthly$Mean_BioVolume_log[which(Phyto_monthly$Mean_BioVolume_log==0)]<-1

Phyto_monthly$Median_BioVolume_log<-Phyto_monthly$Median_BioVolume
Phyto_monthly$Median_BioVolume_log[which(Phyto_monthly$Median_BioVolume_log==0)]<-1000

Phyto_total <- Phyto_summary %>% 
  group_by(STATIONclean, DATE) %>%
  summarize(Total_BioVolume=sum(Total_BioVolume, na.rm=T), Total_Density=sum(Density, na.rm=T), Month=median(Month)) %>%
  filter(STATIONclean != '64')

Phyto_total_monthly <-Phyto_total  %>% 
  group_by(STATIONclean, Month) %>%
  summarize(Mean_BioVolume=mean(Total_BioVolume, na.rm=T), Median_BioVolume=median(Total_BioVolume, na.rm=T), Mean_Density=mean(Total_Density, na.rm=T))


# test<- complete(Phyto_monthly, STATIONclean, DIVISION, Month, fill = list(Total_BioVolume = 0, Density = 0))
# 
# complete(Phyto_monthly, Month, DIVISION, STATIONclean, fill=list(Density=9999))
# 
# 
# Phyto_monthly_DT<-data.table(Phyto_monthly)
# setkey(Phyto_monthly_DT,Month,STATIONclean, DIVISION)
# Phyto_monthly_DT[setkey(Phyto_monthly_DT[, .(min(period):max(period)), by = project], project, V1)]
# 
# EmptyFrame<-expand.grid(unique(Phyto_monthly$Month, weight = seq(100, 300, 50),
#             sex = c("Male","Female"))

# ##############################
# Phyto plots
# ##############################

BioVolExp<-expression(paste("Phytoplankton biovolume (1 x 10"^"9", " ", mu, "m"^"3", " L"^"-1", ")", sep=''))

Median_BioVolExp<-expression(paste("Median phytoplankton biovolume (1 x 10"^"9", " ", mu, "m"^"3", " L"^"-1", ")", sep=''))

#Boxplots by station
colorset<-'Accent'
colors<-brewer.pal(length(unique(Phyto_monthly$DIVISION)), colorset)
colors<-colors[c(2,1,3,4,5)]

#Common theme for boxplot
commonTheme_boxplot<-list(
  scale_fill_manual(values = colors),
  scale_colour_manual(values = colors),
  theme_bw(),
  theme(plot.title = element_text(hjust=0.5), legend.position="bottom")
)

png(paste0(dropbox_dir, "/Figures/Phytos/BoxplotGenusbyStation.png", sep=""), res=300, width=8,height=4, units="in")

ggplot(Phyto_summary_select, aes(x=STATIONclean, y=Total_BioVolume, fill=DIVISION)) + 
  labs(x='Station', y=BioVolExp) +
  commonTheme_boxplot + 
  # scale_y_log10(limits=c(1000, 100000000000)) +
  scale_y_continuous(limits=c(0, 5)) +
  guides(fill=guide_legend(title="")) + 
  geom_boxplot(outlier.size=0.5, na.rm=T, outlier.shape=NA)

dev.off()

png(paste0(dropbox_dir, "/Figures/Phytos/DensityBoxplotGenusbyStation.png", sep=""), res=300, width=8,height=4, units="in")

ggplot(Phyto_summary_select, aes(x=STATIONclean, y=Density, fill=DIVISION)) + 
  labs(x='Station', y='Density') +
  commonTheme_boxplot + 
  scale_y_log10(limits=c(1, 100000000000)) +
  # scale_y_continuous(limits=c(0, 100000000)) +
  guides(fill=guide_legend(title="")) + 
  geom_boxplot(outlier.size=0.5, na.rm=T, outlier.shape=NA)

dev.off()

# Boxplots by month


#Common theme for boxplot
commonTheme_boxplot<-list(
  scale_fill_manual(values = colors),
  scale_colour_manual(values = colors),
  theme_bw(),
  theme(plot.title = element_text(hjust=0.5), legend.position="bottom")
)

png(paste0(dropbox_dir, "/Figures/Phytos/BoxplotGenusbyMonth.png", sep=""), res=300, width=8,height=4, units="in")


ggplot(Phyto_summary_select, aes(x=as.factor(Month), y=Total_BioVolume, fill=DIVISION)) + 
  labs(x='Month', y=BioVolExp) +
  commonTheme_boxplot + 
  # scale_y_log10() + 
  scale_y_continuous(limits=c(0, 5)) +
  geom_boxplot(outlier.size=0.5, na.rm=T, outlier.shape=NA) + 
  guides(fill=guide_legend(title="")) 

dev.off()


#Boxplots by station by month by species

#colors
color.palette = colorRampPalette(c(viridis(6, begin=.2, end=.98), rev(magma(5, begin=.35, end=.98))), bias=1)
colors<-color.palette(length(unique(Phyto_summary_select$Month)))
shapes<-rep(21:25, 5)


#Common theme for boxplot
commonTheme_boxplot<-list(
  scale_fill_manual(values = colors),
  scale_colour_manual(values = colors),
  theme_bw(),
  theme(plot.title = element_text(hjust=0.5), legend.position="bottom")
)

png(paste0(dropbox_dir, "/Figures/Phytos/BoxplotGenusbyMonthbyStation.png", sep=""), res=300, width=12,height=12, units="in")

ggplot(Phyto_summary_select, aes(x= STATIONclean, y=Total_BioVolume, color=as.factor(Month))) + 
  labs(x='Station', y=BioVolExp) +
  commonTheme_boxplot + 
  # scale_y_continuous(limits=c(0, 50)) +
  scale_y_log10(limits=c(0.00001, 50)) +
  geom_boxplot(outlier.size=0.5, na.rm=T, size=1, outlier.shape=NA) + 
  # geom_point(na.rm=T) + 
  facet_grid( DIVISION ~ .) + 
  guides(color=guide_legend(title="Month"))

dev.off()



#Boxplots by month by station by species

#colors
color.palette = colorRampPalette(c(viridis(6, begin=.2, end=.98), rev(magma(5, begin=.35, end=.98))), bias=1)
colors<-color.palette(length(unique(Phyto_summary_select$STATIONclean)))
shapes<-rep(21:25, 5)


#Common theme for boxplot
commonTheme_boxplot<-list(
  scale_fill_manual(values = colors),
  scale_colour_manual(values = colors),
  theme_bw(),
  theme(plot.title = element_text(hjust=0.5), legend.position="bottom")
)

png(paste0(dropbox_dir, "/Figures/Phytos/BoxplotGenusbyStationbyMonth.png", sep=""), res=300, width=12,height=12, units="in")

ggplot(Phyto_summary_select, aes(x= as.factor(Month) , y=Total_BioVolume, color=STATIONclean)) + 
  labs(x='Month', y=BioVolExp) +
  commonTheme_boxplot + 
  scale_y_log10(limits=c(.00001, 50)) + 
  # scale_y_continuous(limits=c(0, 50)) +
  geom_boxplot(outlier.size=0.5, na.rm=T, size=1, outlier.shape=NA) + 
  # geom_point(na.rm=T) + 
  facet_grid( DIVISION ~ .) + 
  guides(color=guide_legend(title="Station"))

dev.off()




#Boxplots by month by station by species geom_area

#colors
colorset<-'Dark2'
colors<-brewer.pal(length(unique(Phyto_summary_select$DIVISION)), colorset)



#Common theme for boxplot
commonTheme_boxplot<-list(
  scale_fill_manual(values = colors),
  scale_colour_manual(values = colors),
  theme_bw(),
  theme(plot.title = element_text(hjust=0.5), legend.position="bottom")
)

# png(paste0(dropbox_dir, "/Figures/Phytos/BoxplotGenusbyStationbyMonth.png", sep=""), res=300, width=12,height=12, units="in")

png(paste0(dropbox_dir, "/Figures/Phytos/StatckedPlotDivisionbyStationbyDate.png", sep=""), res=300, width=6,height=12, units="in")


ggplot(Phyto_summary_select, aes(x= DATE , y=Total_BioVolume, fill=DIVISION)) + 
  labs(x='Month', y=BioVolExp) +
  commonTheme_boxplot + 
  # scale_y_log10(limits=c(10000, 10000000000)) + 
  geom_area(position='stack') + 
  # geom_path()
  # geom_boxplot(outlier.size=0.5, na.rm=T, size=1, outlier.shape=NA) + 
  # geom_point(na.rm=T) + 
  facet_grid( STATIONclean ~ .)

dev.off()


# Stacked Area by month
#colors
colorset<-'Accent'
colors<-brewer.pal(length(unique(Phyto_monthly$DIVISION)), colorset)
colors<-colors[c(2,1,3,4,5)]


#Common theme for boxplot
commonTheme_boxplot<-list(
  scale_fill_manual(values = colors),
  scale_colour_manual(values = colors),
  theme_bw(),
  theme(plot.title = element_text(hjust=0.5), legend.position="bottom", 
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank())
)

png(paste0(dropbox_dir, "/Figures/Phytos/StatckedPlotDivisionbyStationbyMonth.png", sep=""), res=300, width=6,height=12, units="in")

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
  facet_grid( STATIONclean ~ .)

dev.off()



# lines by month/station/division
#colors
colorset<-'Accent'
colors<-brewer.pal(length(unique(Phyto_monthly$DIVISION)), colorset)
colors<-colors[c(2,1,3,4,5)]


#Common theme for boxplot
commonTheme_boxplot<-list(
  scale_fill_manual(values = colors),
  scale_colour_manual(values = colors),
  theme_bw(),
  theme(plot.title = element_text(hjust=0.5), legend.position="bottom", 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
)

png(paste0(dropbox_dir, "/Figures/Phytos/LinePlotDivisionbyStationbyMonth.png", sep=""), res=300, width=6,height=12, units="in")

ggplot(Phyto_monthly, aes(x= Month , y=Median_BioVolume, color=DIVISION)) + 
  labs(x='Month', y=Median_BioVolExp) +
  commonTheme_boxplot + 
  # scale_y_log10() +
  # geom_jitter(size=3, width=0.1) + 
  geom_path(size=2) + 
  # geom_boxplot(outlier.size=0.5, na.rm=T, size=1, outlier.shape=NA) + 
  # geom_point(na.rm=T) + 
  scale_x_continuous(breaks=1:12) + 
  facet_grid( STATIONclean ~ .) + 
  guides(color=guide_legend(title=""))


dev.off()



# Total Phytoplankton biomass
# lines by month/station/division
#colors
colorset<-'Accent'
colors<-brewer.pal(length(unique(Phyto_monthly$DIVISION)), colorset)
colors<-colors[c(2,1,3,4,5)]


#Common theme for boxplot
commonTheme_boxplot<-list(
  scale_fill_manual(values = colors),
  scale_colour_manual(values = colors),
  theme_bw(),
  theme(plot.title = element_text(hjust=0.5), legend.position="bottom", 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
)

png(paste0(dropbox_dir, "/Figures/Phytos/TotalPhyto_LinePlotDivisionbyStationbyMonth.png", sep=""), res=300, width=6,height=12, units="in")

ggplot(Phyto_total_monthly, aes(x= Month , y=Median_BioVolume)) + 
  labs(x='Month', y=Median_BioVolExp) +
  commonTheme_boxplot + 
  # scale_y_log10() +
  # geom_jitter(size=3, width=0.1) + 
  geom_path(size=2) + 
  # geom_boxplot(outlier.size=0.5, na.rm=T, size=1, outlier.shape=NA) + 
  # geom_point(na.rm=T) + 
  scale_x_continuous(breaks=1:12) + 
  facet_grid( STATIONclean ~ .) + 
  guides(color=guide_legend(title=""))


dev.off()



