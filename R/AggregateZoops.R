
library(viridis)
library(ggplot2)
library(gridExtra)
library(plyr)
library(dplyr)
library(tidyr)
library(lubridate)
library(RColorBrewer)

# source('R/CompileZoos.R')

# Project folder where outputs are stored
dropbox_dir<-'C:/Dropbox/USBR Delta Project'

#Where data come from
google_dir<-'C:/GoogleDrive/DeltaNutrientExperiment'


Zoo_df<-read.csv(file=paste(dropbox_dir, 'Data', 'Zoops', 'ZoopsCountsAll.csv', sep='/'), stringsAsFactors = F)
Zoo_df$date<-as.Date(Zoo_df$date)

names(Zoo_df)[match(c("X.individuals.counted", "X....L", "species.biomass..µg.d.w..L."), names(Zoo_df))]<-c("Tally", "NumberPerLiter", "BiomassPerLiter")

# ############################
# Since data are 'count' data, need to populate dataset with zeros for any genus not counted in a given sample
# ############################

#All Zoo species recorded
GenusList<-unique(Zoo_df[c('genus', 'species', 'division')])

#Unique combinations of Date and Site
StationDates<-expand(Zoo_df, nesting(STATIONclean, date))

#Create empty data.frame with every sample site/date with every Zoo genus
StationDatesGenus<-data.frame(matrix(ncol=5, nrow=nrow(StationDates)*nrow(GenusList)))
names(StationDatesGenus)<-c("STATIONclean", "date", "genus", "species", "division")
StationDatesGenus$STATIONclean<-rep(StationDates$STATIONclean, each=nrow(GenusList))
StationDatesGenus$date<-rep(StationDates$date, each=nrow(GenusList))

StationDate<-1
for (StationDate in 1:nrow(StationDates)){
  StationDatesGenus[((StationDate-1)*nrow(GenusList)+1):(StationDate*nrow(GenusList)),]$genus<-GenusList$genus
  StationDatesGenus[((StationDate-1)*nrow(GenusList)+1):(StationDate*nrow(GenusList)),]$species<-GenusList$species
    StationDatesGenus[((StationDate-1)*nrow(GenusList)+1):(StationDate*nrow(GenusList)),]$division<-GenusList$division
}

#Join empty data.frame with observations
Zoo_CompleteList<-full_join(StationDatesGenus, Zoo_df)


#Replace NAs with zeros (not observed)
Zoo_CompleteList$NumberPerLiter[which(is.na(Zoo_CompleteList$Tally))]<-0
Zoo_CompleteList$BiomassPerLiter[which(is.na(Zoo_CompleteList$Tally))]<-0
Zoo_CompleteList$Tally[which(is.na(Zoo_CompleteList$Tally))]<-0




# ##################################################
# Summarize data by division then by calendar month
# ##################################################

# Modify columns structure to summarize and improve plotting
Zoo_CompleteList$Month<-month(Zoo_CompleteList$date)

Zoo_CompleteList$STATIONclean=factor(Zoo_CompleteList$STATIONclean, c('16', '34','44', 'Pro', '56','62', '64','66', '70','74', '76','84', 'WSP'))

#Save complete Zoo record
write.table(Zoo_CompleteList, file=paste(dropbox_dir, 'Data', 'Zoops', 'ZooAllGenus.csv', sep='/'), row.names=F, sep=',')

Zoo_summary<- Zoo_CompleteList %>%
  dplyr::select(STATIONclean, date, division, genus, BiomassPerLiter, NumberPerLiter, Month) %>%
  group_by(STATIONclean, date, division, Month) %>%
  dplyr::summarize(Total_BiomassPerLiter=sum(BiomassPerLiter), Total_NumberPerLiter=sum(NumberPerLiter)) %>%
  drop_na(STATIONclean)

Zoo_summary_select<- Zoo_summary %>%
  filter(division %in% c("Bivalvia", "Cladocera", "Copepoda", "Gastropoda", "Ostracoda", "Rotifera") & STATIONclean != '64')

Zoo_summary_spread <- Zoo_summary_select %>%
  dplyr::select(STATIONclean, date, division, Total_BiomassPerLiter) %>% 
  spread(key = division, value= Total_BiomassPerLiter)

#Export summary by division table to be merged with other datasets (Nutrients, Zoops, etc.)
write.table(Zoo_summary_spread, file=paste(dropbox_dir, 'Data', 'Zoops', 'ZooSummarydivision.csv', sep='/'), row.names=F, sep=',')

#Summarize by month to look at seasonal patterns
Zoo_monthly <- Zoo_summary_select %>% 
  group_by(STATIONclean, division, Month) %>%
  dplyr::summarize(Mean_Biomass=mean(Total_BiomassPerLiter, na.rm=T), Median_Biomass=median(Total_BiomassPerLiter, na.rm=T), Mean_NumberPerLiter=mean(Total_NumberPerLiter, na.rm=T))

Zoo_monthly$Mean_Biomass_log<-Zoo_monthly$Mean_Biomass
Zoo_monthly$Mean_Biomass_log[which(Zoo_monthly$Mean_Biomass_log==0)]<-1

Zoo_monthly$Median_Biomass_log<-Zoo_monthly$Median_Biomass
Zoo_monthly$Median_Biomass_log[which(Zoo_monthly$Median_Biomass_log==0)]<-1000

#Calculate total Zoo biomass and summarize
Zoo_total <- Zoo_summary %>% 
  group_by(STATIONclean, date) %>%
  dplyr::summarize(Total_BiomassPerLiter=sum(Total_BiomassPerLiter, na.rm=T), Total_NumberPerLiter=sum(Total_NumberPerLiter, na.rm=T), Month=median(Month)) %>%
  filter(STATIONclean != '64')

Zoo_total_monthly <-Zoo_total  %>% 
  group_by(STATIONclean, Month) %>%
  dplyr::summarize(Mean_Biomass=mean(Total_BiomassPerLiter, na.rm=T), Median_Biomass=median(Total_BiomassPerLiter, na.rm=T), Mean_NumberPerLiter=mean(Total_NumberPerLiter, na.rm=T))


# ##############################
# Zoo plots
# ##############################

BiomassExp<-expression(paste("Zooplankton biomass ( ", mu, "g d.w. L"^"-1", ")", sep=''))

Median_BiomassExp<-expression(paste("Median Zooplankton bioimass ( ", mu, "g d.w. L"^"-1", ")", sep=''))

#Boxplots by station
colorset<-'Accent'
colors<-brewer.pal(length(unique(Zoo_monthly$division)), colorset)
colors<-colors[c(4,1,2,3,6,5,7)]

#Common theme for boxplot
commonTheme_boxplot<-list(
  scale_fill_manual(values = colors),
  scale_colour_manual(values = colors),
  theme_bw(),
  theme(plot.title = element_text(hjust=0.5), legend.position="bottom")
)

png(paste0(dropbox_dir, "/Figures/Zoops/BoxplotGenusbyStation.png", sep=""), res=300, width=8,height=4, units="in")

ggplot(Zoo_summary_select, aes(x=STATIONclean, y=Total_BiomassPerLiter, fill=division)) + 
  labs(x='Station', y=BiomassExp) +
  commonTheme_boxplot + 
  # scale_y_log10() +
  scale_y_continuous(limits=c(0, 150)) +
  guides(fill=guide_legend(title="")) + 
  geom_boxplot(outlier.size=0.5, na.rm=T, outlier.shape=NA)

dev.off()

png(paste0(dropbox_dir, "/Figures/Zoops/BoxplotGenusbyStation_log.png", sep=""), res=300, width=8,height=4, units="in")

ggplot(Zoo_summary_select, aes(x=STATIONclean, y=Total_BiomassPerLiter, fill=division)) + 
  labs(x='Station', y=BiomassExp) +
  commonTheme_boxplot + 
  scale_y_log10() +
  # scale_y_continuous(limits=c(0, 150)) +
  guides(fill=guide_legend(title="")) + 
  geom_boxplot(outlier.size=0.5, na.rm=T, outlier.shape=NA)

dev.off()

png(paste0(dropbox_dir, "/Figures/Zoops/DensityBoxplotGenusbyStation.png", sep=""), res=300, width=8,height=4, units="in")

ggplot(Zoo_summary_select, aes(x=STATIONclean, y=Total_NumberPerLiter, fill=division)) + 
  labs(x='Station', y='Density') +
  commonTheme_boxplot + 
  scale_y_log10() +
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

png(paste0(dropbox_dir, "/Figures/Zoops/BoxplotGenusbyMonth.png", sep=""), res=300, width=8,height=4, units="in")


ggplot(Zoo_summary_select, aes(x=as.factor(Month), y=Total_BiomassPerLiter, fill=division)) + 
  labs(x='Month', y=BiomassExp) +
  commonTheme_boxplot + 
  # scale_y_log10() + 
  scale_y_continuous(limits=c(0,200)) +
  geom_boxplot(outlier.size=0.5, na.rm=T, outlier.shape=NA) + 
  guides(fill=guide_legend(title="")) 

dev.off()


#Boxplots by station by month by species

#colors
color.palette = colorRampPalette(c(viridis(6, begin=.2, end=.98), rev(magma(5, begin=.35, end=.98))), bias=1)
colors<-color.palette(length(unique(Zoo_summary_select$Month)))
shapes<-rep(21:25, 5)


#Common theme for boxplot
commonTheme_boxplot<-list(
  scale_fill_manual(values = colors),
  scale_colour_manual(values = colors),
  theme_bw(),
  theme(plot.title = element_text(hjust=0.5), legend.position="bottom")
)

png(paste0(dropbox_dir, "/Figures/Zoops/BoxplotGenusbyMonthbyStation.png", sep=""), res=300, width=12,height=12, units="in")

ggplot(Zoo_summary_select, aes(x= STATIONclean, y=Total_BiomassPerLiter, color=as.factor(Month))) + 
  labs(x='Station', y=BiomassExp) +
  commonTheme_boxplot + 
  # scale_y_continuous(limits=c(0, 50)) +
  scale_y_log10() +
  geom_boxplot(outlier.size=0.5, na.rm=T, size=1, outlier.shape=NA) + 
  # geom_point(na.rm=T) + 
  facet_grid( division ~ .) + 
  guides(color=guide_legend(title="Month"))

dev.off()



#Boxplots by month by station by species

#colors
color.palette = colorRampPalette(c(viridis(6, begin=.2, end=.98), rev(magma(5, begin=.35, end=.98))), bias=1)
colors<-color.palette(length(unique(Zoo_summary_select$STATIONclean)))
shapes<-rep(21:25, 5)


#Common theme for boxplot
commonTheme_boxplot<-list(
  scale_fill_manual(values = colors),
  scale_colour_manual(values = colors),
  theme_bw(),
  theme(plot.title = element_text(hjust=0.5), legend.position="bottom")
)

png(paste0(dropbox_dir, "/Figures/Zoops/BoxplotGenusbyStationbyMonth.png", sep=""), res=300, width=12,height=12, units="in")

ggplot(Zoo_summary_select, aes(x= as.factor(Month) , y=Total_BiomassPerLiter, color=STATIONclean)) + 
  labs(x='Month', y=BiomassExp) +
  commonTheme_boxplot + 
  scale_y_log10() + 
  # scale_y_continuous(limits=c(0, 50)) +
  geom_boxplot(outlier.size=0.5, na.rm=T, size=1, outlier.shape=NA) + 
  # geom_point(na.rm=T) + 
  facet_grid( division ~ .) + 
  guides(color=guide_legend(title="Station"))

dev.off()




#Boxplots by month by station by species geom_area

#colors
colorset<-'Accent'
colors<-brewer.pal(length(unique(Zoo_monthly$division)), colorset)
colors<-colors[c(4,1,2,3,6,5,7)]


#Common theme for boxplot
commonTheme_boxplot<-list(
  scale_fill_manual(values = colors),
  scale_colour_manual(values = colors),
  theme_bw(),
  theme(plot.title = element_text(hjust=0.5), legend.position="bottom")
)

# png(paste0(dropbox_dir, "/Figures/Zoops/BoxplotGenusbyStationbyMonth.png", sep=""), res=300, width=12,height=12, units="in")

png(paste0(dropbox_dir, "/Figures/Zoops/StatckedPlotdivisionbyStationbyDate.png", sep=""), res=300, width=6,height=12, units="in")


ggplot(Zoo_summary_select, aes(x= date , y=Total_BiomassPerLiter, fill=division)) + 
  labs(x='Month', y=BiomassExp) +
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
colors<-brewer.pal(length(unique(Zoo_monthly$division)), colorset)
colors<-colors[c(4,1,2,3,6,5,7)]


#Common theme for boxplot
commonTheme_boxplot<-list(
  scale_fill_manual(values = colors),
  scale_colour_manual(values = colors),
  theme_bw(),
  theme(plot.title = element_text(hjust=0.5), legend.position="bottom", 
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank())
)

png(paste0(dropbox_dir, "/Figures/Zoops/StatckedPlotdivisionbyStationbyMonth.png", sep=""), res=300, width=6,height=12, units="in")

ggplot(Zoo_monthly, aes(x= Month , y=Median_Biomass, fill=division)) + 
  labs(x='Month', y=Median_BiomassExp) +
  commonTheme_boxplot + 
  scale_y_continuous() +
  geom_area(position='stack') + 
  # geom_path()
  # geom_boxplot(outlier.size=0.5, na.rm=T, size=1, outlier.shape=NA) + 
  # geom_point(na.rm=T) + 
  scale_x_continuous(breaks=1:12) + 
  guides(fill=guide_legend(title="")) + 
  facet_grid( STATIONclean ~ .)

dev.off()



# lines by month/station/division


#Common theme for boxplot
commonTheme_boxplot<-list(
  scale_fill_manual(values = colors),
  scale_colour_manual(values = colors),
  theme_bw(),
  theme(plot.title = element_text(hjust=0.5), legend.position="bottom", 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
)

png(paste0(dropbox_dir, "/Figures/Zoops/LinePlotdivisionbyStationbyMonth.png", sep=""), res=300, width=6,height=12, units="in")

ggplot(Zoo_monthly, aes(x= Month , y=Median_Biomass, color=division)) + 
  labs(x='Month', y=Median_BiomassExp) +
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



# Total Zooplankton biomass
#
#Common theme for boxplot
commonTheme_boxplot<-list(
  scale_fill_manual(values = colors),
  scale_colour_manual(values = colors),
  theme_bw(),
  theme(plot.title = element_text(hjust=0.5), legend.position="bottom", 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
)

png(paste0(dropbox_dir, "/Figures/Zoops/TotalZoo_LinePlotdivisionbyStationbyMonth.png", sep=""), res=300, width=6,height=12, units="in")

ggplot(Zoo_total_monthly, aes(x= Month , y=Median_Biomass)) + 
  labs(x='Month', y=Median_BiomassExp) +
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



