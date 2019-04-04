





# Code to extract incubation metabolism results

# library(readxl)
library(plyr)
library(dplyr)
library(tidyr)
library(viridis)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(gtools)
library(grid)

# source('R/read_excel_allsheets.R')
# source('R/g_legend.R')
# 
# # Project folder where outputs are stored
# dropbox_dir<-'C:/Dropbox/USBR Delta Project'
# 
# #Where data come from
# google_dir<-'C:/GoogleDrive/DeltaNutrientExperiment'


# Find all filenames in directory
# These will be used to loop through all old data

inc_directory<-paste0(dropbox_dir, "/Data/Rdata/IncubationMetabolism")

filenames<-list.files(inc_directory)

filename<-filenames[1]

rm(IncMetab_AllDates)
for (filename in filenames){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("IncMetab_AllDates")){
    IncMetab_AllDates <- readRDS(paste(inc_directory, filename, sep='/'))
  } else {
    # if the merged dataset does exist, append to it
    temp_dataset <-readRDS(paste(inc_directory, filename, sep='/'))
    IncMetab_AllDates<-bind_rows(IncMetab_AllDates, temp_dataset)
    rm(temp_dataset)
  }
  
}

row.names(IncMetab_AllDates)<-NULL

IncMetab_summary <- IncMetab_AllDates %>%
  mutate(SampleDate = as.Date(SampleDate),
         Site = factor(Site, c('NL74', 'NL70', 'NL64', 'NL34'))) %>%
  drop_na(SampleDate) %>%
  dplyr::select(-Jar) %>%
  group_by(SampleDate, Site, Treatment, Day, Metric) %>%
  summarize_all(c("mean", "sd"))



#Plotting parameters
jitterwidth=0.15
colorset_incubation<-'Dark2'
colors_incubation<-brewer.pal(3, colorset_incubation)[c(1,3,2)]


# View data to see if it makes sense
Day1<-ggplot(IncMetab_summary[which(IncMetab_summary$Day==1),], aes(SampleDate, mean, group=Treatment, colour=Treatment)) +
  theme_bw() + 
  geom_hline(yintercept=0, colour='grey50') + 
  scale_colour_manual(values = colors_incubation) + 
  scale_fill_manual(values = colors_incubation) +
  geom_point() +
  geom_line() +
  theme(axis.title.x=element_blank(), legend.position='none') + 
  scale_x_date(date_labels="%b %Y") + 
  labs(y=expression(paste('mg ', O[2], ' L'^'-1', ' hr'^'-1'))) + 
  facet_grid(Site~Metric)

Day2<-ggplot(IncMetab_summary[which(IncMetab_summary$Day==2),], aes(SampleDate, mean, group=Treatment, colour=Treatment)) +
  theme_bw() + 
  geom_hline(yintercept=0, colour='grey50') + 
  scale_colour_manual(values = colors_incubation) + 
  scale_fill_manual(values = colors_incubation) +
  geom_point() +
  geom_line() +
  theme(axis.title.x=element_blank(), legend.position='none') + 
  scale_x_date(date_labels="%b %Y") + 
  labs(y=expression(paste('mg ', O[2], ' L'^'-1', ' hr'^'-1'))) + 
  facet_grid(Site~Metric)

Day3<-ggplot(IncMetab_summary[which(IncMetab_summary$Day==3),], aes(SampleDate, mean, group=Treatment, colour=Treatment)) +
  theme_bw() + 
  geom_hline(yintercept=0, colour='grey50') + 
  scale_colour_manual(values = colors_incubation) + 
  scale_fill_manual(values = colors_incubation) +
  geom_point() +
  geom_line() +
  theme(axis.title.x=element_blank(), legend.position='bottom') + 
  scale_x_date(date_labels="%b %Y") + 
  labs(y=expression(paste('mg ', O[2], ' L'^'-1', ' hr'^'-1'))) + 
  facet_grid(Site~Metric)


png(paste0(dropbox_dir, '/Figures/Incubations/GPPERNEP_meanbyday.png'), width=7, height=12, units='in', res=200)

grid.newpage()

plots<-grid.draw(rbind(ggplotGrob(Day1 + ggtitle('Day 1')), ggplotGrob(Day2 + ggtitle('Day 2')), ggplotGrob(Day3  + ggtitle('Day 3')), size = "last"))

dev.off()



png(paste0(dropbox_dir, '/Figures/Incubations/GPPERNEP_Day1_Timeseries.png'), width=8, height=6, units='in', res=200)

print(Day1 + theme(legend.position='bottom'))

dev.off()


png(paste0(dropbox_dir, '/Figures/Incubations/GPPERNEP_Day2_Timeseries.png'), width=8, height=6, units='in', res=200)

print(Day2 + theme(legend.position='bottom'))

dev.off()


png(paste0(dropbox_dir, '/Figures/Incubations/GPPERNEP_Day3_Timeseries.png'), width=8, height=6, units='in', res=200)

print(Day3 + theme(legend.position='bottom'))

dev.off()


# YSI_surf <- YSI_AllDepths %>%
#   subset(Depth.feet<3) %>%
#   group_by(Station, Date) %>% 
#   summarize_all(mean, na.rm=T) %>%
#   mutate(DepthStrata = 'lessthan3')
# 
# 
# YSI_mid <- YSI_AllDepths %>%
#   subset(Depth.feet>8 & Depth.feet<12) %>%
#   group_by(Station, Date) %>% 
#   summarize_all(mean, na.rm=T) %>%
#   mutate(DepthStrata = '8to10')
# 
# 
# YSI_deep <- YSI_AllDepths %>%
#   subset(Depth.feet>20) %>%
#   group_by(Station, Date) %>% 
#   summarize_all(mean, na.rm=T) %>%
#   mutate(DepthStrata = 'morethan20')
# 
# 
# YSI_ThreeDepths<-bind_rows(YSI_surf, YSI_mid, YSI_deep) %>%
#   dplyr::arrange(Date, Station, Depth.feet)
# 
# 
# write.csv(YSI_AllDepths, file=paste0(google_dir, '/DataOutputs/YSILongTermSites_AllDepths.csv'), row.names=F)
# saveRDS(YSI_AllDepths , file=paste0(dropbox_dir, '/Data/Rdata/YSI_AllDepths.rds'))
# 
# write.csv(YSI_ThreeDepths, file=paste0(google_dir, '/DataOutputs/YSILongTermSites_ThreeDepths.csv'), row.names=F)
# saveRDS(YSI_ThreeDepths , file=paste0(dropbox_dir, '/Data/Rdata/YSI_ThreeDepths.rds'))






