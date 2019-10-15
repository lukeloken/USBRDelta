





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

fert_dates<-as.Date(c("2019-07-22", "2019-07-23","2019-07-24", "2019-07-25", "2019-08-05", "2019-08-06","2019-08-07", "2019-08-08"))


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

inc_directory<-paste0(dropbox_dir, "/Data/Rdata_SSCN2/IncubationMetabolism")

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
         Site = factor(Site, paste(
           'site', 1:7, sep=''))) %>%
  drop_na(SampleDate, Site) %>%
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
  geom_vline(xintercept = fert_dates, colour='green', linetype='dashed') + 
  scale_colour_manual(values = colors_incubation) + 
  scale_fill_manual(values = colors_incubation) +
  # geom_errorbar(aes(ymin=(mean-sd), ymax=(mean+sd), color=Treatment), width=0, size=1.2) + 
  geom_point() +
  geom_line() +
  theme(axis.title.x=element_blank(), legend.position='bottom') + 
  scale_x_date(date_labels="%b%d", date_breaks="1 week", minor_breaks=NULL) + 
  labs(y=expression(paste('mg ', O[2], ' L'^'-1', ' hr'^'-1')), colour = "Light treatment (%)") + 
  facet_grid(Site~Metric)


png(paste0(dropbox_dir, '/Figures/NutrientExperiment2/Incubations/MetabolismTimeseries.png'), width=10, height=8, units='in', res=200)

print(Day1)

dev.off()

