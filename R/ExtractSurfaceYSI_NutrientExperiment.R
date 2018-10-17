


#Code to extract surface water measurements from profile data at fixed sites

library(readxl)
library(plyr)
library(dplyr)
library(viridis)
library(lubridate)
library(ggplot2)
library(gridExtra)

source('R/read_excel_allsheets.R')
source('R/g_legend.R')

# Project folder where outputs are stored
dropbox_dir<-'C:/Dropbox/USBR Delta Project'

#Where data come from
google_dir<-'C:/GoogleDrive/DeltaNutrientExperiment'

# Find all filenames in directory
# These will be used to loop through all old data
filenames<-list.files(paste0(dropbox_dir, '/Data/NutrientExperiment/YSIVerticalProfiles'))
filenames<-filenames[-grep('EXO', filenames)]

filename<-filenames[1]
directory<-paste0(dropbox_dir, '/Data/NutrientExperiment/YSIVerticalProfiles')
extract_surface<-function(filename, directory){
  df_all<-read.csv(paste(directory, filename, sep='/'), stringsAsFactors = F)
  df_all$DateTime.PT<-as.POSIXct( df_all$DateTime.PT, tz='America/Los_Angeles')
  
  df_surf<- df_all %>%
    subset(Depth.feet<3) %>%
    group_by(Station) %>% 
    summarize_all(mean, na.rm=T)
    
  return(df_surf)
}

biglist<-lapply(filenames, extract_surface, directory=paste0(dropbox_dir, '/Data/NutrientExperiment/YSIVerticalProfiles'))

bigdf<-ldply(biglist, data.frame)
bigdf$Date<-ymd(strftime(bigdf$DateTime.PT, format='%Y-%m-%d'))

output<-bigdf[which(!is.na(bigdf$Date)),]

write.csv(output, file=paste0(dropbox_dir, '/Data/NutrientExperiment/SurfaceChemistry/YSISurface.csv'), row.names=F)

          