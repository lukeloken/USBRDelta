
library(viridis)
library(ggplot2)
library(gridExtra)
library(plyr)
library(dplyr)
library(tidyr)
library(lubridate)


Phyto_df<-read.csv(file=paste(dropbox_dir, 'Data', 'Phyto', 'PhytoCountsAll.csv', sep='/'), stringsAsFactors = F)
Zoo_df <- read.csv(file=paste(dropbox_dir, 'Data', 'Zoops', 'ZoopsCountsAll.csv', sep='/'), stringsAsFactors = F)

Phyto_df$DATE<-as.Date(Phyto_df$DATE)
Zoo_df$date<-as.Date(Zoo_df$date)

plot(Zoo_df$date, Zoo_df$species.biomass..µg.d.w..L.)
plot(Phyto_df$DATE, Phyto_df$TOTAL.BV)

names(Phyto_df)

Phyto_summary<- Phyto_df %>%
  select(STATIONclean,DATE, DIVISION, TOTAL.BV, DENSITY) %>%
  group_by(STATIONclean,DATE, DIVISION) %>%
  summarize(Total_BioVolume=sum(TOTAL.BV), Density=sum(DENSITY))
  

names(Zoo_df)
Zoo_df$GenSpe<-paste(Zoo_df$genus, Zoo_df$species, sep='_')
