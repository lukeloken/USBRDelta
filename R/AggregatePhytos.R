
library(viridis)
library(ggplot2)
library(gridExtra)
library(plyr)
library(dplyr)
library(tidyr)
library(lubridate)
library(RColorBrewer)


Phyto_df<-read.csv(file=paste(dropbox_dir, 'Data', 'Phyto', 'PhytoCountsAll.csv', sep='/'), stringsAsFactors = F)
Zoo_df <- read.csv(file=paste(dropbox_dir, 'Data', 'Zoops', 'ZoopsCountsAll.csv', sep='/'), stringsAsFactors = F)

Phyto_df$DATE<-as.Date(Phyto_df$DATE)
Zoo_df$date<-as.Date(Zoo_df$date)


Phyto_df$Month<-month(Phyto_df$DATE)
Zoo_df$Month<-month(Zoo_df$date)

names(Phyto_df)

Phyto_summary<- Phyto_df %>%
  select(STATIONclean,DATE, DIVISION, TOTAL.BV, DENSITY, Month) %>%
  group_by(STATIONclean,DATE, DIVISION, Month) %>%
  summarize(Total_BioVolume=sum(TOTAL.BV), Density=sum(DENSITY))

Phyto_monthly <- Phyto_summary %>% 
  group_by(STATIONclean, DIVISION, Month) %>%
  summarize(Total_BioVolume=mean(Total_BioVolume), Density=sum(Density))



#Boxplots by station
colorset<-'Dark2'
colors<-brewer.pal(length(unique(Phyto_summary$DIVISION)), colorset)

#Common theme for boxplot
commonTheme_boxplot<-list(
  scale_fill_manual(values = colors),
  scale_colour_manual(values = colors),
  theme_bw(),
  theme(plot.title = element_text(hjust=0.5), legend.position="bottom")
)

ggplot(Phyto_summary, aes(x=STATIONclean, y=Total_BioVolume, fill=DIVISION)) + 
  labs(x='Station', y="BioVolume") +
  commonTheme_boxplot + 
  scale_y_log10() + 
  geom_boxplot(outlier.size=0.5, na.rm=T)




#colors
# color.palette = colorRampPalette(c(viridis(6, begin=.2, end=.98), rev(magma(5, begin=.35, end=.98))), bias=1)
# colors<-color.palette(length(unique(Phyto_summary$STATIONclean)))
# shapes<-rep(21:25, 5)

#Common theme for boxplot
commonTheme_boxplot<-list(
  scale_fill_manual(values = colors),
  scale_colour_manual(values = colors),
  theme_bw(),
  theme(plot.title = element_text(hjust=0.5), legend.position="bottom")
)

ggplot(Phyto_monthly, aes(x=as.factor(Month), y=Total_BioVolume, fill=DIVISION)) + 
  labs(x='Month', y="BioVolume") +
  commonTheme_boxplot + 
  scale_y_log10() + 
  geom_boxplot(outlier.size=0.5, na.rm=T)






names(Zoo_df)
Zoo_df$GenSpe<-paste(Zoo_df$genus, Zoo_df$species, sep='_')
