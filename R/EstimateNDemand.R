

# Estimate NO3 demand

# library(readxl)
library(plyr)
library(dplyr)
library(tidyr)
library(RColorBrewer)
# library(viridis)
# library(lubridate)
# library(ggplot2)
# library(gridExtra)
# 
# 
# source('R/read_excel_allsheets.R')
# source('R/g_legend.R')

# Project folder where outputs are stored
dropbox_dir<-'C:/Dropbox/USBR Delta Project'

#Where data come from
google_dir<-'C:/GoogleDrive/DeltaNutrientExperiment'

Date<-'081618'

DOrate_mean_long_table<-read.csv(file=paste0(dropbox_dir, '/Data/Incubations/MetabolismCalculations/JarMetabolism_', Date, '.csv'))


# df<- DOrate_mean_long_table %>%
#   subset(Metric %in% c('GPP', 'ER') & Treatment %in% c('+N', '+NP')) %>%
#   spread(Metric, Value)

df<- DOrate_mean_long_table %>%
  subset(Metric %in% c('GPP', 'ER', 'NEP')) %>%
  spread(Metric, Value)

#mg O2 per liter per hour
#rough estimates from incubation experiments
GPP=df$GPP
ER=df$ER
NEP=df$NEP

GPPdaily<-GPP*6
ERdaily<-ER*24



#autotrophic respitory constant (How much of GPP goes toward production)
ra=0.5

#C:N molar ratios of autos and heteros. How much N do they need to match their carbon production 
CNa<-20/1
CNh<-20/1
ratios<-c(12,20,25)

#Heterotrophic growth efficiency (HGE = Ph/(Ph + Rh))
HGE<-0.2
HGEs<-c(0.05,0.2)

CalculateNdemand<-function(GPP, ER, ra, CNa, CNh, HGE){
  
  #Autotrohps
  #Autotrophic production (convert to molar units)
  Pa= (ra)*GPP/32
  
  #Autotrophic N demand (molar)
  Ua=Pa/CNa
  
  #Heterotrophs
  #respiration heterotroph (convert to molar)
  Rh<-(ER-(1-ra)*GPP)/32
  
  #production heterotroph
  Ph<-(HGE*Rh)/(1-HGE)
  
  # heterotrohpic N demand (molar)
  Uh<-Ph/CNh
  
  #total N demand (molar)
  U<-Uh+Ua
  
  #convert to mg per liter
  Umg<-U*14.01
  
  return(Umg)
}

U_NEP<-NEP*14.01/32/CNa*24

test<-CalculateNdemand(GPP, ER, ra, CNa, CNh, HGE)

U_ratios1_hour<-sapply(ratios, function (x) CalculateNdemand(GPP, ER, ra, x,x, HGEs[1]))
U_ratios2_hour<-sapply(ratios, function (x) CalculateNdemand(GPP, ER, ra, x,x, HGEs[2]))

U_ratios1<-U_ratios1_hour*24
U_ratios2<-U_ratios2_hour*24

U_bound<-cbind(U_ratios1, U_ratios2)
colnames(U_bound)<-paste('U_', letters[1:ncol(U_bound)])

df2<-cbind(df, U_bound)

df3<- df2 %>%
  gather("U_est", 'Value', 9:14)

df3$U_NEP<-U_NEP

# Assuming 6 hours of light, 18 hours of dark
U_ratios1_daily<-sapply(ratios, function (x) CalculateNdemand(GPPdaily, ERdaily, ra, x,x, HGEs[1]))
U_ratios2_daily<-sapply(ratios, function (x) CalculateNdemand(GPPdaily, ERdaily, ra, x,x, HGEs[2]))

U_bound_daily<-cbind(U_ratios1_daily, U_ratios2_daily)
colnames(U_bound_daily)<-paste('U_', letters[1:ncol(U_bound)])

df2_daily<-cbind(df, U_bound_daily)

df3_daily<- df2_daily %>%
  gather("U_est", 'Value', 9:14) %>%
  group_by(Treatment, Day, Site, Jar) %>% 
  summarize_at('Value', mean, na.rm=T)



#Plotting parameters
jitterwidth=0.15
colorset<-'Dark2'
colors<-brewer.pal(3, colorset)[c(1,3,2)]



#Common theme for all boxplots
commonTheme_boxplot<-list(
  scale_fill_manual(values = colors),
  scale_colour_manual(values = colors),
  theme_bw(),
  theme(plot.title = element_text(hjust=0.5), legend.position="none"),
  geom_boxplot(outlier.size=0.5),
  coord_cartesian(ylim=range(df3$Value, na.rm=T))
)

sites<-unique(df3$Site)[order(unique(df3$Site))]

# Loop through metrics and make a gg object
box_list<-list()
plot_nu<-1
for (plot_nu in 1:length(sites)){
  # Pick data
  box_table<-df3[df3$Site==sites[plot_nu],]
  box_table$Day<-factor(box_table$Day)
  #Plot
  box_list[[plot_nu]] <- ggplot(aes(y = Value, x = Day, fill = Treatment), data = box_table) + 
    labs(x='Day', y=expression(paste(NO[3], ' demand (mg N L'^'-1', ' d'^'-1', ')'))) +
    ggtitle(sites[plot_nu]) +
    commonTheme_boxplot
}


#Add and extract legend from first plot
box_withlegend <- box_list[[1]] + 
  theme(legend.position='bottom') 

mylegend_box<-g_legend(box_withlegend)


# arrange plots without legend
p2_box<-grid.arrange(grobs=box_list, ncol=4, as.table=F)

#Add legend to bottom of figure and save
png(paste0(dropbox_dir, '/Figures/NDemand/', Date, 'PredictedNDemand_IncubationMetabolism_Boxplot.png'), width=8, height=3, units='in', res=200)

grid.arrange(p2_box, mylegend_box, nrow=2,heights=c(10, 1))

dev.off()


#Using NEP

box_list<-list()
plot_nu<-1
for (plot_nu in 1:length(sites)){
  # Pick data
  box_table<-df3[df3$Site==sites[plot_nu],]
  box_table$Day<-factor(box_table$Day)
  #Plot
  box_list[[plot_nu]] <- ggplot(aes(y = U_NEP, x = Day, fill = Treatment), data = box_table) + 
    labs(x='Day', y=expression(paste(NO[3], ' demand (mg N L'^'-1', ' d'^'-1', ')'))) +
    ggtitle(sites[plot_nu]) +
    commonTheme_boxplot
}



# #######################################################
# Using 6 hour lights on/off for GPP/ER daily summation
# #######################################################
#Common theme for all boxplots
commonTheme_boxplot<-list(
  scale_fill_manual(values = colors),
  scale_colour_manual(values = colors),
  theme_bw(),
  theme(plot.title = element_text(hjust=0.5), legend.position="none"),
  geom_boxplot(outlier.size=0.5),
  coord_cartesian(ylim=range(df3_daily$Value, na.rm=T))
)

sites<-unique(df3_daily$Site)[order(unique(df3_daily$Site))]

# Loop through metrics and make a gg object
box_list<-list()
plot_nu<-1
for (plot_nu in 1:length(sites)){
  # Pick data
  box_table<-df3_daily[df3_daily$Site==sites[plot_nu],]
  box_table$Day<-factor(box_table$Day)
  #Plot
  box_list[[plot_nu]] <- ggplot(aes(y = Value, x = Day, fill = Treatment), data = box_table) + 
    labs(x='Day', y=expression(paste(NO[3], ' demand (mg N L'^'-1', ' d'^'-1', ')'))) +
    ggtitle(sites[plot_nu]) +
    commonTheme_boxplot
}

#Add and extract legend from first plot
box_withlegend <- box_list[[1]] + 
  theme(legend.position='bottom') 

mylegend_box<-g_legend(box_withlegend)


# arrange plots without legend
p2_box<-grid.arrange(grobs=box_list, ncol=4, as.table=F)

#Add legend to bottom of figure and save
png(paste0(dropbox_dir, '/Figures/NDemand/', Date, 'PredictedNDemand_6HourLight_IncubationMetabolism_Boxplot.png'), width=8, height=3, units='in', res=200)

grid.arrange(p2_box, mylegend_box, nrow=2,heights=c(10, 1))

dev.off()


