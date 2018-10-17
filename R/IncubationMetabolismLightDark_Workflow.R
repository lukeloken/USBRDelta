
#Load libraies

library(readxl)
library(openxlsx)

library(plyr)
library(dplyr)
library(tidyr)

library(ggplot2)
library(gridExtra)
library(RColorBrewer)

library(lubridate)

#choose starting directory
# data_dir<-("C:/Users/lcloken/Box/SadroLab/Incubation_Experiments/Delta_NutrientExperiment")

dropbox_dir<-'C:/Dropbox/USBR Delta Project'

#Load custom functions
source('R/CalculateIncubationMetabolism_LightDark.R')
source('R/read_excel_allsheets.R')
source('R/g_legend.R')

# Project folder where outputs are stored
results_dir<-paste0(dropbox_dir, "/Data/NutrientExperiment/IncubationMetabolism/MetabolismCalculations")

#Where data come from
data_dir<-paste0(dropbox_dir, "/Data/NutrientExperiment/IncubationMetabolism/OxygenData")

# set number of reads (How many times you read each jar per measurement)
reads <- 5

# Set date %m%d%y
# This number needs to be in the file name!

files<-list.files(data_dir)
# Date<-'101218'

if (length(grep('~', files))>0){
  files<-files[-grep('~', files)]
}

# goodfile<-files[grep(Date, files)]
# if (length(goodfile) == 0){
#   stop('No file found matching date')
# } else if (length(goodfile)>1.5){
#   stop('More than one file contains date indicated')
# } else if (length(goodfile)==1){
#   mysheets <- read_excel_allsheets(paste0(data_dir, '/', goodfile))
# }


# ###################################################
# Code looks into the data directory indicated above
# Finds the file with the matching date. 
# Shouldn't need to change anything below
# ###################################################
goodfile<-files[2]
for (goodfile in files){
  mysheets <- read_excel_allsheets(paste0(data_dir, '/', goodfile))
  
  #Calculate metabolims
  
  results<-CalculateIncubationMetabolism_LightDark(mysheets, reads)
  
  Date<-as.Date(gsub("[^0-9]", "", goodfile), format='%m%d%y')
  # Save to directory listed at the start
  # Metabolism estimates are in mg O2 per liter per hour
  write.csv(results, file=paste0(results_dir, '/JarMetabolism_', Date, '.csv'), row.names=F)
  
  summarytable <- results %>%
    select(Total, Site, Metric) %>%
    group_by(Site, Metric) %>%
    summarize(mean=mean(Total), sd=sd(Total)) 
  
  meantable <- summarytable %>%
    select (Site, Metric, mean) %>%
    spread(key=Metric, value=mean)
  
  meantable$GPP <- meantable$NEP-meantable$ER
  
  write.csv(summarytable, file=paste0(results_dir, '/SummaryMetabolism_', Date, '.csv'), row.names=F)
  

  # ######################
  # Plotting
  # ######################
  
  #Plotting parameters
  # jitterwidth=0.15
  colorset<-'Dark2'
  colors<-brewer.pal(3, colorset)[1:2]
  
  results$Site<-factor(results$Site, c('NL70', 'EC2', 'EC3','EC4','EC5','EC6','EC7','EC8', 'NL76'))
  results$Metric<-factor(results$Metric, c('NEP', 'ER'))
  
  
  # vector of metrics. This is what the loop will run through
  uniquemetrics<-levels(results$Metric)
  
  
  #Common theme for all boxplots
  commonTheme_boxplot<-list(
    theme_bw(),
    theme(plot.title = element_text(hjust=0.5), legend.position="none", axis.title.x=element_blank())
  )
  
  # Loop through metrics and make a gg object
  box_list<-list()
  plot_nu<-1
  for (plot_nu in 1:length(uniquemetrics)){
    # Pick data
    metric<-uniquemetrics[plot_nu]
    box_table<-results[results$Metric==metric,]
    col<-colors[plot_nu]
    #Plot
    box_list[[plot_nu]] <- ggplot(aes(y = Total, x = Site, fill = Treatment), data = box_table) + 
      labs(x=NA, y=metric) +
      geom_boxplot(outlier.size=0.5, col='black', fill=col) + 
      commonTheme_boxplot
  }
  
  
  # arrange plots without legend
  p2_box<-grid.arrange(grobs=box_list, ncol=1, as.table=F)
  
  
  #Add legend to bottom of figure and save
  png(paste0(dropbox_dir, '/Figures/NutrientExperiment/IncubationMetabolism/', Date, '_MetabolismBoxplot.png'), width=5, height=5, units='in', res=200)
  
  grid.arrange(p2_box, top=paste(as.Date(Date, format='%m%d%y')), bottom='Site', nrow=1)
  
  dev.off()
  
}










