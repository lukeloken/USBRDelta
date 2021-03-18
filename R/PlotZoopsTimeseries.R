# plot zooplankton timeseries

library(viridis)
library(ggplot2)
library(gridExtra)
library(plyr)
library(dplyr)
library(tidyr)
library(lubridate)
library(RColorBrewer)
# library(MASS)


# source('R/g_legend.R')
# # source('R/CompilePhytos.R')
# 
# # Project folder where outputs are stored
# dropbox_dir<-'C:/Dropbox/USBR Delta Project'
# 
# #Where data come from
# google_dir<-'C:/GoogleDrive/DeltaNutrientExperiment'

# SSC_joined_data<-readRDS(file=paste0(dropbox_dir, '/Data/Rdata/SSC_joined_data'))

SSC_joined_data <- readRDS(file = file.path(onedrive_dir, "RData", "MonthlyCruises", 
                                           "SSC_joined_data.rds"))

# #############################################
# Plot zooplankton
# #############################################

variables<-c("Bivalvia", "Cladocera", "Copepoda", "Gastropoda", "Ostracoda", "Rotifera", "Zoo_total") 
upperstations<-c('66', '70', '74', '76', '84')


#All Stations
plot_list<-list()

var<-1
for (var in 1:length(variables)){
  
  plot_list[[var]]<-ggplot(SSC_joined_data, aes_string('Date', variables[var], group='Station')) + 
    scale_shape_manual(values=rep(21:25, 5))  + 
    scale_fill_manual(values = colors_stations) + 
    scale_colour_manual(values = colors_stations) +
    geom_line(size=.5, aes(colour=Station,  group=Station)) +    
    geom_point(size=1, aes(fill=Station, shape=Station)) + 
    theme_bw() +
    theme(plot.title = element_text(hjust=0.5))  +
    theme(legend.position='none', axis.title.x = element_blank()) +
    scale_y_log10()
  
  # if (variables[var]=="Chloro.appb"){
  #   plot_list[[var]] <- plot_list[[var]] + 
  #     scale_y_log10()
  # }
  
}

# add legeng to first plot and then extract it
p1<-plot_list[[1]] + 
  theme(legend.position='bottom') +
  guides(shape = guide_legend(nrow = 2, title.position='top', title.hjust=0.5))
mylegend<-g_legend(p1)

# arrange plots without legend
p2<-grid.arrange(grobs=plot_list, ncol=1, as.table=F)


png(file = file.path(onedrive_dir,     
                     "Figures",               
                     "MonthlyCruises",    
                     "Zoops",
                     'AllGenus_TimeSeries.png'), units='in', width=7, height=12, res=400, bg='white')

grid.arrange(p2, mylegend, nrow=2, heights=c(10, 1.2))

dev.off()

