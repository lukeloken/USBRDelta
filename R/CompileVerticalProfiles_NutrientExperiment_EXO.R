

#Code to process YSI profiles from fixed sites

library(readxl)
library(plyr)
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
YSIfilenames<-list.files(paste0(google_dir, "/Data/NutrientExperiment/VerticalProfiles"))

#Exclude non-excel files
YSIfilenames<-YSIfilenames[grep('.xls', YSIfilenames)]
YSIfilenames<-YSIfilenames[grep('EXO', YSIfilenames)]
# YSIfilenames<-YSIfilenames[grep('Event', YSIfilenames)]

#If there is an open excel sheet exclude it from the loop of filenames
if (length(grep('~', YSIfilenames))>0){
  YSIfilenames<-YSIfilenames[-grep('~', YSIfilenames)]
}

# Loop through filenames and read in data
i=1
for (i in 1:length(YSIfilenames)){
YSI_df_i<-read_excel(paste0(google_dir, "/Data/NutrientExperiment/VerticalProfiles/", YSIfilenames[i]), skip=22)

#Order each sheet by depth
depth_name<-names(YSI_df_i)[grep('Depth', names(YSI_df_i))]

stations<-c("SSCN01_NV70", "SSCN02", "SSCN03", "SSCN04", "SSCN05", "SSCN06", "SSCN07", "SSCN08", "SSCN09 NL76")

if  (nrow(YSI_df_i)>1){

  YSI_df_i_noNA<-YSI_df_i[which(YSI_df_i[depth_name]>0.2),]
  
  #Connect Station names with profiles
  # Figure out how to do this!!!!
  
  YSI_df_i_noNA$ProfileNumber<-NA
  row<-2
  for (row in 1:nrow(YSI_df_i_noNA)){
    if (row == 1){
      YSI_df_i_noNA$ProfileNumber[row] <- 1
    } else {
      timediff <- difftime(YSI_df_i_noNA$`Time (HH:MM:SS)`[row], YSI_df_i_noNA$`Time (HH:MM:SS)`[row-1], units='secs')
      depthdiff <- YSI_df_i_noNA[row,c(depth_name)] - YSI_df_i_noNA[(row-1),c(depth_name)]
      if (timediff < 4*60) {
      YSI_df_i_noNA$ProfileNumber[row] <- YSI_df_i_noNA$ProfileNumber[row-1]
      } else {
        YSI_df_i_noNA$ProfileNumber[row] <- YSI_df_i_noNA$ProfileNumber[row-1]+1
      }
    }
  }
  
  summary_df <- YSI_df_i_noNA %>%
    group_by(ProfileNumber) %>%
    summarize_at(c("Depth m"), c(min, mean, max))
  
  GoodProfileNumbers<-summary_df$ProfileNumber[which((summary_df[,4]- summary_df[,2])>4)]
  YSI_df_i_noNA <- YSI_df_i_noNA[which(YSI_df_i_noNA$ProfileNumber %in% GoodProfileNumbers),]
  
  if (length(unique(YSI_df_i_noNA$ProfileNumber))!=9){
    stop ('Incorrect number of profiles') }
  
  #clean up YSI variables
  YSI_df_i_noNA$Time<-strftime(force_tz(YSI_df_i_noNA$"Time (HH:MM:SS)", "America/Los_Angeles"), format="%H:%M:%S")
  YSI_df_i_noNA$DateTime.PT<-ymd_hms(paste(as.Date(YSI_df_i_noNA$"Date (MM/DD/YYYY)"), YSI_df_i_noNA$Time), tz='America/Los_Angeles')
  

  
  YSI_df_goodvars<-YSI_df_i_noNA %>%
    select ("DateTime.PT","ProfileNumber", "Depth m",  "Temp °C", "SpCond µS/cm", "ODO % sat", "ODO mg/L", "Turbidity FNU", "Chlorophyll RFU", "Chlorophyll µg/L", "BGA-PC RFU", "BGA-PC µg/L", "Battery V")

  badnames<-names(YSI_df_goodvars)
  goodnames<-c("DateTime.PT", "ProfileNumber", "Depth_m", "Temp_C", "SPC_uScm", "ODOsat", "ODO_mgL", "Turbidity_FNU", "ChlA_RFU", "ChlA_ug/L", "BGA_RFU", "BGA_ugL", "Battery_V")
  
  names(YSI_df_goodvars)<-goodnames
  
  #Change station names
  
#Used for final name
Date<-median(as.Date(YSI_df_goodvars$DateTime.PT), na.rm=T)


summary2<-YSI_df_goodvars %>%
  filter(Depth_m < 1) %>%
  group_by(ProfileNumber) %>%
  summarize_at("SPC_uScm", c(min, median,  max))


YSI_df_goodvars$Station<-YSI_df_goodvars$ProfileNumber

df_plot<-YSI_df_goodvars[order(YSI_df_goodvars$DateTime.PT),]
df_plot<-df_plot[order(df_plot$Depth_m),]
df_plot<-df_plot[order(df_plot$Station),]
# write.csv(df_plot, file=paste0(dropbox_dir, '/Data/NutrientExperiment/YSIVerticalProfiles/EXO_VertialProfile_', Date, '.csv'), row.names=F)

# ###########
# plotting
# ###########

#colors
color.palette = colorRampPalette(c(viridis(6, begin=.2, end=.98), rev(magma(5, begin=.35, end=.98))), bias=1)
colors<-color.palette(length(unique(df_plot$Station)))


#loop through each variable (skip station and depth)
var_nu=4
depth_col<-which(names(df_plot)=='Depth_m')
plot_list<-list()
for (var_nu in 4:ncol(df_plot)){

  var_name<-names(df_plot)[var_nu]
  
    plot_list[[var_nu-3]] <- ggplot(df_plot, aes_string(as.character(var_name), 'Depth_m', group='Station')) + 
    labs(x=var_nu, y='Depth (m)') +
    scale_y_reverse() + 
    scale_shape_manual(values=rep(21:25, 5))  + 
    scale_fill_manual(values = colors) + 
    scale_colour_manual(values = colors) + 
    geom_path(aes(color=Station), size=1.5) + 
    geom_point(size=3, aes(fill=Station, shape=Station)) + 
    # ggtitle(var_name) +
    theme_bw() +
    theme(plot.title = element_text(hjust=0.5))  + 
    theme(legend.position='none')
      
    # print(plot_list[[var_nu]])
  
  }


# add legeng to first plot and then extract it
p1<-plot_list[[1]] + 
  theme(legend.position='bottom')
mylegend<-g_legend(p1)

# arrange plots without legend
p2<-grid.arrange(grobs=plot_list, ncol=2, as.table=F)

# arrange multi plot with legend below and save to project folder
png(paste0(dropbox_dir, '/Figures/NutrientExperiment/VerticalProfiles/', Date, '_VerticalProfiles.png'), width=8, height=16, units='in', res=200)

grid.arrange(p2, mylegend, nrow=2,heights=c(10, 0.5))

dev.off()

}
