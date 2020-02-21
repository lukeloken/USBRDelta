

#Code to process YSI profiles from fixed sites during experiment 2
library(readxl)
library(plyr)
library(dplyr)
library(tidyr)
library(viridis)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(grid)
library(stringr)
library(RColorBrewer)
library(RcppRoll)
library(ggpubr)

dir.create(file.path(onedrive_dir, "Figures", "NutrientExperiment2", "YSIProfiles"), showWarnings = F)
dir.create(file.path(onedrive_dir, "OutputData", "NutrientExperiment2", "YSIProfiles"), showWarnings = F)
dir.create(file.path(onedrive_dir, "RData", "NutrientExperiment2", "YSIProfiles"), showWarnings = F)


# Find all filenames in directory
# These will be used to loop through all old data
YSIfilenames<-list.files(file.path(onedrive_dir, "RawData", "NutrientExperiment2", "YSIVerticalProfiles"))

#Exclude non-excel files
YSIfilenames<-YSIfilenames[grep('.csv', YSIfilenames)]

#If there is an open excel sheet exclude it from the loop of filenames
if (length(grep('~', YSIfilenames))>0){
  YSIfilenames<-YSIfilenames[-grep('~', YSIfilenames)]
}

# Loop through filenames and read in data
i=27
for (i in 1:length(YSIfilenames)){
YSI_df_i<-read.csv(file.path(onedrive_dir, "RawData", "NutrientExperiment2", "YSIVerticalProfiles", YSIfilenames[i]))

#Order each sheet by depth
depth_name<-names(YSI_df_i)[grep('DEP', names(YSI_df_i))]

sites<-unique(YSI_df_i$Site)




if  (nrow(YSI_df_i)>1){

  YSI_df_i_ordered<- YSI_df_i %>%
    select_if(~sum(!is.na(.)) > 0) %>% #drop columns with all NAs
    dplyr::select(-TSS.mg.L, -BGA.PC.c.mL) %>% #drop columns with zeros
    dplyr::rename( #Change variable names
      Temp_C = 'X.C',
      AirPressure_mmHg = 'mmHg',
      DO_perSat = 'DO..',
      SPC_uScm = 'SPC.uS.cm',
      Turb_FNU= 'FNU',
      BGA_RFU = 'BGA.PC.RFU',
      BGA_ugL = 'BGA.PC.ug.L',
      ChlA_RFU = 'Chl.RFU',
      ChlA_ugL = 'Chl.ug.L',
      Depth_m = 'DEP.m'
    ) %>%
    drop_na(Depth_m) %>% #Drop rows with no depth
    arrange(Site, Depth_m) %>% #Arrange by site and by depth
    mutate(DateTime = as.POSIXct(paste(mdy(Date), Time, sep=' '), tz='UTC'), 
           Date=mdy(Date))
  
  if ("DO.mg.L" %in% names(YSI_df_i_ordered)){
    YSI_df_i_ordered <- dplyr::rename(YSI_df_i_ordered, DO_mgL = 'DO.mg.L')
  }
    
  

  #Create summary table to identify outliers for Chla and Turb
YSI_df_i_summary <- YSI_df_i_ordered %>%
  group_by(Site) %>%
  dplyr::summarize(
    medianChlA = median(ChlA_ugL),
    medianTurb = median(Turb_FNU),
    madChlA = mad(ChlA_ugL),
    madTurb = mad(Turb_FNU)
  )

badChlA <-which(
  YSI_df_i_ordered$Depth_m > 4 & 
  YSI_df_i_ordered$ChlA_ugL > 
    YSI_df_i_summary$medianChlA[match(YSI_df_i_ordered$Site, YSI_df_i_summary$Site)] + 
    YSI_df_i_summary$madChlA[match(YSI_df_i_ordered$Site, YSI_df_i_summary$Site)]*5
)

badTurb <-which(
  YSI_df_i_ordered$Depth_m > 4 & 
    YSI_df_i_ordered$Turb_FNU > 
    YSI_df_i_summary$medianTurb[match(YSI_df_i_ordered$Site, YSI_df_i_summary$Site)] + 
    YSI_df_i_summary$madTurb[match(YSI_df_i_ordered$Site, YSI_df_i_summary$Site)]*5
)

omitrows<-unique(c(badTurb, badChlA))

if (length(omitrows>0)){
  # YSI_df_i_cleaned<-YSI_df_i_ordered[-omitrows,]
  YSI_df_i_cleaned<-YSI_df_i_ordered[-badTurb,]
} else if (length(omitrows)==0){
  YSI_df_i_cleaned<-YSI_df_i_ordered
  
}

#Used for final name
Date<-median(as.Date(YSI_df_i_ordered$Date), na.rm=T)


write.csv(YSI_df_i_cleaned, file=file.path(onedrive_dir, 'OutputData', 'NutrientExperiment2', 'YSIProfiles', paste0('VertialProfile_', Date, '.csv')), row.names=F)
saveRDS(YSI_df_i_cleaned , file=file.path(onedrive_dir, 'RData', 'NutrientExperiment2', 'YSIProfiles', paste0('VerticalProfile_', Date, '.rds')))

# ###########
# plotting
# ###########

#colors
color.palette = colorRampPalette(c(viridis(6, begin=.2, end=.98), rev(magma(5, begin=.35, end=.98))), bias=1)
colors<-color.palette(length(sites))


#loop through each variable (skip station and depth)
var_nu=1
depth_col<-which(names(YSI_df_i_cleaned)=='Depth_m')
plot_list<-list()
plotcleaned_list<-list()

plot_vars<-names(YSI_df_i_cleaned)[-which(names(YSI_df_i_cleaned) %in% c('Date', 'Time', 'Site', 'AirPressure_mmHg', 'Depth_m', 'DateTime'))]
var_nu = 1
for (var_nu in 1:length(plot_vars)){

  var_name<-plot_vars[var_nu]
  
    plot_list[[var_nu]] <- ggplot(YSI_df_i_ordered, aes_string(as.character(var_name), 'Depth_m', group='Site')) + 
    labs(x=var_name, y='Depth (m)') +
    scale_y_reverse() + 
    scale_shape_manual(values=rep(21:25, 5))  + 
    scale_fill_manual(values = colors) + 
    scale_colour_manual(values = colors) + 
    geom_path(aes(color=Site), size=1.5) + 
    geom_point(size=3, aes(fill=Site, shape=Site)) + 
    ggtitle(var_name) +
    theme_bw() +
    theme(plot.title = element_text(hjust=0.5))  + 
    theme(legend.position='none') 
      
    plotcleaned_list[[var_nu]] <- ggplot(YSI_df_i_cleaned, aes_string(as.character(var_name), 'Depth_m', group='Site')) + 
      labs(x=var_name, y='Depth (m)') +
      scale_y_reverse() + 
      scale_shape_manual(values=rep(21:25, 5))  + 
      scale_fill_manual(values = colors) + 
      scale_colour_manual(values = colors) + 
      geom_path(aes(color=Site), size=1.5) + 
      geom_point(size=3, aes(fill=Site, shape=Site)) + 
      ggtitle(var_name) +
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
p2<-grid.arrange(grobs=plot_list, ncol=3, as.table=F)
p_cleaned<-grid.arrange(grobs=plotcleaned_list, ncol=3, as.table=F)

# arrange multi plot with legend below and save to project folder
png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'YSIProfiles', paste0(Date, '_VerticalProfiles_cleaned.png')), width=10, height=12, units='in', res=200)

grid.arrange(p_cleaned, mylegend, nrow=2,heights=c(10, 0.5))

dev.off()



png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'YSIProfiles', paste0(Date, '_VerticalProfiles_Alldata.png')), width=10, height=12, units='in', res=200)

grid.arrange(p2, mylegend, nrow=2,heights=c(10, 0.5))

dev.off()


}

}
