

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

times<-field_df[c('DateTime_start','DateTime_end', 'Site')] 
times$DateTime_start<-times$DateTime_start-60
times$DateTime_end<-times$DateTime_end+60
times$interval<-interval(field_df$DateTime_start, field_df$DateTime_end)

#If there is an open excel sheet exclude it from the loop of filenames
if (length(grep('~', YSIfilenames))>0){
  YSIfilenames<-YSIfilenames[-grep('~', YSIfilenames)]
}

# Loop through filenames and read in data
i=1
for (i in 1:length(YSIfilenames)){
  YSI_df_i<-read_excel(paste0(google_dir, "/Data/NutrientExperiment/VerticalProfiles/", YSIfilenames[i]), skip=22)
  
  names(YSI_df_i)<-c('Date', 'Time', 'Time_Fract.', 'Site_name', 'Fault_code', 'Battery_V', 'CablePwr', 'Temp_C', 'Cond_uScm', 'SPC_uScm', 'Sal_psu', 'nLFCond', 'TDS', 'ODOsat', 'ODO_mgL', 'Turbidity_FNU', 'TSS', 'ChlA_ugL', 'ChlA_RFU', 'BGA_RFU', 'BGA_ugL', 'Press_psi', 'Depth_m')
  #Order each sheet by depth
  depth_name<-names(YSI_df_i)[grep('Depth', names(YSI_df_i))]
  
  stations<-c("NL70", "EC2", "EC3", "EC4", "EC5", "EC6", "EC7", "EC8", "NL76")
  
  if  (nrow(YSI_df_i)>1){
    
    column_names<-names(YSI_df_i)
    Date_name<-column_names[grep('Date',column_names)]
    Time_name<-column_names[grep('Time',column_names)]
    Time_name<-Time_name[-grep('Fract.', Time_name)]
    
    
    if (length(Date_name)==1){
      YSI_df_i$Date<-as.Date(as.data.frame(YSI_df_i)[,Date_name])
    } else if (length(Date_name)==0){
      warning ('No column named Date') 
    } else if  (length(Date_name)==2){
      YSI_df_i$Date<-YSI_df_i[,Date_name[1]]
      YSI_df_i[is.na(YSI_df_i$Date), 'Date']<-YSI_df_i[is.na(YSI_df_i$Date), Date_name[2]]
    }
    
    YSI_df_i$Time<-strftime(force_tz(as.data.frame(YSI_df_i)[,Time_name], "America/Los_Angeles"), format="%H:%M:%S")
    YSI_df_i$DateTime.PT<-ymd_hms(paste(YSI_df_i$Date, YSI_df_i$Time), tz='America/Los_Angeles')
    
    row<-10
    goodexo<-c()
    sitename<-c()
    for (row in 1:nrow(times)){
      interval<-as.data.frame(times[row,1:2])
      site<-as.character(unlist(times[row, c('Site')]))
      newexo<-which(YSI_df_i$DateTime.PT >= interval[,1] & YSI_df_i$DateTime.PT<=interval[,2])
      goodexo<-c(goodexo, newexo)
      sitename<-c(sitename,rep(site, length(newexo)))
    }
    
    #Subset based on time and depth
    YSI_df_goodtimes<-YSI_df_i[goodexo,]
    YSI_df_goodtimes$Site<-sitename
    YSI_df_goodtimes$Site<-factor(YSI_df_goodtimes$Site, stations)
    YSI_df_goodtimes<-YSI_df_goodtimes[order(YSI_df_goodtimes$DateTime.PT),]
    
    YSI_df_i_noNA<-YSI_df_goodtimes[which(YSI_df_goodtimes[depth_name]>0.2),]
    
    #Connect Station names with profiles
    # Figure out how to do this!!!!
    
    YSI_df_i_noNA$ProfileNumber<-NA
    row<-2
    for (row in 1:nrow(YSI_df_i_noNA)){
      if (row == 1){
        YSI_df_i_noNA$ProfileNumber[row] <- 1
      } else {
        timediff <- difftime(YSI_df_i_noNA$DateTime.PT[row], YSI_df_i_noNA$DateTime.PT[row-1], units='secs')
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
      summarize_at(c("Depth_m"), c(min, mean, max))
    
    GoodProfileNumbers<-summary_df$ProfileNumber[which((summary_df[,4]- summary_df[,2])>4)]
    YSI_df_i_noNA <- YSI_df_i_noNA[which(YSI_df_i_noNA$ProfileNumber %in% GoodProfileNumbers),]
    
    if (length(unique(YSI_df_i_noNA$ProfileNumber))!=9){
      stop ('Incorrect number of profiles') }
    
    
    YSI_df_goodvars<-YSI_df_i_noNA %>%
      select ("DateTime.PT", "ProfileNumber", "Site", "Depth_m", "Temp_C", "SPC_uScm", "ODOsat", "ODO_mgL", "Turbidity_FNU", "ChlA_RFU", "ChlA_ugL", "BGA_RFU", "BGA_ugL")

    
    #Used for final name
    Date<-median(as.Date(YSI_df_goodvars$DateTime.PT), na.rm=T)
    
    
    summary2<-YSI_df_goodvars %>%
      filter(Depth_m < 1) %>%
      group_by(Site) %>%
      summarize_at("SPC_uScm", c(min, median,  max))
    
    profile<-unique(YSI_df_goodvars$ProfileNumber)[1]
    list_down<-list()
    for (profile in unique(YSI_df_goodvars$ProfileNumber)){
      df<-YSI_df_goodvars[YSI_df_goodvars$ProfileNumber==profile,]
      max_depth<-max(df$Depth_m, na.rm=T)
      cut_depth<-max_depth-0.5
      first_bottom<-which(df$Depth_m>cut_depth)[1]-1
      df_down<-df[1:first_bottom,]
      list_down[[profile]]<-df_down
    }
    df_plot<-ldply(list_down, data.frame)
    
    
    df_plot<-df_plot[order(df_plot$DateTime.PT),]
    df_plot<-df_plot[order(df_plot$Depth_m),]
    df_plot<-df_plot[order(df_plot$Site),]
    write.csv(df_plot, file=paste0(dropbox_dir, '/Data/NutrientExperiment/YSIVerticalProfiles/EXO_VertialProfile_', Date, '.csv'), row.names=F)
    
    # ###########
    # plotting
    # ###########
    
    #colors
    color.palette = colorRampPalette(c(viridis(6, begin=.2, end=.98), rev(magma(5, begin=.35, end=.98))), bias=1)
    colors<-color.palette(length(unique(df_plot$Site)))
    
    
    #loop through each variable (skip station and depth)
    var_nu=5
    depth_col<-which(names(df_plot)=='Depth_m')
    plot_list<-list()
    for (var_nu in 5:ncol(df_plot)){
      
      var_name<-names(df_plot)[var_nu]
      
      plot_list[[var_nu-4]] <- ggplot(df_plot, aes_string(as.character(var_name), 'Depth_m', group='Site')) + 
        labs(x=var_name, y='Depth (m)') +
        scale_y_reverse() + 
        scale_shape_manual(values=rep(21:25, 5))  + 
        scale_fill_manual(values = colors) + 
        scale_colour_manual(values = colors) + 
        geom_path(aes(color=Site), size=1.5) + 
        geom_point(size=3, aes(fill=Site, shape=Site)) + 
        # ggtitle(var_name) +
        theme_bw() +
        theme(plot.title = element_text(hjust=0.5))  + 
        theme(legend.position='none')
      
      # print(plot_list[[var_nu]])
      
    }
    
    
    plot_withlegend <- plot_list[[1]] + 
      theme(plot.title = element_text(hjust=0.5), legend.position="bottom") +
      guides(shape = guide_legend(nrow = 3, title.position='top', title.hjust=0.5)) 
    
    mylegend<-g_legend(plot_withlegend)
    
    plot_list2<-plot_list
    plot_list2[[length(plot_list)+1]]<-mylegend
    # arrange plots with legend
    p2<-grid.arrange(grobs=plot_list2, nrow=ceiling(length(plot_list2)/2), as.table=F)
    
    
    # arrange multi plot with legend below and save to project folder
    png(paste0(dropbox_dir, '/Figures/NutrientExperiment/VerticalProfiles/', Date, '_EXO_VerticalProfiles.png'), width=8, height=16, units='in', res=200)
    
    grid.arrange(p2)
    
    dev.off()
  
    
  }
}
