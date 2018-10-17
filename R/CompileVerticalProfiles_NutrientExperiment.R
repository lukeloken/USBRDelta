

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
YSIfilenames<-YSIfilenames[-grep('EXO', YSIfilenames)]
YSIfilenames<-YSIfilenames[grep('Event', YSIfilenames)]

#If there is an open excel sheet exclude it from the loop of filenames
if (length(grep('~', YSIfilenames))>0){
  YSIfilenames<-YSIfilenames[-grep('~', YSIfilenames)]
}

# Loop through filenames and read in data
i=7
for (i in 1:length(YSIfilenames)){
YSI_list_i<-read_excel_allsheets(paste0(google_dir, "/Data/NutrientExperiment/VerticalProfiles/", YSIfilenames[i]))

#Order each sheet by depth
depth_name<-names(YSI_list_i[[1]])[grep('Depth', names(YSI_list_i[[1]]))]

stations<-c("SSCN01_NV70", "SSCN02", "SSCN03", "SSCN04", "SSCN05", "SSCN06", "SSCN07", "SSCN08", "SSCN09 NL76")

if  (length(YSI_list_i)>1){

  sheets<-as.numeric(sapply(stations, grep, x=names(YSI_list_i)))
  YSI_list_i2<-YSI_list_i[sheets[is.finite(sheets)]]
  
  
  YSI_list_i_ordered<-lapply(YSI_list_i2, function(l) l[order(l[,depth_name]),])
  YSI_list_i_noNA<-lapply(YSI_list_i_ordered, function(l) l[!is.na(l[,depth_name]),])
  
  #Exclude bad data from the bottom (high Turb and Chla)
  df<-5
  for (df in 1:length(YSI_list_i_noNA)){
  
  sdChl<-sd(YSI_list_i_noNA[[df]]$`Chl RFU`, na.rm=T)
  meanChl<-mean(YSI_list_i_noNA[[df]]$`Chl RFU`, na.rm=T)
  
  badrows<-which(YSI_list_i_noNA[[df]]$`Chl RFU` > meanChl+sdChl*2 & YSI_list_i_noNA[[df]][,c(depth_name)]>=(max(YSI_list_i_noNA[[df]][,c(depth_name)] )-4))
  YSI_list_i_noNA[[df]]$`Chl ug/L`[badrows] <-NA
  YSI_list_i_noNA[[df]]$`Chl RFU`[badrows] <-NA
  
  sdTurb<-sd(YSI_list_i_noNA[[df]]$`Turbid+ NTU`, na.rm=T)
  meanTurb<-mean(YSI_list_i_noNA[[df]]$`Turbid+ NTU`, na.rm=T)
  
  badTurb<-which(YSI_list_i_noNA[[df]]$`Turbid+ NTU` > meanTurb+sdTurb*2 & YSI_list_i_noNA[[df]][,c(depth_name)]>20 )
  
  YSI_list_i_noNA[[df]]$`Turbid+ NTU`[badTurb] <-NA
  
  }
  
  YSI_df_i<-ldply(YSI_list_i_noNA, data.frame)


#Change station names
site<-stations[2]
for (site in stations){
  YSI_df_i$'.id'[grep(site, YSI_df_i$'.id')]<-site
}

}




#Identify column names of compiled data.frame and make corrections if they are in the wrong format (e.g., meters vs ft)
column_names<-names(YSI_df_i)
Date_name<-column_names[grep('Date',column_names)]
Time_name<-column_names[grep('Time',column_names)]


if (length(Date_name)==1){
YSI_df_i$Date<-as.Date(YSI_df_i[,Date_name])
} else if (length(Date_name)==0){
  warning ('No column named Date') 
} else if  (length(Date_name)==2){
  YSI_df_i$Date<-YSI_df_i[,Date_name[1]]
  YSI_df_i[is.na(YSI_df_i$Date), 'Date']<-YSI_df_i[is.na(YSI_df_i$Date), Date_name[2]]
}

YSI_df_i$Time<-strftime(force_tz(YSI_df_i[,Time_name], "America/Los_Angeles"), format="%H:%M:%S")
YSI_df_i$DateTime.PT<-ymd_hms(paste(YSI_df_i$Date, YSI_df_i$Time), tz='America/Los_Angeles')

#Used for final name
Date<-median(as.Date(YSI_df_i$Date), na.rm=T)


if ('Depth.meters' %in% column_names){
  YSI_df_i$Depth.feet <- YSI_df_i$Depth.meters*3.28084
}

if ('Temp.F' %in% column_names){
  YSI_df_i$Temp.C <- (YSI_df_i$Temp.F-32)*5/9
}

if ('ODO..' %in% column_names){
  YSI_df_i$ODOsat.. <- (YSI_df_i$'ODO..')
}




variables<-c("Station", "DateTime.PT", "Depth.feet", "Temp.C", "SpCond.uS", "pH", "Chl.ug.L", "Chl.RFU", "ODOsat..", "ODO.mg.L", "Turbid..NTU")

YSI_df_i$Station<-factor(YSI_df_i$'.id', stations)
#Exclude stations not in list above
YSI_df_i<-YSI_df_i[which(YSI_df_i$Station %in% stations),]

df_plot<-YSI_df_i[intersect(variables, names(YSI_df_i))]
MissingVars<-setdiff(variables, names(YSI_df_i))
if (length(MissingVars)>0){
  warning (paste('Missing variables:', Date, MissingVars, collapse=', '))
}

df_plot<-df_plot[order(df_plot$DateTime.PT),]
df_plot<-df_plot[order(df_plot$Depth.feet),]
df_plot<-df_plot[order(df_plot$Station),]
write.csv(df_plot, file=paste0(dropbox_dir, '/Data/NutrientExperiment/YSIVerticalProfiles/VertialProfile_', Date, '.csv'), row.names=F)

# ###########
# plotting
# ###########

#colors
color.palette = colorRampPalette(c(viridis(6, begin=.2, end=.98), rev(magma(5, begin=.35, end=.98))), bias=1)
colors<-color.palette(length(unique(df_plot$Station)))


#loop through each variable (skip station and depth)
var_nu=4
depth_col<-which(names(df_plot)=='Depth.feet')
plot_list<-list()
for (var_nu in 4:ncol(df_plot)){

  var_name<-names(df_plot)[var_nu]
  
    plot_list[[var_nu-3]] <- ggplot(df_plot, aes_string(as.character(var_name), 'Depth.feet', group='Station')) + 
    labs(x=var_name, y='Depth (ft)') +
    scale_y_reverse() + 
    scale_shape_manual(values=rep(21:25, 5))  + 
    scale_fill_manual(values = colors) + 
    scale_colour_manual(values = colors) + 
    geom_path(aes(color=Station), size=1.5) + 
    geom_point(size=3, aes(fill=Station, shape=Station)) + 
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
p2<-grid.arrange(grobs=plot_list, ncol=2, as.table=F)

# arrange multi plot with legend below and save to project folder
png(paste0(dropbox_dir, '/Figures/NutrientExperiment/VerticalProfiles/', Date, '_VerticalProfiles.png'), width=8, height=16, units='in', res=200)

grid.arrange(p2, mylegend, nrow=2,heights=c(10, 0.5))

dev.off()

}

