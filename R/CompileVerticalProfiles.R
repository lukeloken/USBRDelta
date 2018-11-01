

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
YSIfilenames<-list.files(paste0(google_dir, "/Data/DepthProfiles"))

#Exclude non-excel files
YSIfilenames<-YSIfilenames[grep('.xls', YSIfilenames)]
YSIfilenames<-YSIfilenames[-grep('may 27 2015 report.xlsx', YSIfilenames)]
YSIfilenames<-YSIfilenames[-grep('Channel Data Depth Profiles Oct 21 2014 report', YSIfilenames)]
YSIfilenames<-YSIfilenames[-grep('Channel Data Depth Profiles Oct 23 2014 report', YSIfilenames)]
YSIfilenames<-YSIfilenames[-grep('Sac Ship Channel Surface Summary', YSIfilenames)]

#or enter a single file
YSIfilenames<-c('Channel Data depth Profiles October 18 2018.xlsx')

#If there is an open excel sheet exclude it from the loop of filenames
if (length(grep('~', YSIfilenames))>0){
  YSIfilenames<-YSIfilenames[-grep('~', YSIfilenames)]
}

# Loop through filenames and read in data
i=9
for (i in 1:length(YSIfilenames)){
YSI_list_i<-read_excel_allsheets(paste0(google_dir, "/Data/DepthProfiles/", YSIfilenames[i]))

#Order each sheet by depth
depth_name<-names(YSI_list_i[[1]])[grep('Depth', names(YSI_list_i[[1]]))]

stations<-c('16', '34','44', 'Pro', '56','62', '64','66', '70','74', '76','84', 'WSP')
greps<-c('16', '34','44', 'Pro', '56','62', '64','66', '70','74', '76','84', 'W')

if (length(YSI_list_i)==1){
  #This is only the case for one date (First date June 2012)
  YSI_list_i_noNA<-lapply(YSI_list_i, function(l) l[!is.na(l[,depth_name]),])
  YSI_df_i<-ldply(YSI_list_i_noNA, data.frame)
  YSI_df_i<-YSI_df_i[,-1]
  YSI_df_i$'.id'<-YSI_df_i$Station
  YSI_df_i<-YSI_df_i[which(YSI_df_i$Station %in% stations),]

} else if  (length(YSI_list_i)>1){

  # Omit extra sheets
  # These are the names of excel sheets that identify the profiles of interest
  # Ocassionally data contain additional sheets for other processing/plotting
  sheets<-as.numeric(sapply(greps, grep, x=names(YSI_list_i)))
  YSI_list_i2<-YSI_list_i[sheets[is.finite(sheets)]]
  
  names(YSI_list_i2)[[grep('W', names(YSI_list_i2))]]<-'WSP'
  
  
  # Need to exclude multiple profiles added to the same column
  # See April 28, 2016 as an example
  # Time window between rows should be less than 5 minutes
  # df<-YSI_list_i2[[4]]
  threshold=300 #threshold between observations to note the next profile
  FirstProfile<-function(df){
    diffs<-seconds(diff(df$Time))
    row<-which(diffs>300)[1]
    if(is.na(row)){
      first_df<- df
    } else {
      first_df<-df[1:row,]
    }
    return(first_df)
  }
  
  YSI_list_i3<-lapply(YSI_list_i2, function(l) FirstProfile(l))
  
  
  YSI_list_i_ordered<-lapply(YSI_list_i3, function(l) l[order(l[,depth_name]),])
  YSI_list_i_noNA<-lapply(YSI_list_i_ordered, function(l) l[!is.na(l[,depth_name]),])
  
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
write.csv(df_plot, file=paste0(dropbox_dir, '/Data/YSIVerticalProfiles/VertialProfile_', Date, '.csv'), row.names=F)

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
png(paste0(dropbox_dir, '/Figures/VerticalProfiles/', Date, '_VerticalProfiles.png'), width=8, height=16, units='in', res=200)

grid.arrange(p2, mylegend, nrow=2,heights=c(10, 0.5))

dev.off()

}
