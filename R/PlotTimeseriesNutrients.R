

library(lubridate)
library(viridis)

# Project folder where outputs are stored
dropbox_dir<-'C:/Dropbox/USBR Delta Project'

#Where data come from
google_dir<-'C:/GoogleDrive/DeltaNutrientExperiment'


# setwd("C:/Users/Luke/Dropbox/USBR Delta Project")
# 
# source("R/ReadInMasterData.R")


merge_df <- read.csv(file=paste0(dropbox_dir, '/Data/SurfaceChemistry/YSIChemSurface.csv'), stringsAsFactors = F)
merge_df$Date<-as.Date(merge_df$Date)
merge_df$Month<-as.character(month(merge_df$Date, label = TRUE))
merge_df$Month<-factor(merge_df$Month, month.abb[1:12])

merge_df$Zone<-NA
merge_df$Zone[merge_df$Station %in% c('WSP', '84')]<-'5'
merge_df$Zone[merge_df$Station %in% c('70', '74', '76')]<-'4'
merge_df$Zone[merge_df$Station %in% c('62', '64', '66')]<-'3'
merge_df$Zone[merge_df$Station %in% c('44', '56', 'Pro')]<-'2'
merge_df$Zone[merge_df$Station %in% c('16', '34')]<-'1'


#loop through sites and plot NO3
#colors
color.palette = colorRampPalette(c(viridis(6, begin=.2, end=.98), rev(magma(5, begin=.35, end=.98))), bias=1)
colors<-color.palette(length(unique(merge_df$Station)))

variables<-c('NO3.Nppm', 'NH4.Nppm', 'PO4.Pppm', 'Chloro.appb')
variables<-c('NO3.Nppm', 'NH4.Nppm', 'PO4.Pppm')
stations<-c('66', '70', '74', '76', '84')


# stations<-c( '74', '70')


#All Stations
plot_list<-list()

var<-1
for (var in 1:length(variables)){
  
plot_list[[var]]<-ggplot(merge_df, aes_string('Date', variables[var], group='Station')) + 
  scale_shape_manual(values=rep(21:25, 5))  + 
  scale_fill_manual(values = color.palette(length(unique(merge_df$Station)))) + 
  scale_colour_manual(values = color.palette(length(unique(merge_df$Station)))) +
  geom_line(size=.5, aes(colour=Station,  group=Station)) +    
  geom_point(size=1, aes(fill=Station, shape=Station)) + 
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))  +
  theme(legend.position='none')

if (variables[var]=="Chloro.appb"){
  plot_list[[var]] <- plot_list[[var]] + 
    scale_y_log10()
}

}

# add legeng to first plot and then extract it
p1<-plot_list[[1]] + 
  theme(legend.position='bottom') +
  guides(shape = guide_legend(nrow = 2, title.position='top', title.hjust=0.5))
mylegend<-g_legend(p1)

# arrange plots without legend
p2<-grid.arrange(grobs=plot_list, ncol=1, as.table=F)


png(paste0(dropbox_dir, '/Figures/Timeseries/InorganicNutrients_AllStations_TimeSeries.png'), units='in', width=7, height=7, res=400, bg='white')

grid.arrange(p2, mylegend, nrow=2, heights=c(10, 1.2))

dev.off()


#Upper stations
plot_list<-list()

var<-1
for (var in 1:length(variables)){
  
  plot_list[[var]]<-ggplot(merge_df[merge_df$Station %in% stations,], aes_string('Date', variables[var], group='Station')) + 
    scale_shape_manual(values=rep(21:25, 5))  + 
    scale_fill_manual(values = color.palette(length(stations))) + 
    scale_colour_manual(values = color.palette(length(stations))) +
    geom_line(size=.5, aes(colour=Station,  group=Station)) +    
    geom_point(size=2, aes(fill=Station, shape=Station)) + 
    theme_bw() +
    theme(plot.title = element_text(hjust=0.5))  +
    theme(legend.position='none')
  
  if (variables[var]=="Chloro.appb"){
    plot_list[[var]] <- plot_list[[var]] + 
      scale_y_log10()
  }
  
}

# add legeng to first plot and then extract it
p1<-plot_list[[1]] + 
  theme(legend.position='bottom')
mylegend<-g_legend(p1)

# arrange plots without legend
p2<-grid.arrange(grobs=plot_list, ncol=1, as.table=F)


png(paste0(dropbox_dir, '/Figures/Timeseries/InorganicNutrients_UpperStations_TimeSeries.png'), units='in', width=7, height=7, res=400, bg='white')

grid.arrange(p2, mylegend, nrow=2, heights=c(10, 0.5))

dev.off()




colors<-color.palette(length(unique(merge_df$Zone)))


#Common theme for all boxplots
commonTheme_boxplot<-list(
  scale_fill_manual(values = colors),
  scale_colour_manual(values = colors),
  theme_bw(),
  theme(plot.title = element_text(hjust=0.5), legend.position="none"),
  geom_boxplot(outlier.size=0.5)
)

# Loop through metrics and make a gg object
box_list<-list()
plot_nu<-1
for (plot_nu in 1:length(variables)){
  # Pick data
  metric<-variables[plot_nu]
  #Plot
  box_list[[plot_nu]] <- ggplot(aes_string(y = metric, x = 'Month', fill = 'Zone'), data = merge_df) + 
    labs(x='Month', y=metric) +
    commonTheme_boxplot
  
  if (variables[plot_nu ]=="Chloro.appb"){
    box_list[[plot_nu]]<-   box_list[[plot_nu]] + 
      scale_y_log10()
  }
}



#Add and extract legend from first plot
box_withlegend <- box_list[[1]] + 
  theme(legend.position='bottom') 

mylegend_box<-g_legend(box_withlegend)


# arrange plots without legend
p2_box<-grid.arrange(grobs=box_list, ncol=2, as.table=F)


#Add legend to bottom of figure and save
png(paste0(dropbox_dir, '/Figures/Timeseries/ChemistryByZoneByMonth.png'), width=8, height=5, units='in', res=200)

grid.arrange(p2_box, mylegend_box, nrow=2,heights=c(10, 1))

dev.off()






#Make timeseries plots

vars<-c('NO3.N', 'NH4.N', 'PO4.P', 'Secchi', 'Turb_Lab', 'EC', 'Chloro.a', 'Total.biomass')
labels<-c('NO3', 'NH4', 'SRP', 'Secchi', 'Turbidity', 'Conductivity', 'Chl A', 'Zoo biomass')
months<-7:10
month_names<-month.abb[months]

MainSites$Site<-factor(MainSites$Site, levels = unique(MainSites$Site))

# png('Figures/AllVarsBoxplotsbySitebyMonthLateSummer.png', units='in', width=15, height=20, res=400, bg='white')

par(mfrow=c(length(vars), length(months)))
par(mar=c(1.5,2,0.5,.5), oma=c(2.5,2,2,0))
par(mgp=c(2,0.5,0), tck=-0.02, cex=0.6)


var_no<-1
month_no<-1
for (var_no in 1:length(vars)){
  
  df<-MainSites[c('Site', 'Month', vars[var_no])]
  df<-df[df$Month %in% months,]
  ylim=range(df[,3], na.rm=T)
  
  for (month_no in 1:length(months)){
    
    df2<-df[which(df$Month==months[month_no]),]
    
    
    if (vars[var_no]=='Turb_Lab'){
      
      boxplot(log10(df2[,c(vars[var_no])]) ~ df2$Site, col=rev(colors), boxwex=0.6, ylim=log10(ylim))
      
      if(month_no==1){
        mtext(expression(paste(log[10], ' Turbidity')), 2, 2)}
    } else {
      boxplot((df2[,c(vars[var_no])]) ~ df2$Site, col=rev(colors), boxwex=0.6, ylim=ylim)
      
      if(month_no==1){
        mtext(labels[var_no], 2, 2)}
    }
    if(var_no==1){
      mtext(month_names[month_no], 3, 0.5)}
  }
  
}
mtext('Site ID', 1, 0.75, outer=T)

dev.off()









#Make timeseries plots

png('Figures/InorganicNutrientTimeSeries.png', units='in', width=7, height=10, res=400, bg='white')

par(mfrow=c(4,1))
par(mar=c(1.5,4,0.5,.5), oma=c(1.5,0,0,0))
par(mgp=c(2,0.5,0), tck=-0.02)


plot(MainSites$NO3.N ~ MainSites$Date2, type='n', xlab='', ylab='')
points(Site84$NO3.N ~ Site44$Date2, type='o', bg=colors[1], pch=21, cex=1.2, lwd=0.5)
points(Site76$NO3.N ~ Site76$Date2, type='o', bg=colors[2], pch=21, cex=1.2, lwd=0.5)
points(Site74$NO3.N ~ Site74$Date2, type='o', bg=colors[3], pch=21, cex=1.2, lwd=0.5)
points(Site70$NO3.N ~ Site70$Date2, type='o', bg=colors[4], pch=21, cex=1.2, lwd=0.5)
points(Site66$NO3.N ~ Site66$Date2, type='o', bg=colors[5], pch=21, cex=1.2, lwd=0.5)
points(Site64$NO3.N ~ Site64$Date2, type='o', bg=colors[6], pch=21, cex=1.2, lwd=0.5)
points(Site62$NO3.N ~ Site62$Date2, type='o', bg=colors[7], pch=21, cex=1.2, lwd=0.5)
points(Site56$NO3.N ~ Site56$Date2, type='o', bg=colors[8], pch=21, cex=1.2, lwd=0.5)
points(Site44$NO3.N ~ Site44$Date2, type='o', bg=colors[9], pch=21, cex=1.2, lwd=0.5)
points(Site34$NO3.N ~ Site34$Date2, type='o', bg=colors[10], pch=21, cex=1.2, lwd=0.5)
points(Site16$NO3.N ~ Site16$Date2, type='o', bg=colors[11], pch=21, cex=1.2, lwd=0.5)


mtext(expression(paste(NO[3], ' (mg N L'^'-1', ')')), 2, 2)
legend('topleft', inset=0.01, as.character(rev(c(84,76,74,70,66,64,62,56,44,34,16))), pt.bg=rev(colors), pch=21, pt.cex=1.2, bty='n', title='Downstream                         Station ID                         Upstream', ncol=11)



plot(MainSites$NH4.N ~ MainSites$Date2, type='n', xlab='', ylab='')
points(Site84$NH4.N ~ Site44$Date2, type='o', bg=colors[1], pch=21, cex=1.2, lwd=0.5)
points(Site76$NH4.N ~ Site76$Date2, type='o', bg=colors[2], pch=21, cex=1.2, lwd=0.5)
points(Site74$NH4.N ~ Site74$Date2, type='o', bg=colors[3], pch=21, cex=1.2, lwd=0.5)
points(Site70$NH4.N ~ Site70$Date2, type='o', bg=colors[4], pch=21, cex=1.2, lwd=0.5)
points(Site66$NH4.N ~ Site66$Date2, type='o', bg=colors[5], pch=21, cex=1.2, lwd=0.5)
points(Site64$NH4.N ~ Site64$Date2, type='o', bg=colors[6], pch=21, cex=1.2, lwd=0.5)
points(Site62$NH4.N ~ Site62$Date2, type='o', bg=colors[7], pch=21, cex=1.2, lwd=0.5)
points(Site56$NH4.N ~ Site56$Date2, type='o', bg=colors[8], pch=21, cex=1.2, lwd=0.5)
points(Site44$NH4.N ~ Site44$Date2, type='o', bg=colors[9], pch=21, cex=1.2, lwd=0.5)
points(Site34$NH4.N ~ Site34$Date2, type='o', bg=colors[10], pch=21, cex=1.2, lwd=0.5)
points(Site16$NH4.N ~ Site16$Date2, type='o', bg=colors[11], pch=21, cex=1.2, lwd=0.5)

# mtext('Date', 1,2)
mtext(expression(paste(NH[4], ' (mg N L'^'-1', ')')), 2, 2)
# legend('topright', inset=0.01, as.character(rev(c(84,76,74,70,66,64,62,56,44,34,16))), pt.bg=rev(colors), pch=21, pt.cex=1.2, bty='n', title='Station ID', ncol=2)


plot(MainSites$PO4.P ~ MainSites$Date2, type='n', xlab='', ylab='')
points(Site84$PO4.P ~ Site44$Date2, type='o', bg=colors[1], pch=21, cex=1.2, lwd=0.5)
points(Site76$PO4.P ~ Site76$Date2, type='o', bg=colors[2], pch=21, cex=1.2, lwd=0.5)
points(Site74$PO4.P ~ Site74$Date2, type='o', bg=colors[3], pch=21, cex=1.2, lwd=0.5)
points(Site70$PO4.P ~ Site70$Date2, type='o', bg=colors[4], pch=21, cex=1.2, lwd=0.5)
points(Site66$PO4.P ~ Site66$Date2, type='o', bg=colors[5], pch=21, cex=1.2, lwd=0.5)
points(Site64$PO4.P ~ Site64$Date2, type='o', bg=colors[6], pch=21, cex=1.2, lwd=0.5)
points(Site62$PO4.P ~ Site62$Date2, type='o', bg=colors[7], pch=21, cex=1.2, lwd=0.5)
points(Site56$PO4.P ~ Site56$Date2, type='o', bg=colors[8], pch=21, cex=1.2, lwd=0.5)
points(Site44$PO4.P ~ Site44$Date2, type='o', bg=colors[9], pch=21, cex=1.2, lwd=0.5)
points(Site34$PO4.P ~ Site34$Date2, type='o', bg=colors[10], pch=21, cex=1.2, lwd=0.5)
points(Site16$PO4.P ~ Site16$Date2, type='o', bg=colors[11], pch=21, cex=1.2, lwd=0.5)


mtext(expression(paste('SRP (mg P L'^'-1', ')')), 2, 2)
# legend('topleft', inset=0.01, as.character(rev(c(84,76,74,70,66,64,62,56,44,34,16))), pt.bg=rev(colors), pch=21, pt.cex=1.2, bty='n', title='Station ID', ncol=3)


plot(MainSites$DIN.PO4 ~ MainSites$Date2, type='n', xlab='', ylab='')
points(Site84$DIN.PO4 ~ Site44$Date2, type='o', bg=colors[1], pch=21, cex=1.2, lwd=0.5)
points(Site76$DIN.PO4 ~ Site76$Date2, type='o', bg=colors[2], pch=21, cex=1.2, lwd=0.5)
points(Site74$DIN.PO4 ~ Site74$Date2, type='o', bg=colors[3], pch=21, cex=1.2, lwd=0.5)
points(Site70$DIN.PO4 ~ Site70$Date2, type='o', bg=colors[4], pch=21, cex=1.2, lwd=0.5)
points(Site66$DIN.PO4 ~ Site66$Date2, type='o', bg=colors[5], pch=21, cex=1.2, lwd=0.5)
points(Site64$DIN.PO4 ~ Site64$Date2, type='o', bg=colors[6], pch=21, cex=1.2, lwd=0.5)
points(Site62$DIN.PO4 ~ Site62$Date2, type='o', bg=colors[7], pch=21, cex=1.2, lwd=0.5)
points(Site56$DIN.PO4 ~ Site56$Date2, type='o', bg=colors[8], pch=21, cex=1.2, lwd=0.5)
points(Site44$DIN.PO4 ~ Site44$Date2, type='o', bg=colors[9], pch=21, cex=1.2, lwd=0.5)
points(Site34$DIN.PO4 ~ Site34$Date2, type='o', bg=colors[10], pch=21, cex=1.2, lwd=0.5)
points(Site16$DIN.PO4 ~ Site16$Date2, type='o', bg=colors[11], pch=21, cex=1.2, lwd=0.5)


mtext('DIN:SRP (molar ratio)', 2, 2)
# legend('topleft', inset=0.01, as.character(rev(c(84,76,74,70,66,64,62,56,44,34,16))), pt.bg=rev(colors), pch=21, pt.cex=1.2, bty='n', title='Station ID', ncol=3)


mtext('Date', 1,0, outer=T)


dev.off()



plot(MainSites$Si ~ MainSites$Date2, type='n', xlab='', ylab='')
points(Site84$Si ~ Site44$Date2, type='o', bg=colors[1], pch=21, cex=1.2, lwd=0.5)
points(Site76$Si ~ Site76$Date2, type='o', bg=colors[2], pch=21, cex=1.2, lwd=0.5)
points(Site74$Si ~ Site74$Date2, type='o', bg=colors[3], pch=21, cex=1.2, lwd=0.5)
points(Site70$Si ~ Site70$Date2, type='o', bg=colors[4], pch=21, cex=1.2, lwd=0.5)
points(Site66$Si ~ Site66$Date2, type='o', bg=colors[5], pch=21, cex=1.2, lwd=0.5)
points(Site64$Si ~ Site64$Date2, type='o', bg=colors[6], pch=21, cex=1.2, lwd=0.5)
points(Site62$Si ~ Site62$Date2, type='o', bg=colors[7], pch=21, cex=1.2, lwd=0.5)
points(Site56$Si ~ Site56$Date2, type='o', bg=colors[8], pch=21, cex=1.2, lwd=0.5)
points(Site44$Si ~ Site44$Date2, type='o', bg=colors[9], pch=21, cex=1.2, lwd=0.5)
points(Site34$Si ~ Site34$Date2, type='o', bg=colors[10], pch=21, cex=1.2, lwd=0.5)
points(Site16$Si ~ Site16$Date2, type='o', bg=colors[11], pch=21, cex=1.2, lwd=0.5)


mtext(expression(paste('Si (mg Si L'^'-1', ')')), 2, 2)


png('Figures/TNTPTimeSeries.png', units='in', width=7, height=7.5, res=400, bg='white')

par(mfrow=c(3,1))
par(mar=c(1.5,4,0.5,.5), oma=c(1.5,0,0,0))
par(mgp=c(2,0.5,0), tck=-0.02)

plot(MainSites$TN ~ MainSites$Date2, type='n', xlab='', ylab='', ylim=c(0,2))
points(Site84$TN ~ Site44$Date2, type='o', bg=colors[1], pch=21, cex=1.2, lwd=0.5)
points(Site76$TN ~ Site76$Date2, type='o', bg=colors[2], pch=21, cex=1.2, lwd=0.5)
points(Site74$TN ~ Site74$Date2, type='o', bg=colors[3], pch=21, cex=1.2, lwd=0.5)
points(Site70$TN ~ Site70$Date2, type='o', bg=colors[4], pch=21, cex=1.2, lwd=0.5)
points(Site66$TN ~ Site66$Date2, type='o', bg=colors[5], pch=21, cex=1.2, lwd=0.5)
points(Site64$TN ~ Site64$Date2, type='o', bg=colors[6], pch=21, cex=1.2, lwd=0.5)
points(Site62$TN ~ Site62$Date2, type='o', bg=colors[7], pch=21, cex=1.2, lwd=0.5)
points(Site56$TN ~ Site56$Date2, type='o', bg=colors[8], pch=21, cex=1.2, lwd=0.5)
points(Site44$TN ~ Site44$Date2, type='o', bg=colors[9], pch=21, cex=1.2, lwd=0.5)
points(Site34$TN ~ Site34$Date2, type='o', bg=colors[10], pch=21, cex=1.2, lwd=0.5)
points(Site16$TN ~ Site16$Date2, type='o', bg=colors[11], pch=21, cex=1.2, lwd=0.5)


mtext(expression(paste('Total N (mg N L'^'-1', ')')), 2, 2)
legend('topleft', inset=0.01, as.character(rev(c(84,76,74,70,66,64,62,56,44,34,16))), pt.bg=rev(colors), pch=21, pt.cex=1.2, bty='n', title='Station ID', ncol=3)

plot(MainSites$TP ~ MainSites$Date2, type='n', xlab='', ylab='', ylim=c(0,.5))
points(Site84$TP ~ Site44$Date2, type='o', bg=colors[1], pch=21, cex=1.2, lwd=0.5)
points(Site76$TP ~ Site76$Date2, type='o', bg=colors[2], pch=21, cex=1.2, lwd=0.5)
points(Site74$TP ~ Site74$Date2, type='o', bg=colors[3], pch=21, cex=1.2, lwd=0.5)
points(Site70$TP ~ Site70$Date2, type='o', bg=colors[4], pch=21, cex=1.2, lwd=0.5)
points(Site66$TP ~ Site66$Date2, type='o', bg=colors[5], pch=21, cex=1.2, lwd=0.5)
points(Site64$TP ~ Site64$Date2, type='o', bg=colors[6], pch=21, cex=1.2, lwd=0.5)
points(Site62$TP ~ Site62$Date2, type='o', bg=colors[7], pch=21, cex=1.2, lwd=0.5)
points(Site56$TP ~ Site56$Date2, type='o', bg=colors[8], pch=21, cex=1.2, lwd=0.5)
points(Site44$TP ~ Site44$Date2, type='o', bg=colors[9], pch=21, cex=1.2, lwd=0.5)
points(Site34$TP ~ Site34$Date2, type='o', bg=colors[10], pch=21, cex=1.2, lwd=0.5)
points(Site16$TP ~ Site16$Date2, type='o', bg=colors[11], pch=21, cex=1.2, lwd=0.5)


mtext(expression(paste('Total P (mg P L'^'-1', ')')), 2, 2)

plot(MainSites$TN/MainSites$TP*30.97/14 ~ MainSites$Date2, type='n', xlab='', ylab='', ylim=c(0,45))
points(Site84$TN/Site84$TP*30.97/14 ~ Site44$Date2, type='o', bg=colors[1], pch=21, cex=1.2, lwd=0.5)
points(Site76$TN/Site76$TP*30.97/14 ~ Site76$Date2, type='o', bg=colors[2], pch=21, cex=1.2, lwd=0.5)
points(Site74$TN/Site74$TP*30.97/14 ~ Site74$Date2, type='o', bg=colors[3], pch=21, cex=1.2, lwd=0.5)
points(Site70$TN/Site70$TP*30.97/14 ~ Site70$Date2, type='o', bg=colors[4], pch=21, cex=1.2, lwd=0.5)
points(Site66$TN/Site66$TP*30.97/14 ~ Site66$Date2, type='o', bg=colors[5], pch=21, cex=1.2, lwd=0.5)
points(Site64$TN/Site64$TP*30.97/14 ~ Site64$Date2, type='o', bg=colors[6], pch=21, cex=1.2, lwd=0.5)
points(Site62$TN/Site62$TP*30.97/14 ~ Site62$Date2, type='o', bg=colors[7], pch=21, cex=1.2, lwd=0.5)
points(Site56$TN/Site56$TP*30.97/14 ~ Site56$Date2, type='o', bg=colors[8], pch=21, cex=1.2, lwd=0.5)
points(Site44$TN/Site44$TP*30.97/14 ~ Site44$Date2, type='o', bg=colors[9], pch=21, cex=1.2, lwd=0.5)
points(Site34$TN/Site34$TP*30.97/14 ~ Site34$Date2, type='o', bg=colors[10], pch=21, cex=1.2, lwd=0.5)
points(Site16$TN/Site16$TP*30.97/14 ~ Site16$Date2, type='o', bg=colors[11], pch=21, cex=1.2, lwd=0.5)

mtext('N:P (molar ratio)', 2, 2)

mtext('Date', 1,0, outer=T)

dev.off()


png('Figures/ChlAZoopTimeSeries.png', units='in', width=7, height=5, res=400, bg='white')

par(mfrow=c(2,1))
par(mar=c(1.5,4,0.5,.5), oma=c(1.5,0,0,0))
par(mgp=c(2,0.5,0), tck=-0.02)

plot(MainSites$Chloro.a ~ MainSites$Date2, type='n', xlab='', ylab='', ylim=c(0,40))
points(Site84$Chloro.a ~ Site44$Date2, type='o', bg=colors[1], pch=21, cex=1.2, lwd=0.5)
points(Site76$Chloro.a ~ Site76$Date2, type='o', bg=colors[2], pch=21, cex=1.2, lwd=0.5)
points(Site74$Chloro.a ~ Site74$Date2, type='o', bg=colors[3], pch=21, cex=1.2, lwd=0.5)
points(Site70$Chloro.a ~ Site70$Date2, type='o', bg=colors[4], pch=21, cex=1.2, lwd=0.5)
points(Site66$Chloro.a ~ Site66$Date2, type='o', bg=colors[5], pch=21, cex=1.2, lwd=0.5)
points(Site64$Chloro.a ~ Site64$Date2, type='o', bg=colors[6], pch=21, cex=1.2, lwd=0.5)
points(Site62$Chloro.a ~ Site62$Date2, type='o', bg=colors[7], pch=21, cex=1.2, lwd=0.5)
points(Site56$Chloro.a ~ Site56$Date2, type='o', bg=colors[8], pch=21, cex=1.2, lwd=0.5)
points(Site44$Chloro.a ~ Site44$Date2, type='o', bg=colors[9], pch=21, cex=1.2, lwd=0.5)
points(Site34$Chloro.a ~ Site34$Date2, type='o', bg=colors[10], pch=21, cex=1.2, lwd=0.5)
points(Site16$Chloro.a ~ Site16$Date2, type='o', bg=colors[11], pch=21, cex=1.2, lwd=0.5)


mtext(expression(paste('Chl ', italic('a'), ' (', mu, 'g L'^'-1', ')')), 2, 2)
legend('topright', inset=0.01, as.character(rev(c(84,76,74,70,66,64,62,56,44,34,16))), pt.bg=rev(colors), pch=21, pt.cex=1.2, bty='n', title='Station ID', ncol=3)

plot(MainSites$Total.biomass ~ MainSites$Date2, type='n', xlab='', ylab='', ylim=c(0,350))
points(Site84$Total.biomass ~ Site44$Date2, type='o', bg=colors[1], pch=21, cex=1.2, lwd=0.5)
points(Site76$Total.biomass ~ Site76$Date2, type='o', bg=colors[2], pch=21, cex=1.2, lwd=0.5)
points(Site74$Total.biomass ~ Site74$Date2, type='o', bg=colors[3], pch=21, cex=1.2, lwd=0.5)
points(Site70$Total.biomass ~ Site70$Date2, type='o', bg=colors[4], pch=21, cex=1.2, lwd=0.5)
points(Site66$Total.biomass ~ Site66$Date2, type='o', bg=colors[5], pch=21, cex=1.2, lwd=0.5)
points(Site64$Total.biomass ~ Site64$Date2, type='o', bg=colors[6], pch=21, cex=1.2, lwd=0.5)
points(Site62$Total.biomass ~ Site62$Date2, type='o', bg=colors[7], pch=21, cex=1.2, lwd=0.5)
points(Site56$Total.biomass ~ Site56$Date2, type='o', bg=colors[8], pch=21, cex=1.2, lwd=0.5)
points(Site44$Total.biomass ~ Site44$Date2, type='o', bg=colors[9], pch=21, cex=1.2, lwd=0.5)
points(Site34$Total.biomass ~ Site34$Date2, type='o', bg=colors[10], pch=21, cex=1.2, lwd=0.5)
points(Site16$Total.biomass ~ Site16$Date2, type='o', bg=colors[11], pch=21, cex=1.2, lwd=0.5)


mtext(expression(paste('Zoo biomass (', mu, 'g L'^'-1', ')')), 2, 2)


mtext('Date', 1,0, outer=T)

dev.off()




png('Figures/TurbSecchiTimeSeries.png', units='in', width=7, height=7.5, res=400, bg='white')

par(mfrow=c(3,1))
par(mar=c(1.5,4,0.5,.5), oma=c(1.5,0,0,0))
par(mgp=c(2,0.5,0), tck=-0.02)

plot(MainSites$Turb_Lab ~ MainSites$Date2, type='n', xlab='', ylab='')
points(Site84$Turb_Lab ~ Site44$Date2, type='o', bg=colors[1], pch=21, cex=1.2, lwd=0.5)
points(Site76$Turb_Lab ~ Site76$Date2, type='o', bg=colors[2], pch=21, cex=1.2, lwd=0.5)
points(Site74$Turb_Lab ~ Site74$Date2, type='o', bg=colors[3], pch=21, cex=1.2, lwd=0.5)
points(Site70$Turb_Lab ~ Site70$Date2, type='o', bg=colors[4], pch=21, cex=1.2, lwd=0.5)
points(Site66$Turb_Lab ~ Site66$Date2, type='o', bg=colors[5], pch=21, cex=1.2, lwd=0.5)
points(Site64$Turb_Lab ~ Site64$Date2, type='o', bg=colors[6], pch=21, cex=1.2, lwd=0.5)
points(Site62$Turb_Lab ~ Site62$Date2, type='o', bg=colors[7], pch=21, cex=1.2, lwd=0.5)
points(Site56$Turb_Lab ~ Site56$Date2, type='o', bg=colors[8], pch=21, cex=1.2, lwd=0.5)
points(Site44$Turb_Lab ~ Site44$Date2, type='o', bg=colors[9], pch=21, cex=1.2, lwd=0.5)
points(Site34$Turb_Lab ~ Site34$Date2, type='o', bg=colors[10], pch=21, cex=1.2, lwd=0.5)
points(Site16$Turb_Lab ~ Site16$Date2, type='o', bg=colors[11], pch=21, cex=1.2, lwd=0.5)


mtext(expression(paste('Turbidity (lab)')), 2, 2)
legend('topleft', inset=0.01, as.character(rev(c(84,76,74,70,66,64,62,56,44,34,16))), pt.bg=rev(colors), pch=21, pt.cex=1.2, bty='n', title='Station ID', ncol=3)


plot(MainSites$Secchi ~ MainSites$Date2, type='n', xlab='', ylab='', ylim=c(0,250))
points(Site84$Secchi ~ Site44$Date2, type='o', bg=colors[1], pch=21, cex=1.2, lwd=0.5)
points(Site76$Secchi ~ Site76$Date2, type='o', bg=colors[2], pch=21, cex=1.2, lwd=0.5)
points(Site74$Secchi ~ Site74$Date2, type='o', bg=colors[3], pch=21, cex=1.2, lwd=0.5)
points(Site70$Secchi ~ Site70$Date2, type='o', bg=colors[4], pch=21, cex=1.2, lwd=0.5)
points(Site66$Secchi ~ Site66$Date2, type='o', bg=colors[5], pch=21, cex=1.2, lwd=0.5)
points(Site64$Secchi ~ Site64$Date2, type='o', bg=colors[6], pch=21, cex=1.2, lwd=0.5)
points(Site62$Secchi ~ Site62$Date2, type='o', bg=colors[7], pch=21, cex=1.2, lwd=0.5)
points(Site56$Secchi ~ Site56$Date2, type='o', bg=colors[8], pch=21, cex=1.2, lwd=0.5)
points(Site44$Secchi ~ Site44$Date2, type='o', bg=colors[9], pch=21, cex=1.2, lwd=0.5)
points(Site34$Secchi ~ Site34$Date2, type='o', bg=colors[10], pch=21, cex=1.2, lwd=0.5)
points(Site16$Secchi ~ Site16$Date2, type='o', bg=colors[11], pch=21, cex=1.2, lwd=0.5)


mtext(expression(paste('Secchi depth (cm)')), 2, 2)


plot(MainSites$EC ~ MainSites$Date2, type='n', xlab='', ylab='', ylim=c(0,1500))
points(Site84$EC ~ Site44$Date2, type='o', bg=colors[1], pch=21, cex=1.2, lwd=0.5)
points(Site76$EC ~ Site76$Date2, type='o', bg=colors[2], pch=21, cex=1.2, lwd=0.5)
points(Site74$EC ~ Site74$Date2, type='o', bg=colors[3], pch=21, cex=1.2, lwd=0.5)
points(Site70$EC ~ Site70$Date2, type='o', bg=colors[4], pch=21, cex=1.2, lwd=0.5)
points(Site66$EC ~ Site66$Date2, type='o', bg=colors[5], pch=21, cex=1.2, lwd=0.5)
points(Site64$EC ~ Site64$Date2, type='o', bg=colors[6], pch=21, cex=1.2, lwd=0.5)
points(Site62$EC ~ Site62$Date2, type='o', bg=colors[7], pch=21, cex=1.2, lwd=0.5)
points(Site56$EC ~ Site56$Date2, type='o', bg=colors[8], pch=21, cex=1.2, lwd=0.5)
points(Site44$EC ~ Site44$Date2, type='o', bg=colors[9], pch=21, cex=1.2, lwd=0.5)
points(Site34$EC ~ Site34$Date2, type='o', bg=colors[10], pch=21, cex=1.2, lwd=0.5)
points(Site16$EC ~ Site16$Date2, type='o', bg=colors[11], pch=21, cex=1.2, lwd=0.5)


mtext(expression(paste('Electrical conductivity (', mu, 'S cm'^'-1', ')')), 2, 2)
mtext('Date', 1,0, outer=T)

dev.off()

# End timeseries



plot(MainSites$Chloro.a~ MainSites$Turb_Field)
plot(MainSites$Chloro.a~ MainSites$Turb_Lab)

plot(MainSites$Turb_Lab~ MainSites$Turb_Field)
plot((MainSites$Turb_Lab)~ MainSites$Secchi)


plot(MainSites$Chloro.a~ MainSites$NO3.N)
plot(MainSites$Chloro.a~ as.character(as.numeric(MainSites$Secchi)))


model1<-lm(MainSites$Chloro.a ~ MainSites$Lab * MainSites$NO3.N)
summary(model1)
anova(model1)
head(MainSites)
