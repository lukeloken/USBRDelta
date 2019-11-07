

library(readr) # read in data
library(dplyr,quietly = T) # clean data
library(tidyr) # clean data
library(rLakeAnalyzer) # lake analyses
library(lubridate) # working with time
library(LakeMetabolizer) # lake analyses
library(RcppRoll) # Rolling operations

source('R/g_legend.R')
source('R/lightmodel.R')

# Project folder where outputs are stored
dropbox_dir<-'C:/Dropbox/USBR Delta Project'

#Where data come from
google_dir<-'C:/GoogleDrive/DeltaNutrientExperiment'


#O18 metabolism data
met.final<-read.csv(file = paste(dropbox_dir, "Data", "NutrientExperiment", "MergedData1.csv", sep='/'), stringsAsFactors = F)

met.final$Date<-as.Date(met.final$Date)
met.final$Site<-factor(met.final$Site, c('NL70', 'EC2','EC3','EC4','EC5','EC6','EC7','EC8','NL76'))


#Wind data
wind_avg<-read.csv(file=paste0(dropbox_dir, '/Data/NutrientExperiment/Oxygen18/WindDataAvg.csv'))
wind_avg$Date<-as.Date(wind_avg$Date)

#Event dates
shipdate<-as.Date(c("2018-10-07", "2018-10-06"), format='%Y-%m-%d')
# shipdate<-as.Date(c("2018-10-07"), format='%Y-%m-%d')
fertdate<-as.Date("2018-10-01", format='%Y-%m-%d')


#Buoy Data
df_deploy <- read.csv(file=paste0(dropbox_dir, '/Data/NutrientExperiment/Buoys/MiniDot_Oct2018.csv'))
df_deploy$DateTime_UTC<-as.POSIXct(df_deploy$DateTime_UTC, tz="UTC")
df_deploy$DateTime_PDT <- df_deploy$DateTime_UTC
attributes(df_deploy$DateTime_PDT)$tzone <- 'America/Los_Angeles'

str(df_deploy)

df_deploy$DateTime_PDT_round<-round_date(df_deploy$DateTime_PDT, unit='5 minutes')

buoy_numbers<-unique(df_deploy$BuoyNu)[order(unique(df_deploy$BuoyNu), decreasing=T)]
buoy_numbers[which(buoy_numbers=='15')]<-'NL76'
buoy_numbers[which(buoy_numbers=='1')]<-'NL70'

df_deploy$Buoy<-factor(df_deploy$Buoy,(buoy_numbers))


#Z_mix data
physics_df <-read.csv(file=paste0(dropbox_dir, '/Data/NutrientExperiment/Buoys/Zmix_Oct2018.csv'))
physics_df$DateTime_PDT<-as.POSIXct(physics_df$DateTime, tz="America/Los_Angeles")
physics_df$DateTime_PDT_round<- lubridate::round_date(physics_df$DateTime_PDT, unit="5 minutes")

ggplot(aes(x=DateTime_PDT, y=Zmix_m),data=physics_df) + 
  geom_line() + 
  scale_y_reverse() + 
  theme_bw() + 
  scale_x_datetime(date_breaks='weeks', date_minor_breaks = 'days')

ggplot(aes(x=DateTime_PDT, y=Z_new),data=physics_df) + 
  geom_line() + 
  scale_y_reverse() + 
  theme_bw() + 
  scale_x_datetime(date_breaks='weeks', date_minor_breaks = 'days')

# ###################################
# create input vectors for metabolism
# ###################################


# #all input terms for k estimates
# wind.ms <- met$wind.ms
# wind.height <- met$wind.height.ms
# area <- met$lake.area.km2
# temp <- met$EXOTemp
# 
# #calculating the gas exchange coefficient for O2 empirically from lake area and wind speed:
# u10 <- wind.ms * (1+ (((0.0013^(0.5))/0.41) * (log(10/wind.height)))) #converting wind speed from 3m to 10m height following equation 3 Vachon & Prairie (2013)
# k600cmh <- 2.51 + 1.48*u10 + 0.39*u10*(log10(area)) #k600 in cm/h from table 2 equation B vachon & prairie 2013
# k600md <- k600cmh * 24/100 #converting k600 to m/d
# sco2 <- 1800.6 - (120.1*temp) + (3.7818 * (temp^2)) - (0.047608*(temp^3))#calculating schmidt number for oxygen from Jahne et al (1987)
# ko2md <- k600md * ((sco2/600)^(-2/3)) #converting k600 to ko2 in m/d for use in mass balance




#constants
z.mean<-8
wind.height <- 10
area <- met.final$lake.area.km2[1]

#Average of data used for Oxygen 18 metabolism
ko2md_mean<-2.307741

# ###################
# Metabolism function
# ###################


# ###########################################
# Loop through each buoy and model metabolism
# ###########################################
metab.list<-list()
buoy_nu<-4
for (buoy_nu in 1:length(buoy_numbers)){
  
  buoy_name<-buoy_numbers[buoy_nu]
  
  df_i<-df_deploy %>%
    dplyr::filter(Buoy==buoy_name)
  
  datetime<-df_i$DateTime_PDT_round
  date<-df_i$Date
  do.obs = df_i$Dissolved.Oxygen
  
  # Get equilibrium saturation concentration of oxygen in water
  do.sat = o2.at.sat.base(df_i$Temperature, altitude=0)
  
  # Par and day/night
  # par = metData$par
  
  # For bookeeping irradiance
  dayIrr = physics_df$dayIrr[match(datetime, physics_df$DateTime_PDT_round)] #Sparkling Lake is at 48 degrees N latitude
  
  
  # Surface mixed depth
  # z.mix = rep(z.mean, length(datetime))
  
  #New estimate of z-mix
  #For each day use the max between the observed z-mix at the time or the max for the rest of the day
  # z.mix <- physics_df$Z_new[match(datetime, physics_df$DateTime_PST_round)]
  # z.mix_dark <- physics_df$Zmix_m[match(datetime, physics_df$DateTime_PST_round)]
  # z.mix[which(dayIrr==0)] <- z.mix_dark[which(dayIrr==0)]
  
  z.mix <- physics_df$Zmix_m[match(datetime, physics_df$DateTime_PDT_round)]
  z.mix[z.mix==6] <- 8
  
  z.new <- physics_df$Z_new[match(datetime, physics_df$DateTime_PDT_round)]
  z.new[z.new==6] <- 8
  
  # plot(datetime, z.mix_dark, ylim=c(6,0))
  # points(datetime, z.mix, type='l', col='red')
  # 
  
  # Water temperature at dissolved oxygen sensor depth (0.5 m)
  wtr = df_i$Temperature
  
  #average k
  # k_i<-rep(ko2md_mean, length(datetime))
  
  #use daily average k
  #all input terms for k estimates
  wind.ms <- wind_avg$AWND[match(as.Date(datetime, tz=attributes(datetime)$tzone), wind_avg$Date)]
  
  #calculating the gas exchange coefficient for O2 empirically from lake area and wind speed:
  u10 <- wind.ms * (1+ (((0.0013^(0.5))/0.41) * (log(10/wind.height)))) #converting wind speed from 3m to 10m height following equation 3 Vachon & Prairie (2013)
  k600cmh <- 2.51 + 1.48*u10 + 0.39*u10*(log10(area)) #k600 in cm/h from table 2 equation B vachon & prairie 2013
  k600md <- k600cmh * 24/100 #converting k600 to m/d
  sco2 <- 1800.6 - (120.1*wtr) + (3.7818 * (wtr^2)) - (0.047608*(wtr^3))#calculating schmidt number for oxygen from Jahne et al (1987)
  ko2md <- k600md * ((sco2/600)^(-2/3)) #converting k600 to ko2 in m/d for use in mass balance
  
  k_buoy<-ko2md
  
  
  # ####################
  # loop through each day and
  # Estimate Metabolism
  # ####################
  
  
  # Calculation for multiple days
  # set dates
  metab.dates<-unique(as.Date(datetime, tz=attributes(datetime)$tzone))
  
  model_abbr<-c('bookkeep')
  output = data.frame(matrix(ncol=4, nrow=length(metab.dates), NA))
  names(output)=c('date', 'NEP', 'GPP', 'R')
  output$date<-metab.dates
  #create a vector for dates starting at 5AM (use this for start/stop of each day)
  date_minus6hr<-as.Date(datetime - hours(6), tz="America/Los_Angeles")
  
  # Loop through each day.
  i=10
  for (i in 1:nrow(output)) {
    
    indx <- which(date_minus6hr == output$date[i])
    
    GPPindx <- which(dayIrr[indx] == 1 & date_minus6hr[indx] == output$date[i])
    
    ERindx <-  which(dayIrr[indx] == 0 & date_minus6hr[indx] == output$date[i] & z.mix[indx] == 8)
    
    if(length(GPPindx) > 0){
      ERindx<-ERindx[ERindx>max(GPPindx)]
    }
    
    
    if(length(do.obs[indx][!is.na(do.obs[indx])])>0){
    #Look at DO and mixed surface layer depth
    png(paste0(dropbox_dir, '/Figures/NutrientExperiment/Buoys/DailyDOCurves/Buoy_', buoy_name, '_', as.character(output$date[i]), '.png'), width=6, height=4, units='in', res=200)
    par(mar=c(3,3,1,3))
    plot(datetime[indx], do.obs[indx], type='o', ylab='')
    par(new=T)
  
    plot(datetime[indx], z.mix[indx], col='red', ylim=c(8,0), axes=F, ylab='', type='l', yaxs='i', lwd=2)
    points(datetime[indx], z.new[indx], col='blue', type='h', lty=2)
    
    axis(4, col.axis='red')
    mtext('z_depth (m)',4,2, col='red')
    mtext('DO (mg/L)',2,2)
    legend('topright', c(as.character(output$date[i]), paste0('Buoy #', buoy_name)), lty=0, bty='n')
    
    dev.off()
    
    }
    
    

    
    if(length(which(is.na(do.obs[indx])==T))==0){
      timelength<-difftime(datetime[indx][length(datetime[indx])], datetime[indx][1], units="hours")
      if (timelength > 22){
        
        if (any(z.mix <= 0)) {
          stop("z.mix must be greater than zero.")
        }
        
        freq<-1/as.numeric(median(diff(datetime)), units='days')
        
        dayI <- GPPindx 
        nightI <- ERindx 

        delta.do <- diff(roll_mean(do.obs[indx], 1))
        miss.delta <- sum(is.na(delta.do))
        if (miss.delta != 0) {
          warning(paste(miss.delta, " missing values (", miss.delta/length(delta.do), 
                        "%) in diff(do.obs)", sep = ""))
        }
        
        #flux is from atm into water
        #flux is the volumetric concentration increase per unit time
        gas.flux <- (do.sat[indx] - do.obs[indx]) * (k.gas[indx]/freq)/z.mix[indx]
        delta.do.metab <- delta.do - gas.flux[1:(length(gas.flux) - 1)]
        
        gas.flux.area <- gas.flux*z.mix[indx]
        
        #volume change in oxygen from 6am to 5:59am next day
        #minus the total amount of gas flux
        #mg O2 L-1 d-1
        nep.daily.volume <- mean(tail(do.obs[indx],5), na.rm=T) - mean(head(do.obs[indx],5),na.rm=T) - sum(gas.flux.area)/z.mean 
        # nep.daily.volume <- mean(tail(do.obs[indx],5), na.rm=T) - mean(head(do.obs[indx],5),na.rm=T) - mean(gas.flux)*(freq-5)
        
        #g O2 m-2 d-1
        nep.daily.area <- nep.daily.volume * z.mean
        
        nep.daytime.area <- sum(delta.do.metab[GPPindx] * z.mix[indx][GPPindx])
        
        gpp.daily.area <- nep.daytime.area - nep.daily.area 
        er.daily.area <- nep.daily.area - gpp.daily.area
        
        
        #mg o2 L-1 d-1
        er.night.volume <- mean(delta.do.metab[ERindx], na.rm=T) * freq
        er.daily.area2 <- er.night.volume * z.mean
        gpp.daily.area2 <- nep.daytime.area - er.daily.area2
        
        #Estimate metabolism g O2 per m2 per day
        # nep.day <- delta.do.metab[GPPindx] * z.mix[indx][GPPindx]
        # nep.night <- delta.do.metab[ERindx]* z.mix[indx][ERindx]
        # 
        # GPP <- sum(nep.day, na.rm = TRUE) - R
        # R <- mean(nep.night, na.rm = TRUE) * freq
        # NEP <- GPP + R
        
        # metab <- data.frame(GPP = GPP, R = R, NEP = NEP)
        # return(metab)
        
        
        output$NEP[i] = nep.daily.area
        #GPP
        output$GPP[i] = gpp.daily.area
        #R
        output$R[i] = er.daily.area
        
      }
    }
  }
  
  output$Buoy<-rep(buoy_name, nrow(output))
  
  metab.list[[buoy_nu]]<-output

}
  
  

# 
# metab.loken<-function (do.obs, do.sat, k.gas, z.mix, irr, ...) 
# {
# 
#   nobs <- length(do.obs)
#   mb.args <- list(...)
#   if (any(z.mix <= 0)) {
#     stop("z.mix must be greater than zero.")
#   }
#   
#   freq<-as.numeric(median(diff(datetime)), units='secs')
#   
#   # if ("datetime" %in% names(mb.args)) {
#   #   datetime <- mb.args$datetime
#   #   # freq <- calc.freq(datetime)
#   #   # if (nobs != freq) {
#   #   #   bad.date <- format.Date(datetime[1], format = "%Y-%m-%d")
#   #   #   warning("number of observations on ", bad.date, " (", 
#   #   #           nobs, ") ", "does not equal estimated sampling frequency", 
#   #   #           " (", freq, ")", sep = "")
#   #   # }
#   # }
#   # else {
#   #   warning("datetime not found, inferring sampling frequency from # of observations")
#   #   freq <- nobs
#   # }
#   # if (all(c("datetime", "lake.lat") %in% names(mb.args))) {
#   #   irr <- as.integer(is.day(datetimes = datetime, lat = mb.args$lake.lat))
#   #   dayI <- irr == 1L
#   #   nightI <- irr == 0L
#   # }
#   # else {
#   #   if (!all(irr == 1L | irr == 0L)) {
#   #     stop("either supply datetime & lake.lat arguments, or supply irr as integer vector of 1's and 0's")
#   #   }
#     dayI <- irr == 1L
#     nightI <- irr == 0L
#   # }
#   delta.do <- diff(do.obs)
#   miss.delta <- sum(is.na(delta.do))
#   if (miss.delta != 0) {
#     warning(paste(miss.delta, " missing values (", miss.delta/length(delta.do), 
#                   "%) in diff(do.obs)", sep = ""))
#   }
#   gas.flux <- (do.sat - do.obs) * (k.gas/freq)/z.mix
#   delta.do.metab <- delta.do - gas.flux[1:(length(gas.flux) - 
#                                              1)]
#   nep.day <- delta.do.metab[dayI]
#   nep.night <- delta.do.metab[nightI]
#   R <- mean(nep.night, na.rm = TRUE) * freq
#   NEP <- mean(delta.do.metab, na.rm = TRUE) * freq
#   GPP <- mean(nep.day, na.rm = TRUE) * sum(dayI) - R
#   metab <- data.frame(GPP = GPP, R = R, NEP = NEP)
#   return(metab)
# }
# 
# 
# metab.estimates <- function(kgas) {
#   
#   # Setup empty data.frame matching observations
#   model_abbr<-c('bookkeep')
#   output = data.frame(matrix(ncol=4, nrow=length(metab.dates), NA))
#   names(output)=c('date', 'NEP', 'GPP', 'R')
#   output$date<-metab.dates
#   #create a vector for dates starting at 5AM (use this for start/stop of each day)
#   date_minus6hr<-as.Date(datetime - hours(6), tz="America/Los_Angeles")
# 
#   # Loop through each day.
#   i=2
#   for (i in 1:nrow(output)) {
# 
#     indx <- date_minus6hr == output$date[i]
#     
#     GPPindx <- dayIrr == 1 & date_minus6hr == output$date[i]
#     
#     ERindx <-  dayIrr == 0 & date_minus6hr == output$date[i] & z.mix == 6
#     if(length(which(GPPindx==TRUE) > 0)){
#     ERindx[1:max(which(GPPindx==TRUE))] <- FALSE
#     }
#     
#     if(length(which(is.na(do.obs[indx])==T))==0){
#       timelength<-difftime(datetime[indx][length(datetime[indx])], datetime[indx][1], units="hours")
#       if (timelength > 22){
#     
#     # m.book = metab.bookkeep(do.obs[indx], do.sat[indx], kgas[indx], z.mix[indx], dayIrr[indx], datetime=datetime[indx])
#     
#     m.book = metab.bookkeep(do.obs[indx], do.sat[indx], kgas[indx], z.mix[indx], dayIrr[indx], datetime=datetime[indx])
#     #NEP
#     output$NEP[i] = round(m.book$NEP,3)
#     #GPP
#     output$GPP[i] = round(m.book$GPP,3)
#     #R
#     output$R[i] = round(m.book$R,3)
# 
#     
#     #Use subset of time for calculating GPP and ER
#     # m.book.gpp = metab.bookkeep(do.obs[GPPindx], do.sat[GPPindx], kgas[GPPindx], z.mix[GPPindx], dayIrr[GPPindx], datetime=datetime[GPPindx])
#     # m.book.er = metab.bookkeep(do.obs[ERindx], do.sat[ERindx], kgas[ERindx], z.mix[ERindx], dayIrr[ERindx], datetime=datetime[ERindx])
#     # #NEP
#     # output$NEP[i] = round(m.book.gpp$NEP,3)
#     # 
#     # #R
#     # output$R[i] = round(m.book.er$NEP,3)
#     # 
#     # #GPP
#     # output$GPP[i] = output$NEP[i] - output$R[i]
# 
#     
#       }
#     }
#   }
#   return(output)
# }
# 
# 
# # ###########################################
# # Loop through each buoy and model metabolism
# # ###########################################
# metab.list<-list()
# buoy_nu<-8
# for (buoy_nu in 1:length(buoy_numbers)){
# 
#   buoy_name<-buoy_numbers[buoy_nu]
#   
#   df_i<-df_deploy %>%
#     dplyr::filter(Buoy==buoy_name)
#   
#   datetime<-df_i$DateTime_PST_round
#   date<-df_i$Date
#   do.obs = df_i$Dissolved.Oxygen
#   
#   # Get equilibrium saturation concentration of oxygen in water
#   do.sat = o2.at.sat.base(df_i$Temperature, altitude=0)
#   
#   # Par and day/night
#   # par = metData$par
#   
#   # For bookeeping irradiance
#   dayIrr = physics_df$dayIrr[match(datetime, physics_df$DateTime_PST_round)] #Sparkling Lake is at 48 degrees N latitude
#   
#   
#   # Surface mixed depth
#   # z.mix = rep(z.mean, length(datetime))
#   
#   #New estimate of z-mix
#   #For each day use the max between the observed z-mix at the time or the max for the rest of the day
#   # z.mix <- physics_df$Z_new[match(datetime, physics_df$DateTime_PST_round)]
#   # z.mix_dark <- physics_df$Zmix_m[match(datetime, physics_df$DateTime_PST_round)]
#   # z.mix[which(dayIrr==0)] <- z.mix_dark[which(dayIrr==0)]
# 
#   z.mix <- physics_df$Zmix_m[match(datetime, physics_df$DateTime_PST_round)]
#   
#   # plot(datetime, z.mix_dark, ylim=c(6,0))
#   # points(datetime, z.mix, type='l', col='red')
#   # 
# 
#   # Water temperature at dissolved oxygen sensor depth (0.5 m)
#   wtr = df_i$Temperature
#   
#   #average k
#   # k_i<-rep(ko2md_mean, length(datetime))
#   
#   #use daily average k
#   #all input terms for k estimates
#   wind.ms <- wind_avg$AWND[match(as.Date(datetime, tz=attributes(datetime)$tzone), wind_avg$Date)]
#   
#   #calculating the gas exchange coefficient for O2 empirically from lake area and wind speed:
#   u10 <- wind.ms * (1+ (((0.0013^(0.5))/0.41) * (log(10/wind.height)))) #converting wind speed from 3m to 10m height following equation 3 Vachon & Prairie (2013)
#   k600cmh <- 2.51 + 1.48*u10 + 0.39*u10*(log10(area)) #k600 in cm/h from table 2 equation B vachon & prairie 2013
#   k600md <- k600cmh * 24/100 #converting k600 to m/d
#   sco2 <- 1800.6 - (120.1*wtr) + (3.7818 * (wtr^2)) - (0.047608*(wtr^3))#calculating schmidt number for oxygen from Jahne et al (1987)
#   ko2md <- k600md * ((sco2/600)^(-2/3)) #converting k600 to ko2 in m/d for use in mass balance
#   
#   k_i<-ko2md
#   
#   
#   # ####################
#   # Estimate Metabolism
#   # ####################
#   
#   
#   # Calculation for multiple days
#   # set dates
#   metab.dates<-unique(as.Date(datetime, tz=attributes(datetime)$tzone))
#   
# 
#   #Run function and calculate metabolism or each day (i) using each kgas model
#   metab.i = metab.estimates(k_i)
#   
#   metab.i$Buoy<-rep(buoy_name, nrow(metab.i))
# 
#   metab.list[[buoy_nu]]<-metab.i
#   
# }


metab.df<-ldply(metab.list, data.frame)
metab.df$Buoy<-factor(metab.df$Buoy, buoy_numbers)

color.palette = colorRampPalette(c(viridis(6, begin=.2, end=.98), rev(magma(5, begin=.35, end=.98))), bias=1)
colors<-rev(color.palette(length(unique(metab.df$Buoy))))
xlim<-range(metab.df$date[which(is.finite(metab.df$NEP))], na.rm=T)

#Common theme for all metabolism timeseries panels
commonThemePrint<-list(
  scale_colour_manual(values = colors),
  scale_fill_manual(values = colors),
  scale_shape_manual(values=c(23, 22,22,22,21,21,21,21,22,22,22,23)),
  # geom_smooth(method='loess',  se=F),
  # geom_smooth(method='auto', se=T, alpha=.2),
  # geom_jitter(size=2, width=jitterwidth, height=0, aes(fill=Site, shape=Site)),
  geom_hline(yintercept=0, color='lightgrey', linetype=1.5, size=1), 
  geom_vline(xintercept=shipdate, color='grey', linetype=2, size=1),
  geom_vline(xintercept=fertdate, color='black', linetype=1, size=1),
  theme_bw(),
  theme(plot.title = element_text(hjust=0.5), legend.position="none", axis.title.x=element_blank()), 
  scale_x_date(limits=xlim, date_minor_breaks= "1 days", date_breaks = "2 days", date_labels="%b %d"), 
  guides(color = guide_legend(nrow = 1, title.position='top', title.hjust=0.5)) 
)

NEPplot<-ggplot(metab.df, aes(date, NEP, group=(Buoy))) + 
  commonThemePrint + 
  labs(x='Date', y=expression(paste('Daily NEP (g ', O[2], ' m'^'-2', ' d'^'-1', ')'))) +
  geom_path(aes(color=Buoy, group=Buoy), size=0.5) + 
  geom_point(size=2, aes(fill=Buoy, shape=Buoy)) + 
  ggtitle('Free-water metabolism')

GPPplot<-ggplot(metab.df, aes(date, GPP, group=(Buoy))) + 
  commonThemePrint + 
  labs(x='Date', y=expression(paste('Daily GPP (g ', O[2], ' m'^'-2', ' d'^'-1', ')'))) +
  geom_path(aes(color=Buoy, group=Buoy), size=0.5) + 
  geom_point(size=2, aes(fill=Buoy, shape=Buoy))

ERplot<-ggplot(metab.df, aes(date, R, group=(Buoy))) + 
  commonThemePrint + 
  labs(x='Date', y=expression(paste('Daily ER (g ', O[2], ' m'^'-2', ' d'^'-1', ')'))) +
  geom_path(aes(color=Buoy, group=Buoy), size=0.5) + 
  geom_point(size=2, aes(fill=Buoy, shape=Buoy))


# arrange plots without legend
p2<-grid.arrange(grobs=list(GPPplot, ERplot, NEPplot), ncol=1, as.table=F)

p1<-ERplot + 
  theme(legend.position='bottom') + 
  guides(colour = guide_legend(nrow = 2, title.position='left', title.hjust=0.5))
mylegend<-g_legend(p1)

grid.arrange(p2, mylegend, nrow=2,heights=c(10, 1.5))



png(paste0(dropbox_dir, '/Figures/NutrientExperiment/Buoys/Metabolism_TS_wholewatercolumn.png'), width=5, height=7, units='in', res=200)

grid.newpage()
plots<-grid.draw(rbind(ggplotGrob(NEPplot), ggplotGrob(GPPplot),  ggplotGrob(p1), size = "first"))

dev.off()


write.table(metab.df, file=paste0(dropbox_dir, '/Data/NutrientExperiment/Buoys/MetabolismEstimates_v2.csv'), row.names=F, sep=',')



#Plot Buoy DO Timeseries


deploytimes<-as.POSIXct(c("2018-09-26 20:00:00", "2018-10-10 17:00:00"), format='%Y-%m-%d %H:%M:%S', tz='America/Los_Angeles')
shiptimes<-as.POSIXct(c("2018-10-07 14:00:00", "2018-10-06 11:20:00"), format='%Y-%m-%d %H:%M:%S', tz='America/Los_Angeles')
ferttime<-as.POSIXct("2018-10-01 13:30:00", format='%Y-%m-%d %H:%M:%S', tz='America/Los_Angeles')


#colors
color.palette = colorRampPalette(c(viridis(6, begin=.2, end=.98), rev(magma(5, begin=.35, end=.98))), bias=1)
colors<-rev(color.palette(length(unique(df_deploy$Sensor))))
xlim<-range(df_deploy$DateTime_PST, na.rm=T)

DO_TS <- ggplot(df_deploy, aes(DateTime_PST, Dissolved.Oxygen, group=Buoy)) + 
  labs(x='Date (PDT)', y=expression(paste('DO (mg ', O[2], ' L'^'-1', ')', sep=''))) +
  scale_shape_manual(values=rep(21:25, 5))  + 
  scale_fill_manual(values = colors) + 
  scale_colour_manual(values = colors) + 
  geom_vline(xintercept=shiptimes[1:2], color='grey', linetype=2, size=1) + 
  geom_vline(xintercept=ferttime, color='black', linetype=1, size=1) + 
  geom_path(aes(color=Buoy), size=1) + 
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))  + 
  theme(legend.position='bottom') + 
  scale_y_continuous(limits=c(6.5, 11)) + 
  scale_x_datetime(limits=xlim, date_minor_breaks= "1 days", date_breaks = "2 days", date_labels="%b %d") +
  theme(axis.title.x =element_blank()) + 
  guides(color = guide_legend(nrow = 2, title.position='left', title.hjust=0.5)) 


png(paste0(dropbox_dir, '/Figures/NutrientExperiment/Buoys/DissolvedOxygen_TS_SSCN.png'), width=5, height=3, units='in', res=200)

print(DO_TS) 

dev.off()




