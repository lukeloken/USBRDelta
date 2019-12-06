

##################################################################################
## O18 Metabolic estimates in lakes - mass balance steps
## Copy from Matt Bogard github on Feb 6, 2019
##################################################################################
#importing field and lab data for O18 mass balance metabolism modelling#######


library(viridis)
library(ggplot2)
library(gridExtra)
library(plyr)
library(dplyr)
library(tidyr)
library(lubridate)
library(RColorBrewer)
library(RcppRoll)

merge_df_wind <- readRDS(file=paste0(dropbox_dir, '/Data/Rdata_SSCN2/InputTermsO18Metabolism_SSCN2.rds'))


#load hypso data
Depth_df <- readRDS(file=paste0(dropbox_dir, '/Data/Rdata_SSCN2/HypsoCurveNL74.rds'))
# plot(Depth_df$Area_m2, Depth_df$Depth_m, ylim=c(12,0), type='o', pch=16)

#Approximate volume by depth
Depth_pred25 <- data.frame(approx(x=Depth_df$Depth_m, y=.5*(Depth_df$Volume_m3), xo=seq(0, max(Depth_df$Depth_m), by=0.25)))
Total_volume = sum(Depth_df$Volume_m3, na.rm=T)
Surface_area = Depth_df$Area_m2[which(Depth_df$Depth_m==0)]
Mean_depth <- Total_volume/Surface_area



lake.area.km2<-0.153
zmix.m <- Mean_depth
wind.height.ms <- 10

# met = read.csv("Data/aklakes_met_input_terms.csv", 
#                header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE, stringsAsFactors = F)
summary(merge_df_wind)


#notes:
#Wind speeds measured as hourly averages on Canvasback lake (USGS weather station)
#Wind speed data for trip #3 : 3-day Average (Jun 28 to 30, 2016) of daily average (2.88 m/s) and mean daily STDEV (+/- 1.24 m/s) 
#Wind speed data for trip #4 : 3-day Average (Sept 09-12, 2016) of daily average (1.77 m/s) and mean daily STDEV (+/- 1.18 m/s) 
#Wind data in Sept do not overlap with lake sampling b/c weather station was removed from lake


#function for oxygen isotope mass balance calculations of metabolic rates and balances###########
met.fun <- function(args){
  
  #all input terms added and renamed here
  wind.ms <- merge_df_wind$WS_ms_3day
  wind.height <-rep(wind.height.ms, nrow(merge_df_wind))
  area <- rep(lake.area.km2, nrow(merge_df_wind))
  temp <- merge_df_wind$YSI_Temp_C
  zmix <- rep(zmix.m, nrow(merge_df_wind))
  do <- rowMeans(merge_df_wind[c("FLAMe_EXODOmgL", "YSI_DO_mgL")], na.rm=T)
  do.pct.sat <- merge_df_wind$YSI_DO_perSat
  delo18.o2 <- merge_df_wind$d180_02.vs.VSMOW
  delo18.h2o <- merge_df_wind$d18OVSMOW_mean
  
  #calculating the gas exchange coefficient for O2 empirically from lake area and wind speed:
  u10 <- wind.ms * (1+ (((0.0013^(0.5))/0.41) * (log(10/wind.height)))) #converting wind speed from 3m to 10m height following equation 3 Vachon & Prairie (2013)
  k600cmh <- 2.51 + 1.48*u10 + 0.39*u10*(log10(area)) #k600 in cm/h from table 2 equation B vachon & prairie 2013
  k600md <- k600cmh * 24/100 #converting k600 to m/d
  sco2 <- 1800.6 - (120.1*temp) + (3.7818 * (temp^2)) - (0.047608*(temp^3))#calculating schmidt number for oxygen from Jahne et al (1987)
  ko2md <- k600md * ((sco2/600)^(-2/3)) #converting k600 to ko2 in m/d for use in mass balance
  #2/3 power used for wind speed less than 3.7 m/s following Vachon et al. (2010) and Guerin et al. (2007)
  k.z <- ko2md / zmix #input term for volumetric gas exchange
  
  #atom fractions calculated for input into mass balance:
  dosat <- do * 100/do.pct.sat #calculate dissolved oxygen concentration at equilibrium with atmosphere 
  ro18o2<-((delo18.o2/1000)*(0.0020052))+(0.0020052)#converting del value of DO-O18 back to O18:O16 ratio
  ro18h2o <- ((delo18.h2o/1000)*(0.0020052))+(0.0020052) #converting del value of H2O-O18 back to O18:O16 ratio
  ro18air = ((23.88/1000)*(0.0020052))+(0.0020052) #converting del value of O2-atmosphere back to O18:O16 ratio
  #switched to d18O-air value = 23.88 permil following Barkan & Luz 2005 
  o18o2 <- ro18o2 / (1 + ro18o2) #converting ratio of O18:O16 in DO to atom fraction following Bogard et al. (2017)
  o18h2o <- ro18h2o / (1 + ro18h2o) #converting ratio of O18:O16 in H2O to atom fraction following Bogard et al. (2017)
  o18air <- ro18air/(1 + ro18air) #fixed value: atom fraction of O18 as AF=R/(R+1)
  
  #generic fractionation factors
  ffp <- 1           # fractionation factor associated with photosynthesis
  ffg <- 0.9972      # fractionation factor associated with gas exchange
  ffs <- 1.0007      # fractionation factor associated with DO solubility
  ffr <- 0.985        # fractionation factor associated with ecosystem respiration - a weighted approximate average of all DO consumption processes
  
  # summarized equation terms as presented by Bocaniov et al. 2012
  a <- ffg * ffs * o18air
  b <- ffg * o18o2
  c <- ffr * o18o2
  d <- ffp * o18h2o
  
  #volumetric rates are in mg O2 per liter per day
  gppv <- (k.z * (((do * (b - c)) - (dosat * (a - c))) / (d - c))) #volumetric GPP
  rv <- (k.z * (((do * (b - d)) - (dosat * (a - d))) / (d - c))) #volumetric R
  nepv <- gppv - rv #volumetric NEP
  #areal rates are in grams O2 per m2 per day 
  gppa <- zmix * (k.z * (((do * (b - c)) - (dosat * (a - c))) / (d - c))) #areal GPP
  ra <- zmix * (k.z * (((do * (b - d)) - (dosat * (a - d))) / (d - c))) #areal R
  nepa <- gppa - ra #areal NEP
  # P:R (from Quay 1995 L&O) 
  g <- (ffg*((ffs * o18air) - ((do / dosat) * o18o2))) / (1 - (do / dosat)) #Term G directly from Quay 1995 equation
  #PtoR ratios calculated using a fractionation value of 0.985
  gpptor <- ((o18o2 * ffr) - g) / ((o18h2o * ffp) - g)
  
  output<- data.frame(gppv,gppa,rv,ra,nepv,nepa,gpptor,a,b,c,d,g)
  return(output)
}

#run the metabolism function on the input data frame#####
met.result = met.fun(merge_df_wind)
#print out of metabolism summary stats ####### 
summary(met.result)

met.result <- met.result %>%
  mutate(ra=ra*(-1), rv=rv*(-1))


#mergining metabolism results into the original input data frame #######
met.final <- cbind(merge_df_wind, met.result)


#exporting metabolism data as csv file#####
saveRDS(met.final, file=paste0(dropbox_dir, '/Data/Rdata_SSCN2/merge_df_O18.rds'))
write.csv(met.final, file=paste0(google_dir, '/SSCN2_DataOutputs/merge_df_O18.csv'), row.names=F)


