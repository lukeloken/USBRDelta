
## Functions and script to compute pCO2 concentration from headspace gas samples

#Adapated by Luke Loken for Deep Water Ship Channel nutrient experiment

#Adapted by A. Smits from Matlab code
# Adapted by S. Sadro from R code
# created by J. Coloso based on what J. Cole used at the Carry Institute
# and an excel program created by S. Hamilton 2003
# computations assume negligible salinity of water samples

#Calculations for determining concentration (in umoles/L) of gas in the 
#original liquid (i.e. the lake) of a headspace equilibration

#Need to have the gas concentration of the headspace in ppm
#Need to have the liquid volume in the sampling syringe (WaterVol) 
#Need to have the headspace volume (HSVol) (in L) for each equilibrium

#Note: the default source gas ppm (sgmixing) is set to compute in air (must be measured).  
#Set value to zero if the gas in the headspace is pure (e.g. N2 or He).

#NOTE: this returns the concentration of the gas in the lake water by
#default.

#References for solubility:
#Yamamoto, S., J.B. Alcauskas, and  T.E. Crozier. 1976. Solubility of methane in distilled water and seawater. J. Chem. Eng. Data 21: 78-80.
#Weiss, R.F. 1970. The solubility of nitrogen, oxygen and argon in water and seawater. Deep-Sea Res. 17: 721-735.
#Weiss, R.F. 1974. Carbon dioxide in water and seawater: The solubility of a non-ideal gas. Mar. Chem. 2: 203-215.
#Benson, B.B. and D. Krause, Jr. 1984. The concentration and isotopic fractionation of gases dissolved in freshwater in equilibrium with the atmosphere. 1. Oxygen. Limnol. Oceanogr. 25: 662-671.
#Weiss, R.F. and B.A. Price. 1980. Nitrous oxide solubility in water and seawater. Mar. Chem. 8: 347-359

#Validated by S. Sadro 12/2/2013
#Last updated 10/23/2017 by A. Smits

# List of variables to be input
#Organize variables in an excel file in the order they are shown below for loading: 
#SampleCode = unique identification code
#WaterVol = volume of water used in mixing (L)
#HSVol = volume of air or gas used in mixing (L)
#temp = temperature at which equilibrium performed(C)
#Note: for samples taken at depth, temp can either be surface water temp, temp at depth, or some weighted average of the two
#bp = barometric pressure during equilibrium, assumed to be atmospheric (atm)
#sgmixing = source gas mixing ratios (ppmv, of CO2 in source gas; computed from GC or IRGA data as air sample)
#HSmixing = final mixing ratio (ppmv, of CO2 at equilibrium in source gas; computed from GC or IRGA data as water sample)
##
# clear all

library(stringr)
library(dplyr)
library(wql)

#load field and chem data

# SSC_joined_data <- readRDS(file=paste0(dropbox_dir, '/Data/Rdata/SSC_joined_data.rds'))

SSC_joined_data <- readRDS(file=paste0(dropbox_dir, '/Data/Rdata/SSC_joined_data.rds'))



#load exetainer gas data for SSCN2
gas_df<-read_excel(paste0(dropbox_dir, "/Data/DissolvedGases/SSCN2_DissolvedGasData.xlsx"), skip=0)
names(gas_df) <- str_replace_all(names(gas_df), c(" " = "" , "," = "" ))

water_df<- gas_df %>%
  filter(Type =='Water' & Project =='SSC') %>%
  dplyr::select(-SampleName, -SampleID, -Event, -LocationCode, -LabNumber, -Type, -SampleCode) %>%
  mutate(LocationName = gsub("NL", "", LocationName)) %>%
  dplyr::group_by(LocationName, Date) %>%
  dplyr::summarize(ppmCH4 = mean(ppmCH4),
            ppmCO2 = mean(ppmCO2),
            ppmN2O = mean(ppmN2O),
            WaterVolume_mL = mean(WaterVolume_mL),
            AirVolume_mL = mean(AirVolume_mL)
            )
  

air_df <- gas_df %>%
  filter(Type =='Air' & Project =='SSC') %>%
  dplyr::select(-SampleName, -SampleID, -Event, -LocationCode, -LabNumber, -Type, -AirVolume_mL, -WaterVolume_mL, -Project, -SampleCode) %>%
  mutate(LocationName = gsub("NL", "", LocationName)) %>%
  dplyr::rename(Air_ppmCH4 = ppmCH4, 
         Air_ppmCO2 = ppmCO2, 
         Air_ppmN2O = ppmN2O)

gas_join<-left_join(water_df, air_df) %>% mutate(Date = as.Date(Date)) %>%
  group_by() %>%
  rename(Station = LocationName) %>%
  mutate(Station = factor(Station, levels(SSC_joined_data$Station)))


# merge_df_gas <- right_join (SSC_joined_data, gas_join) 


#load exetainer gas data for CO2 only dates. 
#CO2 ran on IRGA in lab
co2_list<-read_excel_allsheets(paste0(dropbox_dir, "/Data/DissolvedGases/SacDelta_pCO2_corrected.xlsx"))

co2_df<-ldply(co2_list[1:3], data.frame) %>%
  filter(Source=='Sample', SampleType !='DIC') %>%
  dplyr::select(Source, SampleDate, SampleType, Location,  pCO2_corrected) %>%
  dplyr::rename(Date=SampleDate, Station = Location)


water_df2<- co2_df %>%
  filter(SampleType !='Air') %>%
  dplyr::select(-SampleType, -Source) %>%
  mutate(Station = gsub("NL", "", Station)) %>%
  dplyr::group_by(Station, Date) %>%
  dplyr::summarize(ppmCO2 = mean(pCO2_corrected, na.rm=T), 
                   WaterVolume_mL = 110,
                   AirVolume_mL = 30)

air_df2<- co2_df %>%
  filter(SampleType =='Air') %>%
  dplyr::select(-SampleType, -Source) %>%
  mutate(Station = gsub("NL", "", Station)) %>%
  dplyr::group_by(Station, Date) %>%
  dplyr::summarize(Air_ppmCO2 = mean(pCO2_corrected, na.rm=T))

gas_join2<-left_join(water_df2, air_df2) %>% mutate(Date = as.Date(Date)) %>%
  group_by() %>%
  mutate(Station = factor(Station, levels(SSC_joined_data$Station)))



merge_df_gas <- bind_rows(gas_join, gas_join2) %>% 
  left_join (SSC_joined_data)
  


# #######################################
# Make vectors for each variable
# #######################################

#Source gas concentration data. Air used for equilibrations
SG_CO2     <-merge_df_gas$Air_ppmCO2
SG_CH4     <-merge_df_gas$Air_ppmCH4
SG_N2O     <-merge_df_gas$Air_ppmN2O

#Headspace gas concenration data. Concentration after equilibration between water and source gas
HS_CO2     <-merge_df_gas$ppmCO2
HS_CH4     <-merge_df_gas$ppmCH4
HS_N2O     <-merge_df_gas$ppmN2O

#Physical data for Henry's law and Bunsen
Water_Temp <-merge_df_gas$Temp.C #Celsius 
BP_atm     <- 1  #Atm
WaterVol   <-merge_df_gas$WaterVolume_mL/1000 #Liters
HSVol      <-merge_df_gas$AirVolume_mL/1000 #Liters
SPC        <-merge_df_gas$SpCond.uS #uscm
Salinity   <-ec2pss(SPC/1000, Water_Temp, p=0)

AirT <- merge_df_gas$Temp.C #Assumed air temperature in headspace

#convert temperature to kelvin
tempK = Water_Temp +273.15
tempairK = AirT + 273.15


# #######################################
# Calculate dissolved gas concentrations
# #######################################

##Calculate CO2 (Weiss 1974)
co2bunsen = (exp(-58.0931+(90.5069*(100/tempK))+(22.294*log(tempK/100))))*((0.0821*tempK)+((-1636.75+(12.0408*tempK)-(3.27957*0.01*tempK*tempK)+(3.16528*0.00001*tempK*tempK*tempK))/1000))
#Bunsen solubility coefficients for headspace equilibration (L/L*atm)

sourcegas_CO2 = SG_CO2/(0.0821*tempairK)   
#source gas conc. umol/L, 0.0821 is R, the ideal gas constant L*atm/T*mol (T is temp in K)

finalHSconc_CO2 = HS_CO2/(0.0821*tempairK) 
#final headspace conc (umol/L)

finalWaterconc_CO2 = HS_CO2*co2bunsen*BP_atm*(1/(0.0821*tempK)) 
#final concentration in water used for equilibrium(umol/L)

totalgas_CO2 = (finalHSconc_CO2*HSVol)+(finalWaterconc_CO2*WaterVol) 
#total gas in system (umoles)

CO2uM = (totalgas_CO2-(sourcegas_CO2*HSVol))/WaterVol  
#concentration of gas in lake water (umol/L)



##Calculate CH4 (Wiesenburg and Guinasso 1979)
# CH4 constants A1=-68.8862 A2=101.4956 A3=28.7314 B1=-0.076146 B2=0.043970 B3=-0.0068672

ch4bunsen = exp(-68.8862 + 101.4956*(100/tempK) + 28.7314*log(tempK/100) + Salinity*(-0.076146 + 0.043970*(tempK/100) + (-0.0068672)*(tempK/100)^2))*(0.0821*tempK)
#Bunsen solubility coefficients for headspace equilibration (L/L*atm)

sourcegas_CH4 = SG_CH4/(0.0821*tempairK)   
#source gas conc. umol/L, 0.0821 is R, the ideal gas constant L*atm/T*mol (T is temp in K)

finalHSconc_CH4 = HS_CH4/(0.0821*tempairK) 
#final headspace conc (umol/L)

finalWaterconc_CH4 = HS_CH4*ch4bunsen*BP_atm*(1/(0.0821*tempK)) 
#final concentration in water used for equilibrium(umol/L)

totalgas_CH4 = (finalHSconc_CH4*HSVol)+(finalWaterconc_CH4*WaterVol) 
#total gas in system (umoles)

CH4uM = (totalgas_CH4-(sourcegas_CH4*HSVol))/WaterVol  
#concentration of gas in lake water (umol/L)



##Calculate N2O (Weiss et al 1980)
# N2O constants A1=-62.7062 A2=97.3066 A3=24.1406 B1=-0.058420 B2=0.033193 B3=-0.0051313

n2obunsen = exp(-62.7062 + 97.3066*(100/tempK) + 24.1406*log(tempK/100) + Salinity*(-0.058420 + 0.033193*(tempK/100) + (-0.0051313)*(tempK/100)^2))*(0.0821*tempK)
#Bunsen solubility coefficients for headspace equilibration (L/L*atm)

N2O_solubility<-exp(-60.7467 +88.8280/tempK*100 + 21.2531*log(tempK/100))
#Solubility from https://sites.chem.colostate.edu/diverdi/all_courses/CRC%20reference%20data/solubility%20of%20gases%20in%20water.pdf

sourcegas_N2O = SG_N2O/(0.0821*tempairK)   
#source gas conc. umol/L, 0.0821 is R, the ideal gas constant L*atm/T*mol (T is temp in K)

finalHSconc_N2O = HS_N2O/(0.0821*tempairK) 
#final headspace conc (umol/L)

finalWaterconc_N2O = HS_N2O*n2obunsen*BP_atm*(1/(0.0821*tempK)) 
#final concentration in water used for equilibrium(umol/L)

totalgas_N2O = (finalHSconc_N2O*HSVol)+(finalWaterconc_N2O*WaterVol) 
#total gas in system (umoles)

N2OuM = (totalgas_N2O-(sourcegas_N2O*HSVol))/WaterVol  
#concentration of gas in lake water (umol/L)


# Calculate the solubility potential for CO2, CH4, and N2O at field pressure and temp
# based on Henry's law adjusted for temp, field pressure, and atmospheric
# concentration of gas

#KH_t= Henry's law adjusted for temp  Units= mol/L*atm
KH_t_CO2=0.034*exp(2400*((1/tempK)-1/(298.15)))
KH_t_CH4=0.0014*exp(1900*((1/tempK)-1/(298.15)))
KH_t_N2O=0.024*exp(2700*((1/tempK)-1/(298.15)))


#Saturation concentration at ambient temp and pressure
#units: umol/L
CO2sat= SG_CO2* BP_atm * KH_t_CO2
CH4sat= SG_CH4* BP_atm * KH_t_CH4
N2Osat= SG_N2O* BP_atm * KH_t_N2O

#Departure from saturation as uM
CO2dep=CO2uM - CO2sat
CH4dep=CH4uM - CH4sat
N2Odep=N2OuM - N2Osat

#Saturation concentration represented as a percent
CO2sat_pct= CO2uM/ CO2sat*100
CH4sat_pct= CH4uM/ CH4sat*100
N2Osat_pct= N2OuM/ N2Osat*100

#Merge calculated concentrations with rest of data
gas_out<-data.frame(Station=merge_df_gas$Station, Date=merge_df_gas$Date, CO2uM, CO2sat_pct, CH4uM, CH4sat_pct, N2OuM, N2Osat_pct) 
merge_df_gascals<-left_join(SSC_joined_data, gas_out)

#Save
write.csv(merge_df_gascals, file=paste0(google_dir, '/DataOutputs/SSC_joined_data_withGas_Merged.csv'), row.names=F)
saveRDS(merge_df_gascals , file=paste0(dropbox_dir, '/Data/Rdata/SSC_joined_data_withGas.rds'))


# plot(merge_df_gascals$N2OuM ~ merge_df_gascals$Station)
# plot(merge_df_gascals$CH4uM ~ merge_df_gascals$Station)
# plot(merge_df_gascals$CO2uM ~ merge_df_gascals$Station)

CO2plot <- ggplot(merge_df_gascals[which(!is.na(merge_df_gascals$CO2uM)),], aes(x=Station, y=CO2sat_pct, group=as.factor(Date), colour=as.factor(Date))) +
  geom_hline(yintercept=100, linetype='dashed') +
  geom_point() + 
  geom_line() +
  labs(y="CO2 (% sat)", colour='Date') +
  theme_bw() + 
  theme(legend.position="bottom") + 
  guides(color = guide_legend(nrow = 2, title.position='top', title.hjust=0.5)) 

N2Oplot <- ggplot(merge_df_gascals[which(!is.na(merge_df_gascals$CO2uM)),], aes(x=Station, y=N2Osat_pct, group=as.factor(Date), colour=as.factor(Date))) +
  geom_hline(yintercept=100, linetype='dashed') +
  geom_point(size=3) + 
  geom_line()+
  labs(y="N2O (% sat)", colour='Date')+
  theme_bw() + 
  theme() +
  theme(legend.position='none')


CH4plot <- ggplot(merge_df_gascals[which(!is.na(merge_df_gascals$CO2uM)),], aes(x=Station, y=CH4sat_pct, group=as.factor(Date), colour=as.factor(Date))) +
  geom_hline(yintercept=100, linetype='dashed') +
  geom_point(size=3) + 
  geom_line() +
  labs(y="CH4 (% sat)", colour='Date') +
  theme_bw() + 
  theme(legend.position='none')

png(paste0(dropbox_dir, '/Figures/DissolvedGasesByStation.png'), width=6, height=6, units='in', res=200)

grid.newpage()
plots<-grid.draw(rbind(ggplotGrob(CH4plot), ggplotGrob(N2Oplot),  ggplotGrob(CO2plot), size = "first"))

dev.off()
