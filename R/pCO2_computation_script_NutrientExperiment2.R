
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

#load field and chem data
merge_df <- saveRDS(file=paste0(dropbox_dir, '/Data/Rdata_SSCN2/SiteData_Merged.rds'))


#load exetainer gas data

  
    ##Load pCO2 data:
    data <-read.csv(file="02_pCO2_data_cleaned_2017_2018.csv",header=TRUE)
    names(data)
    
    Water_Temp <- data$Water_Temp
    SGmixing <- data$SGmixing
    HSmixing <- data$HSmixing
    BP_atm <- data$BP_atm
    WaterVol <- data$WaterVol
    HSVol <- data$HSVol
    AirT <- 20 #Assumed air temperature in headspace
   ##Calculate CO2 concentration
    tempK = Water_Temp +273.15
    #temp at which equilibrium carried out in K
    tempairK = AirT + 273.15
    
    co2bunsen = (exp(-58.0931+(90.5069*(100/tempK))+(22.294*log(tempK/100))))*((0.0821*tempK)+((-1636.75+(12.0408*tempK)-(3.27957*0.01*tempK*tempK)+(3.16528*0.00001*tempK*tempK*tempK))/1000))
    #Bunsen solubility coefficients for headspace equilibration (L/L*atm)
    
    sourcegas = SGmixing/(0.0821*tempairK)   
    #source gas conc. umol/L, 0.0821 is R, the ideal gas constant L*atm/T*mol (T is temp in K)
    
    finalHSconc = HSmixing/(0.0821*tempairK) 
    #final headspace conc (umol/L)
    
    finalWaterconc = HSmixing*co2bunsen*BP_atm*(1/(0.0821*tempK)) 
    #final concentration in water used for equilibrium(umol/L)
    
    totalgas = (finalHSconc*HSVol)+(finalWaterconc*WaterVol) 
    #total gas in system (umoles)
    
    CO2uM = (totalgas-(sourcegas*HSVol))/WaterVol  
    #concentration of gas in lake water (umol/L)
    
   # Calculate the solubility potential for CO2 at field pressure and temp
   # based on Henry's law adjusted for temp, field pressure, and atmospheric
   # concentration of gas
    
    #KH_t= Henry's law adjusted for temp  Units= mol/L*atm
    KH_t=0.034*exp(2400*((1/tempK)-1/(298.15)))
    
   #Saturation concentration of CO2 at ambient temp and pressure
    #units: umol/L
    CO2sat= SGmixing* BP_atm * KH_t
    
    #Departure from saturation of CO2 as uM
    CO2dep=CO2uM - CO2sat
    
    #Saturation concentration of CO2 represented as a percent
    CO2sat_pct= CO2dep/ CO2sat*100
    
    
    #Express CO2 concentration in alternate units
    pCO2uatm=tempK*0.082057*CO2uM
    pCO2ppmv=pCO2uatm/BP_atm
    
    ##Export csv with pCO2 values in different units:
    pCO2.export <- data.frame(Lake_Name=data$Lake_Name,Coll_Date=data$Coll_Date, Time=data$Time, Depth_m=data$Depth_m, CO2uM=CO2uM, CO2sat_pct=CO2sat_pct, pCO2uatm=pCO2uatm, pCO2ppmv=pCO2ppmv)
    write.csv(pCO2.export, file='03_computed_pCO2_2017_2018.csv')
    