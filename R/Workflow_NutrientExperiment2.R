


#Wrapper code to perform data processing associated with nutrient experiment 2
# July to September 2019

rm(list = ls())

#Code to extract surface water measurements from profile data at fixed sites

library(readxl)
library(plyr)
library(dplyr)
library(viridis)
library(lubridate)
library(ggplot2)
library(gridExtra)


source('R/read_excel_allsheets_skip.R')
source('R/read_excel_allsheets.R')
source('R/g_legend.R')
source('R/lightmodel.R')

# Project folder where outputs are stored
dropbox_dir<-'C:/Dropbox/USBR Delta Project'

#Where data come from
google_dir<-'C:/GoogleDrive/DeltaNutrientExperiment'

#Where data come from
box_dir<-'C:/Users/lcloken/Box/SadroLab/Luke/SSCN2'


#Field data
source('R/CleanFieldData_NutrientExperiment2.R')

#Longitudinal Data (Script works, but takes a long time)
# source('R/PlotLongitudinalProfiles_NutrientExperiment2.R')

#Vertical profiles (Script works, just takes a long time)
# source('R/CleanPlotVerticalProfiles_NutrientExperiment2.R')
# source('R/CompileVerticalProfiles_NutrientExperiment2.R')


#Nutrient data
source('R/CleanNutrientData_NutrientExperiment2.R')

#Merge multiple datasets together. 
#This script is still a work in progress and more datasets can be included in the merger
source('R/MergeSurfaceYSI_FieldData_NutrientExperiment2.R')


#Compute dissolved gases and merge with data
source('R/pCO2_computation_script_NutrientExperiment2.R')


# ######################################################
# All code above is functioning (Oct 9, 2019, Luke Loken)
# ######################################################



#USGS buoy data
# source('R/PlotUSGSBuoyData_NutrientExperiment2.R'))

#Light profiles
# source('R/Calculatekd_NutrientExperiment.R')


#Incubation data
source('R/IncubationMetabolism_SSCN2_Workflow.R')
# source('R/Plot_IncubationMetabolism_Timeseries_SSCN2.R')



#O18 metabolism (still need to add these)
# source('R/CreateO18InputTable.R')
# source('R/CalculateO18metabolism_NutrientExperiment.R')
# source('R/Plot_O18Metabolism.R')

#Estimate Photic depth and scale incubation metabolism to whole ecosystem
# source('R/PredictPhoticDepth_NutrientExperiment.R')

#Buoy Metabolism
# source('R/ModelMetabolismBuoyDO_SSCN1.R')
# source('R/AggregateBuoyMetabolismByZone_NutrientExperiment.R')

#Merge Buoy metabolism
# source('R/MergeBuoyMetabolism_NutrientExperiment.R')

#Plot a bunch of stuff
source('R/PlotTimeseries_NutrientExperiment2.R')

#Zooplankton, Phytoplankton, Picoplankton
source('R/CompilePlankton_NutrientExperiment2.R')
source('R/PlotZoopsTimeseries_NutrientExperiment.R')

# source('R/.R')