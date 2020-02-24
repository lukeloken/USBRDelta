

#Wrapper code to perform data processing associated with nutrient experiment 2
# July to September 2019

rm(list = ls())

#Code to extract surface water measurements from profile data at fixed sites

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

source('R/read_excel_allsheets.R')
source('R/g_legend.R')
source('R/lightmodel.R')
source('R/ImageScale.R')

#As of Feb 2020, new single folder where all data and outputs live
onedrive_dir <- 'C:/Users/lloken/OneDrive - DOI/USBR_DWSC'

# Old locations on Loken-UCD-laptop
# dropbox_dir<-'C:/Dropbox/USBR Delta Project'
# google_dir<-'C:/GoogleDrive/DeltaNutrientExperiment'
# box_dir<-'C:/Users/lcloken/Box/SadroLab/Luke/SSCN2'

fert_dates<-as.Date(c("2019-07-22", "2019-07-23","2019-07-24", "2019-07-25", "2019-08-05", "2019-08-06","2019-08-07", "2019-08-08"))

fert_posix<-as.POSIXct(c("2019-07-22 08:00", "2019-07-23 08:00","2019-07-24 08:00", "2019-07-25 08:00", "2019-08-05 08:00", "2019-08-06 08:00","2019-08-07 08:00", "2019-08-08 08:00"), format='%Y-%m-%d %H:%M', tz="Etc/GMT+8")

#Field data
source('R/CleanFieldData_NutrientExperiment2.R')

#Longitudinal Data (Script works, but takes a long time)
# source('R/PlotLongitudinalProfiles_NutrientExperiment2.R')

#Vertical profiles (Scripts works, just take a long time)
# source('R/CleanPlotVerticalProfiles_NutrientExperiment2.R')
# source('R/CompileVerticalProfiles_NutrientExperiment2.R')

#Nutrient data
source('R/CleanNutrientData_NutrientExperiment2.R')

#Light profiles
source('R/Calculatekd_NutrientExperiment2.R')

#Merge multiple datasets together. 
#This script is still a work in progress and more datasets can be included in the merger
source('R/MergeSurfaceYSI_FieldData_NutrientExperiment2.R')


#Compute dissolved gases and merge with data
source('R/pCO2_computation_script_NutrientExperiment2.R')


#Incubation data
# source('R/IncubationMetabolism_SSCN2_Workflow.R') 
source('R/Plot_IncubationMetabolism_Timeseries_SSCN2.R')

#Wind and solar data
source('R/CleanCIMISWeather.R')

##############################################
# updated to onedrive above
# ############################################

#Oxygen18 metabolism
source('R/CreateO18InputTable_NutrientExperiment2.R')
source('R/CalculateO18metabolism_NutrientExperiment2.R')
source('R/Plot_O18Metabolism_NutrientExperiment2.R')

#Buoy metabolism
source('R/CombineBuoySensorData_SSCN2.R')
source('R/CleanPlotBuoyOxygen_NutrientExperiment2.R')
source('R/ModelMetabolismBuoyDO_SSCN2_wholewatercolumn.R')

#Combine metabolism
source('R/MergeMetabolism_NutrientExperiment2.R')


#Plot a bunch of stuff
source('R/PlotTimeseries_NutrientExperiment2.R')

#Zooplankton, Phytoplankton, Picoplankton
source('R/CompilePlankton_NutrientExperiment2.R')
source('R/PlotZoopsTimeseries_NutrientExperiment2.R')


# ######################################################
# All code above is functioning (Oct 29, 2019, Luke Loken)
# ######################################################



#USGS buoy data
# source('R/PlotUSGSBuoyData_NutrientExperiment2.R'))


#Merge Buoy metabolism
# source('R/MergeBuoyMetabolism_NutrientExperiment.R')


# source('R/.R')