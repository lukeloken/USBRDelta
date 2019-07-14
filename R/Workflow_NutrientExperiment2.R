


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
# source('R/CleanFieldData_NutrientExperiment.R')

#Light profiles
# source('R/Calculatekd_NutrientExperiment.R')

#Nutrient data
# source('R/CleanNutrientData_NutrientExperiment.R')

#Incubation data
# source('R/IncubationMetabolismLightDark_Workflow.R')
# source('R/Plot_IncubationMetabolism_Timeseries.R')

#Vertical profiles
source('R/CompileVerticalProfiles_NutrientExperiment2.R')

# source('R/ExtractSurfaceYSI_NutrientExperiment.R')
# source('R/MergeSurfaceYSIChemistryMetabolism_NutrientExperiment.R')

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
# source('R/PlotTimeseries_NutrientExperiment.R')

#Zooplankton, Phytoplankton, Picoplankton
# source('R/CompilePlankton_NutrientExperiment.R')
# source('R/PlotZoopsTimeseries_NutrientExperiment.R')

# source('R/.R')