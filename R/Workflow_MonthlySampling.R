

# Work in progress
# Wrapper script to execute all the tasks associated with the monthly data collection



#Wrapper code to perform data processing associated with nutrient experiment
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


source('R/read_excel_allsheets.R')
source('R/g_legend.R')

# Project folder where outputs are stored
dropbox_dir<-'C:/Dropbox/USBR Delta Project'

#Where data come from
google_dir<-'C:/GoogleDrive/DeltaNutrientExperiment'



#Field data
# input info from field data sheet and sample inventory
# source('R/CleanFieldData.R')


#Nutrient data
# Locate most recent water chemistry data from Xien
source('R/CleanNutrientData.R')
#saveRDS(WQ_stations , file=paste0(dropbox_dir, '/Data/Rdata/WQ_stations'))


#Mims data

#O18 data

#Incubation data
# Input date to link new folder
# Date<-'102218'
# source('R/IncubationMetabolism_Workflow.R')


#Vertical profiles
# Loop through vertical profiles and plot all
# Might want to change this to only run the most recent date?
# Also should save a surface, mid, and deep measurement for each time/place
source('R/CleanPlotVerticalProfiles.R')
source('R/CompileVerticalProfiles.R')

# Longitudinal profiles
# Loop through longitudinal profiles and plot all
# Might want to change this to only run the most recent date?
# Be careful because this downloads google images and may cost Luke Loken money. 
# source('R/PlotLongitudinalProfiles.R')


# Phytoplankton and zooplankton samples profiles
source('R/CompilePhytosZoops.R') #Load all phytoplankton and zooplankton counts

source('R/AggregatePhytos.R') # Summarize phyto records
source('R/AggregateZoops.R') # Summarize Zoops records

source('R/PlotPhytos.R') # Plot phyto records
source('R/PlotZoops.R') # Plot Zoops records

#Extra script for Phytos to merge with ChlA data. This should likely get merged into a later script for plotting all timeseries
source('R/PlotPhytosChlA.R') # Plot phyto records


#Merge Data

#Timeseries plots

#Other figures (scatterplots)

# Do not run!!!!!

# ##########################################
# ###########################################
# copy paste below from nutrient addition experiment
# Use as a template, but do not run below
# ############################################
# ##############################################



#Field data
source('R/CleanFieldData_NutrientExperiment.R')

#Nutrient data
source('R/CleanNutrientData_NutrientExperiment.R')

#Incubation data
source('R/IncubationMetabolismLightDark_Workflow.R')
source('R/Plot_IncubationMetabolism_Timeseries.R')

#Vertical profiles
source('R/CompileVerticalProfiles_NutrientExperiment.R')
source('R/CompileVerticalProfiles_NutrientExperiment_EXO.R')

source('R/ExtractSurfaceYSI_NutrientExperiment.R')
source('R/MergeSurfaceYSIChemistryMetabolism_NutrientExperiment.R')

#O18 metabolism (still need to add these)
source('R/CreateO18InputTable.R')
source('R/CalculateO18metabolism_NutrientExperiment.R')
source('R/Plot_O18Metabolism.R')


source('R/PlotTimeseries_NutrientExperiment.R')
# source('R/.R')