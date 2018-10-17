


#Wrapper code to perform data processing associated with nutrient experiment


#Code to extract surface water measurements from profile data at fixed sites

library(readxl)
library(plyr)
library(dplyr)
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

source('R/IncubationMetabolismLightDark_Workflow.R')
source('R/Plot_IncubationMetabolism_Timeseries.R')

source('R/CompileVerticalProfiles_NutrientExperiment.R')

source('R/ExtractSurfaceYSI_NutrientExperiment.R')
source('R/MergeSurfaceYSIChemistryMetabolism_NutrientExperiment.R')

source('R/PlotTimeseries_NutrientExperiment.R')
# source('R/.R')