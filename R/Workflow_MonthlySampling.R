

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


#colors and plotting factors
color.palette = colorRampPalette(c(viridis(6, begin=.2, end=.98), rev(magma(5, begin=.35, end=.98))), bias=1)
colors_stations<-color.palette(13)
colors_zone<-color.palette(5)
stationfactors<-c("16", "34", "44", "Pro", "56", "62", "64", "66" ,"70" ,"74" ,"76" ,"84" ,"WSP")



# ##############################
# Nutrient data
# ##############################

# Locate most recent water chemistry data from Xien
source('R/CleanNutrientData.R')

#Mims data

#O18 data

# ####################
# Incubation data
# ####################

# Input date to link new folder
# Date<-'102218'
# source('R/IncubationMetabolism_Workflow.R')


# ##############################
# Vertical profiles
# ##############################

# Loop through vertical profiles and plot all
# Might want to change this to only run the most recent date?
# Also saves a surface, mid, and deep measurement for each time/place
source('R/CleanPlotVerticalProfiles.R')
source('R/CompileVerticalProfiles.R')


# ##############################
# Longitudinal profiles
# ##############################

# Loop through longitudinal profiles and plot all
# Might want to change this to only run the most recent date?
# Be careful because this downloads google images and may cost Luke Loken money. 
# source('R/PlotLongitudinalProfiles.R')


# ##############################
# Phytoplankton and zooplankton 
# ##############################

source('R/CompilePhytosZoops.R') #Load all phytoplankton and zooplankton counts

source('R/AggregatePhytos.R') # Summarize phyto records
source('R/AggregateZoops.R') # Summarize Zoops records

source('R/PlotPhytos.R') # Plot phyto records
source('R/PlotZoops.R') # Plot Zoops records


# ################
# Merge all data
# ################

source('R/MergeChemistryYSIPhytosZoops.R')


# ################
# Timeseries plots
# ################



#script for each variables
source('R/PlotPhytosChlA.R') # Plot phyto timeseries records with chlA
source('R/PlotTimeseriesNutrients.R') # Plot phyto timeseries records with chlA

#Other figures (scatterplots)

# End



