# USBRDelta
Sacramento San Joaquin Delta Project

### updated by Luke Loken February 2020

This repo contains scripts for processing Deep Water Ship Channel data.
Data are grouped into 3 classes: Monthly, Experiment 1, and Experiment 2

Each data group contains a single workflow script that sources a variety of data cleaning, merging, analyzing, and plotting scripts. 

These scripts are entitled:

Workflow_MonthlySampling.R

Workflow_NutrientExperiment.R

Workflow_NutrientExeperiment2.R

Each Script begins by loading a handful of libraries, custom functions, and a character string noting the data directory. As of Feb 2020, data are being moved to a OneDrive folder entitled "USBR_DWSC"

```
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
source('R/not_all_na.R')

#As of Feb 2020, new single folder where all data and outputs live
onedrive_dir <- 'C:/Users/lloken/OneDrive - DOI/USBR_DWSC'

```

For the scripts to run, the object `onedrive_dir` needs to match your computer. 

## Running the workflow

The workflow script calls a number of scripts that each complete a set of tasks. 
These generally follow the order:
1)load data
2)process data
3)combine data
4)analyze data
5)plot data

I apologize but many scripts are lengthy, but I am trying to be better of making each script shorter and only exectue one task. 

```

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

```

Data that have been processed and saved to the onedrive. The script creates two types of data objects (rds and csv files). They are saved in the Rdata, and OutputData folders. 

Figures are saved in the Figures folder and are organized by data types

## Disclaimer

This software is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science. The software has not received final approval by the U.S. Geological Survey (USGS). No warranty, expressed or implied, is made by the USGS or the U.S. Government as to the functionality of the software and related material nor shall the fact of release constitute any such warranty. The software is provided on the condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use of the software.
