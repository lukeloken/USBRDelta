
#Script to loop through all buoy sensor data from Nutrient Experiment 2 in the Sacramento Deep Water Ship Channel
#September, 2019
#Luke Loken and Reed Tran
# 5 buoy arrays were deployed in the channel, each consisting of:
# 3 Hobo temperature sensors
# 3 Hobo conductiviy sensors
# and 3 PME minidot oxygen sensors

#Script binds dataframes from the same sensor
#and outputs a 'raw' csv file for that sensor. 
#Subsequent scripts will cut and clean these data

#some hand packages
library(lubridate)
library(plyr)
library(dplyr)
library(viridis)
library(ggplot2)
library(gridExtra)
library(akima)


#Identify folder where DO, temp, and cond data are
DO_folder <-paste0(onedrive_dir, "/RawData/NutrientExperiment2/BuoyData/DO/9-30-19_Download/DO_9-30-19")
Temp_folder_main <-paste0(onedrive_dir, "/RawData/NutrientExperiment2/BuoyData/Temperature") 
Cond_folder_main <-paste0(onedrive_dir, "/RawData/NutrientExperiment2/BuoyData/Conductivity") 

#Read in each file, change header names, 
#attach serial number, change column formats

#DO
DO_files <- list.files(DO_folder)

file_nu=1
DO_list <- list()
for (file_nu in 1: length(DO_files)){
  
  df_i <- read.table(file = paste(DO_folder, DO_files[file_nu], sep='/'), skip=9, header=F, sep=",", stringsAsFactors = F)
  
  df_names <-  read.table(file = paste(DO_folder, DO_files[file_nu], sep='/'), skip=7, header=T, sep=",")
  
  names(df_i) <- names(df_names)
  
  SerialNumber <- DO_files[file_nu]
  SerialNumber <- gsub(".TXT", '', SerialNumber)
  
  df_i$SerialNumber <- rep(SerialNumber, nrow(df_i))
  
  DO_list[[file_nu]] <-df_i
  
}

DO_df <- ldply(DO_list, data.frame)

DO_df_clean <- DO_df %>%
  dplyr::select(SerialNumber, UTC_Date_._Time, Temperature, Dissolved.Oxygen, Dissolved.Oxygen.Saturation) %>%
  dplyr::rename(Datetime_UTC = UTC_Date_._Time,
                Temp_C = Temperature, 
                DO_mgL = Dissolved.Oxygen,
                DO_PerSat = Dissolved.Oxygen.Saturation) 

DO_df_clean$Datetime_UTC <- as.POSIXct(DO_df_clean$Datetime_UTC, tz="UTC", format = "%Y-%m-%d %H:%M:%S")

head(DO_df_clean)

#Save to file
write.table(DO_df_clean, file=file.path(onedrive_dir, 'OutputData', 'NutrientExperiment2', 'Buoy', 'Buoy_DO_raw.csv'), row.names=F, sep=',')

saveRDS(DO_df_clean, file=file.path(onedrive_dir, 'RData', 'NutrientExperiment2', 'Buoy', 'Buoy_DO_raw.rds'))


#Temperature

Temp_folders<-list.files(Temp_folder_main)

folder = 1
Folder_list = list()
# Temp_list <- list.dirs(Temp_folders) 
for (folder in 1 : length(Temp_folders)){
  #Identify first folder to retrieve temp data
  
  Temp_folder_i <- Temp_folders[folder]
  
  temp_files <- list.files(paste(Temp_folder_main, Temp_folder_i, sep='/'))

  Temperature_list <- list()
  
  file=1 
  for (file in 1 : length(temp_files)){
    
    df_temp <- read.csv(file = paste(Temp_folder_main, Temp_folder_i, temp_files[file], sep='/'), skip=1, header=T, sep=",", stringsAsFactors = F)
    
    df_temp <- select(df_temp, 2:3)
    
    names(df_temp) <- c("Datetime_PST", "Temp_C")
    
    SerialNumber <- temp_files[file]
    
    SerialNumber <- gsub(".csv", '', SerialNumber)
    
    df_temp$SerialNumber <- rep(SerialNumber, nrow(df_temp))
    
    Temperature_list[[file]] <- df_temp
    
  }
  
  Temp_df <- ldply(Temperature_list, data.frame)
  
  Folder_list[[folder]] <- Temp_df
  
}

Final_temp <- ldply(Folder_list, data.frame) %>%
  mutate(Datetime_PST = as.POSIXct(Datetime_PST, tz="Etc/GMT+8", format = "%m/%d/%y %I:%M:%S %p")) 

Final_temp$Datetime_UTC <- Final_temp$Datetime_PST
attributes(Final_temp$Datetime_UTC)$tzone <- 'UTC'
Final_temp <- dplyr::select(Final_temp, Datetime_UTC, Temp_C, SerialNumber)


#Save to file
write.table(Final_temp, file=file.path(onedrive_dir, 'OutputData', 'NutrientExperiment2', 'Buoy', 'Buoy_Temp_raw.csv'), row.names=F, sep=',')

saveRDS(Final_temp, file=file.path(onedrive_dir, 'RData', 'NutrientExperiment2', 'Buoy', 'Buoy_Temp_raw.rds'))



#Conductivity
Cond_folders<-list.files(Cond_folder_main)
Cond_folders<-Cond_folders[str_detect(Cond_folders, 'Specific')]

folders = 1
Folders_list = list()

for (folders in 1 : length(Cond_folders)){
  
  Cond_folder_i <- Cond_folders[folders]
  
  Cond_files <- list.files(paste(Cond_folder_main, Cond_folder_i, sep='/'))
  
  
  Conductivity_list <- list()
  
  #files=8 is the problematic file
  files=8
  for (files in 1 : length(Cond_files)){
    #insert the vector of the list.files() call above
    
    df_Cond <- read.csv(file = paste(Cond_folder_main, Cond_folder_i, Cond_files[files], sep='/'), skip=1, header=T, sep=",", stringsAsFactors = F)
    
    df_Cond <- dplyr::select(df_Cond, 2:5)
    
    names(df_Cond) <- c("Datetime_PST", "RawCond_uScm", "Temp_C", 'SpecCond_uScm')
    
    SerialNumber <- Cond_files[files]
    SerialNumber <- gsub(".csv", '', SerialNumber)
    
    df_Cond$SerialNumber <- rep(SerialNumber, nrow(df_Cond))
    
    Conductivity_list[[files]] <- df_Cond
    
  }
  
  Cond_df <- ldply(Conductivity_list, data.frame)
  
  Folders_list[[folders]] <- Cond_df
  
}
Final_Cond <- ldply(Folders_list, data.frame) %>%
  mutate(Datetime_PST = as.POSIXct(Datetime_PST, tz="Etc/GMT+8", format = "%m/%d/%y %I:%M:%S %p")) 

Final_Cond$Datetime_UTC <- Final_Cond$Datetime_PST
attributes(Final_Cond$Datetime_UTC)$tzone <- 'UTC'
Final_Cond <- dplyr::select(Final_Cond, Datetime_UTC, Temp_C, SpecCond_uScm, RawCond_uScm, SerialNumber)


#Save to file
write.table(Final_Cond, file=file.path(onedrive_dir, 'OutputData', 'NutrientExperiment2', 'Buoy', 'Buoy_Cond_raw.csv'), row.names=F, sep=',')

saveRDS(Final_Cond, file=file.path(onedrive_dir, 'RData', 'NutrientExperiment2', 'Buoy', 'Buoy_Cond_raw.rds'))


rm(Cond_df, Conductivity_list, df_Cond, df_i, df_names, df_temp, DO_df, DO_df_clean, DO_list, Final_Cond, Final_temp, Folder_list, Folders_list, Temp_df, Temperature_list, Cond_files, Cond_folder_i, Cond_folder_main, Cond_folders, DO_files, DO_folder, file, file_nu, files, folder, folders, temp_files, Temp_folder_i, Temp_folder_main, Temp_folders, SerialNumber)

