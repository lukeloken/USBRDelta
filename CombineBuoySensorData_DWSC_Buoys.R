
# Script to loop through all buoy sensor data from 2021-2024 DWSC agreement. 
# 3 Buoy-arrays were positioned at channel markers 66, 74, and 84
# Raw data are saved within 'box_dir' and organized by 
# 1. Download interval (date1 to date2)
# 2. Buoy name (i.e., location)
# 3. Sensor name (DO minidots, PAR, Temperature/condcutivity deployed at multiple depths)


# 
# box_dir = "C:/Users/lloken/OneDrive - DOI/USBR_DWSC/CopyFromLokenComputer/DeepWaterShipChannel/DeepWaterShipChannel"
# box_dir = "C:/Users/lloken/OneDrive - DOI/USBR_DWSC/CopyFromLokenComputer/DeepWaterShipChannel_2024-08-09/DeepWaterShipChannel"
# box_dir = "C:/Users/lloken/OneDrive - DOI/USBR_DWSC/CopyFromLokenComputer/DeepWaterShipChannel_2024-08-12/DeepWaterShipChannel" 
# box_dir = "C:/Users/lloken/OneDrive - DOI/USBR_DWSC/CopyFromLokenComputer/DeepWaterShipChannel_2024-11-01/DeepWaterShipChannel" 

library(dplyr)
library(akima)
library(ggplot2)
library(lubridate)



ReadBuoySensorData <- function(box_dir, save_to_file = FALSE){
  
  #load some packages
  library(lubridate)
  # library(plyr)
  library(dplyr)
  # library(viridis)
  # library(ggplot2)
  # library(gridExtra)
  # library(akima)
  library(readxl)
  
  #Identify folders with data
  dir_folders <- list.files(file.path(box_dir, "DWSC_Data_Raw")) 
  IntervalName <- dir_folders[grepl("^[[:digit:]]+", dir_folders)]
  Interval <- as.numeric(substr(IntervalName, 1, 2))
  
  interval_df <- data.frame(Interval, IntervalName)
  
  #Get deployment info
  deployment_details <- readxl::read_excel(file.path(box_dir, 
                                                     "DWSC_Data_Raw", 
                                                     "Deployment intervals.xlsx"), 
                                           skip = 2)
  
  names(deployment_details) <- c("Interval", 
                                 "DateTime_UTC_Start",
                                 "DateTime_UTC_End", 
                                 "CalibrationPerson")
  
  # Convert to correct timezone, then to UTC
  # By default read_excel will load datetimes, and use the computer's timezone. 
  deployment_details <- deployment_details %>%
    mutate(across(contains("DateTime"), ~as.character(.x))) %>%
    mutate(across(contains("DateTime"), ~as.POSIXct(.x, tz = "Etc/GMT+8"))) %>%
    filter(!is.na(DateTime_UTC_End)) %>%
    left_join(interval_df, by = "Interval")
  
  attributes(deployment_details$DateTime_UTC_Start)$tzone <- "UTC"
  attributes(deployment_details$DateTime_UTC_End)$tzone <- "UTC"
  
  
  
  # deployment_details$IntervalName <- c("1-16AUG22_20MAR23",
  #                                      "2-30MAR23_7JUN23",
  #                                      "3-7JUN23_26SEPT23",
  #                                      "4-29SEPT23_8JAN24",
  #                                      "5-16Jan24_9APR24",
  #                                      "6-18APR24_29JUL24"
  #                                      # "7-1AUG24_",
  #                                      )
  # 
  
  #CDOM, Turbidity, and ChlA are already compiled into a single file
  #Download 1
  # correction_folders <- dir_folders[grepl("corrections", dir_folders)]
  # cdom_folder <- correction_folders[grepl("cDOM", correction_folders)]
  # turb_folder <- correction_folders[grepl("Turbidity", correction_folders)]
  # chla_folder <- correction_folders[grepl("Chla", correction_folders)]
  
  #CDOM, Turbidity, and ChlA are already compiled into a single file
  #Download 2, Aug 9, 2024
  correction_folders <- dir_folders[grepl("corrections", dir_folders)]
  correction_folders2 <- list.files(file.path(box_dir, "DWSC_Data_Raw", correction_folders[1]))
  correction_folders2 <- correction_folders2[which(grepl("corrections",  correction_folders2))]
  
  cdom_folder <- correction_folders2[grepl("cDOM", correction_folders2)]
  turb_folder <- correction_folders2[grepl("Turbidity", correction_folders2)]
  chla_folder <- correction_folders2[grepl("Chla", correction_folders2)]
  
  #Luke needs to fix for second deployment
  correction_list <- list()
  folder <- correction_folders2[2]
  for (folder in correction_folders2){
    # files <- list.files(file.path(box_dir, "DWSC_Data_Raw", folder)) #download 1
    files <- list.files(file.path(box_dir, "DWSC_Data_Raw", correction_folders[1], folder)) #download 2
    
    loadfile <- files[endsWith(files, "correction.xlsx")]
    
    if(grepl("Turbidity", loadfile)){sheet_name = "Corrected"}
    if(grepl("cDOM", loadfile)){sheet_name = "Raw download"}
    if(grepl("Chla", loadfile)){sheet_name = "Corrected"}
    
    cat("\n", "Processing ", paste("File =", loadfile," Sheet = ", sheet_name))
    
    df_i <- suppressMessages(readxl::read_excel(file.path(box_dir,
                                                          "DWSC_Data_Raw", 
                                                          correction_folders[1],
                                                          folder,
                                                          loadfile),
                                                sheet = sheet_name,
                                                skip = 1))
    
    df_header1_i <- suppressMessages(readxl::read_excel(file.path(box_dir,
                                                                  "DWSC_Data_Raw", 
                                                                  correction_folders[1],
                                                                  folder,
                                                                  loadfile),
                                                        sheet = sheet_name,
                                                        skip = 0)) %>%
      names()
    
    head(df_i)
    df_header1_i
    time_cols <- which(grepl("Time", df_header1_i) | grepl("GMT", df_header1_i))
    tzones_string <- df_header1_i[time_cols]
    if(length(time_cols) == 0){
      time_cols <- which(grepl("Time", names(df_i)) | grepl("GMT",  names(df_i)))
      tzones_string <- names(df_i)[time_cols]
      
    }
    
    # tzones <- ifelse(grepl("Pacific Standard", tzones_string), "Etc/GMT+7", 
    #                  ifelse(grepl("GMT-08", tzones_string), "Etc/GMT+8", 
    #                         NA))
    
    tzones <- ifelse(grepl("Pacific Standard", tzones_string), "America/Los_Angeles", 
                     ifelse(grepl("GMT-08", tzones_string), "Etc/GMT+8", 
                            NA))
    
    
    names(df_i)[time_cols] <- paste0("Datetime_", 1:length(time_cols))
    
    columns_clean <- gsub("\\..*","", df_header1_i) 
    serial_numbers <- suppressWarnings(unique(as.numeric(columns_clean)))
    serial_numbers <- serial_numbers[which(!is.na(serial_numbers))]
    
    x = serial_numbers[1]
    column_list <- lapply(serial_numbers, function(x) c(time_cols[which(serial_numbers == x)], which(columns_clean == x)))
    
    data_list <- lapply(column_list, function(l) df_i[,l])
    data_list <- lapply(data_list, function(l) setNames(l, nm =  gsub("\\..*","", names(l))))
    data_list <- lapply(data_list, function(l) setNames(l, nm =  c("Datetime", names(l)[2:ncol(l)])))
    data_list <- lapply(1:length(data_list), function(i) mutate(data_list[[i]], Datetime = as.POSIXct(Datetime, 
                                                                                                      format = "%Y-%m-%d %H:%M:%S",
                                                                                                      tz = tzones[i])))
    data_list <- lapply(data_list, function(l) mutate(l, 
                                                      Datetime_UTC = lubridate::with_tz(Datetime, "UTC")) %>%
                          select(-Datetime))
    
    names(data_list) <- serial_numbers
    
    correction_list[[which(correction_folders2 == folder)]] <- bind_rows(data_list, .id = "SerialNumber") %>%
      mutate(SerialNumber = as.numeric(SerialNumber))
    
    if(grepl("cDOM", folder)) {names(correction_list)[[which(correction_folders2 == folder)]] <- "cdom_corrected_df"}
    if(grepl("Chla", folder)) {names(correction_list)[[which(correction_folders2 == folder)]] <- "chl_corrected_df"}
    if(grepl("Turb", folder)) {names(correction_list)[[which(correction_folders2 == folder)]] <- "turb_corrected_df"}
    
  }
  
  
  #DO, temp, cond, and PAR data are with 'interval folders'
  interval_folders <- c(dir_folders[endsWith(dir_folders, "23")], 
                        dir_folders[endsWith(dir_folders, "24")],
                        dir_folders[endsWith(dir_folders, "25")])
  
  
  # outermost loop goes through deployment intervals. 
  # Typically 3-4 month deployments
  interval_folder_nu = 1
  interval_list_do <- interval_list_temp <- interval_list_par <- 
    interval_list_turb <- interval_list_chl <- interval_list_cdom <- 
    config_list <- list()
  status_df <- data.frame()
  for (interval_folder_nu in 1:length(interval_folders)){
    
    #Identify the three folders (one for for each buoy array)
    buoy_folders_i <- list.files(file.path(box_dir, 
                                           "DWSC_Data_Raw", 
                                           interval_folders[interval_folder_nu]))
    
    #Find the sensor configuration file that contains depths and locations for each sensor
    configuration_file <- buoy_folders_i[endsWith(buoy_folders_i, "configuration.xlsx")]
    
    config_i <- suppressMessages(readxl::read_excel(file.path(box_dir, 
                                                              "DWSC_Data_Raw", 
                                                              interval_folders[interval_folder_nu], 
                                                              configuration_file), skip = 1))
    
    config_i_header <- suppressMessages(names(readxl::read_excel(file.path(box_dir, 
                                                                           "DWSC_Data_Raw", 
                                                                           interval_folders[interval_folder_nu], 
                                                                           configuration_file), skip = 0)))
    
    #replace serial number with info from data
    config_i_header[grepl("\\.\\.\\.",config_i_header)] <- ""
    
    names(config_i)[which(config_i_header != "")] <- config_i_header[which(config_i_header != "")]
    
    config_i_select <- config_i %>%
      select(contains("Instrument"),  contains("Distance"), contains("Site")) %>%
      filter(!is.na(`Instrument type`)) %>%
      mutate(across(everything(), as.character)) %>%
      tidyr::pivot_longer(cols = contains("Site"), names_to = "Site", values_to = "SerialNumber") %>%
      mutate(Site = gsub("Site ", "CM", Site), 
             IntervalName = interval_folders[interval_folder_nu]) %>%
      rename("InstrumentType" = `Instrument type`,
             "Depth_cm" = `Distance below the surface (cm)`)
    
    
    # config_i_select
    
    config_list[[interval_folder_nu]] <- config_i_select
    
    rm(config_i_select)
    
    #Identify the buoy folders that contain all of the data
    buoy_folders_i <- buoy_folders_i[startsWith(buoy_folders_i, "CM")]
    
    # next loop goes through each buoy array (CM66, CM74, CM84)
    buoy_folder_nu = 1
    buoy_list_do <- buoy_list_temp <- buoy_list_par <- 
      buoy_list_turb <- buoy_list_chl <- buoy_list_cdom <- list()
    for (buoy_folder_nu in 1:length(buoy_folders_i)){
      
      #information to add to status data.frame
      BuoyName <- buoy_folders_i[buoy_folder_nu]
      IntervalName <- interval_folders[interval_folder_nu]
      
      cat("\n\n Merging", IntervalName, BuoyName, "...")
      
      
      # Identify the folders for each depth/sensor. 
      # One sensor per depth, usually four sensors on each buoy-array.
      sensor_folders_j <- list.files(file.path(box_dir, 
                                               "DWSC_Data_Raw", 
                                               interval_folders[interval_folder_nu], 
                                               buoy_folders_i[buoy_folder_nu]))
      
      #identify which folders to loop through
      DO_folders_j <- sensor_folders_j[startsWith(sensor_folders_j, "DO")]
      PAR_folders_j <- sensor_folders_j[startsWith(sensor_folders_j, "PAR")]
      Temp_folders_j <- sensor_folders_j[suppressWarnings(which(!is.na(as.numeric(sensor_folders_j))))]
      
      CDOM_folders_j <- sensor_folders_j[startsWith(sensor_folders_j, "CDOM") | startsWith(sensor_folders_j, "cDOM")]
      TURB_folders_j <- sensor_folders_j[startsWith(sensor_folders_j, "TURB")]
      CHL_folders_j <- sensor_folders_j[startsWith(sensor_folders_j, "CHL")]
      
      
      # inner loop goes through each DO sensor (serial number)
      cat("\n\n", "Dissolved Oxygen")
      
      DO_folder_nu <- 1
      DO_list <- list()
      for (DO_folder_nu in 1:length(DO_folders_j)){
        
        # Identify the file containing the concatenated data (.csv). 
        DO_files_k <- list.files(file.path(box_dir, 
                                           "DWSC_Data_Raw", 
                                           interval_folders[interval_folder_nu], 
                                           buoy_folders_i[buoy_folder_nu], 
                                           DO_folders_j[DO_folder_nu]), 
                                 recursive = TRUE)
        
        DO_files_k <- DO_files_k[endsWith(DO_files_k, "Cat.TXT")]
        
        
        #Identify sensor serial number, buoy name, and time interval
        SerialNumber <- gsub("DO_", "", DO_folders_j[DO_folder_nu])
        FileLength <- length(DO_files_k)
        
        
        
        if(length(DO_files_k) == 1){
          
          # read data
          DO_ijk <- read.table(file = file.path(box_dir, 
                                                "DWSC_Data_Raw", 
                                                interval_folders[interval_folder_nu], 
                                                buoy_folders_i[buoy_folder_nu], 
                                                DO_folders_j[DO_folder_nu], 
                                                DO_files_k), 
                               skip = 9, header = F, sep=",", stringsAsFactors = F, fill = TRUE)
          
          #Drop the last row in case data are missing
          DO_ijk <- DO_ijk[-nrow(DO_ijk),]
          
          #read in header
          DO_ijk_names <-  read.table(file = file.path(box_dir, 
                                                       "DWSC_Data_Raw", 
                                                       interval_folders[interval_folder_nu], 
                                                       buoy_folders_i[buoy_folder_nu], 
                                                       DO_folders_j[DO_folder_nu], 
                                                       DO_files_k), 
                                      skip=7, header=T, sep=",", nrow = 10)
          
          #rename data with proper column names
          names(DO_ijk) <- names(DO_ijk_names)
          
          
          #read in serial number
          DO_ijk_serial <-  read.table(file = file.path(box_dir, 
                                                        "DWSC_Data_Raw", 
                                                        interval_folders[interval_folder_nu], 
                                                        buoy_folders_i[buoy_folder_nu], 
                                                        DO_folders_j[DO_folder_nu], 
                                                        DO_files_k), 
                                       skip=0, header=F, sep=",", nrows = 6)
          
          serial_string <- DO_ijk_serial[2,]
          
          #replace serial number with info from data
          SerialNumber <- sub('.*7450-', '', serial_string)
          
          cat("\n\n", "Finished  ", paste(IntervalName, "  ", BuoyName, "  ", SerialNumber))
          
        } else {
          cat("\n\n", "No data for:  ", paste(IntervalName, "  ", BuoyName, "  ", SerialNumber))
          DO_ijk <- data.frame()
        }
        
        
        DO_ijk$SerialNumber <- rep(SerialNumber, nrow(DO_ijk))
        DO_ijk$BuoyName <- rep(BuoyName, nrow(DO_ijk))
        DO_ijk$IntervalName <- rep(IntervalName, nrow(DO_ijk))
        
        DO_list[[DO_folder_nu]] <- DO_ijk
        
        status_df <- bind_rows(status_df, 
                               data.frame(IntervalName, 
                                          BuoyName, 
                                          SerialNumber, 
                                          FileLength, 
                                          SensorType = "DO"))
        
        
      }
      
      #Step 2, loop through temperature/conductivity sensors
      # inner loop goes through each DO sensor (serial number)
      
      cat("\n\n", "Temperature/Conductivity")
      
      Temp_folder_nu <- 1
      Temp_list <- list()
      if(length(Temp_folders_j) > 0){
        for (Temp_folder_nu in 1:length(Temp_folders_j)){
          
          
          # Identify the file containing the concatenated data (.csv). 
          Temp_files_k <- list.files(file.path(box_dir, 
                                               "DWSC_Data_Raw", 
                                               interval_folders[interval_folder_nu], 
                                               buoy_folders_i[buoy_folder_nu], 
                                               Temp_folders_j[Temp_folder_nu]), 
                                     recursive = TRUE)
          
          Temp_files_k <- Temp_files_k[endsWith(Temp_files_k, ".csv")]
          
          
          #Identify sensor serial number, buoy name, and time interval
          SerialNumber <- Temp_folders_j[Temp_folder_nu]
          FileLength_Temp <- length(Temp_files_k)
          
          
          if(length(Temp_files_k) == 1){
            
            # read data
            Temp_ijk <- read.table(file = file.path(box_dir, 
                                                    "DWSC_Data_Raw", 
                                                    interval_folders[interval_folder_nu], 
                                                    buoy_folders_i[buoy_folder_nu], 
                                                    Temp_folders_j[Temp_folder_nu], 
                                                    Temp_files_k), 
                                   skip = 1, header = T, sep=",", stringsAsFactors = F)
            
            #Drop the last row in case data are missing
            Temp_ijk <- Temp_ijk[-nrow(Temp_ijk),]
            
            #rename data with proper column names
            names(Temp_ijk)[which(grepl("Temp", names(Temp_ijk)))] <- "Temp_C"
            names(Temp_ijk)[which(grepl("Full.Range", names(Temp_ijk)))] <- "SPC_uScm"
            
            #identify timezone from header
            time_col <- which(grepl("Date.Time", names(Temp_ijk)))
            time_string <- names(Temp_ijk)[time_col]
            time_string_split <- unlist(stringr::str_split(time_string, "\\."))
            
            tz_nu <- suppressWarnings(as.numeric(time_string_split[which(!is.na(as.numeric(time_string_split)))[1]]))
            tz_string <- paste0("Etc/GMT+", tz_nu)
            
            names(Temp_ijk)[time_col] <- "Datetime"
            
            #Figure out datetime format
            
            Temp_ijk <- Temp_ijk %>% 
              dplyr::select(dplyr::any_of(c("Datetime", "Temp_C", "SPC_uScm"))) %>% 
              dplyr::mutate(Datetime = as.POSIXct(Datetime,
                                                  # format = "%m/%d/%Y %I:%M:%S %p",
                                                  tryFormats = c("%m/%d/%y %I:%M:%S %p", 
                                                                 "%m/%d/%Y %I:%M:%S %p"
                                                  ),
                                                  tz = tz_string)) %>%
              dplyr::mutate(Datetime_UTC = lubridate::with_tz(Datetime, "UTC"))
            
            #read in serial number
            Temp_ijk_serial <-  read.table(file = file.path(box_dir, 
                                                            "DWSC_Data_Raw", 
                                                            interval_folders[interval_folder_nu], 
                                                            buoy_folders_i[buoy_folder_nu], 
                                                            Temp_folders_j[Temp_folder_nu], 
                                                            Temp_files_k), 
                                           skip = 0, header = F, sep=",", nrows = 1)
            
            #replace serial number with info from data
            SerialNumber <- sub('Plot Title: ', '', Temp_ijk_serial)
            
            cat("\n\n", "Finished  ", paste(IntervalName, "  ", BuoyName, "  ", SerialNumber, tz_string))
            
            
          } else {
            cat("\n\n", "No data for:  ", paste(IntervalName, "  ", BuoyName, "  ", SerialNumber))
            Temp_ijk <- data.frame()
          }
          
          
          Temp_ijk$SerialNumber <- rep(SerialNumber, nrow(Temp_ijk))
          Temp_ijk$BuoyName <- rep(BuoyName, nrow(Temp_ijk))
          Temp_ijk$IntervalName <- rep(IntervalName, nrow(Temp_ijk))
          
          Temp_list[[Temp_folder_nu]] <- Temp_ijk
          
          status_df <- bind_rows(status_df, 
                                 data.frame(IntervalName, 
                                            BuoyName, 
                                            SerialNumber, 
                                            FileLength, 
                                            SensorType = "Temp", 
                                            SensorTZ = tz_string))
          
          
        } #end temperature
      }
      
      #Step 3, loop through PAR folder
      # inner loop goes through each DO sensor (serial number)
      
      cat("\n\n", "PAR")
      
      PAR_folder_nu <- 1
      PAR_list <- list()
      for (PAR_folder_nu in 1:length(PAR_folders_j)){
        
        
        # Identify the file containing the concatenated data (.csv). 
        PAR_files_k <- list.files(file.path(box_dir, 
                                            "DWSC_Data_Raw", 
                                            interval_folders[interval_folder_nu], 
                                            buoy_folders_i[buoy_folder_nu], 
                                            PAR_folders_j[PAR_folder_nu]), 
                                  recursive = TRUE)
        
        PAR_files_k <- PAR_files_k[endsWith(PAR_files_k, "Cat.TXT")]
        
        
        #Identify sensor serial number, buoy name, and time interval
        SerialNumber <- gsub("PAR_", "", PAR_folders_j[PAR_folder_nu])
        FileLength <- length(PAR_files_k)
        
        
        
        if(length(PAR_files_k) == 1){
          
          # read data
          PAR_ijk <- read.table(file = file.path(box_dir, 
                                                 "DWSC_Data_Raw", 
                                                 interval_folders[interval_folder_nu], 
                                                 buoy_folders_i[buoy_folder_nu], 
                                                 PAR_folders_j[PAR_folder_nu], 
                                                 PAR_files_k), 
                                skip = 7, header = F, sep=",", stringsAsFactors = F)
          
          #Drop the last row in case data are missing
          PAR_ijk <- PAR_ijk[-nrow(PAR_ijk),]
          
          #read in header
          PAR_ijk_names <-  read.table(file = file.path(box_dir, 
                                                        "DWSC_Data_Raw", 
                                                        interval_folders[interval_folder_nu], 
                                                        buoy_folders_i[buoy_folder_nu], 
                                                        PAR_folders_j[PAR_folder_nu], 
                                                        PAR_files_k), 
                                       skip = 5, header=T, sep=",", nrow = 6)
          
          #rename data with proper column names
          names(PAR_ijk) <- names(PAR_ijk_names)
          
          
          #read in serial number
          PAR_ijk_serial <-  read.table(file = file.path(box_dir, 
                                                         "DWSC_Data_Raw", 
                                                         interval_folders[interval_folder_nu], 
                                                         buoy_folders_i[buoy_folder_nu], 
                                                         PAR_folders_j[PAR_folder_nu], 
                                                         PAR_files_k),
                                        skip = 0, header = F, sep=",", nrows = 2)
          
          serial_string <- PAR_ijk_serial[2,]
          
          #replace serial number with info from data
          SerialNumber <- sub('.*7530-', '', serial_string)
          
          cat("\n\n", "Finished  ", paste(IntervalName, "  ", BuoyName, "  ", SerialNumber))
          
        } else {
          cat("\n\n", "No data for:  ", paste(IntervalName, "  ", BuoyName, "  ", SerialNumber))
          PAR_ijk <- data.frame()
        }
        
        
        PAR_ijk$SerialNumber <- rep(SerialNumber, nrow(PAR_ijk))
        PAR_ijk$BuoyName <- rep(BuoyName, nrow(PAR_ijk))
        PAR_ijk$IntervalName <- rep(IntervalName, nrow(PAR_ijk))
        
        PAR_list[[PAR_folder_nu]] <- PAR_ijk
        
        status_df <- bind_rows(status_df, 
                               data.frame(IntervalName, 
                                          BuoyName, 
                                          SerialNumber, 
                                          FileLength, 
                                          SensorType = "PAR"))
        
        
        
      }  # end par
      
      
      # CDOM sensor
      cat("\n\n", "CDOM")
      
      CDOM_folder_nu <- 1
      CDOM_list <- list()
      if(length(CDOM_folders_j) > 0) {
        for (CDOM_folder_nu in 1:length(CDOM_folders_j)){
          
          # Identify the file containing the concatenated data (.csv). 
          CDOM_files_k <- list.files(file.path(box_dir, 
                                               "DWSC_Data_Raw", 
                                               interval_folders[interval_folder_nu], 
                                               buoy_folders_i[buoy_folder_nu], 
                                               CDOM_folders_j[CDOM_folder_nu]), 
                                     recursive = TRUE)
          
          CDOM_files_k <- CDOM_files_k[endsWith(CDOM_files_k, "Cat.TXT")]
          
          
          #Identify sensor serial number, buoy name, and time interval
          SerialNumber <- gsub("CDOM_", "", CDOM_folders_j[CDOM_folder_nu])
          FileLength <- length(CDOM_files_k)
          
          
          
          if(length(CDOM_files_k) == 1){
            
            sep = ","
            
            # read data
            CDOM_ijk <- read.table(file = file.path(box_dir, 
                                                    "DWSC_Data_Raw", 
                                                    interval_folders[interval_folder_nu], 
                                                    buoy_folders_i[buoy_folder_nu], 
                                                    CDOM_folders_j[CDOM_folder_nu], 
                                                    CDOM_files_k), 
                                   skip = 7, header = F, sep = sep, stringsAsFactors = F, fill = TRUE)
            
            if(ncol(CDOM_ijk)<2){ #if only one column, try changing separator   
              sep = "\t"
              
              CDOM_ijk <- read.table(file = file.path(box_dir, 
                                                      "DWSC_Data_Raw", 
                                                      interval_folders[interval_folder_nu], 
                                                      buoy_folders_i[buoy_folder_nu], 
                                                      CDOM_folders_j[CDOM_folder_nu], 
                                                      CDOM_files_k), 
                                     skip = 7, header = F, sep = sep, stringsAsFactors = F, fill = TRUE)
              
            }
            
            #Drop the last row in case data are missing
            CDOM_ijk <- CDOM_ijk[-nrow(CDOM_ijk),]
            
            #read in header
            CDOM_ijk_names <-  read.table(file = file.path(box_dir, 
                                                           "DWSC_Data_Raw", 
                                                           interval_folders[interval_folder_nu], 
                                                           buoy_folders_i[buoy_folder_nu], 
                                                           CDOM_folders_j[CDOM_folder_nu], 
                                                           CDOM_files_k), 
                                          skip=5, header=T, sep = sep, nrow = 10)
            
            #rename data with proper column names
            names(CDOM_ijk) <- names(CDOM_ijk_names)
            
            
            #read in serial number
            CDOM_ijk_serial <-  read.table(file = file.path(box_dir, 
                                                            "DWSC_Data_Raw", 
                                                            interval_folders[interval_folder_nu], 
                                                            buoy_folders_i[buoy_folder_nu], 
                                                            CDOM_folders_j[CDOM_folder_nu], 
                                                            CDOM_files_k), 
                                           skip=0, header=F, sep = sep, nrows = 2)
            
            serial_string <- CDOM_ijk_serial[2,1]
            
            #replace serial number with info from data
            SerialNumber <- sub('.*7377-', '', serial_string)
            
            cat("\n\n", "Finished  ", paste(IntervalName, "  ", BuoyName, "  ", SerialNumber))
            
          } else {
            cat("\n\n", "No data for:  ", paste(IntervalName, "  ", BuoyName, "  ", SerialNumber))
            CDOM_ijk <- data.frame()
          }
          
          
          CDOM_ijk$SerialNumber <- rep(SerialNumber, nrow(CDOM_ijk))
          CDOM_ijk$BuoyName <- rep(BuoyName, nrow(CDOM_ijk))
          CDOM_ijk$IntervalName <- rep(IntervalName, nrow(CDOM_ijk))
          
          CDOM_list[[CDOM_folder_nu]] <- CDOM_ijk
          
          status_df <- bind_rows(status_df, 
                                 data.frame(IntervalName, 
                                            BuoyName, 
                                            SerialNumber, 
                                            FileLength, 
                                            SensorType = "CDOM"))
          
          
        } # end CDOM
      }
      
      # Chlorophyll sensor
      cat("\n\n", "CHL")
      
      CHL_folder_nu <- 1
      CHL_list <- list()
      if(length(CHL_folders_j) > 0){
        for (CHL_folder_nu in 1:length(CHL_folders_j)){
          
          # Identify the file containing the concatenated data (.csv). 
          CHL_files_k <- list.files(file.path(box_dir, 
                                              "DWSC_Data_Raw", 
                                              interval_folders[interval_folder_nu], 
                                              buoy_folders_i[buoy_folder_nu], 
                                              CHL_folders_j[CHL_folder_nu]), 
                                    recursive = TRUE)
          
          CHL_files_k <- CHL_files_k[endsWith(CHL_files_k, "Cat.TXT")]
          
          
          #Identify sensor serial number, buoy name, and time interval
          SerialNumber <- gsub("CHL_", "", CHL_folders_j[CHL_folder_nu])
          FileLength <- length(CHL_files_k)
          
          
          
          if(length(CHL_files_k) == 1){
            
            sep = ","
            
            # read data
            CHL_ijk <- read.table(file = file.path(box_dir, 
                                                   "DWSC_Data_Raw", 
                                                   interval_folders[interval_folder_nu], 
                                                   buoy_folders_i[buoy_folder_nu], 
                                                   CHL_folders_j[CHL_folder_nu], 
                                                   CHL_files_k), 
                                  skip = 7, header = F, sep = sep, stringsAsFactors = F, fill = TRUE)
            
            if(ncol(CHL_ijk)<2){ #if only one column, try changing separator   
              sep = "\t"
              
              CHL_ijk <- read.table(file = file.path(box_dir, 
                                                     "DWSC_Data_Raw", 
                                                     interval_folders[interval_folder_nu], 
                                                     buoy_folders_i[buoy_folder_nu], 
                                                     CHL_folders_j[CHL_folder_nu], 
                                                     CHL_files_k), 
                                    skip = 7, header = F, sep = sep, stringsAsFactors = F, fill = TRUE)
            }
            
            #Drop the last row in case data are missing
            CHL_ijk <- CHL_ijk[-nrow(CHL_ijk),]
            
            #read in header
            CHL_ijk_names <-  read.table(file = file.path(box_dir, 
                                                          "DWSC_Data_Raw", 
                                                          interval_folders[interval_folder_nu], 
                                                          buoy_folders_i[buoy_folder_nu], 
                                                          CHL_folders_j[CHL_folder_nu], 
                                                          CHL_files_k), 
                                         skip=5, header=T, sep=sep, nrow = 10)
            
            #rename data with proper column names
            names(CHL_ijk) <- names(CHL_ijk_names)
            
            
            #read in serial number
            CHL_ijk_serial <-  read.table(file = file.path(box_dir, 
                                                           "DWSC_Data_Raw", 
                                                           interval_folders[interval_folder_nu], 
                                                           buoy_folders_i[buoy_folder_nu], 
                                                           CHL_folders_j[CHL_folder_nu], 
                                                           CHL_files_k), 
                                          skip=0, header=F, sep=sep, nrows = 3)
            
            serial_string <- CHL_ijk_serial[2,1]
            
            
            
            #replace serial number with info from data
            SerialNumber <- sub('.*7377-', '', serial_string)
            
            cat("\n\n", "Finished  ", paste(IntervalName, "  ", BuoyName, "  ", SerialNumber))
            
          } else {
            cat("\n\n", "No data for:  ", paste(IntervalName, "  ", BuoyName, "  ", SerialNumber))
            CHL_ijk <- data.frame()
          }
          
          
          CHL_ijk$SerialNumber <- rep(SerialNumber, nrow(CHL_ijk))
          CHL_ijk$BuoyName <- rep(BuoyName, nrow(CHL_ijk))
          CHL_ijk$IntervalName <- rep(IntervalName, nrow(CHL_ijk))
          
          CHL_list[[CHL_folder_nu]] <- CHL_ijk
          
          status_df <- bind_rows(status_df, 
                                 data.frame(IntervalName, 
                                            BuoyName, 
                                            SerialNumber, 
                                            FileLength, 
                                            SensorType = "CHL"))
          
          
        } # end CHL
      }
      
      # Turbidity sensor
      cat("\n\n", "Turbidity")
      
      TURB_folder_nu <- 1
      TURB_list <- list()
      if(length(TURB_folders_j) > 0){
        for (TURB_folder_nu in 1:length(TURB_folders_j)){
          
          # Identify the file containing the concatenated data (.csv). 
          TURB_files_k <- list.files(file.path(box_dir, 
                                               "DWSC_Data_Raw", 
                                               interval_folders[interval_folder_nu], 
                                               buoy_folders_i[buoy_folder_nu], 
                                               TURB_folders_j[TURB_folder_nu]), 
                                     recursive = TRUE)
          
          TURB_files_k <- TURB_files_k[endsWith(TURB_files_k, "Cat.TXT")]
          
          
          #Identify sensor serial number, buoy name, and time interval
          SerialNumber <- gsub("TURB_", "", TURB_folders_j[TURB_folder_nu])
          FileLength <- length(TURB_files_k)
          
          
          
          if(length(TURB_files_k) == 1){
            
            sep = ","
            
            # read data
            TURB_ijk <- read.table(file = file.path(box_dir, 
                                                    "DWSC_Data_Raw", 
                                                    interval_folders[interval_folder_nu], 
                                                    buoy_folders_i[buoy_folder_nu], 
                                                    TURB_folders_j[TURB_folder_nu], 
                                                    TURB_files_k), 
                                   skip = 7, header = F, sep=sep, stringsAsFactors = F, fill = TRUE)
            
            if(ncol(TURB_ijk) < 2){
              sep = "\t"
              
              TURB_ijk <- read.table(file = file.path(box_dir, 
                                                      "DWSC_Data_Raw", 
                                                      interval_folders[interval_folder_nu], 
                                                      buoy_folders_i[buoy_folder_nu], 
                                                      TURB_folders_j[TURB_folder_nu], 
                                                      TURB_files_k), 
                                     skip = 7, header = F, sep = sep, stringsAsFactors = F, fill = TRUE)
              
            }
            
            #Drop the last row in case data are missing
            TURB_ijk <- TURB_ijk[-nrow(TURB_ijk),]
            
            #read in header
            TURB_ijk_names <-  read.table(file = file.path(box_dir, 
                                                           "DWSC_Data_Raw", 
                                                           interval_folders[interval_folder_nu], 
                                                           buoy_folders_i[buoy_folder_nu], 
                                                           TURB_folders_j[TURB_folder_nu], 
                                                           TURB_files_k), 
                                          skip = 5, header = T, sep = sep, nrow = 10)
            
            #rename data with proper column names
            names(TURB_ijk) <- names(TURB_ijk_names)
            
            
            #read in serial number
            TURB_ijk_serial <-  read.table(file = file.path(box_dir, 
                                                            "DWSC_Data_Raw", 
                                                            interval_folders[interval_folder_nu], 
                                                            buoy_folders_i[buoy_folder_nu], 
                                                            TURB_folders_j[TURB_folder_nu], 
                                                            TURB_files_k), 
                                           skip=0, header=F, sep=",", nrows = 3)
            
            serial_string <- TURB_ijk_serial[2,1]
            
            #replace serial number with info from data
            SerialNumber <- sub('.*7377-', '', serial_string)
            
            cat("\n\n", "Finished  ", paste(IntervalName, "  ", BuoyName, "  ", SerialNumber))
            
          } else {
            cat("\n\n", "No data for:  ", paste(IntervalName, "  ", BuoyName, "  ", SerialNumber))
            TURB_ijk <- data.frame()
          }
          
          
          TURB_ijk$SerialNumber <- rep(SerialNumber, nrow(TURB_ijk))
          TURB_ijk$BuoyName <- rep(BuoyName, nrow(TURB_ijk))
          TURB_ijk$IntervalName <- rep(IntervalName, nrow(TURB_ijk))
          
          TURB_list[[TURB_folder_nu]] <- TURB_ijk
          
          status_df <- bind_rows(status_df, 
                                 data.frame(IntervalName, 
                                            BuoyName, 
                                            SerialNumber, 
                                            FileLength, 
                                            SensorType = "TURB"))
          
          
        } # end Turbidity
      }
      
      #Join all sensors together for each buoy
      buoy_list_do[[buoy_folder_nu]] <- dplyr::bind_rows(DO_list)
      buoy_list_temp[[buoy_folder_nu]] <- dplyr::bind_rows(Temp_list)
      buoy_list_par[[buoy_folder_nu]] <- dplyr::bind_rows(PAR_list)
      buoy_list_turb[[buoy_folder_nu]] <- dplyr::bind_rows(TURB_list)
      buoy_list_chl[[buoy_folder_nu]] <- dplyr::bind_rows(CHL_list)
      buoy_list_cdom[[buoy_folder_nu]] <- dplyr::bind_rows(CDOM_list)
      
    }
    
    #Join all buoys together for each interval
    interval_list_do[[interval_folder_nu]] <- dplyr::bind_rows(buoy_list_do)
    interval_list_temp[[interval_folder_nu]] <- dplyr::bind_rows(buoy_list_temp)
    interval_list_par[[interval_folder_nu]] <- dplyr::bind_rows(buoy_list_par)
    interval_list_turb[[interval_folder_nu]] <- dplyr::bind_rows(buoy_list_turb)
    interval_list_chl[[interval_folder_nu]] <- dplyr::bind_rows(buoy_list_chl)
    interval_list_cdom[[interval_folder_nu]] <- dplyr::bind_rows(buoy_list_cdom)
    
  }
  
  #Join each interval into a data.frames. One for each sensor type
  #Mutate column names
  #Join deployment information
  
  config_df <- dplyr::bind_rows(config_list) %>%
    mutate(Depth_cm = stringr::str_pad(Depth_cm, 3, pad = "0"), 
           SerialNumber = suppressWarnings(as.numeric(SerialNumber)))
  
  DO_df <- dplyr::bind_rows(interval_list_do) %>%
    dplyr::select(SerialNumber, BuoyName, IntervalName, 
                  UTC_Date_._Time, Temperature, Dissolved.Oxygen, Dissolved.Oxygen.Saturation) %>%
    dplyr::rename(Datetime_UTC = UTC_Date_._Time,
                  Temp_C = Temperature, 
                  DO_mgL = Dissolved.Oxygen,
                  DO_PerSat = Dissolved.Oxygen.Saturation) %>%
    dplyr::mutate(Datetime_UTC = as.POSIXct(Datetime_UTC, tz="UTC", format = "%Y-%m-%d %H:%M:%S"), 
                  SerialNumber = suppressWarnings(as.numeric(SerialNumber))) %>%
    left_join(select(config_df, Depth_cm, Site, SerialNumber, IntervalName), 
              relationship = "many-to-one", by = c("SerialNumber", "IntervalName")) %>%
    left_join(select(deployment_details, IntervalName, DateTime_UTC_Start, DateTime_UTC_End), 
              relationship = "many-to-one", by = c("IntervalName")) %>%
    mutate(inwater = ifelse(Datetime_UTC > DateTime_UTC_Start & 
                              Datetime_UTC < DateTime_UTC_End, 
                            TRUE, 
                            FALSE)) %>%
    distinct()
  
  
  temp_df <- dplyr::bind_rows(interval_list_temp) %>%
    dplyr::select(SerialNumber, BuoyName, IntervalName, 
                  Datetime_UTC, Temp_C, SPC_uScm)  %>% 
    dplyr::mutate(SerialNumber = suppressWarnings(as.numeric(SerialNumber))) %>%
    left_join(select(config_df, Depth_cm, Site, SerialNumber, IntervalName), 
              relationship = "many-to-one", by = c("SerialNumber", "IntervalName")) %>%
    left_join(select(deployment_details, IntervalName, DateTime_UTC_Start, DateTime_UTC_End), 
              relationship = "many-to-one", by = c("IntervalName")) %>%
    mutate(inwater = ifelse(Datetime_UTC > DateTime_UTC_Start & 
                              Datetime_UTC < DateTime_UTC_End, 
                            TRUE, 
                            FALSE)) %>%
    distinct()
  
  par_df <- dplyr::bind_rows(interval_list_par) %>%
    dplyr::select(SerialNumber, BuoyName, IntervalName, 
                  UTC_Date_._Time, Temperature, PAR, 
                  Acceleration.X, Acceleration.Y, Acceleration.Z) %>%
    dplyr::rename(Datetime_UTC = UTC_Date_._Time,
                  Temp_C = Temperature, 
                  PAR_uM_per_s_per_m2 = PAR,
                  Acceleration_X = Acceleration.X, 
                  Acceleration_Y = Acceleration.Y, 
                  Acceleration_Z = Acceleration.Z) %>%
    dplyr::mutate(Datetime_UTC = as.POSIXct(Datetime_UTC, tz="UTC", format = "%Y-%m-%d %H:%M:%S"),
                  SerialNumber = suppressWarnings(as.numeric(SerialNumber))) %>%
    left_join(select(config_df, Depth_cm, Site, SerialNumber, IntervalName), 
              relationship = "many-to-one", by = c("SerialNumber", "IntervalName")) %>%
    left_join(select(deployment_details, IntervalName, DateTime_UTC_Start, DateTime_UTC_End), 
              relationship = "many-to-one", by = c("IntervalName")) %>%
    mutate(inwater = ifelse(Datetime_UTC > DateTime_UTC_Start & 
                              Datetime_UTC < DateTime_UTC_End, 
                            TRUE, 
                            FALSE)) %>%
    distinct()
  
  cdom_df <- dplyr::bind_rows(interval_list_cdom) %>%
    dplyr::select(SerialNumber, BuoyName, IntervalName, 
                  UTC_Date_._Time, Temperature, Sensor, Gain) %>%
    dplyr::rename(Datetime_UTC = UTC_Date_._Time,
                  Temp_C = Temperature, 
                  CDOM_ppb = Sensor) %>%
    dplyr::mutate(Datetime_UTC = as.POSIXct(Datetime_UTC, tz="UTC", format = "%Y-%m-%d %H:%M:%S"), 
                  SerialNumber = suppressWarnings(as.numeric(SerialNumber))) %>%
    left_join(select(config_df, Depth_cm, Site, SerialNumber, IntervalName), 
              relationship = "many-to-one", by = c("SerialNumber", "IntervalName")) %>%
    left_join(select(deployment_details, IntervalName, DateTime_UTC_Start, DateTime_UTC_End), 
              relationship = "many-to-one", by = c("IntervalName")) %>%
    mutate(inwater = ifelse(Datetime_UTC > DateTime_UTC_Start & 
                              Datetime_UTC < DateTime_UTC_End, 
                            TRUE, 
                            FALSE)) %>%
    distinct()
  
  
  chl_df <- dplyr::bind_rows(interval_list_chl) %>%
    dplyr::select(SerialNumber, BuoyName, IntervalName, 
                  UTC_Date_._Time, Temperature, Sensor, Gain) %>%
    dplyr::rename(Datetime_UTC = UTC_Date_._Time,
                  Temp_C = Temperature, 
                  chl_ugL = Sensor) %>%
    dplyr::mutate(Datetime_UTC = as.POSIXct(Datetime_UTC, tz="UTC", format = "%Y-%m-%d %H:%M:%S"), 
                  SerialNumber = suppressWarnings(as.numeric(SerialNumber))) %>%
    left_join(select(config_df, Depth_cm, Site, SerialNumber, IntervalName), 
              relationship = "many-to-one", by = c("SerialNumber", "IntervalName")) %>%
    left_join(select(deployment_details, IntervalName, DateTime_UTC_Start, DateTime_UTC_End), 
              relationship = "many-to-one", by = c("IntervalName")) %>%
    mutate(inwater = ifelse(Datetime_UTC > DateTime_UTC_Start & 
                              Datetime_UTC < DateTime_UTC_End, 
                            TRUE, 
                            FALSE)) %>%
    distinct()
  
  turb_df <- dplyr::bind_rows(interval_list_turb) %>%
    dplyr::select(SerialNumber, BuoyName, IntervalName, 
                  UTC_Date_._Time, Temperature, Sensor, Gain) %>%
    dplyr::rename(Datetime_UTC = UTC_Date_._Time,
                  Temp_C = Temperature, 
                  turb_NTU = Sensor) %>%
    dplyr::mutate(Datetime_UTC = as.POSIXct(Datetime_UTC, tz="UTC", format = "%Y-%m-%d %H:%M:%S"), 
                  SerialNumber = suppressWarnings(as.numeric(SerialNumber))) %>%
    left_join(select(config_df, Depth_cm, Site, SerialNumber, IntervalName), 
              relationship = "many-to-one", by = c("SerialNumber", "IntervalName")) %>%
    left_join(select(deployment_details, IntervalName, DateTime_UTC_Start, DateTime_UTC_End), 
              relationship = "many-to-one", by = c("IntervalName")) %>%
    mutate(inwater = ifelse(Datetime_UTC > DateTime_UTC_Start & 
                              Datetime_UTC < DateTime_UTC_End, 
                            TRUE, 
                            FALSE)) %>%
    distinct()
  
  
  #Upload status. Did all of the sensors have a file? 
  status_df <- status_df %>%
    mutate(SerialNumber = suppressWarnings(as.numeric(SerialNumber))) %>%
    left_join(select(config_df, Depth_cm, Site, SerialNumber, IntervalName), 
              relationship = "many-to-one")
  
  cat("\n\n", "Missing data for the following: ", "\n")
  
  print(filter(status_df, FileLength != 1))
  
  
  #Join things in a list for returning
  list_out <- list(deployment_details, config_df, status_df, 
                   DO_df, temp_df, par_df, 
                   cdom_df, chl_df, turb_df)
  
  names(list_out) <- c("deployment_df", "config_df", "status_df", 
                       "DO_df", "temp_df", "par_df", 
                       "cdom_df", "chl_df", "turb_df")
  
  # add corrected data.frames
  if(length(correction_list) > 0){
    list_out[(length(list_out) + 1):(length(list_out) + length(correction_list))] <- correction_list[1:length(correction_list)]
    
    names(list_out)[(length(list_out) - length(correction_list) + 1) :length(list_out)] <- names(correction_list)
  }
  
  #combine data tables
  #Merge 'corrected data' with 'raw data
  chla_cor_prep <- list_out$chl_corrected_df %>%
    dplyr::select(Datetime_UTC, 
                  SerialNumber, 
                  Chl_gain_corrected = Gain,
                  Temp_C_corrected = `(deg C)`, 
                  Chl_ugL_corrected = Corrected, 
                  Chl_ugL_raw = ppb, 
                  Chl_volts = Volts, 
                  Chl_bat_volts = `Bat volt`)   %>%
    filter(!is.na(Datetime_UTC)) %>%
    arrange(SerialNumber, Datetime_UTC)
  
  
  chla_merge <- full_join(list_out$chl_df, chla_cor_prep) %>%
    filter(!is.na(Datetime_UTC)) %>%
    arrange(BuoyName, Datetime_UTC)
  
  ggplot(chla_merge) +
    geom_boxplot(aes(y = Temp_C - Temp_C_corrected, 
                     x = as.character(SerialNumber)))
  
  ggplot(chla_merge %>% 
           slice_sample(prop = 0.01)) +
    geom_point(aes(y = Temp_C - Temp_C_corrected, 
                   x = Datetime_UTC)) +
    facet_wrap(~SerialNumber, ncol = 1)
  
  
  ggplot(chla_merge) +
    geom_hline(yintercept = 0.1) + 
    geom_boxplot(aes(y = 0.1 + abs(chl_ugL  - Chl_ugL_raw ), 
                     x = as.character(SerialNumber))) +
    scale_y_log10()
  
  
  turb_cor_prep <- list_out$turb_corrected_df %>%
    dplyr::select(Datetime_UTC, 
                  SerialNumber, 
                  Turb_gain_corrected = Gain,
                  Temp_C_corrected = `(deg C)`, 
                  Turb_NTU_corrected = `Corrected Turbidity`, 
                  Turb_NTU_raw = `( NTU)`, 
                  Turb_volts = Voltage, 
                  Turb_bat_volts = `Bat volt`)
  
  
  turb_merge <- full_join(list_out$turb_df, turb_cor_prep) %>%
    filter(!is.na(Datetime_UTC)) %>%
    arrange(BuoyName, Datetime_UTC)
  
  ggplot(turb_merge) +
    geom_boxplot(aes(y = Temp_C - Temp_C_corrected, 
                     x = as.character(SerialNumber)))
  
  ggplot(turb_merge %>% 
           slice_sample(prop = 0.01)) +
    geom_point(aes(y = Temp_C - Temp_C_corrected, 
                   x = Datetime_UTC)) +
    facet_wrap(~SerialNumber, ncol = 1)
  
  
  ggplot(turb_merge) +
    geom_hline(yintercept = 0.1) + 
    geom_boxplot(aes(y = 0.1 + abs(turb_NTU - Turb_NTU_raw), 
                     x = as.character(SerialNumber))) +
    scale_y_log10()
  
  
  #merge temperature records
  temp_prep <- list_out$temp_df %>%
    dplyr::select(SerialNumber, BuoyName, Site, Depth_cm, 
                  IntervalName, Datetime_UTC, inwater, 
                  Temp_C, SPC_uScm) %>%
    dplyr::mutate(SensorType = "TEMP/SPC")
  
  do_prep <- list_out$DO_df %>%
    dplyr::select(SerialNumber, BuoyName, Site, Depth_cm, 
                  IntervalName, Datetime_UTC, inwater, 
                  Temp_C, DO_mgL, DO_PerSat) %>%
    dplyr::mutate(SensorType = "DO")
  
  par_prep <- list_out$par_df %>%
    dplyr::select(SerialNumber, BuoyName, Site, Depth_cm, 
                  IntervalName, Datetime_UTC, inwater, 
                  Temp_C, PAR_uM_per_s_per_m2) %>%
    dplyr::mutate(SensorType = "PAR")
  
  cdom_prep <- list_out$cdom_df %>%
    dplyr::select(SerialNumber, BuoyName, Site, Depth_cm, 
                  IntervalName, Datetime_UTC, inwater, 
                  Temp_C, CDOM_ppb, CDOM_gain = Gain) %>%
    dplyr::mutate(SensorType = "CDOM")
  
  # chl_prep <- list_out$chl_df %>%
  #   dplyr::select(SerialNumber, BuoyName, Site, Depth_cm, 
  #                 IntervalName, Datetime_UTC, inwater, 
  #                 Temp_C, Chl_ugL = chl_ugL, Chl_gain = Gain) %>%
  #   dplyr::mutate(SensorType = "CHL")
  # 
  chl_prep <- chla_merge %>%
    dplyr::select(SerialNumber, BuoyName, Site, Depth_cm, 
                  IntervalName, Datetime_UTC, inwater, 
                  Temp_C, Chl_ugL = chl_ugL, Chl_gain = Gain, 
                  Chl_gain_corrected, Temp_C_corrected, Chl_ugL_corrected,
                  Chl_ugL_raw, Chl_volts, Chl_bat_volts) %>%
    dplyr::mutate(SensorType = "CHL")
  
  
  # turb_prep <- list_out$turb_df %>%
  #   dplyr::select(SerialNumber, BuoyName, Site, Depth_cm, 
  #                 IntervalName, Datetime_UTC, inwater) %>%
  #   dplyr::mutate(SensorType = "TURB")
  # 
  turb_prep <- turb_merge %>%
    dplyr::select(SerialNumber, BuoyName, Site, Depth_cm, 
                  IntervalName, Datetime_UTC, inwater, 
                  Temp_C, Turb_NTU = turb_NTU, Turb_gain = Gain, 
                  Turb_gain_corrected, Temp_C_corrected, Turb_NTU_corrected,
                  Turb_NTU_raw, Turb_volts, Turb_bat_volts) %>%
    dplyr::mutate(SensorType = "TURB")
  
  
  sensor_merge_df <- temp_prep %>%
    full_join(do_prep) %>%
    full_join(par_prep) %>%
    full_join(cdom_prep) %>%
    full_join(chl_prep) %>%
    full_join(turb_prep) %>%
    distinct() %>%
    arrange(Site, Depth_cm, SensorType, Datetime_UTC)
  
  list_out$sensor_merge_df <- sensor_merge_df
  
  #Save to file
  if (save_to_file){
    
    cat("\n\n", "Saving files in:", "\n", file.path(box_dir, "DWSC_Data_Processed"))
    
    opendir <- function(dir = getwd()){
      if (.Platform['OS.type'] == "windows"){
        shell.exec(dir)
      } else {
        system(paste(Sys.getenv("R_BROWSER"), dir))
      }
    }
    shell.exec(file.path(box_dir, "DWSC_Data_Processed"))
    
    utils::browseURL(file.path(box_dir, "DWSC_Data_Processed"))
    
    #DO
    write.table(DO_df, file = file.path(box_dir, 
                                        "DWSC_Data_Processed",
                                        'Buoy_DO_raw.csv'), 
                row.names=F, sep=',')
    
    saveRDS(DO_df, file = file.path(box_dir,
                                    "DWSC_Data_Processed",
                                    'Buoy_DO_raw.rds'))
    #temp
    write.table(temp_df, 
                file = file.path(box_dir,
                                 "DWSC_Data_Processed",
                                 'Buoy_Temp_raw.csv'), 
                row.names=F, sep=',')
    
    saveRDS(temp_df, file = file.path(box_dir, 
                                      "DWSC_Data_Processed", 
                                      'Buoy_Temp_raw.rds'))
    #par
    write.table(par_df, file = file.path(box_dir, 
                                         "DWSC_Data_Processed",
                                         'Buoy_PAR_raw.csv'), 
                row.names=F, sep=',')
    
    saveRDS(par_df, file = file.path(box_dir, 
                                     "DWSC_Data_Processed", 
                                     'Buoy_PAR_raw.rds'))
    #cdom
    write.table(cdom_df, file = file.path(box_dir, 
                                          "DWSC_Data_Processed",
                                          'Buoy_CDOM_raw.csv'), 
                row.names=F, sep=',')
    
    saveRDS(cdom_df, file = file.path(box_dir, 
                                      "DWSC_Data_Processed", 
                                      'Buoy_CDOM_raw.rds'))
    #chl
    write.table(chl_df, file = file.path(box_dir, 
                                         "DWSC_Data_Processed",
                                         'Buoy_CHL_raw.csv'), 
                row.names=F, sep=',')
    
    saveRDS(chl_df, file = file.path(box_dir, 
                                     "DWSC_Data_Processed", 
                                     'Buoy_CHL_raw.rds'))
    #turb
    write.table(turb_df, file = file.path(box_dir, 
                                          "DWSC_Data_Processed",
                                          'Buoy_Turb_raw.csv'), 
                row.names=F, sep=',')
    
    saveRDS(turb_df, file = file.path(box_dir, 
                                      "DWSC_Data_Processed", 
                                      'Buoy_Turb_raw.rds'))
    
    #all sensors
    write.table(sensor_merge_df, file = file.path(box_dir, 
                                                  "DWSC_Data_Processed",
                                                  'Buoy_AllSensors_raw.csv'), 
                row.names=F, sep=',')
    
    saveRDS(sensor_merge_df, file = file.path(box_dir, 
                                              "DWSC_Data_Processed", 
                                              'Buoy_AllSensors_raw.rds'))
    
    
    #everything
    saveRDS(list_out, file = file.path(box_dir, 
                                       "DWSC_Data_Processed",
                                       'Buoy_Sensor_List.rds'))
    
  }
  
  return(list_out) # End function 1
}



