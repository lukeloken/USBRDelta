

# box_dir = "C:/Users/lloken/OneDrive - DOI/USBR_DWSC/CopyFromLokenComputer/DeepWaterShipChannel/DeepWaterShipChannel"
# box_dir = "C:/Users/lloken/OneDrive - DOI/USBR_DWSC/CopyFromLokenComputer/DeepWaterShipChannel_2024-08-09/DeepWaterShipChannel"
# box_dir = "C:/Users/lloken/OneDrive - DOI/USBR_DWSC/CopyFromLokenComputer/DeepWaterShipChannel_2024-08-12/DeepWaterShipChannel" 
# box_dir = "C:/Users/lloken/OneDrive - DOI/USBR_DWSC/CopyFromLokenComputer/DeepWaterShipChannel_2024-11-01/DeepWaterShipChannel"



# df_list <- readRDS(file.path(box_dir, "DWSC_Data_Processed", "Buoy_Sensor_List.rds"))


PlotBuoySensorData <- function(df_list, save_to_file = FALSE, box_dir = NULL){
  
  if(save_to_file){
    if(is.null(box_dir)){
      warning("Save to file indicated, but no box_dir specified. No images will be saved.")
      save_to_file = FALSE
    }
  }
  
  #Empty objects to fill with figures and plotting information
  fig_list <- list() #save all figures in a list
  fig_details_df <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(fig_details_df) <- c("name", "width", "height")
  
  
  #quick look at data
  
  names(df_list)
  data.frame(df_list[[1]]) #deployment_df
  head(df_list[[2]]) #config_df
  head(df_list[[3]]) #status_df
  head(df_list[[4]]) #DO_df
  head(df_list[[5]]) #temp_df
  head(df_list[[6]]) #par_df
  head(df_list[[7]]) #cdom_df
  head(df_list[[8]]) #chl_df
  head(df_list[[9]]) #turb_df
  head(df_list[[10]]) #cdom_corrected_df 
  head(df_list[[11]]) #chl_corrected_df  
  head(df_list[[12]]) #turb_corrected_df
  head(df_list[[13]]) #sensor_merge_df
  unique(df_list[[13]]$IntervalName)
  summary(df_list[[13]]$Datetime_UTC)
  

  #data for polygon during non-deployments  
  poly_start <- as.POSIXct(c(-Inf, df_list[[1]]$DateTime_UTC_End), tz = "UTC")
  poly_end <- as.POSIXct(c(df_list[[1]]$DateTime_UTC_Start, Inf))
  
  poly <- data.frame(poly_start, poly_end)
  

  
  #summaries
  summary_DO <- df_list$DO_df %>%
    dplyr::select(SerialNumber, BuoyName, IntervalName) %>%
    arrange(BuoyName, IntervalName, SerialNumber)
  
  
  
  #Visualization
  theme_timeseries_common <- list(theme_bw(), 
                                  scale_x_datetime(date_labels = "%b%e\n%Y", 
                                                   date_breaks = "year"), 
                                  theme(strip.background = element_rect(fill = NA, color = NA), 
                                        panel.grid.minor = element_blank()))
  
  
  # #First go at figures
  # #by serial number
  # ggplot(sample_frac(filter(df_list$temp_df, 
  #                           !is.na(SPC_uScm), 
  #                           # inwater
  # ), 
  # size = 0.01)) +
  #   geom_point(aes(x = Datetime_UTC, y = SPC_uScm)) +
  #   facet_wrap(~SerialNumber, nrow = 3) +
  #   theme_timeseries_common
  # 
  # ggplot(sample_frac(filter(df_list$temp_df, 
  #                           !is.na(Temp_C), 
  #                           inwater), 
  #                    size = 0.01)) +
  #   geom_point(aes(x = Datetime_UTC, y = Temp_C)) +
  #   facet_wrap(~SerialNumber, nrow = 3) +
  #   theme_timeseries_common
  # 
  # ggplot(sample_frac(filter(df_list$DO_df, 
  #                           !is.na(DO_mgL), 
  #                           inwater), 
  #                    size = .01)) +
  #   geom_point(aes(x = Datetime_UTC, y = DO_mgL)) +
  #   facet_wrap(~SerialNumber, nrow = 3) +
  #   theme_timeseries_common
  # 
  # ggplot(sample_frac(filter(df_list$par_df, 
  #                           !is.na(PAR_uM_per_s_per_m2), 
  #                           inwater), 
  #                    size = .01)) +
  #   geom_point(aes(x = Datetime_UTC, y = PAR_uM_per_s_per_m2)) +
  #   facet_wrap(~SerialNumber, nrow = 3) +
  #   theme_timeseries_common
  # 
  # ggplot(sample_frac(filter(df_list$cdom_df, 
  #                           !is.na(CDOM_ppb ), 
  #                           inwater), 
  #                    size = .01)) +
  #   geom_point(aes(x = Datetime_UTC, y = CDOM_ppb )) +
  #   facet_wrap(~SerialNumber, nrow = 3) +
  #   theme_timeseries_common
  # 
  # ggplot(sample_frac(filter(df_list$turb_df, 
  #                           !is.na(turb_NTU  ), 
  #                           inwater), 
  #                    size = .01)) +
  #   geom_point(aes(x = Datetime_UTC, y = turb_NTU  )) +
  #   facet_wrap(~SerialNumber, nrow = 3) +
  #   theme_timeseries_common
  # 
  # ggplot(sample_frac(filter(df_list$chl_df, 
  #                           !is.na(chl_ugL), 
  #                           inwater), 
  #                    size = .01)) +
  #   geom_point(aes(x = Datetime_UTC, y = chl_ugL)) +
  #   facet_wrap(~SerialNumber, nrow = 3) +
  #   theme_timeseries_common
  # 
  # #by site
  # 
  # ggplot(sample_frac(filter(df_list$temp_df, 
  #                           !is.na(SPC_uScm), 
  #                           inwater), 
  #                    size = 0.01)) +
  #   geom_point(aes(x = Datetime_UTC, y = SPC_uScm, color = Depth_cm)) +
  #   facet_wrap(~Site, nrow = 3) +
  #   theme_timeseries_common
  # 
  # ggplot(sample_frac(filter(df_list$temp_df, 
  #                           !is.na(Temp_C), 
  #                           inwater), 
  #                    size = 0.01)) +
  #   geom_point(aes(x = Datetime_UTC, y = Temp_C, color = Depth_cm)) +
  #   facet_wrap(~Site, nrow = 3) +
  #   theme_timeseries_common
  # 
  # ggplot(sample_frac(filter(df_list$DO_df, 
  #                           !is.na(DO_mgL), 
  #                           inwater), 
  #                    size = .01)) +
  #   geom_point(aes(x = Datetime_UTC, y = DO_mgL, color = Depth_cm)) +
  #   facet_wrap(~Site, nrow = 3) +
  #   theme_timeseries_common
  # 
  # ggplot(sample_frac(filter(df_list$par_df, 
  #                           !is.na(PAR_uM_per_s_per_m2), 
  #                           inwater), 
  #                    size = .01)) +
  #   geom_point(aes(x = Datetime_UTC, y = PAR_uM_per_s_per_m2, color = Depth_cm)) +
  #   facet_wrap(~Site, nrow = 3) +
  #   theme_timeseries_common
  # 
  # ggplot(sample_frac(filter(df_list$chl_df, 
  #                           !is.na(chl_ugL), 
  #                           inwater), 
  #                    size = .01)) +
  #   geom_point(aes(x = Datetime_UTC, y = chl_ugL, color = Depth_cm)) +
  #   facet_wrap(~Site, nrow = 3) +
  #   theme_timeseries_common
  # 
  # ggplot(sample_frac(filter(df_list$cdom_df, 
  #                           !is.na(CDOM_ppb), 
  #                           inwater), 
  #                    size = .01)) +
  #   geom_point(aes(x = Datetime_UTC, y = CDOM_ppb, color = Depth_cm)) +
  #   facet_wrap(~Site, nrow = 3) +
  #   theme_timeseries_common
  # 
  # ggplot(sample_frac(filter(df_list$turb_df, 
  #                           !is.na(turb_NTU), 
  #                           inwater), 
  #                    size = .01)) +
  #   geom_point(aes(x = Datetime_UTC, y = turb_NTU, color = Depth_cm)) +
  #   facet_wrap(~Site, nrow = 3) +
  #   theme_timeseries_common +
  #   scale_y_sqrt()
  
  # better figures with start/end
  
  p_do1 <- ggplot(sample_frac(filter(df_list$DO_df, 
                                     !is.na(DO_mgL), 
                                     inwater), 
                              size = 1), 
                  aes(x = Datetime_UTC, 
                      y = DO_mgL, 
                      color = Depth_cm)) +
    geom_vline(xintercept = df_list$deployment_df$DateTime_UTC_Start, 
               linetype = "dashed", color = "green", linewidth = 0.25) +
    geom_vline(xintercept = df_list$deployment_df$DateTime_UTC_End, 
               linetype = "dashed", color = "red", linewidth = 0.25) + 
    geom_point(size = .5) +
    theme_timeseries_common + 
    facet_grid(vars(Depth_cm), vars(BuoyName)) +
    scale_color_brewer(palette = "RdYlBu")
  
  # print(p_do1)
  
  if(exists("p_do1")){
    fig_list[[length(fig_list) + 1]] <- p_do1
    fig_details_df[length(fig_list), "name"] <- c("All_DO_Data.png")
    fig_details_df[length(fig_list), c("width", "height")] <- c(10,6)
  }
  
  # date_test <- as.POSIXct(c("2024-06-30", "2024-07-31"))
  p_temp <- ggplot(sample_frac(filter(df_list$temp_df, 
                                      !is.na(Temp_C), 
                                      # Datetime_UTC > date_test[1], 
                                      # Datetime_UTC < date_test[2], 
                                      
                                      inwater), 
                               size = .01), 
                   aes(x = Datetime_UTC, 
                       y = Temp_C, 
                       color = Depth_cm,
                       group = IntervalName)) +
    geom_vline(xintercept = df_list$deployment_df$DateTime_UTC_Start, 
               linetype = "dashed", color = "green", linewidth = 0.25) +
    geom_vline(xintercept = df_list$deployment_df$DateTime_UTC_End, 
               linetype = "dashed", color = "red", linewidth = 0.25) + 
    # geom_point(size = .5) + 
    geom_line(linewidth = 0.5) +
    theme_timeseries_common + 
    facet_grid(rows = vars(forcats::fct_rev(BuoyName))) +
    scale_color_brewer(palette = "RdYlBu")
  
  # print(p_temp)
  if(exists("p_temp")){
    fig_list[[length(fig_list) + 1]] <- p_temp
    fig_details_df[length(fig_list), "name"] <- c("All_Temp_Data.png")
    fig_details_df[length(fig_list), c("width", "height")] <- c(10,6)
  }
  
  p_temp2 <- ggplot(sample_frac(filter(df_list$temp_df, 
                                       !is.na(Temp_C), 
                                       inwater), 
                                size = 1), 
                    aes(x = Datetime_UTC, 
                        y = Temp_C, 
                        # color = Depth_cm,
                        group = IntervalName)) +
    geom_vline(xintercept = df_list$deployment_df$DateTime_UTC_Start, 
               linetype = "dashed", color = "green", linewidth = 0.25) +
    geom_vline(xintercept = df_list$deployment_df$DateTime_UTC_End, 
               linetype = "dashed", color = "red", linewidth = 0.25) + 
    # geom_point(size = .5) + 
    geom_line(linewidth = 0.5) +
    theme_timeseries_common + 
    facet_grid(vars(Depth_cm), vars(BuoyName)) +
    scale_color_brewer(palette = "RdYlBu")
  
  # print(p_temp2)
  if(exists("p_temp2")){
    fig_list[[length(fig_list) + 1]] <- p_temp2
    fig_details_df[length(fig_list), "name"] <- c("All_Temp_Data_v2.png")
    fig_details_df[length(fig_list), c("width", "height")] <- c(10,6)
  }
  
  # 
  # #test delete between
  # test_df <- df_list$temp_df %>%
  #   filter(IntervalName == "07_AUG24_DEC24", 
  #          SerialNumber == "20539692") %>%
  #   mutate(Site = "CM74", 
  #          Depth_cm = "150") %>%
  #   bind_rows(df_list$temp_df)
  # 
  # test <- ggplot(sample_frac(filter(test_df, 
  #                                   !is.na(Temp_C), 
  #                                   inwater, 
  #                                   IntervalName == "07_AUG24_DEC24", 
  #                                   month(Datetime_UTC) == "9"
  # ), 
  # size = 1), 
  # aes(x = Datetime_UTC, 
  #     y = Temp_C, 
  #     # color = Depth_cm,
  #     group = IntervalName)) +
  #   geom_vline(xintercept = df_list$deployment_df$DateTime_UTC_Start, 
  #              linetype = "dashed", color = "green", linewidth = 0.25) +
  #   geom_vline(xintercept = df_list$deployment_df$DateTime_UTC_End, 
  #              linetype = "dashed", color = "red", linewidth = 0.25) + 
  #   # geom_point(size = .5) + 
  #   geom_line(linewidth = 0.5) +
  #   theme_timeseries_common + 
  #   facet_grid(vars(Depth_cm), vars(Site)) +
  #   scale_color_brewer(palette = "RdYlBu") +
  #   geom_path(data = filter(test_df, 
  #                           SerialNumber == "20539692", 
  #                           IntervalName == "07_AUG24_DEC24", 
  #                           month(Datetime_UTC) == "9")
  #             , color = "red")
  # 
  # print(test)
  # 
  # #end test
  
  #Merged temp
  temp_subset <- filter(df_list[[13]], 
                        SensorType %in% c("PAR", "DO", "TEMP/SPC")) %>%
    filter(SensorType %in% c("PAR", "DO") | Depth_cm != "050") %>%
    arrange(BuoyName, desc(Depth_cm), Datetime_UTC)
  
  
  p_temp_heat <- ggplot(sample_frac(filter(temp_subset, 
                                           !is.na(Temp_C), 
                                           inwater), 
                                    size = 1), 
                        aes(x = Datetime_UTC, 
                            y = as.numeric(Depth_cm), 
                            fill = Temp_C,
                            group = IntervalName)) +
    geom_vline(xintercept = df_list$deployment_df$DateTime_UTC_Start, 
               linetype = "dashed", color = "green", linewidth = 0.25) +
    geom_vline(xintercept = df_list$deployment_df$DateTime_UTC_End, 
               linetype = "dashed", color = "red", linewidth = 0.25) + 
    # geom_point(size = .5) + 
    geom_tile() +
    theme_timeseries_common + 
    labs(y = "Depth (cm)") + 
    facet_grid(rows = vars(forcats::fct_rev(BuoyName))) +
    scale_fill_distiller(palette = "RdYlBu") +
    scale_y_reverse()
  
  # print(p_temp_heat)
  
  if(exists("p_temp_heat")){
    fig_list[[length(fig_list) + 1]] <- p_temp_heat
    fig_details_df[length(fig_list), "name"] <- c("All_Temp_heat.png")
    fig_details_df[length(fig_list), c("width", "height")] <- c(10,6)
  }
  
  
  p_temp3 <- ggplot(sample_frac(filter(temp_subset, 
                                       !is.na(Temp_C), 
                                       inwater), 
                                size = 1), 
                    aes(x = Datetime_UTC, 
                        y = Temp_C, 
                        color = SensorType,
                        group = IntervalName)) +
    geom_rect(data = poly, 
              aes(xmin = poly_start,
                  xmax = poly_end,
                  ymin = -Inf, ymax = Inf), 
              inherit.aes = FALSE, 
              alpha = .3, fill = "grey50", color = NA) + 
    # 
    # geom_vline(xintercept = df_list$deployment_df$DateTime_UTC_Start, 
    #            linetype = "dashed", color = "green", linewidth = 0.25) +
    # geom_vline(xintercept = df_list$deployment_df$DateTime_UTC_End, 
    #            linetype = "dashed", color = "red", linewidth = 0.25) + 
    # geom_point(size = .5) + 
    geom_line(linewidth = 0.5) +
    theme_timeseries_common + 
    # theme(panel.background = element_rect(fill = "lightgrey")) + 
    facet_grid(vars(Depth_cm), vars(BuoyName)) +
    scale_color_brewer(palette = "Dark2")
  
  # print(p_temp3)
  
  if(exists("p_temp3")){
    fig_list[[length(fig_list) + 1]] <- p_temp3
    fig_details_df[length(fig_list), "name"] <- c("All_Temp_Data_v3.png")
    fig_details_df[length(fig_list), c("width", "height")] <- c(10,6)
  }
  
  p_temp4 <- ggplot(filter(temp_subset, 
                           !is.na(Temp_C), 
                           # Datetime_UTC > date_test[1], 
                           # Datetime_UTC < date_test[2], 
                           inwater) %>%
                      arrange(BuoyName, desc(Depth_cm), Datetime_UTC), 
                    aes(x = Datetime_UTC, 
                        y = Temp_C, 
                        color = Depth_cm,
                        group = IntervalName)) +
    geom_rect(data = poly, 
              aes(xmin = poly_start,
                  xmax = poly_end,
                  ymin = -Inf, ymax = Inf), 
              inherit.aes = FALSE, 
              alpha = .3, fill = "grey50", color = NA) + 
    # geom_vline(xintercept = df_list$deployment_df$DateTime_UTC_Start, 
    #            linetype = "dashed", color = "green", linewidth = 0.25) +
    # geom_vline(xintercept = df_list$deployment_df$DateTime_UTC_End, 
    #            linetype = "dashed", color = "red", linewidth = 0.25) + 
    # geom_point(size = .5) + 
    geom_line(linewidth = 0.5) +
    theme_timeseries_common + 
    facet_grid(rows = vars(forcats::fct_rev(BuoyName))) +
    scale_color_brewer(palette = "RdYlBu")
  
  # print(p_temp4)
  
  if(exists("p_temp4")){
    fig_list[[length(fig_list) + 1]] <- p_temp4
    fig_details_df[length(fig_list), "name"] <- c("All_Temp_Data_v4.png")
    fig_details_df[length(fig_list), c("width", "height")] <- c(10,6)
  }
  
  p_do1 <- ggplot(sample_frac(filter(df_list$DO_df, 
                                     !is.na(DO_mgL), 
                                     inwater), 
                              size = 1), 
                  aes(x = Datetime_UTC, 
                      y = DO_PerSat, 
                      color = Depth_cm,
                      group = IntervalName)) +
    geom_vline(xintercept = df_list$deployment_df$DateTime_UTC_Start, 
               linetype = "dashed", color = "green", linewidth = 0.25) +
    geom_vline(xintercept = df_list$deployment_df$DateTime_UTC_End, 
               linetype = "dashed", color = "red", linewidth = 0.25) + 
    # geom_point(size = .5) + 
    geom_line(linewidth = 0.5) +
    theme_timeseries_common + 
    facet_grid(rows = vars(forcats::fct_rev(BuoyName))) +
    scale_color_brewer(palette = "RdYlBu")
  
  # print(p_do1)
  
  if(exists("p_do1")){
    fig_list[[length(fig_list) + 1]] <- p_do1
    fig_details_df[length(fig_list), "name"] <- c("All_DO_Data_v2.png")
    fig_details_df[length(fig_list), c("width", "height")] <- c(10,6)
  }
  
  
  p_cond <- ggplot(sample_frac(filter(df_list$temp_df, 
                                      !is.na(SPC_uScm), 
                                      inwater), 
                               size = 1), 
                   aes(x = Datetime_UTC, 
                       y = SPC_uScm, 
                       color = Depth_cm,
                       group = IntervalName)) +
    geom_vline(xintercept = df_list$deployment_df$DateTime_UTC_Start, 
               linetype = "dashed", color = "green", linewidth = 0.25) +
    geom_vline(xintercept = df_list$deployment_df$DateTime_UTC_End, 
               linetype = "dashed", color = "red", linewidth = 0.25) + 
    # geom_point(size = .5) + 
    geom_line(linewidth = 0.5) +
    theme_timeseries_common + 
    facet_grid(rows = vars(forcats::fct_rev(BuoyName))) +
    scale_color_brewer(palette = "RdYlBu")
  
  # print(p_cond)
  
  if(exists("p_cond")){
    fig_list[[length(fig_list) + 1]] <- p_cond
    fig_details_df[length(fig_list), "name"] <- c("All_SPC_Data.png")
    fig_details_df[length(fig_list), c("width", "height")] <- c(10,6)
  }
  
  
  p_par <- ggplot(sample_frac(filter(df_list$par_df, 
                                     !is.na(PAR_uM_per_s_per_m2), 
                                     inwater), 
                              size = 1), 
                  aes(x = Datetime_UTC, 
                      y = PAR_uM_per_s_per_m2, 
                      color = Depth_cm,
                      group = IntervalName)) +
    geom_vline(xintercept = df_list$deployment_df$DateTime_UTC_Start, 
               linetype = "dashed", color = "green", linewidth = 0.25) +
    geom_vline(xintercept = df_list$deployment_df$DateTime_UTC_End, 
               linetype = "dashed", color = "red", linewidth = 0.25) + 
    geom_point(size = .5) +
    # geom_line(linewidth = 0.5) +
    theme_timeseries_common + 
    facet_grid(rows = vars(forcats::fct_rev(BuoyName))) +
    scale_color_brewer(palette = "RdYlBu")
  
  # print(p_par)
  
  if(exists("p_par")){
    fig_list[[length(fig_list) + 1]] <- p_par
    fig_details_df[length(fig_list), "name"] <- c("All_PAR_Data.png")
    fig_details_df[length(fig_list), c("width", "height")] <- c(10,6)
  }
  
  p_cdom <- ggplot(sample_frac(filter(df_list$cdom_df, 
                                      !is.na(CDOM_ppb ), 
                                      inwater), 
                               size = 1), 
                   aes(x = Datetime_UTC, 
                       y = CDOM_ppb , 
                       color = Depth_cm,
                       group = IntervalName)) +
    geom_vline(xintercept = df_list$deployment_df$DateTime_UTC_Start, 
               linetype = "dashed", color = "green", linewidth = 0.25) +
    geom_vline(xintercept = df_list$deployment_df$DateTime_UTC_End, 
               linetype = "dashed", color = "red", linewidth = 0.25) + 
    geom_point(size = .5) +
    # geom_line(linewidth = 0.5) +
    theme_timeseries_common + 
    facet_grid(rows = vars(forcats::fct_rev(BuoyName))) +
    scale_color_brewer(palette = "RdYlBu")
  
  # print(p_cdom)
  
  if(exists("p_cdom")){
    fig_list[[length(fig_list) + 1]] <- p_cdom
    fig_details_df[length(fig_list), "name"] <- c("All_CDOM_Data.png")
    fig_details_df[length(fig_list), c("width", "height")] <- c(10,6)
  }
  
  
  p_chla <- ggplot(sample_frac(filter(df_list$sensor_merge_df, 
                                      !is.na(Chl_ugL_corrected), 
                                      inwater), 
                               size = 1), 
                   aes(x = Datetime_UTC, 
                       y = Chl_ugL_corrected, 
                       color = Depth_cm,
                       group = IntervalName)) +
    geom_vline(xintercept = df_list$deployment_df$DateTime_UTC_Start, 
               linetype = "dashed", color = "green", linewidth = 0.25) +
    geom_vline(xintercept = df_list$deployment_df$DateTime_UTC_End, 
               linetype = "dashed", color = "red", linewidth = 0.25) + 
    geom_point(size = .5) +
    # geom_line(linewidth = 0.5) +
    theme_timeseries_common + 
    facet_grid(rows = vars(forcats::fct_rev(BuoyName))) +
    scale_color_brewer(palette = "RdYlBu") +
    labs(y = "Corrected chl-a (ugL)")
  
  # print(p_chla)
  
  if(exists("p_chla")){
    fig_list[[length(fig_list) + 1]] <- p_chla
    fig_details_df[length(fig_list), "name"] <- c("All_Chla_Data.png")
    fig_details_df[length(fig_list), c("width", "height")] <- c(10,6)
  }
  
  p_chla_raw <- ggplot(sample_frac(filter(df_list$chl_df, 
                                          !is.na(chl_ugL ), 
                                          inwater), 
                                   size = 1), 
                       aes(x = Datetime_UTC, 
                           y = chl_ugL , 
                           color = Depth_cm,
                           group = IntervalName)) +
    geom_vline(xintercept = df_list$deployment_df$DateTime_UTC_Start, 
               linetype = "dashed", color = "green", linewidth = 0.25) +
    geom_vline(xintercept = df_list$deployment_df$DateTime_UTC_End, 
               linetype = "dashed", color = "red", linewidth = 0.25) + 
    geom_point(size = .5) +
    # geom_line(linewidth = 0.5) +
    theme_timeseries_common + 
    facet_grid(rows = vars(forcats::fct_rev(BuoyName))) +
    scale_color_brewer(palette = "RdYlBu") +
    labs(y = "Raw chl-a (ugL)")
  
  # print(p_chla_raw)
  
  if(exists("p_chla_raw")){
    fig_list[[length(fig_list) + 1]] <- p_chla_raw
    fig_details_df[length(fig_list), "name"] <- c("All_Chla_Raw_Data.png")
    fig_details_df[length(fig_list), c("width", "height")] <- c(10,6)
  }
  
  p_turb_raw <- ggplot(sample_frac(filter(df_list$turb_df, 
                                          !is.na(turb_NTU), 
                                          inwater), 
                                   size = 1), 
                       aes(x = Datetime_UTC, 
                           y = turb_NTU, 
                           color = Depth_cm,
                           group = IntervalName)) +
    geom_vline(xintercept = df_list$deployment_df$DateTime_UTC_Start, 
               linetype = "dashed", color = "green", linewidth = 0.25) +
    geom_vline(xintercept = df_list$deployment_df$DateTime_UTC_End, 
               linetype = "dashed", color = "red", linewidth = 0.25) + 
    geom_point(size = .5) +
    # geom_line(linewidth = 0.5) +
    theme_timeseries_common + 
    facet_grid(rows = vars(forcats::fct_rev(BuoyName))) +
    scale_color_brewer(palette = "RdYlBu") +
    labs(y = "Raw turbidity (NTU)")
  
  # print(p_turb_raw)
  
  if(exists("p_turb_raw")){
    fig_list[[length(fig_list) + 1]] <- p_turb_raw
    fig_details_df[length(fig_list), "name"] <- c("All_Turb_Raw_Data.png")
    fig_details_df[length(fig_list), c("width", "height")] <- c(10,6)
  }
  
  p_turb <- ggplot(sample_frac(filter(df_list$sensor_merge_df, 
                                      !is.na(Turb_NTU_corrected), 
                                      inwater), 
                               size = 1), 
                   aes(x = Datetime_UTC, 
                       y = Turb_NTU_corrected, 
                       color = Depth_cm,
                       group = IntervalName)) +
    geom_vline(xintercept = df_list$deployment_df$DateTime_UTC_Start, 
               linetype = "dashed", color = "green", linewidth = 0.25) +
    geom_vline(xintercept = df_list$deployment_df$DateTime_UTC_End, 
               linetype = "dashed", color = "red", linewidth = 0.25) + 
    geom_point(size = .5) +
    # geom_line(linewidth = 0.5) +
    theme_timeseries_common + 
    facet_grid(rows = vars(forcats::fct_rev(BuoyName))) +
    scale_color_brewer(palette = "RdYlBu") +
    labs(y = "Corrected turbidity (NTU)")
  
  # print(p_turb)
  
  if(exists("p_turb")){
    fig_list[[length(fig_list) + 1]] <- p_turb
    fig_details_df[length(fig_list), "name"] <- c("All_Turb_Data.png")
    fig_details_df[length(fig_list), c("width", "height")] <- c(10,6)
  }
  
  if (save_to_file){
    fig_i <- 1
    for (fig_i in 1:length(fig_list)){
      
      ggsave(file.path(box_dir, "Figures", fig_details_df$name[fig_i]),
             plot = fig_list[[fig_i]],
             height = fig_details_df$height[fig_i], 
             width = fig_details_df$width[fig_i], 
             units = "in")
    }
  }
  
  
}
