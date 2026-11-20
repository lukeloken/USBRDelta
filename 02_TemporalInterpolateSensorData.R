

library(dplyr)
library(tidyr)

#functions to perform temporal interpolation of each sensor based on multiple groups
#Uses output of CombineBuoySensorData script
#Data must be in longformat

interpolate_mybuoydata <- function(data_in,
                                   time_col = "Datetime_UTC",
                                   value_col = NULL,
                                   group_cols = NULL,
                                   minute_interval = 10) {
  
  
  
  if(is.null(value_col)){
    value_col = "value"
  }
  
  if(is.null(group_cols)){
    group_cols <- c("SerialNumber", "IntervalName", "variable")
  }
  
  # Check required columns
  required_cols <- c(time_col, group_cols, value_col)
  
  missing_cols <- setdiff(required_cols, names(data_in))
  
  if (length(missing_cols) > 0) {
    stop("Missing columns: ",
         paste(missing_cols, collapse = ", "))
  }
  
  data_in <- data_in %>%
    arrange(.data[[time_col]]) %>%
    select(all_of(c(group_cols, time_col, value_col))) %>%
    filter(!is.na(.data[[value_col]]), 
           !is.na(.data[[time_col]])) %>%
    group_by(across(all_of(group_cols)))
  
  
  #Make a grid of time to interpolate
  
  first_time <- as.POSIXct(
    ceiling(as.numeric(min(data_in[[time_col]])) / 600) * 600,
    origin = "1970-01-01",
    tz = attr(data_in[[time_col]], "tzone")
  )
  
  end_time <- as.POSIXct(
    floor(as.numeric(max(data_in[[time_col]])) / 600) * 600,
    origin = "1970-01-01",
    tz = attr(data_in[[time_col]], "tzone")
  )
  
  new_times_all <- seq(first_time, end_time, by = paste(minute_interval, "min"))
  
  #Create empty data.frame with the same column names
  data_out <- data_in[0,]
  
  
  
  result <- data_in %>%
    group_by(across(all_of(group_cols))) %>%
    group_modify(~ {
      
      
      group_data <- .x %>%
        arrange(.data[[time_col]])
      
      # Create 10-minute time sequence for each group
      new_time <- new_times_all[which(new_times_all >= min(group_data[[time_col]], na.rm = TRUE) & 
                                        new_times_all <= max(group_data[[time_col]], na.rm = TRUE))]
      
      
      # Interpolate each measurement variable
      tibble(
        !!time_col := new_time,
        value = interpolate_variable(
          group_data[[time_col]],
          group_data$value,
          new_time
        )
      )
      
    }) %>%
    ungroup()
  
  return(result)
  
}


# Function to interpolate one variable
interpolate_variable <- function(time, value, new_time) {
  
  valid <- !is.na(time) & !is.na(value)
  
  time <- as.numeric(time[valid])
  value <- value[valid]
  
  if (length(time) < 2) {
    return(rep(NA_real_, length(new_time)))
  }
  
  # Sort observations by time
  ord <- order(time)
  time <- time[ord]
  value <- value[ord]
  
  # Handle duplicate timestamps by averaging values
  temp <- aggregate(value, by = list(time = time),
                    FUN = mean)
  
  approx(
    x = temp$time,
    y = temp$x,
    xout = as.numeric(new_time),
    method = "linear",
    rule = 1
  )$y
}



#Workflow
#load here to save time
df_list <- readRDS(file = file.path(box_dir, 
                                    "DWSC_Data_Processed",
                                    'Buoy_Sensor_List.rds'))

#prepare data
mydata <- df_list$sensor_merge_df %>% filter(inwater) %>%
  select(-BuoyName, -inwater) %>%
  select(SerialNumber, SensorType, 
         Site, Depth_cm, 
         IntervalName, Datetime_UTC, 
         everything())

#convert to long format
mydata_long <- mydata %>%
  pivot_longer(cols = 7:ncol(mydata), 
               names_to = "variable", 
               values_to = "value",
               values_drop_na = TRUE) %>%
  arrange(Site, Depth_cm, variable, Datetime_UTC, SensorType)

# head(mydata_long)

#set function arguments
data_in <- mydata_long
time_col = "Datetime_UTC"
value_col <- "value"
group_cols <- c("SerialNumber", "SensorType",
                "Site", "Depth_cm",
                "IntervalName", "variable")
minute_interval <- 10

#Interpolate it baby!
myinterp <- interpolate_mybuoydata(data_in = mydata_long, 
                                   time_col = time_col,
                                   value_col = value_col,
                                   group_cols = group_cols, 
                                   minute_interval = 10)
dim(myinterp)
head(myinterp)
dim(mydata_long)
summary(myinterp)

myinterp$Site <- factor(as.character(myinterp$Site), c( "CM66","CM74", "CM84"))

# ggplot(slice_sample(filter(myinterp, variable == "DO_mgL", 
#                            # grepl("02_", IntervalName), 
#                            as.Date(Datetime_UTC) == "2025-08-01", 
#                            hour(Datetime_UTC) %in% 11:13), 
#                     prop = 1),
#        aes(x = Datetime_UTC, y = value, color = SensorType)) + 
#   facet_grid(cols = vars(Site), 
#              rows = vars(Depth_cm)) +
#   geom_point() +
#   geom_point(data = filter(mydata_long, variable == "DO_mgL", 
#                            # grepl("02_", IntervalName), 
#                            as.Date(Datetime_UTC) == "2025-08-01", 
#                            hour(Datetime_UTC)  %in% 11:13), 
#              color = "blue", size = 0.1)
# 
# 
# 
# 

myinterp

#all sensors
write.table(myinterp, file = file.path(box_dir, 
                                       "DWSC_Data_Processed",
                                       'Buoy_AllSensors_interpolated_10min.csv'), 
            row.names=F, sep=',')

saveRDS(sensor_merge_df, file = file.path(box_dir, 
                                          "DWSC_Data_Processed", 
                                          'Buoy_AllSensors_interpolated_10min.rds'))

#data for polygon during non-deployments  
poly_start <- as.POSIXct(c(-Inf, df_list[[1]]$DateTime_UTC_End), tz = "UTC")
poly_end <- as.POSIXct(c(df_list[[1]]$DateTime_UTC_Start, Inf))

poly <- data.frame(poly_start, poly_end)



#filter to temp data
myinterp_temp <- myinterp %>%
  filter(SensorType %in% c("PAR", "DO", "TEMP/SPC"), 
         variable == "Temp_C") %>%
  filter(SensorType %in% c("PAR", "DO") | Depth_cm != "050") %>%
  arrange(Site, desc(Depth_cm), Datetime_UTC) %>%
  mutate(Depth_cm = as.factor(as.numeric(Depth_cm)))

    

#Visualization
theme_timeseries_common <- list(theme_bw(), 
                                scale_x_datetime(date_labels = "%b%e\n%Y", 
                                                 breaks =  as.POSIXct(seq.Date(from = "2022-07-01",
                                                                                    to = "2026-01-01",
                                                                                    by = "6 months"),
                                                                           tz = "UTC"),  
                                                 limits = as.POSIXct(c("2022-08-01", "2025-12-29"), 
                                                                     tz = "UTC"), 
                                                 expand = expansion(mult = 0)), 
                                theme(strip.background = element_rect(fill = NA, color = NA), 
                                      panel.grid.minor = element_blank()))

sample_frac_prop = 1

p_temp_heat <- ggplot(sample_frac(filter(myinterp_temp, 
                                         !is.na(value)), 
                                  size = sample_frac_prop), 
                      aes(x = Datetime_UTC, 
                          y = as.numeric(as.character(Depth_cm)), 
                          fill = value,
                          group = IntervalName)) +
  geom_vline(xintercept = df_list$deployment_df$DateTime_UTC_Start, 
             linetype = "dashed", color = "green", linewidth = 0.25) +
  geom_vline(xintercept = df_list$deployment_df$DateTime_UTC_End, 
             linetype = "dashed", color = "red", linewidth = 0.25) + 
  # geom_point(size = .5) + 
  geom_tile() +
  theme_timeseries_common + 
  labs(y = "Depth (cm)", fill = expression(paste("Temp (", degree, "C)"))) + 
  facet_grid(rows = vars(forcats::fct_rev(Site))) +
  scale_fill_distiller(palette = "RdYlBu") +
  scale_y_reverse()

print(p_temp_heat)



p_temp3 <- ggplot(sample_frac(filter(myinterp_temp, 
                                     !is.na(value)), 
                              size = sample_frac_prop), 
                  aes(x = Datetime_UTC, 
                      y = value, 
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
  facet_grid(vars(Depth_cm), vars(Site)) +
  scale_color_brewer(palette = "Dark2") +
  labs(y = "Temp")

print(p_temp3)

ggsave(filename = file.path(box_dir, "Figures", "All_Temp_Data_Interpolated_v3.png"),
       plot = p_temp3,
       height = 6, 
       width = 10, 
       units = "in")



p_temp4 <- ggplot(sample_frac(filter(myinterp_temp, 
                         !is.na(value) 
                         # Datetime_UTC > date_test[1], 
                         # Datetime_UTC < date_test[2],
                         ),
                         size = sample_frac_prop) %>%
                    arrange(Depth_cm, Site, Datetime_UTC), 
                  aes(x = Datetime_UTC, 
                      y = value, 
                      color = Depth_cm,
                      group = IntervalName)) +
  geom_hline(yintercept = c(22), 
             linetype = c("dotted")) + 
  geom_hline(yintercept = c(25), 
             linetype = c("solid")) + 
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
  theme(panel.grid = element_blank()) + 
  facet_grid(rows = vars(forcats::fct_rev(Site))) +
  scale_color_brewer(palette = "RdYlBu") +
  labs(x = "Time", 
       y = expression(paste("Temperature (", degree, "C)")), 
       color = "Depth (cm)")

# print(p_temp4)


ggsave(filename = file.path(box_dir, "Figures", "All_Temp_Data_v4_forposter.png"),
       plot = p_temp4,
       height = 6, 
       width = 10, 
       units = "in")

#temp only sensors
write.table(myinterp_temp, file = file.path(box_dir, 
                                       "DWSC_Data_Processed",
                                       'Buoy_TempSensorsBest_interpolated_10min.csv'), 
            row.names=F, sep=',')


saveRDS(myinterp_temp, file = file.path(box_dir, 
                                          "DWSC_Data_Processed", 
                                          'Buoy_TempSensorsBest_interpolated_10min.rds'))

#vertical interpolate
# Define the target depths you want to interpolate onto
target_depths <- seq(min(as.numeric(as.character(myinterp_temp$Depth_cm))), 
                     max(as.numeric(as.character(myinterp_temp$Depth_cm))), by = 50)

# Interpolate 'value' across 'depth' for each 'time'
df_interp <- myinterp_temp %>%
  mutate(depth_numeric = as.numeric(as.character(Depth_cm))) %>%
  group_by(Datetime_UTC, variable, Site, IntervalName) %>%
  arrange(depth_numeric, .by_group = TRUE) %>%
  reframe(
    depth = target_depths,
    value = approx(x = depth_numeric, y = value, xout = target_depths, rule = 2)$y
  )

head(df_interp)

df_interp <- df_interp %>%
  mutate(Depth_cm = as.character(depth)) %>%
  left_join(distinct(select(myinterp_temp, 
                              IntervalName, variable, Datetime_UTC, SerialNumber, SensorType, Site, 
                              Depth_cm)))

df_interp$SensorType[which(is.na(df_interp$SensorType))] <- "INTERP"
df_interp$SerialNumber[which(is.na(df_interp$SerialNumber))] <- (-999999)

p_temp3_interp <- ggplot(sample_frac(filter(df_interp, 
                                     !is.na(value)), 
                              size = sample_frac_prop), 
                  aes(x = Datetime_UTC, 
                      y = value, 
                      group = IntervalName, 
                      color = SensorType)) +
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
  facet_grid(vars(depth ), vars(Site)) +
  scale_color_brewer(palette = "Paired") +
  labs(y = "Temp")

print(p_temp3_interp)

ggsave(filename = file.path(box_dir, "Figures", "All_Temp_Data_InterpolatedTimeVertical_v3.png"),
       plot = p_temp3_interp,
       height = 6, 
       width = 10, 
       units = "in")



#temp only sensors
write.table(df_interp, file = file.path(box_dir, 
                                                 "DWSC_Data_Processed",
                                                 'Buoy_TempSensorsBest_interpolated_10min_50cm.csv'), 
            row.names=F, sep=',')


saveRDS(df_interp, file = file.path(box_dir, 
                                             "DWSC_Data_Processed", 
                                             'Buoy_TempSensorsBest_interpolated_10min_50cm.rds'))




# may 1 2024 - aug 31 2024
df_interp_snip <- df_interp %>%
  filter(Datetime_UTC >= as.POSIXct("2024-05-01", tz = "UTC"), 
         Datetime_UTC < as.POSIXct("2024-09-01", tz = "UTC"), 
  )

#temp only sensors example 10 min 50cm resolution
write.table(df_interp_snip, file = file.path(box_dir, 
                                                 "DWSC_Data_Processed",
                                                 'Buoy_TempSensorsBest_interpolated_10min_50cm_example.csv'), 
            row.names=F, sep=',')


saveRDS(df_interp_snip, file = file.path(box_dir, 
                                             "DWSC_Data_Processed", 
                                             'Buoy_TempSensorsBest_interpolated_10min_50cm_example.rds'))





vertical_summary <- df_interp %>%
  group_by(Datetime_UTC, variable, Site, IntervalName) %>%
  mutate(temp_relative = value - value[which(Depth_cm == 500)]) %>%
  mutate(month = month(Datetime_UTC, label = TRUE, abbr = TRUE))


head(data.frame(vertical_summary))

vertical_summary_avg <- vertical_summary %>%
  group_by(month, variable, Site, depth, Depth_cm) %>%
  summarize(temp_relative_mean = mean(temp_relative, na.rm = TRUE),
            temp_relative_sd = sd(temp_relative)) %>%
  mutate(temp_max = temp_relative_mean + temp_relative_sd/2, 
         temp_min = temp_relative_mean - temp_relative_sd/2) %>%
  filter(is.finite(temp_relative_mean))

tornado_fig <- ggplot(vertical_summary_avg) + 
  theme_bw() + 
  geom_line(aes(x = temp_relative_mean, y = depth/100, color = Site),
            alpha = 1, linewidth = 1, 
            orientation = "y") +
  geom_ribbon(aes(xmin = temp_min,
                  xmax = temp_max,
                  y = depth/100,
                  fill = Site),
            alpha = .4) +
  facet_grid(rows = vars(month), 
             cols = vars(Site)
             ) + 
  scale_y_reverse(breaks = seq(1,5,2)) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_continuous(breaks = c(0,1)) + 
  scale_fill_brewer(palette = "Dark2") +
  labs(y = "Depth (m)", 
       x = expression(paste("Relative temperature (", degree, "C)"))) +
  theme(legend.position = "none")

tornado_fig

ggsave(filename = file.path(box_dir, "Figures", "Vertical_Profile_Summary_bySite_byMonth.png"),
       plot = tornado_fig,
       height = 10, 
       width = 6, 
       units = "in")


tornado_fig2 <- ggplot(vertical_summary_avg) + 
  theme_bw() + 
  geom_vline(xintercept = 0, linetype = "solid", color = "black") + 
  geom_line(aes(x = temp_relative_mean, y = depth/100, color = Site),
            alpha = 1, linewidth = 1, 
            orientation = "y") +
  # geom_ribbon(aes(xmin = temp_min,
  #                 xmax = temp_max,
  #                 y = depth/100,
  #                 fill = Site),
  #             alpha = .4) +
  facet_wrap(vars(month), nrow = 3,
             # cols = vars(Site)
  ) + 
  scale_y_reverse(breaks = seq(1,5,2)) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_continuous(breaks = seq(-0.5, 1.5, 0.5)) + 
  scale_fill_brewer(palette = "Dark2") +
  labs(y = "Depth (m)", 
       x = expression(paste("Relative temperature (", degree, "C)"))) +
  guides(color = guide_legend(override.aes = list(fill = NA))) +
  theme(legend.position.inside = c(1,0), 
        legend.position = "inside", 
        legend.justification = c(1,0),
        legend.title = element_blank(), 
        legend.background = element_blank(), 
        legend.key = element_rect(fill = NA, colour = NA), 
        legend.key.height = unit(0.7, "lines") ,
        legend.spacing.x = unit(0.05, "cm"),                 # Reduce gap between key & text
        strip.background = element_blank())

tornado_fig2

ggsave(filename = file.path(box_dir, "Figures", "Vertical_Profile_Summary_bySite_byMonthMean.png"),
       plot = tornado_fig2,
       height = 5, 
       width = 5, 
       units = "in")


#event analysis
#time (10min intervals) beyond a threshold veritcal temperature difference. 



df_interp_diff <- df_interp %>%
  mutate(strata = case_when(depth > 175 ~ "lower", 
                            depth < 175 ~ "upper")) %>%
group_by(Datetime_UTC, variable, Site) %>%
  summarize(temp_diff = max(value[which(strata == "upper")]) -
              min(value[which(strata == "lower")])
            ) %>%
  mutate(month = month(Datetime_UTC, label = TRUE, abbr = TRUE))

df_interp_diff


ggplot(df_interp_diff) +
  theme_bw() + 
  geom_point(aes(x = Datetime_UTC, 
                 y = temp_diff, 
                 color = Site), size = 0.1) +
  facet_wrap(~Site, ncol = 1) +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none", 
        strip.background = element_blank()) + 
  labs(x = "Time", 
       y = expression(paste("Vertical temperature difference (", degree, "C)")))




temp_delta_box <- ggplot(df_interp_diff) + 
  theme_bw() + 
  geom_boxplot(aes(y = temp_diff, x = month, fill = Site), outliers = FALSE) +
  # geom_violin(aes(y = temp_diff, x = month, fill = Site)) +
  # facet_wrap(~Site, ncol = 1) +
  scale_y_sqrt() +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Month", 
       y = expression(paste(Delta, "temperature (", degree, "C)")))

temp_delta_box
  
ggsave(filename = file.path(box_dir, "Figures", "Boxplot_deltaT_monthly.png"),
       plot = temp_delta_box,
       height = 3, 
       width = 7, 
       units = "in")

