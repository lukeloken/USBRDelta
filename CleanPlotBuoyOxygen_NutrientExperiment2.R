#some handy packages
library(lubridate)
library(plyr)
library(dplyr)
library(viridis)
library(ggplot2)
library(gridExtra)
library(akima)
library(stringr)
library(RColorBrewer)

DO_df_clean <- readRDS(file=paste0(dropbox_dir, '/Data/Rdata_SSCN2/Buoy/Buoy_DO_raw.rds'))
Temp_df_clean <- readRDS(file=paste0(dropbox_dir, '/Data/Rdata_SSCN2/Buoy/Buoy_Temp_raw.rds'))
Cond_df_clean <- readRDS(file=paste0(dropbox_dir, '/Data/Rdata_SSCN2/Buoy/Buoy_Cond_raw.rds'))


buoy_meta<-read.csv(paste0(box_dir, '/Data/BuoyData/SSCN2 Buoy Information.csv'))
buoy_meta$Serial[which(buoy_meta$Type=='Dissolved Oxygen')]<-str_pad(buoy_meta$Serial[which(buoy_meta$Type=='Dissolved Oxygen')], 6, pad='0')
buoy_meta$Serial[which(buoy_meta$Type=='Dissolved Oxygen')]<-paste0("7450-", buoy_meta$Serial[which(buoy_meta$Type=='Dissolved Oxygen')])



#dates buoys were deployed in the Sacramento Ship Channel
deploydates <- as.POSIXct(c("2019-07-02 16:00:00", "2019-09-18 08:00:00"), tz = 'America/Los_Angeles')

#Clean and match DO data
DO_df_clean2 <- DO_df_clean %>%
  dplyr:: filter(Datetime_UTC > deploydates[1], 
                 Datetime_UTC < deploydates[2]) %>%
  mutate(Site = factor(buoy_meta$Site[match(SerialNumber, buoy_meta$Serial)], sitetable$site1),
         Depth = as.character(buoy_meta$Depth_m[match(SerialNumber, buoy_meta$Serial)])) %>%
  dplyr::arrange(Site, Depth)

DO_df_clean2$DO_mgL[which(DO_df_clean2$DO_mgL>16)]  <- NA
DO_df_clean2$DO_mgL[which(DO_df_clean2$DO_mgL<5)]  <- NA

#Clean and match Cond data
Cond_df_clean2 <- Cond_df_clean %>%
  dplyr:: filter(Datetime_UTC > deploydates[1], 
                 Datetime_UTC < deploydates[2]) %>%
  mutate(Site = factor(buoy_meta$Site[match(SerialNumber, buoy_meta$Serial)], sitetable$site1),
         Depth = as.character(buoy_meta$Depth_m[match(SerialNumber, buoy_meta$Serial)])) %>%
  dplyr::arrange(Site, Depth)

Cond_df_clean2$Cond_uScm[which(Cond_df_clean2$Cond_uScm>1200)]  <- NA
Cond_df_clean2$Cond_uScm[which(Cond_df_clean2$Cond_uScm<650)]  <- NA

#Clean and match Temp data
Temp_df_clean2 <- Temp_df_clean %>%
  dplyr:: filter(Datetime_UTC > deploydates[1], 
                 Datetime_UTC < deploydates[2]) %>%
  mutate(Site = factor(buoy_meta$Site[match(SerialNumber, buoy_meta$Serial)], sitetable$site1),
         Depth = as.character(buoy_meta$Depth_m[match(SerialNumber, buoy_meta$Serial)])) %>%
  bind_rows(Cond_df_clean2, DO_df_clean2) %>%
  dplyr::select(-Cond_uScm, -DO_mgL, -DO_PerSat) %>%
  dplyr::arrange(Site, Depth)

Temp_df_clean2$Temp_C[which(Temp_df_clean2$Temp_C>30)]  <- NA
Temp_df_clean2$Temp_C[which(Temp_df_clean2$Temp_C<22.5)]  <- NA


saveRDS(Temp_df_clean2, file=paste0(dropbox_dir, '/Data/Rdata_SSCN2/Buoy/Buoy_Temp_cleaned.rds'))
saveRDS(Cond_df_clean2, file=paste0(dropbox_dir, '/Data/Rdata_SSCN2/Buoy/Buoy_Cond_cleaned.rds'))
saveRDS(DO_df_clean2, file=paste0(dropbox_dir, '/Data/Rdata_SSCN2/Buoy/Buoy_DO_cleaned.rds'))

write.table(Temp_df_clean2, file=paste0(google_dir, '/SSCN2_DataOutputs/Buoy/Buoy_Temp_cleaned.csv'), row.names=F, sep=',')
write.table(Cond_df_clean2, file=paste0(google_dir, '/SSCN2_DataOutputs/Buoy/Buoy_Cond_cleaned.csv'), row.names=F, sep=',')
write.table(DO_df_clean2, file=paste0(google_dir, '/SSCN2_DataOutputs/Buoy/Buoy_DO_cleaned.csv'), row.names=F, sep=',')


# ######################
# plotting
# ######################

DO_SerialNumbers <- unique(DO_df_clean2$SerialNumber)
i=1
for( i in 1: length(DO_SerialNumbers)){
  
  DO_data <- DO_df_clean2 %>%
    dplyr::filter(SerialNumber == DO_SerialNumbers[i])
  
  png(paste0(dropbox_dir, "/Figures/NutrientExperiment2/Buoys/DO/", DO_SerialNumbers[i], ".png"), units = 'in', width=10, height=5, res=200)
  
  print(
    ggplot(aes(x=Datetime_UTC, y=DO_mgL), data=DO_data) +
      geom_point(size = 0.5, colour="#a8ddb5") +
      geom_path( colour="#a8ddb5") +
      theme_bw() +
      labs(x="", y="(mg/L)") +
      ggtitle(DO_SerialNumbers[i])
  )
  
  dev.off()
  
}


Cond_SerialNumbers <- unique(Cond_df_clean2$SerialNumber)
i=1
for( i in 1: length(Cond_SerialNumbers)){
  
  Cond_data <- Cond_df_clean2 %>%
    dplyr::filter(SerialNumber == Cond_SerialNumbers[i])
  
  png(paste0(dropbox_dir, "/Figures/NutrientExperiment2/Buoys/Cond/", Cond_SerialNumbers[i], ".png"), units = 'in', width=10, height=5, res=200)
  
  print(
    ggplot(aes(x=Datetime_UTC, y=Cond_uScm), data=Cond_data) +
      geom_point(size = 0.5, colour="purple") +
      geom_path( colour="purple") +
      theme_bw() +
      labs(x="", y="(uS/cm)") +
      ggtitle(Cond_SerialNumbers[i])
  )
  
  dev.off()
  
}




Temp_SerialNumbers <- unique(Temp_df_clean2$SerialNumber)
i=1
for( i in 1: length(Temp_SerialNumbers)){
  
  Temp_data <- Temp_df_clean2 %>%
    dplyr::filter(SerialNumber == Temp_SerialNumbers[i])
  
  
  png(paste0(dropbox_dir, "/Figures/NutrientExperiment2/Buoys/Temp/", Temp_SerialNumbers[i], ".png"), units = 'in', width=10, height=5, res=200)
  
  print(
    ggplot(aes(x=Datetime_UTC, y=Temp_C), data=Temp_data) +
      geom_point(size = 0.5, colour="#3690c0") +
      geom_path( colour="#3690c0") +
      theme_bw() +
      labs(x="", y="Degrees C") +
      ggtitle(Temp_SerialNumbers[i])
  )
  dev.off()
}



#Grid plots

png(paste0(dropbox_dir, "/Figures/NutrientExperiment2/Buoys/DO/", "DO_GridPlot1", ".png"), units = 'in', width=20, height=8, res=200)

print(
  ggplot(aes(x=Datetime_UTC, y=DO_mgL, color=Depth), data=DO_df_clean2) +
    geom_vline(xintercept=fert_posix, linetype="dashed", color = "green", size=0.5) + 
    geom_point(size = 0.5) +
    geom_path(size = 0.5) +
    scale_colour_manual(values = brewer.pal(6, 'Blues')[4:6]) + 
    facet_grid(Depth~Site) + 
    theme_bw() + 
    theme(legend.position='bottom') + 
    guides(color = guide_legend(nrow = 1))
)
dev.off()


png(paste0(dropbox_dir, "/Figures/NutrientExperiment2/Buoys/Cond/", "Cond_GridPlot1", ".png"), units = 'in', width=20, height=8, res=200)

print(
  ggplot(aes(x=Datetime_UTC, y=Cond_uScm, color=Depth), data=Cond_df_clean2) +
    geom_vline(xintercept=fert_posix, linetype="dashed", color = "green", size=0.5) + 
    geom_point(size = 0.5) +
    geom_path(size = 0.5) +
    scale_colour_manual(values = brewer.pal(6, 'Blues')[4:6]) + 
    facet_grid(Depth~Site) + 
    theme_bw() + 
    theme(legend.position='bottom') + 
    guides(color = guide_legend(nrow = 1))
)
dev.off()




png(paste0(dropbox_dir, "/Figures/NutrientExperiment2/Buoys/Temp/", "Temp_GridPlot1", ".png"), units = 'in', width=20, height=16, res=200)

print(
  ggplot(aes(x=Datetime_UTC, y=Temp_C, color=Depth), data=Temp_df_clean2) +
    geom_vline(xintercept=fert_posix, linetype="dashed", color = "green", size=0.5) + 
    geom_point(size = 0.5) +
    geom_path(size = 0.5) +
    scale_colour_manual(values = brewer.pal(10, 'RdYlBu')) + 
    facet_grid(Depth~Site) + 
    theme_bw() + 
    theme(legend.position='bottom') + 
    guides(color = guide_legend(nrow = 1))
)
dev.off()


png(paste0(dropbox_dir, "/Figures/NutrientExperiment2/Buoys/Temp/", "Temp_SurfBot", ".png"), units = 'in', width=7, height=8, res=200)

print(
  ggplot(aes(x=Datetime_UTC, y=Temp_C, color=Depth, group=Depth), data=Temp_df_clean2[which(Temp_df_clean2$Depth %in% c('0.5', '4.5')),]) +
    geom_vline(xintercept=fert_posix, linetype="dashed", color = "green", size=0.5) + 
    # geom_point(size = 0.5) +
    geom_path(size = 1) +
    scale_colour_manual(values = brewer.pal(10, 'RdYlBu')[c(1,9)]) + 
    facet_grid(rows=vars(Site)) + 
    theme_bw() + 
    theme(legend.position='bottom') + 
    guides(color = guide_legend(nrow = 1))
)
dev.off()



png(paste0(dropbox_dir, "/Figures/NutrientExperiment2/Buoys/Temp/", "Temp_AllDepths", ".png"), units = 'in', width=7, height=8, res=200)

print(
  ggplot(aes(x=Datetime_UTC, y=Temp_C, color=Depth, group=Depth), data=Temp_df_clean2) +
    geom_vline(xintercept=fert_posix, linetype="dashed", color = "green", size=0.5) + 
    # geom_point(size = 0.5) +
    geom_path(size = 1) +
    scale_colour_manual(values = brewer.pal(10, 'RdYlBu')) + 
    facet_grid(rows=vars(Site)) + 
    theme_bw() + 
    theme(legend.position='bottom') + 
    guides(color = guide_legend(nrow = 1))
)

dev.off()


print(
  ggplot(aes(x=Datetime_UTC, y=Temp_C, color=Depth, group=Depth), data=Temp_df_clean2[which(Temp_df_clean2$Datetime_UTC<= as.POSIXct('2019-07-07')),]) +
    geom_vline(xintercept=fert_posix, linetype="dashed", color = "green", size=0.5) + 
    # geom_point(size = 0.5) +
    geom_path(size = 1) +
    scale_colour_manual(values = brewer.pal(10, 'RdYlBu')) + 
    facet_grid(rows=vars(Site)) + 
    theme_bw() + 
    theme(legend.position='bottom') + 
    guides(color = guide_legend(nrow = 1))
)

png(paste0(dropbox_dir, "/Figures/NutrientExperiment2/Buoys/DO/", "DO_AllDepths", ".png"), units = 'in', width=7, height=8, res=200)

print(
  ggplot(aes(x=Datetime_UTC, y=DO_mgL, color=Depth, group=Depth), data=DO_df_clean2) +
    geom_vline(xintercept=fert_posix, linetype="dashed", color = "green", size=0.5) + 
    # geom_point(size = 0.5) +
    geom_path(size = 1) +
    scale_colour_manual(values = brewer.pal(10, 'RdYlBu')[c(1,4,9)]) + 
    facet_grid(rows=vars(Site)) + 
    theme_bw() + 
    theme(legend.position='bottom') + 
    guides(color = guide_legend(nrow = 1))
)

dev.off()

png(paste0(dropbox_dir, "/Figures/NutrientExperiment2/Buoys/Cond/", "Cond_AllDepths", ".png"), units = 'in', width=7, height=8, res=200)

print(
  ggplot(aes(x=Datetime_UTC, y=Cond_uScm, color=Depth, group=Depth), data=Cond_df_clean2) +
    geom_vline(xintercept=fert_posix, linetype="dashed", color = "green", size=0.5) + 
    # geom_point(size = 0.5) +
    geom_path(size = 1) +
    scale_colour_manual(values = brewer.pal(10, 'RdYlBu')[c(1,4,9)]) + 
    facet_grid(rows=vars(Site)) + 
    theme_bw() + 
    theme(legend.position='bottom') + 
    guides(color = guide_legend(nrow = 1))
)

dev.off()
