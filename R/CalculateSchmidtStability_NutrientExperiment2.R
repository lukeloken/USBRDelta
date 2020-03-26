

#Use temperature data to calculate schmidt stability
library(rLakeAnalyzer)

Temp_df_clean2 <- readRDS(file=file.path(onedrive_dir, 'Rdata', 'NutrientExperiment2', 'Buoy', 'Buoy_Temp_cleaned.rds'))

#Prep Temp data
Temp_df_clean2 <- Temp_df_clean2 %>%
  mutate(Datetime_PDT = Datetime_UTC)
attributes(Temp_df_clean2$Datetime_PDT)$tzone = 'America/Los_Angeles'
Temp_df_clean2$Datetime_PDT_round = round_date(Temp_df_clean2$Datetime_PDT, unit="5 minutes")


#load hypso data
Depth_df <- readRDS(file=file.path(onedrive_dir, 'Rdata', 'HypsoCurveNL74.rds'))
#Vectors of depth and area
bthA = Depth_df$Area_m2
bthD = Depth_df$Depth_m

#Look at data
# ggplot(Temp_df_clean2, aes(x=Datetime_PDT_round, y=Temp_C, col=Site)) +
#   facet_grid(Depth~Site)+
#   geom_path()

# ############################
#Calculate schmidt stability
# ############################



buoy_names<-unique(Temp_df_clean2$Site)

strat.list<-list()
buoy_nu <- 5
for (buoy_nu in 1:length(buoy_names)){
  
  site_name <- buoy_names[buoy_nu]
  
  #prepare temp data
  temp_buoy <- Temp_df_clean2 %>%
    dplyr::filter(Site == site_name) %>%
    tidyr::drop_na(Temp_C)
  
  temp_times = temp_buoy$Datetime_PDT_round
  temp_depths=as.numeric(temp_buoy$Depth)
  temp_values =temp_buoy$Temp_C
  
  wrt1<-spread(temp_buoy[c('Datetime_PDT_round', 'Depth', 'Temp_C')], key=Depth, value=Temp_C, fill=NA )
  wrt3<-wrt1[which(!is.na(rowSums(wrt1[,2:10]))),]
  
  
  sta<-apply(wrt3[,2:10], 1, function (x) schmidt.stability(wtr=x,
                                                            depths=as.numeric(names(wrt3[,2:10])),
                                                            bthA=bthA, bthD=bthD))
  # sta2<-apply(wrt3[,c(2,10)], 1, function (x) schmidt.stability(wtr=x,
  # depths=as.numeric(names(wrt3[,c(2,10)])),
  # bthA=bthA, bthD=bthD))
  
  strat_df <-data.frame(Datetime_UTC_round = wrt3[,1],
                        Schmidt = sta)%>%
    # arrange(Datetime_UTC_round) %>%
    filter(!is.na(Schmidt))
  
  strat_df$Schmidt[strat_df$Schmidt<=.1] <- 0.1
  
  strat.list[[buoy_nu]] <- strat_df
  
  
  # plot(strat_df$Datetime_UTC_round, strat_df$Schmidt, type='p', cex=.1, pch=16)
  # abline(h=06, col='red')
  # 
  # strat_df$Schmidt_roll <- roll_mean(strat_df$Schmidt, n=7, align='center', fill=NA)
  # 
  # points(strat_df$Datetime_UTC_round, strat_df$Schmidt_roll, type='l', cex=5, col='blue')

  
}
names(strat.list) <- buoy_names

strat_df <- ldply(strat.list, data.frame, .id='Site')


saveRDS(strat_df, file=file.path(onedrive_dir, 'RData', 'NutrientExperiment2', 'Buoy', 'Schmidt_Stability.rds'))

write.table(strat_df, file=file.path(onedrive_dir, 'OutputData', 'NutrientExperiment2', 'Buoy', 'Schmidt_Stability.csv'), row.names=F, sep=',')
