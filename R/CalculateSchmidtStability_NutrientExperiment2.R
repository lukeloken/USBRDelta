

#Use temperature data to calculate schmidt stability
library(rLakeAnalyzer)
library(RColorBrewer)

#Huey buoy data
huey_df_distinct <- readRDS(file=file.path(onedrive_dir, 'RData', 'NutrientExperiment2', 'USGSBuoys', 'Huey_cleaned.rds'))

YSI_AllDepths <- readRDS(file=file.path(onedrive_dir, 'RData', 'NutrientExperiment2', 'YSIProfiles_AllDepths.rds'))


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
  
  strat_df <-data.frame(Datetime_PDT_round = wrt3[,1],
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



attributes(huey_df_distinct$DateTime)$tzone <- 'America/Los_Angeles'

huey_withstrat <- strat_df %>%
  filter(Site == 'NL74') %>%
  rename(DateTime = Datetime_PDT_round) %>%
  full_join(huey_df_distinct) %>%
  arrange(DateTime) %>%
  mutate(Date = as.Date(DateTime, tz='America/Los_Angeles'))

head(huey_withstrat)

full_dates <- huey_withstrat %>%
  group_by(Date) %>%
  filter(!is.na(Primary.Power)) %>%
  summarize_at(vars(Temperature:ODO, Turbidity., Chlorophyll.ug.L, 
                    BGA.PC.ug.L, Nitrate.uM, Schmidt), funs(sum(!is.na(.)))) %>%
  filter(Chlorophyll.ug.L >86.4, Schmidt >43.2)

huey_daily <- huey_withstrat %>%
  group_by(Date) %>%
  filter(!is.na(Primary.Power)) %>%
  summarize_at(vars(Temperature:ODO, Turbidity., Chlorophyll.ug.L, 
                    BGA.PC.ug.L, Nitrate.uM, Schmidt), 
               list(mean=mean, max=max,
                    Q3 = quantile), probs = c(0.75), na.rm=T) 

huey_daily_full<- huey_daily %>%
  right_join(select(full_dates, Date))

Q3_plot <- ggplot(huey_daily_full, aes(x=Schmidt_Q3, y=Chlorophyll.ug.L_Q3)) +
  geom_point(size=2, col='darkgreen') +
  theme_bw() +
  labs(x="Schmidt stability (Q3)",
       y="Chl a (Q3)") +
  ggtitle ('Daily 75% quantile')

print(Q3_plot)

mean_plot <- ggplot(huey_daily_full, aes(x=Schmidt_mean, y=Chlorophyll.ug.L_mean)) +
  geom_point(size=2, col='darkgreen') +
  theme_bw() +
  labs(x="Schmidt stability (mean)",
       y="Chl a (mean)") +
  ggtitle ('Daily mean')

print(mean_plot)


max_plot <- ggplot(huey_daily_full, aes(x=Schmidt_max, y=Chlorophyll.ug.L_max)) +
  geom_point(size=2, col='darkgreen') +
  theme_bw() +
  labs(x="Schmidt stability (max)",
       y="Chl a (max)") +
  ggtitle ('Daily max')

print(max_plot)


png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'Buoys', 'DailySchmidt_DailyChla_NL74.png'), width=3, height=9, units='in', res=200)

grid.newpage()
plots<-grid.draw(rbind(ggplotGrob(mean_plot), ggplotGrob(max_plot), ggplotGrob(Q3_plot), size = "last"))

dev.off()




#3 panel scatterplot
#drivers of daily max chlA

chla_no3 <- ggplot(huey_daily_full, aes(x=Nitrate.uM_mean, y=Chlorophyll.ug.L_Q3)) +
  geom_point(size=2, col='darkgreen') +
  theme_bw() +
  labs(x=expression(paste("daily mean nitrate (", mu, "M)")),
       y=expression(paste("daily 75"^"th", " Quantile Chl ", italic(a), " (", mu, 'g L'^"-1", ")"))) 
  # ggtitle ('Daily max')

print(chla_no3)

chla_turb <- ggplot(huey_daily_full, aes(x=Turbidity._mean, y=Chlorophyll.ug.L_Q3)) +
  geom_point(size=2, col='darkgreen') +
  theme_bw() +
  labs(x=expression(paste("daily mean turbidity (FNU)")),
       y='')
       # y=expression(paste("daily 75"^"th", " Quantile Chl ", italic(a), " (", mu, 'g L'^"-1", ")"))) 

print(chla_turb)

chla_temp <- ggplot(huey_daily_full, aes(x=Temperature_mean , y=Chlorophyll.ug.L_Q3)) +
  geom_point(size=2, col='darkgreen') +
  theme_bw() +
  labs(x=expression(paste("daily mean temperature (", degree, " C)")),
       y='')
# y=expression(paste("daily 75"^"th", " Quantile Chl ", italic(a), " (", mu, 'g L'^"-1", ")"))) 

print(chla_temp)

chla_strat <- ggplot(huey_daily_full, aes(x=Schmidt_mean, y=Chlorophyll.ug.L_Q3)) +
  geom_point(size=2, col='darkgreen') +
  theme_bw() +
  labs(x=expression(paste("daily mean Schmidt stability (J m"^"2", ")")),
       y='')
       # y=expression(paste("daily 75"^"th", " Quantile Chl ", italic(a), " (", mu, 'g L'^"-1", ")"))) 
  # ggtitle ('Daily max')

print(chla_strat)

chla_predictors_scatter_withtemp <- grid.arrange(chla_no3, chla_turb, chla_temp, chla_strat, nrow=1)
chla_predictors_scatter <- grid.arrange(chla_no3, chla_turb, chla_strat, nrow=1)

ggsave(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'Buoys', 'DailyChlA_Drivers_3panel.png'),  chla_predictors_scatter, width=9, height=3, units='in')

ggsave(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'Buoys', 'DailyChlA_Drivers_4panel.png'),  chla_predictors_scatter_withtemp, width=12, height=3, units='in')



strat_turb <- ggplot(huey_daily_full, aes(x=Schmidt_mean, y=Turbidity._mean)) +
  geom_point(size=2, col='darkgreen') +
  theme_bw() +
  labs(x=expression(paste("daily mean Schmidt stability (J m"^"2", ")")),
       y='')



chla_model <- lm(Chlorophyll.ug.L_Q3 ~ + Nitrate.uM_mean + Turbidity._mean + Schmidt_mean , data=huey_daily)

summary(chla_model)
anova(chla_model)



huey_daily_sample <- huey_daily %>%
  filter(Date %in% event_df$Date) %>%
  select(Date, Turbidity._mean, Schmidt_mean, Chlorophyll.ug.L_Q3, Nitrate.uM_mean) %>%
  mutate(Fert = ifelse(Date %in% fert_dates , 'Fertilized', 'Not fertilized'),
         Strat = case_when(Schmidt_mean >=15 ~ 'Strong',
                           Schmidt_mean <15 & Schmidt_mean>7 ~ 'Medium',
                           Schmidt_mean <=7 ~ 'Weak')) %>%
  select(Date, Fert, Strat)



YSI_spatialaverage <- YSI_AllDepths %>%
  select(-Site, -DateTime) %>%
  group_by(Date, Depth_m) %>%
  filter(ChlA_ugL <= 10) %>%
  filter(Depth_m > 0.2) %>%
  summarize_all(mean, na.rm=T) %>%
  full_join(huey_daily_sample) %>%
  filter(!is.na(Strat), !is.na(Fert)) %>%
  mutate(Strat = factor(Strat, c('Weak', 'Medium', 'Strong')))

  
# date_colors <- sample(c(brewer.pal(8, 'Paired'), brewer.pal(8, 'Accent'), brewer.pal(8, 'Pastel2'))) 

date_colors <- c("#FFFF99", "#1F78B4", "#E6F5C9", "#A6CEE3", "#F1E2CC", "#BEAED4",
                 "#33A02C", "#CBD5E8", "#7FC97F", "#FFF2AE", "#FB9A99", "#FDC086",
                 "#B3E2CD", "#CCCCCC", "#FDBF6F", "#E31A1C", "#BF5B17", "#386CB0",
                 "#B2DF8A", "#FDCDAC", "#FF7F00", "#F4CAE4", "#F0027F", "#666666")

  YSIprofiles_byStratandFert <- ggplot (YSI_spatialaverage, aes(y=ChlA_ugL, x=Depth_m, group=Date, color=as.factor(Date))) +
    facet_grid(Fert~Strat) + 
    labs(y=expression(paste('Chl ', italic(a), ' (', mu, 'g L'^'-1', ')')), x='Depth (m)') +
    # scale_y_sqrt(limits=c(0,NA)) + 
    scale_y_continuous(limits=c(0,NA)) +
    scale_x_reverse(limits=c(10.5,0), expand=c(0,0)) + 
    # scale_shape_manual(values=rep(21:25, 5))  + 
    # scale_fill_manual(values = colors) + 
    scale_color_manual(values = date_colors) + 
    scale_fill_manual(values = date_colors) + 
    # geom_path(size=1.5) + 
    geom_point(size=1, alpha=.7) + 
    geom_smooth(se=F, size=1.5) + 
    ggtitle('Stratification') +
    theme_bw() +
    theme(plot.title = element_text(hjust=0.5))  + 
    theme(legend.position='none') +
    coord_flip() +
    theme(strip.background=element_rect(fill=NA, color=NA),
          plot.title=element_text(size=10, hjust=0.5))

  print(YSIprofiles_byStratandFert)
    
  ggsave(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'YSIProfiles', 'VerticalChlA_ByStratAndFert_facet.png'),  YSIprofiles_byStratandFert, width=6, height=5, units='in')
  
  
  