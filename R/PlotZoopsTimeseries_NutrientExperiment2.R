library(dplyr)
library(tidyr)

Phyto_FullRecord <- readRDS(file=file.path(onedrive_dir, 'RData', 'NutrientExperiment2', 'Phytos', 'Phyto_FullRecord_NutExp2.rds'))

Zoo_FullRecord <-readRDS(file=file.path(onedrive_dir, 'RData', 'NutrientExperiment2', 'Zoops', 'Zoo_FullRecord_NutExp2.rds'))

# pico_totals<- readRDS(file=paste0(dropbox_dir, '/Data/Rdata_SSCN2/Picos_FullRecord_NutExp2.rds'))

# pico_totals_narrow <- pico_totals %>%
  # dplyr::select(Date, Station, Total_picocyanobacteria, Total_bacteria) %>%
  # group_by(Date, Station) %>%
  # gather(key='Division', value='TOTAL_BV_um3PerLiter', 3:4)


#Zooplankton processing

Zoo_totals_genus<- Zoo_FullRecord %>%
  distinct() %>%
  dplyr::select(Date, Station, genus, SpeciesBiomass_ugdwL) %>%
  group_by(Date, Station, genus) %>%
  dplyr::summarize(SpeciesBiomass_ugdwL = sum(SpeciesBiomass_ugdwL))

head(Zoo_totals_genus)

Zoo_totals_species_spread <- Zoo_FullRecord %>%
  distinct() %>%
  dplyr::select(Date, Station, genus, species, SpeciesBiomass_ugdwL) %>%
  replace_na(list(genus = "", species = "")) %>% 
  unite("genus_species", c("genus","species"), sep='_') %>%
  spread(key=genus_species, value=SpeciesBiomass_ugdwL) %>%
  group_by(Date, Station)
  

Zoo_totals_genus_spread <- Zoo_FullRecord %>%
  distinct() %>%
  dplyr::select(Date, Station, genus, SpeciesBiomass_ugdwL) %>%
  group_by(Date, Station, genus) %>%
  dplyr::summarize(SpeciesBiomass_ugdwL = sum(SpeciesBiomass_ugdwL)) %>%
  spread(key=genus, value=SpeciesBiomass_ugdwL)

Zoo_totals_division_spread <- Zoo_FullRecord %>%
  distinct() %>%
  dplyr::select(Date, Station, division, SpeciesBiomass_ugdwL) %>%
  group_by(Date, Station, division) %>%
  dplyr::summarize(SpeciesBiomass_ugdwL = sum(SpeciesBiomass_ugdwL)) %>%
  spread(key=division, value=SpeciesBiomass_ugdwL)


write.csv(Zoo_totals_division_spread, file=file.path(onedrive_dir, 'OutputData', 'NutrientExperiment2', 'Zoops', "Zooplankton_Division_SSCN2.csv"), row.names=F)

write.csv(Zoo_totals_genus_spread, file=file.path(onedrive_dir, 'OutputData', 'NutrientExperiment2', 'Zoops', "Zooplankton_Genus_SSCN2.csv"), row.names=F)

write.csv(Zoo_totals_species_spread, file=file.path(onedrive_dir, 'OutputData', 'NutrientExperiment2', 'Zoops', "Zooplankton_Species_SSCN2.csv"), row.names=F)

#Save R data files
saveRDS(Zoo_totals_division_spread , file=file.path(onedrive_dir, 'RData', 'NutrientExperiment2', 'Zoops', 'Zooplankton_Division_SSCN2.rds'))
saveRDS(Zoo_totals_genus_spread , file=file.path(onedrive_dir, 'RData', 'NutrientExperiment2', 'Zoops', 'Zooplankton_Genus_SSCN2.rds'))
saveRDS(Zoo_totals_species_spread , file=file.path(onedrive_dir, 'RData', 'NutrientExperiment2', 'Zoops', 'Zooplankton_Species_SSCN2.rds'))



Allgenus<-table(Zoo_totals_genus$genus)
Majorgenus<-names(which(Allgenus>50))

Zoo_totals_majorgenus <- filter(Zoo_totals_genus, genus %in% Majorgenus)


Zoo_totals_division<- Zoo_FullRecord %>%
  dplyr::select(Date, Station, division, SpeciesBiomass_ugdwL) %>%
  group_by(Date, Station, division) %>%
  dplyr::summarize(SpeciesBiomass_ugdwL = sum(SpeciesBiomass_ugdwL))


Zoo_totals_all<- Zoo_FullRecord %>%
  dplyr::select(Date, Station, SpeciesBiomass_ugdwL) %>%
  group_by(Date, Station) %>%
  dplyr::summarize(SpeciesBiomass_ugdwL = sum(SpeciesBiomass_ugdwL))


#Phyto proessing

Phyto_totals_genus<- Phyto_FullRecord %>%
  dplyr::select(Date, Station, GENUS, TOTAL.BV) %>%
  group_by(Date, Station, GENUS) %>%
  dplyr::summarize(TOTAL.BV = sum(TOTAL.BV))

head(Phyto_totals_genus)

AllgenusPhyto<-table(Phyto_totals_genus$GENUS)
MajorgenusPhyto<-names(which(AllgenusPhyto>15))

Phyto_totals_majorgenus <- filter(Phyto_totals_genus, GENUS %in% MajorgenusPhyto)

Phyto_totals_division<- Phyto_FullRecord %>%
  dplyr::select(Date, Station, DIVISION, TOTAL.BV) %>%
  group_by(Date, Station, DIVISION) %>%
  dplyr::summarize(TOTAL.BV = sum(TOTAL.BV))

head(Phyto_totals_division)

Phyto_totals_all<- Phyto_FullRecord %>%
  dplyr::select(Date, Station, TOTAL.BV) %>%
  group_by(Date, Station) %>%
  dplyr::summarize(TOTAL.BV = sum(TOTAL.BV)) %>%
  mutate(Station = factor(Station, levels(Zoo_FullRecord$Station)))



#Plotting


#colors
color.palette = colorRampPalette(c(viridis(6, begin=.2, end=.98), rev(magma(5, begin=.35, end=.98))), bias=1)
colors<-color.palette(length(unique(Zoo_totals_genus$Station)))
shapes<-rep(21:25, 5)


png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'Zoops', 'Zoops_AllGenus_TimeSeries.png'), units='in', width=8, height=6, res=400, bg='white')

print(
  ggplot(aes(x=Date, y=SpeciesBiomass_ugdwL, group=Station, colour=Station, shape=Station, fill=Station), data=Zoo_totals_genus) + 
    scale_shape_manual(values=shapes)  + 
    scale_fill_manual(values = colors) + 
    scale_colour_manual(values = colors) + 
    geom_vline(xintercept=fert_dates, linetype="dashed", color = "green", size=0.5) + 
    # geom_path(aes(color=Station), size=1.5) +
    # geom_point(size=3, aes(fill=Station, shape=Station)) +
  geom_point() +
  geom_path() +
  facet_wrap(~genus) + 
  theme_bw()
)
  
dev.off()


png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'Zoops', 'Zoops_CommonGenus_TimeSeries.png'), units='in', width=8, height=6, res=400, bg='white')

print(
  ggplot(aes(x=Date, y=SpeciesBiomass_ugdwL, group=Station, colour=Station, shape=Station, fill=Station), data=Zoo_totals_majorgenus) + 
    scale_shape_manual(values=rep(21:25, 5))  + 
    scale_fill_manual(values = colors) + 
    scale_colour_manual(values = colors) +
    geom_vline(xintercept=fert_dates, linetype="dashed", color = "green", size=0.5) + 
    # geom_path(aes(color=Station), size=1.5) +
    # geom_point(size=3, aes(fill=Station, shape=Station)) +
    geom_point() +
    geom_path() +
    facet_wrap(~genus) + 
    theme_bw()
)

dev.off()


png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'Zoops', 'Zoops_Totals_Division_TimeSeries.png'), units='in', width=8, height=3, res=400, bg='white')

print(
  ggplot(aes(x=Date, y=SpeciesBiomass_ugdwL, group=Station, colour=Station, fill=Station, shape=Station), data=Zoo_totals_division) + 
    scale_shape_manual(values=rep(21:25, 5))  + 
    scale_fill_manual(values = colors) + 
    scale_colour_manual(values = colors) + 
    geom_vline(xintercept=fert_dates, linetype="dashed", color = "green", size=0.5) +
    geom_point() +
    geom_path() + 
    facet_wrap(~division) + 
    theme_bw()
)

dev.off()


png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'Zoops', 'Zoops_TotalsBiomass_TimeSeries.png'), units='in', width=5, height=3, res=400, bg='white')

print(
  ggplot(aes(x=Date, y=SpeciesBiomass_ugdwL, group=Station, colour=Station, fill=Station, shape=Station), data=Zoo_totals_all) + 
    scale_shape_manual(values=rep(21:25, 5))  + 
    scale_fill_manual(values = colors) + 
    scale_colour_manual(values = colors) + 
    geom_vline(xintercept=fert_dates, linetype="dashed", color = "green", size=0.5) + 
    geom_point() +
    geom_path() + 
    theme_bw()
)

dev.off()




#Picoplankton

# png(paste0(dropbox_dir, '/Figures/NutrientExperiment2/Picos/Picos_Totals_TimeSeries.png'), units='in', width=8, height=3, res=400, bg='white')
# 
# print(
#   ggplot(aes(x=Date, y=TOTAL_BV_um3PerLiter, group=Station, colour=Station), data=pico_totals_narrow) + 
#     geom_point() +
#     geom_path() + 
#     facet_wrap(~Division) + 
#     theme_bw()
# )
# 
# dev.off()




#Phytoplankton


png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'Phytos', 'Phytos_Totals_Genus_TimeSeries.png'), units='in', width=12, height=6, res=400, bg='white')

print(
  ggplot(aes(x=Date, y=TOTAL.BV, group=Station, colour=Station), data=Phyto_totals_genus) +
    scale_shape_manual(values=rep(21:25, 5))  + 
    scale_fill_manual(values = colors) + 
    scale_colour_manual(values = colors) +
    geom_vline(xintercept=fert_dates, linetype="dashed", color = "green", size=0.5) + 
    geom_point() +
    geom_path() +
    facet_wrap(~GENUS) +
    theme_bw() +
    labs(y='Total Biovolume')
)

dev.off()



png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'Phytos', 'Phytos_CommonGenus_TimeSeries.png'), units='in', width=8, height=6, res=400, bg='white')

print(
  ggplot(aes(x=Date, y=TOTAL.BV, group=Station, colour=Station), data=Phyto_totals_majorgenus) +
    scale_shape_manual(values=rep(21:25, 5))  + 
    scale_fill_manual(values = colors) + 
    scale_colour_manual(values = colors) +
    geom_vline(xintercept=fert_dates, linetype="dashed", color = "green", size=0.5) + 
    # geom_path(aes(color=Station), size=1.5) +
    # geom_point(size=3, aes(fill=Station, shape=Station)) +
    geom_point() +
    geom_path() +
    facet_wrap(~GENUS) + 
    theme_bw() +
    scale_y_log10()
)

dev.off()



png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'Phytos', 'Phytos_Totals_Division_TimeSeries.png'), units='in', width=8, height=6, res=400, bg='white')

print(
  ggplot(aes(x=Date, y=TOTAL.BV, group=Station, colour=Station), data=filter(Phyto_totals_division, !is.na(DIVISION))) +
    scale_shape_manual(values=rep(21:25, 5))  + 
    scale_fill_manual(values = colors) + 
    scale_colour_manual(values = colors) +
    geom_vline(xintercept=fert_dates, linetype="dashed", color = "green", size=0.5) + 
    geom_point() +
    geom_path() +
    facet_wrap(~DIVISION) +
    theme_bw() +
    labs(y='Total Biovolume') +
    scale_y_log10()
)

dev.off()


png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'Phytos', 'Phytos_TotalBiomass_TimeSeries.png'), units='in', width=5, height=3, res=400, bg='white')

print(
  ggplot(aes(x=Date, y=TOTAL.BV, group=Station, colour=Station), data=Phyto_totals_all) +
    scale_shape_manual(values=rep(21:25, 5))  + 
    scale_fill_manual(values = colors) + 
    scale_colour_manual(values = colors) +
    geom_vline(xintercept=fert_dates, linetype="dashed", color = "green", size=0.5) + 
    geom_point() +
    geom_path() +
    theme_bw() +
    labs(y='Total Biovolume')
    # scale_y_log10()
)

dev.off()







png(file.path(onedrive_dir, 'Figures', 'NutrientExperiment2', 'Timeseries', 'Phytos_Zoops_TotalBiomass_TimeSeries.png'), units='in', width=4, height=5, res=400, bg='white')

phyto_ts<-  ggplot(aes(x=Date, y=TOTAL.BV/1000000000, group=Station, shape=Station), data=Phyto_totals_all) +
    scale_shape_manual(values=shapes)  + 
    scale_fill_manual(values = colors) + 
    scale_colour_manual(values = colors) +
    geom_vline(xintercept=fert_dates, linetype="dashed", color = "green", size=0.5) + 
    geom_path(aes(colour=Station)) +
    geom_point(aes(fill=Station), size=1.5) +
    theme_bw() +
    labs(y=expression(paste('Phyto biovolume (10'^'9', ' X ', mu, 'm'^'3', ' L'^'-1', ')'))) +
  theme(axis.title.x = element_blank()) + 
  theme(legend.position='none')
  



zoo_ts<-  ggplot(aes(x=Date, y=SpeciesBiomass_ugdwL, group=Station,shape=Station), data=Zoo_totals_all) + 
    scale_shape_manual(values=shapes)  + 
    scale_fill_manual(values = colors) + 
    scale_colour_manual(values = colors) + 
    geom_vline(xintercept=fert_dates, linetype="dashed", color = "green", size=0.5) + 
    geom_path(aes(colour=Station)) +
    geom_point(aes(fill=Station), size=1.5) +
    theme_bw() +
    labs(y=expression(paste('Zoo biomass (', mu, 'g d.w. L'^'-1', ')'))) +
    theme(axis.title.x = element_blank()) + 
  theme(legend.position='bottom')

# grid.arrange(phyto_ts, zoo_ts)


grid.newpage()
boxes<-grid.draw(rbind(ggplotGrob(phyto_ts), ggplotGrob(zoo_ts), size = "last"))


dev.off()



