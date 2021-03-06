library(dplyr)
library(tidyr)

Phyto_FullRecord <- readRDS(file=paste0(dropbox_dir, '/Data/Rdata/Phyto_FullRecord_NutExp1.rds'))

Zoo_FullRecord <-readRDS(file=paste0(dropbox_dir, '/Data/Rdata/Zoo_FullRecord_NutExp1.rds'))

pico_totals<- readRDS(file=paste0(dropbox_dir, '/Data/Rdata/Picos_FullRecord_NutExp1.rds'))

pico_totals_narrow <- pico_totals %>%
  dplyr::select(Date, Station, Total_picocyanobacteria, Total_bacteria) %>%
  group_by(Date, Station) %>%
  gather(key='Division', value='TOTAL_BV_um3PerLiter', 3:4)


#Phyto proessing

Phyto_totals_genus<- Phyto_FullRecord %>%
  dplyr::select(Date, Station, GENUS, TOTAL.BV) %>%
  group_by(Date, Station, GENUS) %>%
  dplyr::summarize(TOTAL.BV = sum(TOTAL.BV))

head(Phyto_totals_genus)

Phyto_totals_division<- Phyto_FullRecord %>%
  dplyr::select(Date, Station, DIVISION, TOTAL.BV) %>%
  group_by(Date, Station, DIVISION) %>%
  dplyr::summarize(TOTAL.BV = sum(TOTAL.BV))

head(Phyto_totals_division)

Phyto_totals_all<- Phyto_FullRecord %>%
  dplyr::select(Date, Station, TOTAL.BV) %>%
  group_by(Date, Station) %>%
  dplyr::summarize(TOTAL.BV = sum(TOTAL.BV))


#Zooplankton processing

Zoo_totals_genus<- Zoo_FullRecord %>%
  dplyr::select(Date, Station, genus, SpeciesBiomass_ugdwL) %>%
  group_by(Date, Station, genus) %>%
  dplyr::summarize(SpeciesBiomass_ugdwL = sum(SpeciesBiomass_ugdwL))

head(Zoo_totals_genus)


Zoo_totals_division<- Zoo_FullRecord %>%
  dplyr::select(Date, Station, division, SpeciesBiomass_ugdwL) %>%
  group_by(Date, Station, division) %>%
  dplyr::summarize(SpeciesBiomass_ugdwL = sum(SpeciesBiomass_ugdwL))


Zoo_totals_all<- Zoo_FullRecord %>%
  dplyr::select(Date, Station, SpeciesBiomass_ugdwL) %>%
  group_by(Date, Station) %>%
  dplyr::summarize(SpeciesBiomass_ugdwL = sum(SpeciesBiomass_ugdwL))

#Plotting

png(paste0(dropbox_dir, '/Figures/NutrientExperiment/Zoops/Zoops_AllGenus_TimeSeries.png'), units='in', width=8, height=6, res=400, bg='white')

print(
  ggplot(aes(x=Date, y=SpeciesBiomass_ugdwL, group=Station, colour=Station), data=Zoo_totals_genus) + 
  geom_point() +
  geom_path() + 
  facet_wrap(~genus) + 
  theme_bw()
)
  
dev.off()


png(paste0(dropbox_dir, '/Figures/NutrientExperiment/Zoops/Zoops_Totals_Division_TimeSeries.png'), units='in', width=8, height=3, res=400, bg='white')

print(
  ggplot(aes(x=Date, y=SpeciesBiomass_ugdwL, group=Station, colour=Station), data=Zoo_totals_division) + 
    geom_point() +
    geom_path() + 
    facet_wrap(~division) + 
    theme_bw()
)

dev.off()


png(paste0(dropbox_dir, '/Figures/NutrientExperiment/Zoops/Zoops_TotalsBiomass_TimeSeries.png'), units='in', width=5, height=3, res=400, bg='white')

print(
  ggplot(aes(x=Date, y=SpeciesBiomass_ugdwL, group=Station, colour=Station), data=Zoo_totals_all) + 
    geom_point() +
    geom_path() + 
    theme_bw()
)

dev.off()




#Picoplankton

png(paste0(dropbox_dir, '/Figures/NutrientExperiment/Picos/Picos_Totals_TimeSeries.png'), units='in', width=8, height=3, res=400, bg='white')

print(
  ggplot(aes(x=Date, y=TOTAL_BV_um3PerLiter, group=Station, colour=Station), data=pico_totals_narrow) + 
    geom_point() +
    geom_path() + 
    facet_wrap(~Division) + 
    theme_bw()
)

dev.off()




#Phytoplankton


png(paste0(dropbox_dir, '/Figures/NutrientExperiment/Phytos/Phytos_Totals_Genus_TimeSeries.png'), units='in', width=12, height=6, res=400, bg='white')

print(
  ggplot(aes(x=Date, y=TOTAL.BV, group=Station, colour=Station), data=Phyto_totals_genus) + 
    geom_point() +
    geom_path() + 
    facet_wrap(~GENUS) + 
    theme_bw() + 
    labs(y='Total Biovolume')
)

dev.off()



png(paste0(dropbox_dir, '/Figures/NutrientExperiment/Phytos/Phytos_Totals_Division_TimeSeries.png'), units='in', width=6, height=4, res=400, bg='white')

print(
  ggplot(aes(x=Date, y=TOTAL.BV, group=Station, colour=Station), data=Phyto_totals_division) + 
    geom_point() +
    geom_path() + 
    facet_wrap(~DIVISION) + 
    theme_bw() + 
    labs(y='Total Biovolume')
)

dev.off()


png(paste0(dropbox_dir, '/Figures/NutrientExperiment/Phytos/Phytos_TotalBiomass_TimeSeries.png'), units='in', width=5, height=3, res=400, bg='white')

print(
  ggplot(aes(x=Date, y=TOTAL.BV, group=Station, colour=Station), data=Phyto_totals_all) + 
    geom_point() +
    geom_path() + 
    theme_bw() + 
    labs(y='Total Biovolume')
)

dev.off()


