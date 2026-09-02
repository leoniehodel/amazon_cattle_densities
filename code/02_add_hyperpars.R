library(sf)
library(tidyverse)
library(arm)
library(units)
library(texreg)
library(vtable)

################################
#'## HYPERPARAMETERS
################################
min_area_car = 5
min_pasture = 4
max_area_car  = 800
min_ncattle = 4
#min_ncattle = 1
sigma_cut = 0.90
#CV_cut = 0.8
min_prop_mun = 10
sr_max = 10
scale_factor = 1.3
################################
#'## Read in data
################################

output<- 'regression_vars_db_v2.csv'

#new regession pars added like population number
ds_r<-st_read('intermediate_files/regression_pars_final_raw.geojson')
ds_r$agricultural_credits
# this municipality will be excluded 
ds_r[ds_r$IBGE_CODE == 1504703,]
# for figure 3
ds_r[ds_r$COD_IMOVEL == 'AC-1200054-CC52E27E6FF646FD865EBF6D22B83A8A',]

# filter out the newest images (the rest wil be filtered out in the regression analysis script)
ds_r_filtered <- ds_r %>% filter( imagedate < '2021-01-01')
agg_df <- aggregate(ds_r_filtered$COD_IMOVEL, by=list(ds_r_filtered$SIGLA_UF), FUN=length)
ds_r_filtered$forest_201819_perc
ds_r_clean<- ds_r_filtered  %>% dplyr::select(SIGLA_UF,
                                     n_cattle,
                                     n_cattle_sd,
                                     pasture_201819_tot,
                                     pasture_201819_perc,
                                     defo_abs_13_17,
                                     defo_perc_13_17,
                                     defo_perc_wo_refo_13_17,
                                     defo_buffer_abs_13_17,
                                     defo_buffer_perc_13_17,
                                     mod_deg_sum_201819,
                                     mod_deg_perc_201819,
                                     sev_deg_sum_201819,
                                     sev_deg_perc_201819,
                                     crop_201819_tot,
                                     crop_201819_perc,
                                     agricultural_credits,
                                     agricultural_credits_ratio,
                                     defo_perc_2013_2014,
                                     defo_perc_2014_2015, 
                                     defo_perc_2015_2016,
                                     defo_perc_2016_2017,
                                     defo_tot_2013_2014,
                                     defo_tot_2014_2015,
                                     defo_tot_2015_2016,
                                     defo_tot_2016_2017,
                                     forest_tot_2013_2014,
                                     forest_perc_2013_2014,
                                     forest_201819_perc,
                                     refo_perc_13_17,
                                     mean_mun_msg4_1317,
                                     mean_mun_msG4_Tac_1317,
                                     area,
                                     buffer_forest_tot_2013,
                                     buffer_forest_perc_2013,
                                     precipitation_2018_19,
                                     mean_temp_18_19,
                                     pop_density_avg_18_19,
                                     nearest_fedSh,
                                     stocking_rate_2013,
                                     IBGE_CODE,
                                     outline_id, imagedate)

#ds_r_clean$area<- st_area(ds_r_clean) %>% drop_units()

ds_r_clean<- ds_r_clean%>% as.data.frame()  %>% distinct()
ds_r_clean<- ds_r_clean[ds_r_clean$n_cattle_sd < quantile(ds_r_clean$n_cattle_sd, sigma_cut),]



ds<-ds_r_clean
ds$nearest_fedSh_km <- ds$nearest_fedSh/1000 # also in km
mean(ds$nearest_fedSh)
mean(ds$nearest_fedSh_km)
ds$pop_density_avg_18_19 <- ds$pop_density_avg_18_19
ds$buffer_forest_tot_2013 <- ds$buffer_forest_tot_2013 #*0.01
# calculate the mean of the five years
ds$agricultural_credits_1000R <- ds$agricultural_credits*0.001/5  #  in 1000R$ devived by 5 years
mean(ds$agricultural_credits_1000R)

ds$nearest_fedSh_km <-ds$nearest_fedSh_km /100
# in 100km
mean(ds$nearest_fedSh_km) 
# [1] 0.8488004
#dsdrop$defo_perc_13_17_ha <-dsdrop$defo_perc_13_17_ha
mean(ds$agricultural_credits_1000R)
# in 1'000'000
ds$agricultural_credits_1000R <-ds$agricultural_credits_1000R/1000
mean(ds$agricultural_credits_1000R)
#[1] 0.7938942 



################################
#'## Preprocess
################################
ds$area

dsfilter<-ds%>%
  mutate(area = round(area,3), stocking_rate = (n_cattle*scale_factor)/pasture_201819_tot,  buffer_forest_perc_2013 = buffer_forest_perc_2013*100) %>%
  filter(area > min_area_car, area< max_area_car, n_cattle > min_ncattle, pasture_201819_tot>min_pasture) %>%
  group_by(IBGE_CODE) %>% filter(n() >min_prop_mun) %>% ungroup

# filter out sr higher than 10, these areas have been checked and are artefacts  
# confinement systems are therefore most likely not included

#dsfilter<- dsfilter  %>% replace(is.na(.), 0) %>% filter(stocking_rate<sr_max)

################################
#'##  Drop variables not used in the regression 
###############################
drop <- c("COD_IMOVEL","date",
          'geometry','cv' )
dsdrop = dsfilter[,!(names(dsfilter) %in% drop)] 
dsdrop$stocking_rate <- round(dsdrop$stocking_rate,3)
dsdrop$stocking_rate
# rename variables in dsdrop

dsdrop<-dsdrop %>%  dplyr::select(
  State= SIGLA_UF,
  n_cattle,
  n_cattle_sd,
  area_ha = area,
  pastue_tot_201819_ha = pasture_201819_tot,
  stocking_rate_ha = stocking_rate, 
  municipality =IBGE_CODE,
  forest_tot_2013_2014_ha = forest_tot_2013_2014,
  forest_perc_2013_2014,
  defo_perc_2013_14 = defo_perc_2013_2014,
  defo_perc_2014_15= defo_perc_2014_2015, 
  defo_perc_2015_16= defo_perc_2015_2016,
  defo_perc_2016_17=defo_perc_2016_2017,
  defo_perc_13_17 = defo_perc_13_17,
  defo_buffer_abs_13_17,
  defo_buffer_perc_13_17,
  stocking_rate_2013,
  nearest_fedSh_km = nearest_fedSh_km, 
  imagedate,
 #other controls 
 buffer_forest_tot_2013,
 buffer_forest_perc_2013 = buffer_forest_perc_2013,
 precipitation_2018_19,
 mean_temp_18_19,
 pop_density_avg_18_19
 
)
write.csv(dsdrop, output, row.names = FALSE)

