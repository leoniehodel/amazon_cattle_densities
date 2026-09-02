library(sf)
library(data.table)
library(tidyverse)
library(scales)
library(reshape)
library(units)
library(ggmap)
library(openxlsx)

################################
#'## Read in SR data
################################
# The data is filtered for the years 2018-2019 here 
df <- st_read('intermediate_files/all_car_contained_cattle.geojson') %>% 
  dplyr::select(COD_IMOVEL= cod_imovel, IBGE_CODE, SIGLA_UF, imagedate, outline_id, n_cattle,n_cattle_sd ) #%>% filter(imagedate > '2018-01-01')
df$area <- st_area(df) #%>% drop_units()%>%
df <- df%>%mutate(area = round(area*0.0001,4))
 
agg_df <- aggregate(df$COD_IMOVEL, by=list(df$SIGLA_UF), FUN=length)
agg_df
length(unique(df$COD_IMOVEL))

################################
#'## Mean land use in the years 2018-2019 using MapBiomas v7 
################################

all_car_LU_2019<- st_read('intermediate_files/land_use_selected_car_20182019.geojson') %>% 
  dplyr::rename('COD_IMOVEL' = 'cod_imovel')%>% as.data.frame() %>% dplyr::select(COD_IMOVEL,  year, class,class_name, mean_area, area_tot, area_perc)
df_unique <- df[!duplicated(df$COD_IMOVEL), ]
df<- df_unique
selected_car_LU_2019 <- df %>% left_join(all_car_LU_2019,by='COD_IMOVEL')%>% as.data.frame() %>% dplyr::select(COD_IMOVEL, IBGE_CODE, SIGLA_UF, year, class,class_name, mean_area, area_tot, area_perc)

length(selected_car_LU_2019$COD_IMOVEL)
length(unique(selected_car_LU_2019$COD_IMOVEL))

#add levels
selected_car_LU_2019 <- within(selected_car_LU_2019, class <- factor(class, levels=names(sort(table(class), decreasing=TRUE))))

# print a col plot with all LU
ggplot(selected_car_LU_2019,aes(class_name,area_tot)) + 
  geom_col(na.rm=TRUE)+
  #scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))


# funtion to merge land use data with the selected car data
process_land_use <- function(data, luclass, new_col_prefix, target_df) {
  result <- data %>% filter(class == luclass)%>%
    group_by(COD_IMOVEL) %>%
    summarise(!!paste0(new_col_prefix, "_tot") := round(sum(mean_area * 0.0001),4),
               !!paste0(new_col_prefix, "_perc") := sum(area_perc)) %>%
    dplyr::select(COD_IMOVEL, !!paste0(new_col_prefix, "_tot"), !!paste0(new_col_prefix, "_perc"))

  print(paste0('Number of CAR with this LU: ', length(result$COD_IMOVEL)))
  target_df <- merge(target_df, result, by = "COD_IMOVEL",all.x = TRUE)
  print(paste0('Total CAR: ',length(target_df$COD_IMOVEL)))
  return(target_df)
}
#[1] "Number of CAR with this LU: 5293"
#[1] "Total CAR: 5298"

df <- process_land_use(selected_car_LU_2019, "15", "pasture_201819", df)
df <- process_land_use(selected_car_LU_2019, "3", "forest_201819", df)
# the function allows to combine various LU classes
#df <- process_land_use(selected_car_LU_2019, c('21','29',"48", "41"), "crop_201819", df)
# 203 properties have land use class 41, and 28 have soybeans (39)
df <- process_land_use(selected_car_LU_2019, '41', "crop_201819", df)
df <- process_land_use(selected_car_LU_2019, '39', "soy_201819", df)
df$crop_201819_tot <- df$crop_201819_tot+ df$soy_201819_tot
df$crop_201819_perc <- df$crop_201819_perc+ df$soy_201819_perc
#df <- process_land_use(selected_car_LU_2019, '12', "grass_201819", df)
#df <- process_land_use(selected_car_LU_2019, '4', "savanna_201819", df)


################################
#'## Pasture degradation 2018-2019
################################
# pasture deg 
deg_all_car <- st_read('intermediate_files/pasture_deg_selected_car_201819.geojson')

length(deg_all_car$cod_imovel)
length(unique(deg_all_car$cod_imovel))
# change to mean_area
selected_car_deg_2019 <- deg_all_car %>% as.data.frame() %>%  dplyr::rename(COD_IMOVEL=cod_imovel)%>%
  dplyr::select(COD_IMOVEL, IBGE_CODE, SIGLA_UF, year, mean_area,class_name, area_tot, area_perc)%>% filter(COD_IMOVEL %in% df$COD_IMOVEL)

length(unique(selected_car_deg_2019$COD_IMOVEL))

# 
sev_deg <- selected_car_deg_2019 %>%
  filter(class_name == "Severe degradation") %>% group_by(COD_IMOVEL)%>%
  summarize(sev_deg_perc_201819 = area_perc, sev_deg_sum_201819 = round(mean_area* 0.0001,4))

mod_deg <- selected_car_deg_2019 %>%
  filter(class_name == "Moderate degradation") %>%group_by(COD_IMOVEL)%>%
  summarize(mod_deg_perc_201819 = area_perc, mod_deg_sum_201819 = round(mean_area* 0.0001,4))

length(mod_deg$COD_IMOVEL)
length(unique(mod_deg$COD_IMOVEL))
#[1] 5298

df <- df %>%
  left_join(sev_deg, by = 'COD_IMOVEL') %>%
  left_join(mod_deg, by = 'COD_IMOVEL') %>%
  replace(is.na(.), 0)

df_unique <- df[!duplicated(df$COD_IMOVEL), ]
df<- df_unique
################################
#'## Deforestation and buffer 
################################
# Function to process deforestation and reforestation data
process_luc_selected_car <- function(year_range, class_from, class_to, target_df, LUC_type) {
  result <- st_read(paste0('intermediate_files/LUC_selected_car_', year_range, '.geojson')) %>% as.data.frame() %>% 
    dplyr::select(cod_imovel,LUC_class_from,LUC_class_to,sum_area,area_perc) %>%
    filter(LUC_class_from == class_from, LUC_class_to == class_to) %>%
    group_by(cod_imovel) %>%
    dplyr::reframe(!!paste0(LUC_type,'_perc_', year_range) := area_perc,!!paste0(LUC_type,'_tot_', year_range) := round(sum_area * 0.0001, 4)) %>%
    dplyr::select(COD_IMOVEL = cod_imovel, !!paste0(LUC_type,'_tot_', year_range),!!paste0(LUC_type,'_perc_', year_range))
  
  target_df <- merge(target_df, result, by = "COD_IMOVEL", all.x = TRUE)
  print(target_df)
  return(target_df)
}
df2<-df%>%dplyr::select(COD_IMOVEL)

# Process deforestation data for different years
defo_years <- c("2013_2014", "2014_2015", "2015_2016", "2016_2017",'2017_2018')
for (year in defo_years) {
  df2 <- process_luc_selected_car(year, "Forest Formation", "Pasture", df2, 'defo')
}

# Process reforestation data for different years
refo_years <- c("2013_2014", "2014_2015", "2015_2016", "2016_2017", '2017_2018')

for (year in refo_years) {
  df2 <- process_luc_selected_car(year, "Pasture", "Forest Formation", df2, 'refo')
}

forest_years <- c("2013_2014")
for (year in forest_years) {
  df2 <- process_luc_selected_car(year, "Forest Formation", "Forest Formation", df2, 'forest')
}

df2 <- df2%>%replace(is.na(.), 0)
df2$defo_perc_wo_refo_13_17 <- (df2$defo_perc_2013_2014+df2$defo_perc_2014_2015+df2$defo_perc_2015_2016+df2$defo_perc_2016_2017) -(df2$refo_perc_2013_2014+df2$refo_perc_2014_2015+df2$refo_perc_2015_2016+df2$refo_perc_2016_2017)
df2$defo_perc_13_17 <- (df2$defo_perc_2013_2014+df2$defo_perc_2014_2015+df2$defo_perc_2015_2016+df2$defo_perc_2016_2017) #-(df2$refo_perc_2013_2014+df2$refo_perc_2014_2015+df2$refo_perc_2015_2016+df2$refo_perc_2016_2017)

df2$defo_abs_13_17 <- (df2$defo_tot_2013_2014+df2$defo_tot_2014_2015+df2$defo_tot_2015_2016+df2$defo_tot_2016_2017)  

# include the individual years for deforestation 
df2<- df2%>%as.data.frame()%>% dplyr::select(COD_IMOVEL,
                                             defo_abs_13_17,
                                             defo_perc_13_17,
                                             defo_perc_2013_2014,
                                             defo_perc_2014_2015, 
                                             defo_perc_2015_2016,
                                             defo_perc_2016_2017,
                                             defo_tot_2013_2014,
                                             defo_tot_2014_2015,
                                             defo_tot_2015_2016,
                                             defo_tot_2016_2017,
                                             defo_perc_wo_refo_13_17,forest_tot_2013_2014,forest_perc_2013_2014 )

df <- left_join(df,df2, by = "COD_IMOVEL" )#, all.x = TRUE)

# Process buffer forest data for 2013
buffer_forest2013 <- st_read('intermediate_files/LUC_selected_car_buffer_2013_2014.geojson')%>%as.data.frame() %>%
  filter(LUCclass_id == 303) %>%
  group_by(cod_imovel) %>%
  dplyr::select(cod_imovel, buffer_forest_perc_2013 = area_perc, buffer_forest_tot_2013 = sum_area) %>%
  dplyr::summarise(buffer_forest_tot_2013 = sum(buffer_forest_tot_2013 * 0.0001), buffer_forest_perc_2013= sum(buffer_forest_perc_2013 * 0.0001)) %>%
  dplyr::rename(COD_IMOVEL = cod_imovel) %>%
  mutate(buffer_forest_perc_2013 , buffer_forest_tot_2013 = round(buffer_forest_tot_2013, 4))

# das muss irgendwie ein fehler beim berechnen sein, weil nur ein paar eintraege doppelt sind
duplicates <- buffer_forest2013 %>%
  group_by(COD_IMOVEL) %>%
  filter(n() > 1) %>%
  ungroup()

df <- left_join(df, buffer_forest2013, by = "COD_IMOVEL")
length(unique(df$COD_IMOVEL))
length(df$COD_IMOVEL)

# Process deforestation buffer data
process_luc_selected_car_buffer <- function(year_range, class_from, class_to, target_df, LUC_type) {
  result <- st_read(paste0('intermediate_files/LUC_selected_car_buffer_', year_range, '.geojson')) %>% as.data.frame() %>%
    dplyr::select(cod_imovel,LUC_class_from,LUC_class_to,sum_area,area_perc)%>%
    filter(LUC_class_from == class_from, LUC_class_to == class_to) %>%
    # buffer has to be summed up because several img outlines from mapbiomas coould be in the buffer
    group_by(cod_imovel) %>%
    dplyr::reframe(!!paste0(LUC_type,'_perc_', year_range) := sum(area_perc),!!paste0(LUC_type,'_tot_', year_range) := round(sum(sum_area) * 0.0001, 4)) %>%
    dplyr::select(COD_IMOVEL = cod_imovel, !!paste0(LUC_type,'_tot_', year_range),!!paste0(LUC_type,'_perc_', year_range))  
  target_df <- merge(target_df, result, by = "COD_IMOVEL", all.x = TRUE)
  return(target_df)
}
df3<-df%>%dplyr::select(COD_IMOVEL)
defo_years <- c("2013_2014","2014_2015", "2015_2016", "2016_2017")
for (year in defo_years) {
  df3 <- process_luc_selected_car_buffer(year, "Forest Formation", "Pasture", df3, 'defo_buffer')
}
length(unique(df3$COD_IMOVEL))
df3 <- df3%>%  replace(is.na(.), 0)
df3$defo_buffer_perc_13_17 <- df3$defo_buffer_perc_2013_2014+df3$defo_buffer_perc_2014_2015+df3$defo_buffer_perc_2015_2016+df3$defo_buffer_perc_2016_2017 #- (df3$refo_perc_2013+df3$refo_perc_2014+df3$refo_perc_2015+df3$refo_perc_2016+df3$refo_perc_2017)
df3$defo_buffer_abs_13_17 <- df3$defo_buffer_tot_2013_2014+df3$defo_buffer_tot_2014_2015+df3$defo_buffer_tot_2015_2016+df3$defo_buffer_tot_2016_2017
df3<- df3%>%as.data.frame()%>% dplyr::select(COD_IMOVEL,defo_buffer_abs_13_17,defo_buffer_perc_13_17)

df <- left_join(df,df3, by = "COD_IMOVEL")
length(df$COD_IMOVEL)

################################
#'## Precipitation
################################

# these files have been prepossessed on Google Earth Engine using municipality borders
# check the downloaded files 

# and convert the units 
# in mm/ha
# https://developers.google.com/earth-engine/datasets/catalog/NASA_GPM_L3_IMERG_V06
pre20189 <- read_csv('input/abiotic/precipitation_muns_.csv') %>% dplyr::select(1:10) %>% mutate(precipitation_18_19 = precipitation ) %>% dplyr::select(CD_MUN,precipitation_18_19 )
#write.csv(pre20189, 'intermediate_files/precipitation_mm_he_munis_2018.csv')

all_car_ncattle_lu2019_lucin3ybuffer_deg<-df
# precipitation
perc <-pre20189 %>% dplyr::rename(IBGE_CODE = CD_MUN) %>% mutate(IBGE_CODE=as.character(IBGE_CODE),precipitation_2018_19 = as.double(precipitation_18_19))%>%
  dplyr::select(IBGE_CODE, precipitation_2018_19)
    
all_car_ncattle_perc <- all_car_ncattle_lu2019_lucin3ybuffer_deg %>% left_join(perc, by='IBGE_CODE')

################################
#'## Temperature
################################
# https://developers.google.com/earth-engine/datasets/catalog/JAXA_GCOM-C_L3_LAND_LST_V3
# temperature -change from K to Celcius
temp20189 <- read_csv('input/abiotic/temp_muns.csv')  %>% dplyr::select(1:7) %>% mutate(mean_temp_18_19 = LST_AVE-273.15) %>% dplyr::select(CD_MUN,mean_temp_18_19 )
#write.csv(temp20189, 'intermediate_files/temp_munis_2018_2019.csv')

temp <-temp20189 %>% dplyr::rename(IBGE_CODE = CD_MUN) %>% mutate(IBGE_CODE=as.character(IBGE_CODE),mean_temp_18_19 = as.double(mean_temp_18_19))%>%
  dplyr::select(IBGE_CODE, mean_temp_18_19)

all_car_ncattle_perc_tem <- all_car_ncattle_perc %>% left_join(temp, by='IBGE_CODE')

################################
#'## ABC
################################
# include credit on the muni level
a<-read.csv('intermediate_files/cust_all_allactividades_fno_2013_2017.csv') %>% dplyr::select(IBGE_CODE,abc_inv_all_2013_2017,credits_val_2013_2017)

a$agricultural_credits<-a$abc_inv_all_2013_2017 #/a$credits_val_2013_2017
#a$agricultural_credits<-a$credits_val_2013_2017

a$IBGE_CODE<-as.character(a$IBGE_CODE)
all_car_ncattle_perc_tem<-all_car_ncattle_perc_tem%>% left_join(a,by='IBGE_CODE')

################################
#'## historic stocking rates
################################
sr_2013 <- read_csv('input/ppm/map_pasture_2013.csv') %>% dplyr::rename(IBGE_CODE=CD_MUN, stocking_rate_2013 = stocking_rate) %>% 
  mutate(IBGE_CODE = as.character(IBGE_CODE)) %>% 
  dplyr::select(IBGE_CODE,stocking_rate_2013)
all_car_ncattle_perc_tem<-all_car_ncattle_perc_tem %>% left_join(sr_2013, by="IBGE_CODE" )

################################
#'## Population number 
################################
# include population number
# https://ftp.ibge.gov.br/Estimativas_de_Populacao/
pop_density_19<- read.xlsx('input/distances/POP2019_20220905.xlsx',sheet= 2,startRow = 2)
pop_density_18<- read.xlsx('input/distances/POP2018_20220905.xlsx',sheet= 2,startRow = 2)

pop_density_19 <- pop_density_19 %>%
  mutate(
    population_2019 = as.numeric(POPULAÇÃO.ESTIMADA),
    IBGE_CODE = paste0(COD..UF, COD..MUNIC)
  )

pop_density_18 <- pop_density_18 %>%
  mutate(
    population_2018 = as.numeric(POPULAÇÃO.ESTIMADA),
    IBGE_CODE = paste0(COD..UF, COD..MUNIC)
  )
#Warning message:
# ! NAs introduced by coercion --> bc of some footnotes in the data

pop_data <- full_join(pop_density_18[, c("IBGE_CODE", "population_2018")],
                      pop_density_19[, c("IBGE_CODE", "population_2019")],
                      by = "IBGE_CODE")

pop_data$population_2018_19 <- pop_data$population_2018+pop_data$population_2019/2
# read in municipality size
muns <- st_read('../../../../hodel_data/vectors/reference/brazil_municipalities/BR_Municipios_2020.shp')

pop_area_merged <- left_join(muns, pop_data, by = c("CD_MUN" = "IBGE_CODE"))
pop_area_merged <- pop_area_merged %>% as.data.frame() #%>%  

pop_area_merged<- pop_area_merged %>%   dplyr::mutate(
    pop_density_avg_18_19 = population_2018_19 / AREA_KM2,
    IBGE_CODE = CD_MUN
  ) %>% dplyr::select(pop_density_avg_18_19,IBGE_CODE)


all_car_ncattle_perc_tem<- all_car_ncattle_perc_tem%>%left_join(pop_area_merged,by='IBGE_CODE')

#all_car_ncattle_perc_tem = all_car_ncattle_perc_tem[,!(names(all_car_ncattle_perc_tem) %in% c('geometry.x.x','geometry.x','geometry.x.y'))]



################################
#'## Distance to Slaughterhouses
################################
# from Trase.
all_car_ncattle_perc_tem$centroids <- all_car_ncattle_perc_tem$geometry%>% st_centroid()

sh_loc <- read.csv2('input/distances/2021-02-25-br_beef_logistics_map_v2.csv') %>% drop_na(LONG)%>% st_as_sf(coords=c('LONG','LAT'), crs=4674)
sifs <- sh_loc %>% filter(INSPECTION_LEVEL=='SIF', STATE %in% c('AC','AM','PA','RO'), STATUS == "ATIVO") 
sifs <- st_transform(sifs, 4674)
dist <- st_distance(all_car_ncattle_perc_tem$centroids, sifs)

all_car_ncattle_perc_tem$nearest_fedSh <- apply(dist,1,min)

# include also SH form other states
#all_sh <- sh_loc %>% filter(STATE %in% c('AC','AM','PA','RO'), STATUS == "ATIVO")
#dist <- st_distance(all_car_ncattle_perc_tem$centroids, all_sh)
#all_car_ncattle_perc_tem$nearest_Sh <- apply(dist,1,min)
# 31 features

################################
#'## ZDC
################################

mun_marketshareall <- read.csv('input/distances/Levyetal2023_muni.csv')%>% mutate(IBGE_CODE=as.character(code_ibge))%>% dplyr::select(IBGE_CODE,year,g4_ms) %>%
  filter(year %in% c('2013','2014','2015','2016','2017','2018'))%>% group_by(IBGE_CODE)%>% #replace(is.na(.), 0) %>%
  summarize(mean_mun_msg4_1317= mean(g4_ms)) 

all_car_ncattle_perc_tem<-  all_car_ncattle_perc_tem%>% left_join(mun_marketshareall, by='IBGE_CODE')

################################
#'## save the variables in intermediate_files folder
################################
st_write(all_car_ncattle_perc_tem,"intermediate_files/regression_pars_final_raw.geojson" )
# Writing 5301 features with 45 fields and geometry type Polygon.

