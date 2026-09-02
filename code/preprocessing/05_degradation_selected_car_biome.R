library(sf)
library(terra)
library(exactextractr)
library(parallel)
library(data.table)
library(tictoc)
library(tidyverse)
library(patchwork)
################################
#'## Read data
################################

#For Mapbiomas datasets use the Mapbiomas user toolkit
#https://code.earthengine.google.com/?accept_repo=users/mapbiomas/user-toolkit
#and download the following:
# - mapbiomas brazil collection pasture quality (v7) for the Amazon biome for the years 2018 and 2019 and put into data_raw_secundary/spatial/03_pasture_quality

if(packageVersion("exactextractr")<= "0.7.2") {
  warning("Needs higher/dev version due to bug https://github.com/isciences/exactextractr/issues/68")
  devtools::install_github("isciences/exactextractr")
}

source("code/src/rcode_EPL_clean_data.R")

MPB_meta <-read_csv('input/Mapbiomas/mapbiomas-brazil-collection-70-pasture-quality-amazonia-area.csv')
input_data <- st_read('intermediate_files/all_car_contained_cattle.geojson')
degradation_2019 <- terra::rast('input/Mapbiomas/mapbiomas-brazil-collection-70-pasture-quality-amazonia-2019.tif')
degradation_2018 <- terra::rast('input/Mapbiomas/mapbiomas-brazil-collection-70-pasture-quality-amazonia-2018.tif')

################################
#'## Clean meta
################################

## prep MPB meta
MPB_meta_clean <- MPB_meta %>% 
  dplyr::select(band, class , class_name) 

MPB_meta_clean

### different by year?
MPB_meta_clean %>% 
  distinct(class, class_name) %>% 
  add_count(class, class_name) %>% 
  arrange(n)

## final version:
MPB_meta_clean2 <- MPB_meta_clean %>% 
  distinct(class, class_name) %>% 
  dplyr::rename(deg_class = class)%>% 
  reframe(add_row(cur_data(), deg_class = 0, class_name = 'No Data'))

MPB_meta_clean2


################################
#'## 
################################

epl_ras_extract_table(degradation_2019,input_data[1:3,], cols_keep = c("cod_imovel",'IBGE_CODE', 'SIGLA_UF'))
degradation_car_2019 <- epl_ras_extract_table(degradation_2019,input_data, cols_keep = c("cod_imovel",'IBGE_CODE', 'SIGLA_UF'))
degradation_car_2018 <- epl_ras_extract_table(degradation_2018,input_data, cols_keep = c("cod_imovel",'IBGE_CODE', 'SIGLA_UF'))


#somehow the entire ds is duplicated..
ddegradation_car_2019 <- unique( degradation_car_2019[ , c('cod_imovel','value','IBGE_CODE', 'SIGLA_UF', 'sum_area') ] )
ddegradation_car_2018 <- unique( degradation_car_2019[ , c('cod_imovel','value','IBGE_CODE', 'SIGLA_UF', 'sum_area') ] )

ddegradation_car_2019$year <- 2019
ddegradation_car_2018$year <- 2018

deg<- rbind(ddegradation_car_2018,ddegradation_car_2019)

out_LUC <-  deg %>% 
  dplyr::rename(deg_class = value) %>% 
  left_join(MPB_meta_clean2, by = "deg_class") %>% 
  mutate(across(c(IBGE_CODE, deg_class), as.integer)) %>% 
  group_by(cod_imovel,deg_class,IBGE_CODE,SIGLA_UF,class_name) %>% 
  summarise(mean_area = mean(sum_area)) %>% 
  ungroup() %>%
  group_by(cod_imovel)%>%
  mutate(area_tot = sum(mean_area)) %>% 
  mutate(area_perc = round(100 * mean_area/area_tot,2)) %>% 
  
  ungroup() %>% 
  #rename(deg_class_id = value) %>% 
  mutate(year = '2018-2019') %>%
  relocate(year, .after = SIGLA_UF) 

length(unique(out_LU$cod_imovel))

st_write(out_LUC, "intermediate_files/pasture_deg_selected_car_201819.geojson")

# visual inspection
car_shape <- st_as_sf(input_data[133,])
car_shape
# crop raster
r2 <- crop(degradation_2019, car_shape)
# and plot it
r3 <- mask(r2, car_shape,touches=FALSE)

plot(r3)
r3_poly <-terra::as.polygons(r3)
# convert in sf object
r3_poly <-st_as_sf(r3_poly)
r3_poly$centroid <- r3_poly$geometry%>% st_centroid()


