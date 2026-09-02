library(sf)
library(terra)
library(exactextractr)
library(parallel)
library(data.table)
library(tictoc)
library(tidyverse)
library(patchwork)

packageVersion("exactextractr")
#[1] ‘0.10.0’

#For Mapbiomas datasets use the Mapbiomas user toolkit
#https://code.earthengine.google.com/?accept_repo=users/mapbiomas/user-toolkit
#and download the following:
# - mapbiomas brazil collection coverage (v7) for the states of Acre, Amazonas, Rondonia and Para for the years 2018 and 2019 and put into data_raw_secundary/spatial/01_coverage
# - mapbiomas brazil collection transitions (v7) for the states of Acre, Amazonas, Rondonia and Para for the years 2013-2014 until 2018-2019 and put into data_raw_secundary/spatial/02_transitions


################################
#'## Read data
################################
if(packageVersion("exactextractr")<= "0.7.2") {
  warning("Needs higher/dev version due to bug https://github.com/isciences/exactextractr/issues/68")
  devtools::install_github("isciences/exactextractr")
}

source("code/src/rcode_EPL_clean_data.R")

# Read cattle count files
input_data <- st_read('intermediate_files/all_car_contained_cattle.geojson')

# Read in raw secondary spatial maps
lu_2019 <- terra::rast('input/Mapbiomas/mapbiomas-brazil-collection-70-amazonia-2019.tif')
lu_2018 <- terra::rast('input/Mapbiomas/mapbiomas-brazil-collection-70-amazonia-2018.tif')
MPB_meta <-read_csv('input/Mapbiomas/mapbiomas-brazil-collection-70-amazonia-area.csv')
crs(lu_2019)
################################
#'## Clean meta
################################

## prep MPB meta
MPB_meta_clean <- MPB_meta %>% 
  dplyr::select(band, class , class_name) %>% 
  mutate(year = str_extract(band, "[0-9]+") %>% as.integer())

MPB_meta_clean<-MPB_meta_clean%>% dplyr::select(class, class_name)
# Check differences by year
MPB_meta_clean %>% 
  distinct(class, class_name) %>% 
  add_count(class, class_name) %>% 
  arrange(n)


################################
#'## extract table
################################

epl_ras_extract_table(lu_2019,input_data[1:3,], cols_keep = c("cod_imovel",'IBGE_CODE', 'SIGLA_UF'))

car_2019 <- epl_ras_extract_table(lu_2019,input_data, cols_keep = c("cod_imovel",'IBGE_CODE', 'SIGLA_UF'))
car_2018 <- epl_ras_extract_table(lu_2018,input_data, cols_keep = c("cod_imovel",'IBGE_CODE', 'SIGLA_UF'))

#somehow the entire ds is duplicated..
dcar_2019 <- unique( car_2019[ , c('cod_imovel','value','IBGE_CODE', 'SIGLA_UF', 'sum_area') ] )
dcar_2018 <- unique( car_2018[ , c('cod_imovel','value','IBGE_CODE', 'SIGLA_UF', 'sum_area') ] )
dcar_2019$year <- 2019
dcar_2018$year <- 2018

# Combine data for both years
dcar_both<- rbind(dcar_2018,dcar_2019)

# Join with MPB meta
out_LU <-  dcar_both %>% 
  dplyr::rename(class = value) %>% 
  left_join(MPB_meta_clean, by = "class") %>% 
  mutate(across(c(IBGE_CODE, class), as.integer)) %>% 
  group_by(cod_imovel,class,IBGE_CODE,SIGLA_UF,class_name) %>% 
  summarise(mean_area =mean(sum_area), )%>%
  ungroup() %>%
  group_by(cod_imovel)%>%
  mutate(area_tot = sum(mean_area)) %>% 
  mutate(area_perc = round(100 * mean_area/area_tot,2)) %>% 
  ungroup() %>% 
  #rename(deg_class_id = value) %>% 
  mutate(year = '2018-2019') %>%
  relocate(year, .after = SIGLA_UF) 

length(unique(out_LU$cod_imovel))
st_write(out_LU, "intermediate_files/land_use_selected_car_20182019.geojson")

# visual inspection
car_shape <- st_as_sf(input_data[1,])
car_shape
# # crop raster 
r2 <- crop(lu_2019, car_shape)
# # and plot it 
r3 <- mask(r2, car_shape,touches=FALSE)
# plot(r3)
r3_poly <-terra::as.polygons(r3)
# # convert in sf object
r3_poly <-st_as_sf(r3_poly)
plot(r3_poly)



################################
#'## extract table
################################


