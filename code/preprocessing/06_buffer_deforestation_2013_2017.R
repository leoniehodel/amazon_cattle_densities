library(sf)
library(terra)
library(exactextractr)
library(parallel)
library(data.table)
library(tictoc)
library(tidyverse)
library("raster")
################################
#'## Read data
################################
if(packageVersion("exactextractr")<= "0.7.2") {
  warning("Needs higher/dev version due to bug https://github.com/isciences/exactextractr/issues/68")
  devtools::install_github("isciences/exactextractr")
}

source("code/src/rcode_EPL_clean_data.R")

MPB_meta <-read_csv('input/Mapbiomas/mapbiomas_transitions_v7_amazoina/mapbiomas-brazil-collection-70-amazonia-area.csv')

################################
#'## HYPERPARAMETERS
################################
input_data <- st_read('intermediate_files/all_car_contained_cattle.geojson')
buffer_size <- 10000
################################
#'## Clean meta
################################

## prep MPB meta
MPB_meta_clean <- MPB_meta %>% 
  dplyr::select(band, class, from_class, to_class) %>% 
  mutate(year = str_extract(band, "[0-9]+") %>% as.integer())

MPB_meta

### different by year?
MPB_meta_clean %>% 
  distinct(class, from_class, to_class, year) %>% 
  add_count(class, from_class, to_class) %>% 
  arrange(n)

## final version:
MPB_meta_clean2 <- MPB_meta_clean %>% 
  distinct(class, from_class, to_class) %>% 
  dplyr::rename(LUC_class = class)

MPB_meta_clean2

################################
#'## deforestation on property 
################################

CAR_buffer <- input_data %>% st_buffer(buffer_size)

yearfile<- c('2013_2014','2014_2015','2015_2016','2016_2017','2017_2018')
#0215-2016

process_raster <- function(raster_file, input_data) {
  # Read the raster
  print(raster_file)
  luc <- rast(raster_file) #
  
  # Create a spatially-representative color object
  rsrc <- sprc(luc)
  
  # Create a mosaic (assuming you want to mosaic multiple rasters)
  mo <- mosaic(rsrc)
  
  # Set the coordinate reference system
  crs(mo) <- "EPSG:4674"
  
  # Find polygons fully covered by the raster
  #covered_polygons <- input_data[st_within(input_data, st_as_sf(mo), sparse = FALSE), ]
  
  # Process the selected polygons
  LUC <- epl_ras_extract_table(mo, input_data, cols_keep = c("cod_imovel", 'IBGE_CODE', 'SIGLA_UF'))
  
  return(LUC)
}


for(i in 1:length(yearfile)){
  
  # read in raster
  raster_paths <-tibble(full_path = list.files("input/Mapbiomas/mapbiomas_transitions_v7_amazoina/",full.names = TRUE, pattern = yearfile[i]))  
  
  LUC = map(raster_paths$full_path, ~ process_raster(.x, input_data))
  LUC_combined <- bind_rows(LUC[sapply(LUC, nrow) > 0])
  unique(LUC_combined$cod_imovel)
  
  # luc <- map(raster_path[[1]][], terra::rast)
  # rsrc<- terra::sprc(luc)
  # mo <-mosaic(rsrc)
  # crs(mo)  <- "epsg:4674"
  # 
  # # calculte LUC for porperty boundaries
  # epl_ras_extract_table(mo,input_data[1:3,], cols_keep = c("cod_imovel",'IBGE_CODE', 'SIGLA_UF'))
  # LUC <- epl_ras_extract_table(mo,input_data, cols_keep = c("cod_imovel",'IBGE_CODE', 'SIGLA_UF'))
  LUC_combined <- unique( LUC_combined[ , c('cod_imovel','value','IBGE_CODE', 'SIGLA_UF', 'sum_area') ] )
  # 
  out_LUC <-  LUC_combined %>% 
    dplyr::rename(LUC_class = value) %>% 
    left_join(MPB_meta_clean2, by = "LUC_class") %>% 
    mutate(across(c(IBGE_CODE, LUC_class), as.integer)) %>% 
    group_by(cod_imovel) %>% 
    mutate(area_perc = round(100 * sum_area/sum(sum_area),3)) %>% 
    mutate(area_tot = sum(sum_area))%>% 
    ungroup() %>% 
    dplyr::rename(LUC_class_from = from_class,
           LUC_class_to = to_class,
           LUCclass_id = LUC_class) %>% 
    mutate(year =  yearfile[i]) 

  out_LUC
  out_LUC %>% filter(is.na(LUC_class_from)) #%>% summarise(sum(sum_area))
  st_write(out_LUC, paste0("intermediate_files/LUC_selected_car_",yearfile[i],".geojson"))
  
  #   deforestation in BUFFER region
  
  LUC_b = map(raster_paths$full_path, ~ process_raster(.x, CAR_buffer))
  LUC_b_combined <- bind_rows(LUC_b[sapply(LUC_b, nrow) > 0])
  LUC_b_combined <- unique( LUC_b_combined[ , c('cod_imovel','value','IBGE_CODE', 'SIGLA_UF', 'sum_area') ] )
  #
  
  out_LUC_b <-  LUC_b_combined %>% 
    dplyr::rename(LUC_class = value) %>% 
    left_join(MPB_meta_clean2, by = "LUC_class") %>% 
    mutate(across(c(IBGE_CODE, LUC_class), as.integer)) %>% 
    group_by(cod_imovel) %>% 
    mutate(area_perc = round(100 * sum_area/sum(sum_area),3)) %>% 
    mutate(area_tot = sum(sum_area))%>% 
    ungroup() %>% 
    dplyr::rename(LUC_class_from = from_class,
                  LUC_class_to = to_class,
                  LUCclass_id = LUC_class) %>% 
    mutate(year =  yearfile[i]) 
  
  out_LUC %>% filter(is.na(LUC_class_from)) #%>% summarise(sum(sum_area))
  st_write(out_LUC_b, paste0("intermediate_files/LUC_selected_car_buffer_",yearfile[i],".geojson"))
  gc()
  
}


out_LUC_b_check<-   LUC_b_combined %>% filter(value==303)
