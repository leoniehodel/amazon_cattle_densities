library(sf)
library(tidyverse)
library(terra)
################################
#'## Read data
################################

selected_car <- st_read('intermediate_files/all_car_contained.geojson') %>%
  st_make_valid()

selected_car_intersection <- st_read('intermediate_files/all_car_intersected.geojson') %>%
  st_make_valid()
selected_car$area <- st_area(selected_car)

# both these are still in WGS84
all_outlines_read <- read_excel("input/Table_S2.xlsx")
outlines <- st_as_sf(all_outlines_read, wkt = "geometry", crs = 4326)

all_inf_cattle <- st_read('input/S3_cattle_maps.xlsx')
outlines<-outlines %>%  st_transform(4674)
all_inf_cattle<-all_inf_cattle %>%  st_transform(4674)

##################################################
#'## count cattle inside and outside CAR properties
##################################################
intersections_cattle_CAR <- st_intersects(all_inf_cattle, selected_car_intersection)

has_intersection <- sapply(intersections_cattle_CAR, function(x) length(x) > 0)

# Filter the points from all_inf_cattle_19 that intersect with 
cattle_in_selected_polygons <- all_inf_cattle[has_intersection, ]
sum_cattle <- sum(cattle_in_selected_polygons$n_cattle, na.rm = TRUE)
cat('Total number of cattle: ',sum_cattle,'\n')

cat('Number of cattle outside of CAR: ',sum(all_inf_cattle$n_cattle)- sum_cattle,'\n')
cat( 'Percentage of cattle outside of CAR properties:', round((1- sum_cattle/sum(all_inf_cattle$n_cattle))*100),'\n')

# check the intersecting area
intersect_pct <-selected_car_intersection %>% st_union()  %>% st_make_valid()
st_intersection(intersect_pct,outlines) %>% 
  mutate(geom = st_union(x)) %>%
  mutate(geom = st_sfc(geom),
         area = st_area(geom)) 

unique_geoms <- unique(selected_car_intersection$geometry)
duplicate_indices <- which(duplicated(selected_car_intersection$geometry))
duplicate_polygons <- selected_car_intersection[duplicate_indices, ]
print(duplicate_polygons)

single_polygon <- selected_car_intersection %>%  st_union() #%>% st_make_valid()
single_polygon<- single_polygon %>% st_make_valid()
single_polygon_intersection <- st_intersection(single_polygon,outlines)
single_polygon_intersection<-single_polygon_intersection  %>% st_make_valid()
single_polygon_intersection$area <- single_polygon_intersection %>% st_area() 
outlines$area <-st_area(outlines) 

cat( 'Percentage:', round(sum(single_polygon_intersection$area/1000000)/ sum(outlines$area/1000000)*100),'\n')

################################
#' count cattle in fully contained CAR properties
#' and calculate cattle/CAR 
################################

all_car_with_cattle <- data.frame()
cor_mean <-c()
cor_cattlenumber <-c()
inference_list<- outlines$id
#inference_list <- list.files(inf_dir)

for(inf in inference_list){
  
  #find id to compare with date_table
  #id =  inf #strsplit(inf, "[.]")[[1]][1]
  id = strsplit(inf, "[.]")[[1]][1]
  date <- outlines$date[outlines$id == id]
  
  
  # the geopoints are stored in EPGS:4674 already, just have to be assigned
  #geopoints <- all_inf_cattle[all_inf_cattle$id_bbox== id,] %>% mutate(n_cattle = floor(n_cattle))
  geopoints <- st_read(file.path(inf_dir,inf)) %>% st_set_crs(4674) %>% mutate(n_cattle = floor(n_cattle))
  
  binary <-st_covered_by(selected_car, outlines[outlines$id==id,])
  a = as.vector.data.frame(binary)

  car_select <- selected_car %>%  mutate(covered = ifelse(a>0,1,0)) %>% 
    filter(covered!=0) 
  
  # If no car is selected, skip to the next inference file
  if (nrow(car_select) == 0) {
    cat('no car in this outline.. ', id, '\n')
    next
  }
  
  r <- relate(vect(car_select), vect(geopoints), "intersects")
  #st_intersects(vect(car_select), vect(geopoints))
  car_select$n_cattle  <- apply(r, 1, function(i) sum(geopoints$n_cattle[i]))
  
  # take the mean of the sd, so the sd is independent of the number of geopoints (independent of area)
  #print(car_select$area)
  
  car_select$n_cattle_sd <- apply(r, 1, function(i) mean(geopoints$n_cattle_sd[i], na.rm = TRUE))
  #car_select$n_cattle_mean <- apply(r, 1, function(i) mean((geopoints$n_cattle[i])))

  #check the correlation of the standard deviation and cattle number and area of the property
  cor_cattlenumber <- c(cor_cattlenumber, cor(car_select$n_cattle, car_select$n_cattle_sd))
  #cor_mean <- c(cor_mean, cor(car_select$n_cattle_mean, car_select$n_cattle_sd))
  date <- geopoints$img_date[1]
  
  car_select$imagedate <- date
  car_select$outline_id <- id
  #add the car to all_car_with_cattle
  all_car_with_cattle <- rbind(all_car_with_cattle,car_select)
}


################################
#'## Print out numbers
################################
cat('CAR observed:' ,length(all_car_with_cattle$seq))
omitzeros<- all_car_with_cattle%>%filter(round(n_cattle)>=1)

cat('CAR with cattle observed:' ,length(omitzeros$seq))
cat('Percentage of CAR where cattle is observed:' ,round(100* length(omitzeros$seq)/length(all_car_with_cattle$seq)) )
omitzeros <- omitzeros %>% dplyr::select(cod_imovel,IBGE_CODE,SIGLA_UF,area,n_cattle,n_cattle_sd, outline_id, imagedate,)
st_write(omitzeros,paste0('intermediate_files/all_car_contained_cattle.geojson'), append=TRUE)




