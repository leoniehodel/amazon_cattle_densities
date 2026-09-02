library(sf)
library(tidyverse)
library(writexl)
library(readxl)
################################
#'## Read data
################################

inf_dir <- 'input/raw_inference_files/'
inf_dir_clean <- 'intermediate_files/raw_inference_files_clean/'



# Create directory if it doesn't exist
if (!dir.exists(inf_dir_clean)) {
  dir.create(inf_dir_clean, recursive = TRUE)
}

# Read outline data
# also in  
# https://earth.google.com/earth/d/1WnwLEpbj2K8Blt4D8S2s6GehudwahILx?usp=sharing

# read in excel
all_outlines_read <- read_excel("input/S2.xlsx")
# Convert to sf object
all_outlines <- st_as_sf(all_outlines_read, wkt = "geometry", crs = 4326)

nrow(all_outlines[all_outlines$Sensor=='maxar',])
nrow(all_outlines[all_outlines$Sensor=='airbus',])

# the inference files are in 4326
all_outlines
################################
# Add date to individual inference files 
################################
add_date_intersection <- function(id_bbox) {
  print(id_bbox)
  
  # Find the file corresponding to the given ID
  filepath <- list.files(inf_dir, pattern = id_bbox, all.files = TRUE, full.names = TRUE)
  print(filepath)
  
  # Extract date from `all_outlines`
  date <- as.Date(all_outlines$Image_date[all_outlines$Id == id_bbox])
  state<- all_outlines$State[all_outlines$Id == id_bbox]
  image_outline<- all_outlines$geometry[all_outlines$Id == id_bbox]
  print(date)
  # projection is in 4674 form deep learning output
  
  if (identical(filepath, character(0))) {
    cat(paste0(id_bbox, " is not in the dir, maybe does not exist in inference database\n"))
  } else {
    # Read the inference file
    temp <- st_read(filepath) %>% 
      mutate(img_date = date, id_bbox = id_bbox, state=state ) %>% st_set_crs(4674) %>% st_transform(4326)
    
    # Ensure intersection with `image_outline`
    intersection <- st_intersection(temp, image_outline)
    
    # Save only the intersected portion
    if (nrow(intersection) > 0) {
      st_write(intersection, paste0(inf_dir_clean, id_bbox, ".geojson"), delete_dsn = TRUE)
    } else {
      cat(paste0("No intersection found for ", id_bbox, "\n"))
    }
  }
}
lapply(all_outlines$Id, add_date_intersection)

# Read all inference data
all_inf_paths <- tibble(full_path = list.files(inf_dir_clean,full.names = TRUE)) 
all_inference <- do.call(rbind, lapply(all_inf_paths$full_path, function(x) st_read(x)))

################################
# Print out numbers 
################################
nrow(all_inference)
#[1] 774488
sum(all_inference$n_cattle)
#[1] 372366.4

sum(all_outlines$Area_km2)

# Convert id_bbox to factor
all_inference$id_bbox <-as.factor(all_inference$id_bbox)

#check if all 170 img patches were processed
length(levels(as.factor(all_inference$id_bbox)))

all_inference_clean <-all_inference%>% filter(round(n_cattle)>=1)%>%
  mutate(n_cattle = round(n_cattle))


nrow(all_inference_clean)
#[1] 96296
sum(all_inference_clean$n_cattle)
#[1] 367511

################################
#  Save S3 in geojson and excel 
################################


# save a file with all inference data (in WGS 84)
st_write(all_inference_clean,'input/S3_cattle_maps.geojson')

# save file as excel
all_inference_clean$geometry_wkt <- st_as_text(all_inference_clean$geometry)
all_inference_clean_text<- all_inference_clean %>% as.data.frame() %>% dplyr::select(-geometry)

# write excel in WGS 84
write.xlsx(all_inference_clean_text, 'input/S3_cattle_maps.xlsx')

