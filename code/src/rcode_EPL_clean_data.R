## variable to run testthat test contained in this script
test_here <- FALSE
if(test_here) library(testthat)

#' ---
#' Title: "Various functions likely to be useful when working with EPL data"
#' Author: "Matthieu"
#' Date: 2022-02-16
#' ---

## This file is not git-tracked, but a copy is in: ONGOING_RESEARCH/ZDCinBrazil/Analyses/BRA_IndirectSuppliers


#' convert a sf to tibble
#' 
#' @param shp a sf object
epl_st_to_df <- function(shp){
  if (!inherits(shp, c("sf", "sfc"))) {
    return(tibble::as_tibble(shp))
  }
  res <- shp %>%
    sf::st_set_geometry(NULL) %>% 
    tibble::as_tibble()
  res
}


#' Compute area of intersection
#' 
#' @param shp1 First shape object, reference for the computation
#' @param shp2 Second shape object, 
#' @param cols_1,cols_2 names of key columns to retain in each dataset. Should accepty a tidyselect...
#' 
#' @return a tibble with cols_1 and cols_2 and new variable share
epl_get_area_intersection <- function(shp1, shp2, cols_1=colnames(shp1), cols_2=colnames(shp2)){
  shape_intersect <- shp1 %>% 
    st_intersection(shp2) %>% 
    select(c(!!cols_1, !!cols_2))
  if(any(!st_is_valid(shape_intersect))) {
    shape_intersect <- shape_intersect %>% 
      st_make_valid()
  }
  
  ## area orig
  area_orig <- shp1 %>% 
    select(!!cols_1) %>% 
    mutate(area_orig = st_area(.)) %>% 
    st_drop_geometry() %>% 
    as_tibble()
  
  ## add area
  shape_intersect %>% 
    mutate(area_inter = st_area(.)) %>% 
    st_drop_geometry() %>% 
    left_join(area_orig, by = cols_1) %>% 
    mutate(share = 100*units::drop_units(area_inter)/units::drop_units(area_orig)) %>% 
    select(-area_inter, -area_orig) %>% 
    arrange(across(c(cols_1, cols_2))) %>% 
    as_tibble()
  
}

#' convert a gee output
#' 
#' @param list output from rgee read with jsonlite::fromJSON
#' @param col_name new name for column
#' @param pattern_restrict Assemble only columns with given pattern
#' 
#' @author mstigler
#' @examples 
#' typical_out <- list(data.frame(MPB_classification_2000 = 1:3, sum = c(94.1175, 2.338, 46.058)),
#'                     data.frame(MPB_classification_2001 = 1:3, sum = c(95.362, 1.13, 45.843)))
#' epl_gee_tidy_1(typical_out, col_name = "Land_class")
epl_gee_tidy_1 <- function(list, col_name = Class, var_name = var, pattern_restrict=NULL){
  list <- list[lapply(list,length)>1]
  
  ## return NA df if empty
  if(all(sapply(list, length)==0)) {
    return(tibble({{col_name}}:=NA))
  }
  
  ## Restrict columns if pattern provided
  if(!is.null(pattern_restrict)) {
    cols_keep <- map_lgl(list, ~any(str_detect(colnames(.), pattern_restrict)))
    if(!any(cols_keep)) warning("Pattern not detected?")
    list <- list[cols_keep]
  }
  
  
  purrr::map_dfr(list, ~dplyr::mutate(., {{var_name}} := colnames(.)[1]) %>%
                   dplyr::rename({{col_name}}:=1)) %>% 
    tibble::as_tibble()
}

#' @param json output from jsonlite::fromJSON
#' @param histo_col the name in the EE output of the column with a reducer output
#' @param col_name,var_name the name in the function's output for the class/variable name
#' @param pattern_restrict Assemble only columns with given pattern
#' @param n_head How many rows to process? Useful for initial testing
epl_gee_tidy <- function(json, histo_col = table, col_name = Class, 
                         var_name = var, n_head =Inf, pattern_restrict = NULL){
  
  # histo_col = rlang::quo(table)
  # var_name = rlang::quo(var)
  # col_name <-  rlang::quo(Class)
  histo_col_char <- rlang::as_name(rlang::enquo(histo_col))
  
  ## read relevant part of json
  if(intrn_check_is_json(json)) {
    dat <- as_tibble(json$features$properties)   
  } else if(inherits(json, "data.frame")){
    dat <- json
  } else {
    stop("Object is not output of jsonlite::fromJSON or a data.frame?")
  }
  dat <- dat %>% 
    head(n_head)
  
  if(!histo_col_char %in% colnames(dat)) {
    warning(paste0("Column '", histo_col_char, "' not in data? Change it with argument 'histo_col'"))
  }
  
  ## if table contains multiple sub tables (i.e. multiple bands), use epl_gee_tidy_1
  if(inherits(pull(dat, {{histo_col}}), "data.frame") && colnames(pull(dat, {{histo_col}}))=="groups") {
    dat <- dat %>% 
      unnest(table) %>% 
      rename(table=groups)
  }
  if(all(map_lgl(pull(dat, {{histo_col}}), ~inherits(., "list")))) {
    if(length(pull(dat, {{histo_col}}))!=nrow(dat)) stop("Problem with column 'histo_col_char' not of expected type")
    dat <- dat %>% 
      mutate({{histo_col}} := map({{histo_col}}, ~epl_gee_tidy_1(., col_name={{col_name}}, var_name = {{var_name}}, pattern_restrict = pattern_restrict))) 
  }
  
  ## now unnest
  dat %>% 
    unnest({{histo_col}}, keep_empty = TRUE)
}

intrn_check_is_json <- function(json){
  check_1 <- all(c("type", "features") %in% names(json))
  check_2 <- check_1 && "properties" %in% names(json$features)
  all(c(check_1, check_2))
}




## TESTs:
if(test_here){
  library(tidyverse)
  check_class <- function(json){
    df <- as_tibble(json$features$properties)$table
    
    res <- c(class(df),
             unique(map_chr(df, class)))
    if(res[2]=="list") {
      # l <- map_int(df, length)
      # class_l3 <- unique(map_chr(df[[which(l>0)[1]]], class))
      # unique(map_chr(df, ~unique(map_chr(., class))))
      res <- c(res, class(df[[1]][[1]]))
      # if(res[3]=="list") res <- c(res, class(df[[1]][[1]][[1]]))
    }
    res
  }
  
  ## single reducer per feature: `table` is a list of data frames
  a <- jsonlite::fromJSON("~/shared_epl/public/ONGOING_RESEARCH/ZDCsInIndonesia/05 Data and analyses/Analyses/ZDC_deforest_impact_ana/01_Raw_Data/data_from_GEE/IND_grid_gee_for_loss_all_2020_ALTER_small.geojson")
  check_class(a)
  epl_gee_tidy(json=a)
  epl_gee_tidy(json=as_tibble(a$features$properties))
  
  test_that("epl_gee_tidy on: standard input band", expect_no_error(epl_gee_tidy(json=a, n_head = 6)))
  
  ## TMF in IND: `table` is a list of list
  TMF_100000_df_raw <- jsonlite::fromJSON("~/shared_epl/public/ONGOING_RESEARCH/ZDCsInIndonesia/05 Data and analyses/Analyses/ZDC_deforest_impact_ana/01_Raw_Data/data_from_GEE/IND_grid_100_km_gee_for_TMF_rgee.geojson")
  check_class(json = TMF_100000_df_raw)
  epl_gee_tidy(json=TMF_100000_df_raw, n_head = 6) %>% count(var)
  epl_gee_tidy(json=TMF_100000_df_raw, n_head = 6,
               var_name = Variable, col_name = Sum)
  test_that("epl_gee_tidy on: multiple bands", expect_no_error(epl_gee_tidy(json=TMF_100000_df_raw, n_head = 6)))
  
  
  TMF_10000_jsn <- jsonlite::fromJSON("~/shared_epl/public/ONGOING_RESEARCH/ZDCsInIndonesia/05 Data and analyses/Analyses/ZDC_deforest_impact_ana/01_Raw_Data/data_from_GEE/IND_grid_10000_gee_for_loss_all_2020_rgee.geojson")
  check_class(TMF_10000_jsn)
  as_tibble(TMF_10000_jsn$features$properties)
  epl_gee_tidy(json=TMF_10000_jsn, n_head = 6)
  test_that("epl_gee_tidy on: multiple bands 2", expect_no_error(epl_gee_tidy(json=TMF_10000_jsn, n_head = 6)))
  
  
  ## Avoid weird cases: data frame of list
  Han_weird_formatted <- jsonlite::fromJSON("~/shared_epl/public/ONGOING_RESEARCH/ZDCsInIndonesia/05 Data and analyses/Analyses/ZDC_deforest_impact_ana/01_Raw_Data/data_from_GEE/deforstY_Hansen_IND_grid_100_km_rgee_rNsSkIn.geojson")
  check_class(json = Han_weird_formatted)
  epl_gee_tidy(json=Han_weird_formatted, n_head = 6)
  test_that("epl_gee_tidy on: table$groups", expect_no_error(epl_gee_tidy(json=Han_weird_formatted, n_head = 6)))
  TMF_10000_jsn
  
  ## SLOW
  if(FALSE){
    dat_CAR_forest <- jsonlite::fromJSON("~/shared_epl/public/ONGOING_RESEARCH/ZDCinBrazil/Analyses/BR_smallholder_innov_deforest/01_Raw_Data/BR_MapBio_PRODES_byCAR_muni_type_biomes_full.geojson")
    epl_gee_tidy(json=dat_CAR_forest, n_head = 6)
  }
}

################################
#'## parallel
################################

## from https://www.spatialanalytics.co.nz/post/2018/04/01/fixing-st-par/
## quite a lot of changes by matthieu, regarding class determination
epl_st_parallel <- function(sf_df, sf_func, n_cores, ...){
  
  
  # Create a vector to split the data set up by.
  split_vector <- rep(1:n_cores, each = max(nrow(sf_df)/n_cores, 1), length.out = nrow(sf_df))
  
  # Perform GIS analysis
  split_results <- split(sf_df, split_vector) %>%
    parallel::mclapply(function(x) sf_func(x, ...), mc.cores = n_cores)
  
  
  # Define the output_class. If length is greater than two, then grab the second variable.
  # output_class <- class(split_results[[1]])
  # if (length(output_class) == 2){
  #   output_class <- output_class[2]
  # }
  
  # Combine results back together. Method of combining depends on the output from the function.
  if (inherits(split_results[[1]], "matrix")){
    result <- do.call("rbind", split_results)
    names(result) <- NULL
  } else if (inherits(split_results[[1]], "sfc")) {
    result <- do.call("c", split_results)
    result <- sf_func(result) # do.call combines the list but there are still n_cores of the geometry which had been split up. Running st_union or st_collect gathers them up into one, as is the expected output of these two functions. 
  } else if (inherits(split_results[[1]], c('list', 'sgbp'))){
    result <- do.call("c", split_results)
    names(result) <- NULL
  } else if (inherits(split_results[[1]], "data.frame") ){ ##changed by mat
    result <- do.call("rbind", split_results)
  } else if (inherits(split_results[[1]], "try-error") ){ ##added by mat
    stop("At least one node had an error")
  } else {
    stop("Unknown class. st_parallel only accepts the following outputs at present: sfc, list, sf, matrix, sgbp.")
  }
  
  # Return result
  return(result)
}


################################
#'## sf misc
################################

#' Add area to the shp
#' 
#' @param shp the input
#' @param area_name name of variable
#' @examples 
#' library(sf)
epl_add_area <- function(shp, area_name = area_km2, unit = "km^2"){
  shp %>% 
    mutate({{area_name}} := st_area(.) %>% units::set_units(unit, mode = "standard"))
}

################################
#'## extract land cover
################################

#' Aggregation function used for exactextractr::exact_extract, dplyr version
intrl_count_dplyr <-  function(df, cols_by = NULL, cols_value = "value") {
  
  ## pivot longer if has multiple variables  
  if(length(cols_value)>1){
    df <- df |>
      tidyr::gather(variable, value, tidyselect::contains(tidyselect::all_of(cols_value)))
    cols_by_all <- c(cols_by, "variable", "value")
  } else {
    cols_by_all <- c(cols_by, "value") # we ignore the name, as extractr will just return "value"
  }
  
  ## now aggregate
  df |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(cols_by_all))) |>
    dplyr::summarise(n_pixels=dplyr::n(),
                     sum_area=sum(coverage_fraction*area),
                     .groups="drop")
}

#' Aggregation function used for exactextractr::exact_extract, data.table version
intrl_count_data_table <- function(df, cols_by = NULL, cols_value = "value") {
  
  data.table::setDT(df)
  
  ## pivot longer if has multiple variables  
  if(length(cols_value)>1){
    df <- data.table::melt(df, measure.vars = cols_value, variable.factor =FALSE)
    cols_by_all <- c(cols_by, "variable", "value")
  } else {
    cols_by_all <- c(cols_by, "value") # we ignore the name, as extractr will just return "value"
  }
  
  df[, .(n_pixels = .N, 
         sum_area = sum(coverage_fraction*area)), 
     keyby = cols_by_all]
}

## TEST
if(FALSE){
  rast <- raster::raster(matrix(rep(1:5, each=20), ncol=10), xmn=0, ymn=0, xmx=10, ymx=10)
  rast2 <- raster::raster(matrix(rep(5:1, each=20), ncol=10), xmn=0, ymn=0, xmx=10, ymx=10)
  rast_12 <- raster::stack(rast,rast2)
  poly <- sf::st_sf(my_id=c("a", "b"),
                    rbind(sf::st_as_sfc('POLYGON ((2 2, 7 6, 4 9, 2 2))'),
                          sf::st_as_sfc('POLYGON ((4 2, 6 4, 8 2, 4 2))')))
  
  df <- exactextractr::exact_extract(rast_12, poly, 
                                     include_cols="my_id",
                                     include_area = TRUE,
                                     progress = FALSE)[[1]]
  
  
  intrl_count_dplyr(df, cols_value = c("layer.1", "layer.2"))
  intrl_count_data_table(df, cols_value = c("layer.1", "layer.2"))
}



#' Extract frequency table for a given shp
#'
#' @param raster the raster of interest
#' @param vector the feature/shp data
#' @param cols_keep columns in 'vector' to keep
#' @param pkg Which package to use for aggregation. Data table should be faster?
#' 
#' @examples 
#' rast <- raster::raster(matrix(rep(1:5, each=20), ncol=10), xmn=0, ymn=0, xmx=10, ymx=10)
#' poly <- sf::st_sf(my_id=c("a", "b"),
#'                  rbind(sf::st_as_sfc('POLYGON ((2 2, 7 6, 4 9, 2 2))'),
#'                        sf::st_as_sfc('POLYGON ((4 2, 6 4, 8 2, 4 2))')))
#'                        
#' plot(rast)
#' plot(poly, add=TRUE)
#' epl_ras_extract_table(rast, poly)
#' epl_ras_extract_table(rast, poly, cols_keep = "my_id")
epl_ras_extract_table <- function(raster, vector,
                                  cols_keep = NULL, pkg = c("data.table", "dplyr")){
  
  fun_aggregate <- switch(match.arg(pkg),
                          "dplyr"=intrl_count_dplyr,
                          "data.table"=intrl_count_data_table)
  if(packageVersion("exactextractr")<= "0.7.2") {
    warning("Needs higher/dev version due to bug https://github.com/isciences/exactextractr/issues/68.
            Use: devtools::install_github('isciences/exactextractr')")
  }
  
  ## count n layers
  n_layers <- length(names(raster))
  
  ##
  if(is.null(cols_keep)){
    print("No variable provided for 'cols_keep', creating a  'row_id' identifier")
    vector <- vector |>
      dplyr::mutate(row_id = 1:dplyr::n())
    cols_keep <- "row_id"
  } else {
    if(!all(cols_keep %in% colnames(vector))) stop("Some columns in cols_keep not in data!")
  }
  require(data.table)
  exactextractr::exact_extract(raster, vector, 
                               summarize_df = TRUE,
                               include_cols=cols_keep,
                               include_area = TRUE,
                               progress = FALSE,
                               fun= function(df) fun_aggregate(df, cols_by = cols_keep, cols_value = names(raster))) |>
    tibble::as_tibble() |> 
    dplyr::mutate(value = ifelse(!is.finite(value), NA, value))
}

if(FALSE){
  rast <- raster::raster(matrix(rep(1:5, each=20), ncol=10), xmn=0, ymn=0, xmx=10, ymx=10)
  rast2 <- raster::raster(matrix(rep(5:1, each=20), ncol=10), xmn=0, ymn=0, xmx=10, ymx=10)
  rast_12 <- raster::stack(rast,rast2)
  poly <- sf::st_sf(my_id=c("a", "b"),
                    rbind(sf::st_as_sfc('POLYGON ((2 2, 7 6, 4 9, 2 2))'),
                          sf::st_as_sfc('POLYGON ((4 2, 6 4, 8 2, 4 2))')))
  
  raster::plot(rast)
  plot(poly, add=TRUE)
  
  epl_ras_extract_table(rast, poly)
  epl_ras_extract_table(rast, poly, cols_keep = "my_id")
  
  ## multi layers?
  epl_ras_extract_table(raster = rast_12, vector = poly)
  epl_ras_extract_table(rast_12, poly, cols_keep = "my_id")
  
  ## Compare pkgs
  all.equal(epl_ras_extract_table(raster = rast_12, vector = poly, pkg = "data.table"),
            epl_ras_extract_table(raster = rast_12, vector = poly, pkg = "dplyr"),
            check.attributes=FALSE)
  
  ## dt faster? YES, 2-3 times!
  microbenchmark::microbenchmark(dplyr = epl_ras_extract_table(raster = rast_12, vector = poly, pkg = "dplyr"),
                                 dt = epl_ras_extract_table(raster = rast_12, vector = poly, pkg = "data.table"))
  
  microbenchmark::microbenchmark(dplyr = epl_ras_extract_table(raster = rast, vector = poly, pkg = "dplyr"),
                                 dt = epl_ras_extract_table(raster = rast, vector = poly, pkg = "data.table"))
}

################################
#'## PRODES clean
################################

intrnl_is_m_to_km2 <- function(x) {
  units::set_units(x, "m2") |>
    units::set_units("km2")
}

#' just for 1, actually not used as more efficient epl_PRD_transfo is used
#' @param .PRD_class_var name of variable with the metadata values
epl_PRD_transfo_1 <- function(df, .PRD_class_var = PRD_class) {
  
  # if("MUN_CODE" %in% colnames(df) && n_distinct(df$MUN_CODE)>1) stop("Only for one group!")
  
  ## 
  .PRD_class_var_char <- rlang::as_name(rlang::ensym(.PRD_class_var))
  need_vars <- c("sum_area", .PRD_class_var_char)
  if(!all(need_vars %in% colnames(df))) {
    miss_vars <- need_vars[!need_vars %in% colnames(df)]
    warning("Missing: ", miss_vars)
  }
  
  ## filter for only (de)forest
  df_floresta <- filter(df, str_detect({{.PRD_class_var}}, "d[0-9]|^FLORESTA|^Floresta"))
  
  # compute sums: all or only deforest
  full_sum_area <- sum(df$sum_area)  
  for_sum_area <- sum(df_floresta$sum_area)
  
  ## If only floresta, return 0/NA
  has_only_floresta <- nrow(df_floresta)==1 && unique(df_floresta$PRODES_class)=="FLORESTA"
  if(has_only_floresta) {
    res <- df_floresta %>% 
      mutate(dfrt_year = NA_integer_,
             dfrt_area_by_tot = 0,
             dfrt_area_by_forest = 0,
             area_total=intrnl_is_m_to_km2(full_sum_area),
             area_forest_initial=intrnl_is_m_to_km2(for_sum_area),
             sum_area = intrnl_is_m_to_km2(sum_area)) %>% 
      select(-{{.PRD_class_var}}) %>% 
      rename(dfrt_area=sum_area, any_of(c(dfrt_area_in_pixels="n_pixels")))   
  } else {
    res <- df_floresta %>% 
      filter(str_detect({{.PRD_class_var}}, "d[0-9]")) %>%
      mutate(dfrt_year = str_remove({{.PRD_class_var}}, "^d") %>% 
               as.integer,
             dfrt_area_by_tot = 100*sum_area/full_sum_area,
             dfrt_area_by_forest = 100*sum_area/for_sum_area,
             area_total=intrnl_is_m_to_km2(full_sum_area),
             area_forest_initial=intrnl_is_m_to_km2(for_sum_area),
             sum_area = intrnl_is_m_to_km2(sum_area)) %>% 
      select(-{{.PRD_class_var}}) %>% 
      rename(dfrt_area=sum_area, any_of(c(dfrt_area_in_pixels="n_pixels"))) 
  }
  res
}

epl_PRD_transfo_old <- function(df, .group_vars, .PRD_class_var = PRD_class) {
  df %>% 
    group_by(across({{.group_vars}})) %>% 
    group_modify(~epl_PRD_transfo_1(.x, .PRD_class_var= {{.PRD_class_var}})) %>% 
    ungroup()%>% 
    relocate(dfrt_year, dfrt_area, dfrt_area_in_pixels, .after = {{.group_vars}})
}

#' Small function to avoid NA message
intrnl_get_year <- function(x) {
  x[!str_detect(x, "^d")] <- NA_integer_
  x[!is.na(x) & str_detect(x, "^d")] <- str_remove(x[!is.na(x) & str_detect(x, "^d")], "^d") 
  as.integer(x)
}


if(FALSE) {
  intrnl_get_year(x=c("d2012", "FLORESTA", "NUVEM"))
}


#' @examples 
#'  df_test_PRD_raw <- tibble(PRODES_class_id = c(1,2,3, 1,15,16,
#'                            1,2,16, 2,3),
#'                            sum_area = 10000,
#'                            cell_id=rep(c("No def", "Normal_full_forest", "Normal_half_forest", "No deforestable"), c(3,3,3, 2)),
#'                            dfrt_area_in_pixels=2L)
#' df_test
#' df_test %>%
#'   epl_PRD_add_categs(PRD_year=2021) %>% 
#'   epl_PRD_transfo(.group_vars=cell_id, .PRD_class_var = PRODES_class)
#' 
#' df_test %>% 
#'   epl_PRD_transfo_NEW(PRD_year = 2021, units_is = "m2", units_set = "km2")
epl_PRD_transfo <- function(df, .group_vars, .PRD_class_var = PRD_class) {
  
  ## check args
  if(missing(.group_vars)) warning("Need arg .group_vars for now")
  
  
  ## check columns
  need_vars <- c("sum_area", rlang::as_name(rlang::ensym(.PRD_class_var)))
  if(!all(need_vars %in% colnames(df))) {
    miss_vars <- need_vars[!need_vars %in% colnames(df)]
    warning("Missing: ", miss_vars)
  }
  
  ## check no NA in categoeries
  if(any(is.na(pull(df, {{.PRD_class_var}})))) {
    warning("Class variable contain NAs?")
  }
  
  ## Now add total area, and deforestation by year
  df %>% 
    group_by(across({{.group_vars}})) %>% 
    mutate(area_total = sum(sum_area),
           area_forest_initial = sum(sum_area[str_detect({{.PRD_class_var}}, "d[0-9]|^FLORESTA|^Floresta")]),
           dfrt_any = any(str_detect({{.PRD_class_var}}, "d[0-9]")),
           row = 1:n()) %>% 
    ungroup() %>% 
    filter(str_detect({{.PRD_class_var}}, "d[0-9]") & dfrt_any | !dfrt_any & row==1) %>%## keep only def years, or just 1 row if no defor
    rename(any_of(c(dfrt_area_in_pixels="n_pixels"))) %>% 
    mutate(dfrt_year = intrnl_get_year({{.PRD_class_var}}),
           dfrt_area_by_tot = ifelse(dfrt_any, 100*sum_area/area_total,0),
           dfrt_area_by_forest = ifelse(dfrt_any, 100*sum_area/area_forest_initial,0),
           area_total=intrnl_is_m_to_km2(area_total),
           area_forest_initial=intrnl_is_m_to_km2(area_forest_initial),
           sum_area = if_else(dfrt_any, intrnl_is_m_to_km2(sum_area),units::set_units(0, "km2")),
           dfrt_area_in_pixels = if_else(dfrt_any, dfrt_area_in_pixels, 0L)) %>% 
    select(-{{.PRD_class_var}}, -row) %>% 
    rename(dfrt_area=sum_area) %>% 
    relocate(dfrt_any, starts_with("area"), dfrt_year, dfrt_area, any_of("dfrt_area_in_pixels"), .after = {{.group_vars}}) %>% 
    arrange({{.group_vars}}, dfrt_year)
}

epl_PRD_transfo_ARCHIVE <- function(df, .group_vars, .PRD_class_var = PRD_class) {
  
  ## check args
  if(missing(.group_vars)) warning("Need arg .group_vars for now")
  
  
  ## check columns
  need_vars <- c("sum_area", rlang::as_name(rlang::ensym(.PRD_class_var)))
  if(!all(need_vars %in% colnames(df))) {
    miss_vars <- need_vars[!need_vars %in% colnames(df)]
    warning("Missing: ", miss_vars)
  }
  
  ## check no NA in categoeries
  if(any(is.na(pull(df, {{.PRD_class_var}})))) {
    warning("Class variable contain NAs?")
  }
  
  ## Now add total area, and deforestation by year
  df %>% 
    group_by(across({{.group_vars}})) %>% 
    mutate(area_total = sum(sum_area),
           area_forest_initial = sum(sum_area[str_detect({{.PRD_class_var}}, "d[0-9]|^FLORESTA|^Floresta")])) %>% 
    ungroup() %>% 
    filter(str_detect({{.PRD_class_var}}, "d[0-9]")) %>% 
    mutate(dfrt_year = str_remove({{.PRD_class_var}}, "^d") %>% 
             as.integer,
           dfrt_area_by_tot = 100*sum_area/area_total,
           dfrt_area_by_forest = 100*sum_area/area_forest_initial,
           area_total=intrnl_is_m_to_km2(area_total),
           area_forest_initial=intrnl_is_m_to_km2(area_forest_initial),
           sum_area = intrnl_is_m_to_km2(sum_area)) %>% 
    select(-{{.PRD_class_var}}) %>% 
    rename(dfrt_area=sum_area, any_of(c(dfrt_area_in_pixels="n_pixels"))) %>% 
    relocate(starts_with("area"), dfrt_year, dfrt_area, any_of("dfrt_area_in_pixels"), .after = {{.group_vars}})
}

#' Add meta from PRODES
#' @param df any dataset containing the raw PRODES codes
#' @param PRD_meta_df a dataset with the metadata for PRODES
#' 
#' @examples
#' df_test <- tibble(PRODES_class_id = c(1,2,3), area = c(10, 12, 13))
#' df_test_manyY <- tibble(PRODES_data_year = c(2014, 2020, 2021), PRODES_class_id = c(1,1,1), area = c(10, 12, 13))
#' df_test_manyY_probs <- tibble(PRODES_data_year = c(2014, 2020, 2021), PRODES_class_id = c(0,1,99), area = c(10, 12, 13))
#' 
#' ## standard use: 1 year
#' epl_PRD_add_categs(df_test, PRD_year=2021)
#' epl_PRD_add_categs(df_test, PRD_year=2021,
#'  meta_cols_keep = c("PRODES_class", "PRODES_class_id", "PRODES_year_deforest"))
#'  
#'  ## with many years
#'  epl_PRD_add_categs(df_test_manyY, meta_cols_keep = c("PRODES_class", "PRODES_class_id"))
#'  
#'  ## should detect missing/invalid
#'  epl_PRD_add_categs(df_test_manyY_probs, meta_cols_keep = c("PRODES_class", "PRODES_class_id"))
epl_PRD_add_categs <- function(df, PRD_meta_df, PRD_year = NULL, meta_cols_keep = "PRODES_class"){
  
  if(!any(str_detect(colnames(df), "PRODES_class"))) {
    stop("Argument 'df' should contain a column with name PRODES_class_id/PRODES_class")
  }
  
  if(missing(PRD_meta_df)) {
    # abs_path <- "~/shared_epl/public/ONGOING_RESEARCH/ZDCinBrazil/Data/Spatial/PRODES_deforestation/03_Final_Data/PRODES_meta_2020.csv"
    abs_path <- "~/shared_epl/public/ONGOING_RESEARCH/ZDCinBrazil/Data/Spatial/PRODES_deforestation/03_Final_Data/PRODES_meta_many_years.csv"
    PRD_meta_df <- readr::read_csv(abs_path,
                                   col_types = readr::cols(.default = col_integer(),
                                                           PRODES_class = col_character()))
  }
  
  ###
  if(is.null(PRD_year)) {
    if(!"PRODES_data_year" %in% colnames(df)) {
      stop("Need to either specify 'PRD_year' or have a column 'PRODES_data_year' in the data")
    } else {
      join_vars <- c("PRODES_class_id", "PRODES_data_year")
    }
  } else {
    PRD_meta_df <- PRD_meta_df %>% 
      dplyr::filter(PRODES_data_year==PRD_year)
    join_vars <- "PRODES_class_id"
  }
  
  ### Add categories
  out <- df %>% 
    dplyr::rename(any_of(c(PRODES_class_id="PRODES_class"))) %>% 
    dplyr::left_join(PRD_meta_df %>% 
                       dplyr::select(all_of(c(join_vars, meta_cols_keep))),
                     by = join_vars) 
  
  if(!"PRODES_class_id" %in% meta_cols_keep) {
    out <- out %>%
      dplyr::select(-PRODES_class_id)
  }
  
  ## Check all categories matched:
  if(any(is.na(out$PRODES_class))) {
    df_prob <- out %>% 
      dplyr::filter(is.na(PRODES_class)) %>% 
      dplyr::distinct(.data$PRODES_class_id, .data$PRODES_class)
    
    warning("Some codes in df not matched in metadata (replaced with 'Other'):")
    print(df_prob)
    out <- out%>%  
      tidyr::replace_na(list(PRODES_class= "Other"))
  }
  
  out
}



epl_prd_prep <- function(df, 
                         .group_vars=cell_id,
                         year_var=PRODES_year_deforest,
                         area_var = sum_area,
                         PRODES_class_var = PRODES_class){
  df %>%
    rename(dfrt_year={{year_var}}) %>% 
    group_by(across(-c({{area_var}}, {{PRODES_class_var}}, any_of("dfrt_area_in_pixels")))) %>% 
    summarise({{area_var}} := sum({{area_var}}),
              across(any_of("dfrt_area_in_pixels"), ~sum(.)),
              {{PRODES_class_var}} := paste({{PRODES_class_var}}, collapse = "|"),
              .groups = "drop") %>% 
    ungroup() %>% 
    group_by({{.group_vars}}) %>% 
    mutate(area_total= sum({{area_var}}),
           area_forest_initial = sum({{area_var}}[dfrt_year>0]),
           n = n(),
           n_NA = sum(dfrt_year %in% c(0,9999))) %>% 
    slice(if(all(n==2 & n_NA==2)) 1L else 1:unique(n)) %>%
    ungroup() %>% 
    mutate(dfrt_year = if_else(dfrt_year%in% c(0, 9999), NA_integer_, dfrt_year)) %>% 
    select(-n, -n_NA)
}

## New version building on epl_han
epl_PRD_transfo_NEW <- function(df, PRD_year,
                                .group_vars = cell_id,
                                area_var = sum_area,
                                year_var=PRODES_year_deforest,
                                PRODES_class_var = PRODES_class,
                                units_is = NA,
                                units_set=NA){
  if(missing(PRD_year)){
    stop("Please provide `PRD_year`, the year of the data to retrieve the corresponding metadata")
  }
  
  df %>% 
    epl_PRD_add_categs(PRD_year=PRD_year, meta_cols_keep = c("PRODES_year_deforest", "PRODES_class")) %>% 
    epl_prd_prep(.group_vars={{.group_vars}},
                 year_var={{year_var}},
                 area_var = {{area_var}},
                 PRODES_class_var = {{PRODES_class_var}}) %>% 
    select(-any_of("dfrt_area_in_pixels"), -{{PRODES_class_var}}) %>%
    epl_han_intrnl_compute(.group_vars = cell_id,
                           area_var = sum_area,
                           year_var=dfrt_year,
                           area_mask_forest_var=area_forest_initial,
                           units_is = units_is, units_set = units_set) 
}


################################
#'## Hansen
################################

## prep hansen
epl_han_intrnl_format <- function(df, year_var = lossyear){
  
  no_loss_year <- as.integer(9999)
  df %>% 
    rename(dfrt_year={{year_var}}) %>% 
    mutate(dfrt_year := if_else(dfrt_year==0, no_loss_year, as.integer(dfrt_year+2000L)))
}

#' @param recompute_forest Should  'initial_forest' be (re) computed? 
#' Is needed when `area_mask_forest_var` is not provided. 
#' When it is provided, `recompute_forest=TRUE`` will either 
#'  overwrite `area_initial_forest` (`overwrite_mask_forest=TRUE`) 
#'  or add it as `area_initial_forest_recomputed` (`overwrite_mask_forest=TRUE`) 
epl_han_intrnl_compute <- function(df, year_var = lossyear, 
                                   .group_vars = cell_id,
                                   full_area_var = area_total,
                                   area_var = area,
                                   area_mask_forest_var = NULL,
                                   recompute_forest=FALSE,
                                   overwrite_mask_forest=TRUE,
                                   dfrt_val_if_no_forest= NA_real_,
                                   units_is = NA,
                                   units_set=NA){
  
  no_loss_year <- as.integer(9999)
  dfrt_val_if_no_forest <- as.numeric(dfrt_val_if_no_forest)
  
  ## rename, or recompute
  if(!rlang::quo_is_null(enquo(area_mask_forest_var))){
    df <- df %>% 
      rename(area_forest_initial={{area_mask_forest_var}})
  } else {
    if(!recompute_forest) {
      warning("No initial forest 'area_mask_forest_var' provided, computing it by setting `recompute_forest=TRUE`, 
              assuming total area = total (de)forest(ed)")
      recompute_forest <- TRUE
    }
  }
  
  ## compute forest initial (sum def and still forest)
  if(recompute_forest){
    name_mask_out <- if_else(overwrite_mask_forest,
                             "area_forest_initial",
                             "area_forest_initial_recompute")
    df <- df %>%
      group_by(across({{.group_vars}})) %>%
      mutate(!!rlang::sym(name_mask_out) := sum({{area_var}})) %>%
      ungroup()
    if(!overwrite_mask_forest){
      df <- df %>%
        relocate(area_forest_initial_recompute, .after = area_forest_initial)
    }
  }
  
  ## main operations
  df_out <- df %>% 
    rename(dfrt_year={{year_var}},
           area_total={{full_area_var}},
           dfrt_area={{area_var}}) %>% 
    ### Add vars dfrt_area_by_ tot/forest
    mutate(dfrt_area_by_tot = if_else(area_forest_initial>0,
                                      100*dfrt_area/area_total,
                                      dfrt_val_if_no_forest),
           dfrt_area_by_forest = if_else(area_forest_initial>0,
                                         100*dfrt_area/area_forest_initial,
                                         dfrt_val_if_no_forest)) %>% 
    ## compute cell-level info: dfrt_any and area_forest_final
    group_by(across({{.group_vars}})) %>% 
    mutate(dfrt_any = any(dfrt_year!=no_loss_year, na.rm=TRUE),
           area_forest_final=ifelse(any(dfrt_year==no_loss_year, na.rm=TRUE),
                                    dfrt_area[dfrt_year==no_loss_year],
                                    0)) %>% 
    ungroup() %>% 
    ## remove "0" (now 9999) loss year indicating remaining forest
    filter((dfrt_any & dfrt_year!=no_loss_year)|!dfrt_any) %>%
    ## overwrite dfrt_area_... for special cases
    mutate(across(starts_with("dfrt_area"), \(x)case_when(dfrt_any ~x,
                                                          !dfrt_any & area_forest_initial>0~ 0,
                                                          !dfrt_any & area_forest_initial==0~ dfrt_val_if_no_forest)),
           dfrt_year = if_else(dfrt_year ==no_loss_year, NA_integer_, dfrt_year)) %>% 
    relocate(dfrt_any, starts_with("area"), dfrt_year, starts_with("dfrt_area"),  .after = {{.group_vars}}) %>% 
    arrange(across({{.group_vars}}), dfrt_year)
  
  ## units
  if(!is.na(units_is)){
    if(is.na(units_set)) units_set <- units_is
    df_out <- df_out %>% 
      mutate(across(c(starts_with("area_"), dfrt_area), ~units::set_units(., units_is, mode = "standard") %>% 
                      units::set_units(units_set, mode = "standard")))
    
  }
  df_out
}

## process Hansen: combine the two steps
epl_han_process <-  function(df, year_var = lossyear, 
                             .group_vars = cell_id,
                             full_area_var = area_total,
                             area_var = area,
                             area_mask_forest_var = NULL,
                             recompute_forest=FALSE,
                             overwrite_mask_forest=TRUE,
                             dfrt_val_if_no_forest= NA_real_){
  
  
  epl_han_intrnl_format(df, year_var={{year_var}})  %>%
    epl_han_intrnl_compute(year_var = dfrt_year,
                           .group_vars = {{.group_vars}},
                           full_area_var = {{full_area_var}},
                           area_var = {{area_var}},
                           area_mask_forest_var = {{area_mask_forest_var}},
                           recompute_forest= {{recompute_forest}},
                           overwrite_mask_forest=overwrite_mask_forest,
                           dfrt_val_if_no_forest= dfrt_val_if_no_forest)
}


if(test_here){
  library(testthat)
  library(tidyverse)
  
  ## Case 1: only defY, no info on initial mask
  df_test_Han <- tibble(cell_id=rep(c("Normal", "No def", "Full def"), c(3,1, 1)),
                        lossyear = as.integer(c(0,2,3, 0, 5)),
                        area = 10000) %>%   
    group_by(cell_id) %>%
    mutate(area_total = sum(area)) %>%
    ungroup()
  
  out_1 <- df_test_Han %>% 
    epl_han_process(recompute_forest=TRUE)
  # datapasta::dpasta(out_1%>% select(-any_of(colnames(df_test_Han))))
  res_out <- tibble::tribble(
    ~dfrt_any, ~area_forest_initial, ~area_forest_final, ~dfrt_year, ~dfrt_area, ~dfrt_area_by_tot, ~dfrt_area_by_forest,
    TRUE,                10000,                  0,      2005L,      10000,               100,                  100,
    FALSE,                10000,              10000,         NA,          0,                 0,                    0,
    TRUE,                30000,              10000,      2002L,      10000,  33.3333333333333,     33.3333333333333,
    TRUE,                30000,              10000,      2003L,      10000,  33.3333333333333,     33.3333333333333
  )
  testthat::test_that("new output matches recorded output",
                      expect_equal(res_out,
                                   out_1%>% select(-any_of(colnames(df_test_Han)))))
  
  
  ## Case 2: info on initial forest area
  df_test_Han_2 <- tibble(cell_id=rep(c("Normal", "Zero def", "No deforestable", "some def"), c(3,1, 1,1)),
                          lossyear = as.integer(c(0,2,3,0,NA,5)),
                          area = 10000) %>%   
    group_by(cell_id) %>%
    mutate(area_total = sum(area)) %>%
    ungroup() %>% 
    mutate(area_forest_initial = if_else(is.na(lossyear), 0, area_total))
  
  df_test_Han_2
  
  test_that("Creates warning if initial there but not provided",
            expect_warning(
              df_test_Han_2 %>% 
                epl_han_process() ,
              "No initial forest 'area_mask_forest_var' provided, computing it by setting `recompute_forest=TRUE`, 
              assuming total area = total (de)forest(ed)",
              fixed = TRUE)
  )
  
  out_2A <- df_test_Han_2 %>% 
    epl_han_process(area_mask_forest_var = area_forest_initial) 
  
  # datapasta::dpasta(out_2A)
  out_2A_out <- tibble::tribble(
    ~cell_id, ~dfrt_any, ~area_total, ~area_forest_initial, ~area_forest_final, ~dfrt_year, ~dfrt_area, ~dfrt_area_by_tot, ~dfrt_area_by_forest,
    "No deforestable",     FALSE,       10000,                    0,                  0,         NA,         NA,                NA,                   NA,
    "Normal",      TRUE,       30000,                30000,              10000,      2002L,      10000,  33.3333333333333,     33.3333333333333,
    "Normal",      TRUE,       30000,                30000,              10000,      2003L,      10000,  33.3333333333333,     33.3333333333333,
    "some def",      TRUE,       10000,                10000,                  0,      2005L,      10000,               100,                  100,
    "Zero def",     FALSE,       10000,                10000,              10000,         NA,          0,                 0,                    0)
  testthat::test_that("epl_han_process with area_mask_forest_var: output recorded",
                      expect_equal(out_2A,
                                   out_2A_out %>% arrange(cell_id, dfrt_year)))
  
  
  ## overriding area forest initial
  out_2B <- df_test_Han_2 %>% 
    epl_han_process(area_mask_forest_var = area_forest_initial, recompute_forest=TRUE) 
  
  test_that("recomputing forest will be different when inital forest is provided",
            expect_false(isTRUE(all.equal(out_2B, out_2A)))
  )
  
  ## no overwrite
  df_test_Han_2 %>% 
    epl_han_process(area_mask_forest_var = area_forest_initial,
                    recompute_forest=TRUE, overwrite_mask_forest = FALSE)
  
  ## Multiple group vars?
  set.seed(123)
  df_test_Han_3 <- tibble(cell_id=rep(c("A", "B"), each=6),
                          cell_group=rep(c("1", "2"), 6)) %>% 
    arrange(cell_id, cell_group) %>% 
    mutate(lossyear = rep(c(0, 1, 2), 4),
           area = rnorm(n(), mean=10)) %>%   
    group_by(cell_id, cell_group) %>%
    mutate(area_total = sum(area)) %>%
    ungroup() 
  
  out_by_2groups_join <- df_test_Han_3 %>% 
    epl_han_process(.group_vars = c(cell_id, cell_group),recompute_forest=TRUE)
  
  out_by_2groups_sep <- bind_rows(df_test_Han_3 %>% 
                                    filter(cell_group==1) %>% 
                                    epl_han_process(.group_vars = c(cell_id, cell_group),recompute_forest=TRUE),
                                  df_test_Han_3 %>% 
                                    filter(cell_group==2) %>% 
                                    epl_han_process(.group_vars = c(cell_id, cell_group),recompute_forest=TRUE)) %>% 
    arrange(cell_id, cell_group)
  
  test_that("With 2 grouping vars, doing join is same as separate",
            expect_equal(out_by_2groups_join,out_by_2groups_sep)
  )
  
  ## multiple categories of deforestation
  ## one should not add it as additional grouping var!!!
  df_test_Han_4 <- tibble(cell_id=rep(c("A", "B"), each=6),
                          land_categ=rep(c("1", "2"), 6)) %>% 
    arrange(cell_id, land_categ) %>% 
    mutate(lossyear = rep(c(0, 1, 2), 4),
           area = rnorm(n(), mean=10)) %>%   
    filter(!(cell_id=="B" & land_categ==2 & lossyear %in% 1:2)) %>% 
    group_by(cell_id) %>%
    mutate(area_total = sum(area)) %>%
    ungroup() 
  
  df_test_Han_4
  df_test_Han_4 %>% 
    epl_han_process(.group_vars = c(cell_id),recompute_forest=TRUE) %>% 
    select(-"dfrt_area_by_tot")%>%
    arrange(cell_id, land_categ, dfrt_year)
  df_test_Han_4 %>% 
    epl_han_process(.group_vars = c(cell_id, land_categ),recompute_forest=TRUE) %>% 
    select(-"dfrt_area_by_tot")%>%
    arrange(cell_id, land_categ, dfrt_year)
  
}


################################
#'## Re-add forest cover
################################

#' @param area_var total area of polygon
#' @param frst_initial_var Var with initial forest coverage
epl_han_add_frst_cover <- function(df, .group_vars = cell_id,
                                   year_var = dfrt_year,
                                   area_var = area_total,
                                   frst_initial_var=area_forest_initial){
  .Deprecated("Should use epl_add_forest instead!")
  df %>% 
    group_by(across({{.group_vars}})) %>% 
    mutate({{year_var}}:=if_else({{year_var}}==0,
                                 2000L,{{year_var}}), 
           dfrt_area = if_else({{year_var}}==2000,
                               0, dfrt_area),
           frst_area = {{frst_initial_var}}-cumsum(dfrt_area),
           dfrt_area = if_else({{year_var}}==2000,
                               NA_real_, dfrt_area)) %>% 
    ungroup() %>% 
    mutate(frst_perc_by_area = 100*frst_area/{{area_var}})
  
}

################################
#'## Show differences with 
#' new area forest initial
################################

#' Show differences
#' 
#' @param df output from `epl_han_process()` with `recompute_forest = TRUE` and `overwrite_mask_forest = FALSE`
#' @param tol for comparison
#' @param distinct,.group_vars options to disinct() output
epl_han_check_initial_frst <- function(df, tol = 1e-12, distinct=FALSE, .group_vars = cell_id){
  
  if(!all(c("area_forest_initial", "area_forest_initial_recompute") %in% colnames(df))) {
    stop("df should contain area_forest_initial and area_forest_initial_recompute, obtained with 
         `recompute_forest = TRUE` and `overwrite_mask_forest = FALSE`")
  }
  
  res <- df %>% 
    mutate(diff=abs(area_forest_initial-area_forest_initial_recompute)) %>% 
    filter(diff>tol) %>% 
    arrange(desc(diff)) 
  if(distinct) {
    res <- res%>% 
      distinct(across({{.group_vars}}), area_forest_initial, area_forest_initial_recompute, diff)
  }
  res
}

################################
#'## Overwrite forest initial/final
################################

epl_han_overwrite_forest <- function(df, .group_vars = cell_id){
  
  # required_vars <- c("area_forest_initial", "area_forest_initial_recompute", "dfrt_area", "area_forest_final")
  # if(!all(required_vars %in%colnames(df))) stop(paste("Need vars")
  
  df %>% 
    mutate(area_forest_initial = if_else(area_forest_initial==0,
                                         area_forest_initial,
                                         area_forest_initial_recompute)) %>% 
    select(-area_forest_initial_recompute) %>%
    ## recompute area_forest_final
    group_by(across({{.group_vars}})) %>% 
    mutate(area_forest_final = area_forest_initial-sum(dfrt_area)) %>% 
    ungroup() %>% 
    ## recompute dfrt_area_by_forest
    mutate(area_forest_final = if_else(area_forest_initial==0,0,area_forest_final),
           dfrt_area_by_forest = 100*dfrt_area/area_forest_initial)
}


################################
#'## Check Hansen
################################

epl_han_check_final <- function(df, tol = 1e-15, .group_vars = cell_id,
                                area_var = area_total,
                                stop_if_not_passed=TRUE,
                                has_forest_cover_0_year=FALSE){
  
  do_stop <- FALSE
  
  ## Check 0: illegal NAs
  cat("Test 1: no unexpected NAs\n")
  if(anyNA(df %>% select(area_forest_initial, area_forest_final, any_of(c("frst_area", "frst_area_by_tot"))))){
    cat("\tNot passed! 😱\n")
    dat_prob <- df %>% 
      filter(if_any(c(area_forest_initial, area_forest_final, any_of(c("frst_area", "frst_area_by_tot"))), 
                    ~is.na(.))) %>% 
      distinct(across({{.group_vars}}), area_forest_initial, area_forest_final, across(any_of(c("frst_area", "frst_area_by_tot"))))
    print(dat_prob)
    if(stop_if_not_passed) do_stop <- TRUE
  } else {
    cat("\tPassed! 🎉\n")
  }
  
  ## check 1: forest final 
  test_1_df <- df %>% 
    group_by(across(c({{.group_vars}}, area_forest_initial, area_forest_final))) %>% 
    summarise(dfrt_total= sum(dfrt_area), .groups="drop") %>% 
    ungroup() %>% 
    mutate(dfrt_total_final_initial = area_forest_initial-area_forest_final,
           diff_init_final = abs(dfrt_total_final_initial-dfrt_total)) %>% 
    arrange(desc(diff_init_final)) 
  cat("Test 2: area_final = area_initial - sum(dfrt_area)\n")
  if(any(test_1_df$diff_init_final>tol, na.rm=TRUE)) {
    cat("\tNot passed! 😱\n")
    print(filter(test_1_df, diff_init_final>tol))
    if(stop_if_not_passed) do_stop <- TRUE
  } else {
    cat("\tPassed! 🎉\n")
  }
  
  ## Test 2: if !dfrt_any, then NA dfrt_a*
  test_2_A_df <- df %>% 
    filter(area_forest_initial>0) %>% 
    # filter(dfrt_any) %>% 
    select(dfrt_year, starts_with(c("dfrt_area", "area_"))) %>% 
    filter(if_any(everything(), ~is.na(.)|!is.finite(.))) 
  
  if(has_forest_cover_0_year){
    test_2_A_df <- test_2_A_df %>% 
      filter(dfrt_year!=min(test_2_A_df$dfrt_year))
  }
  
  test_2_B_df <- df %>%
    filter(area_forest_initial==0) %>% 
    # filter(!dfrt_any) %>% 
    select(starts_with(c("dfrt_area"))) %>% 
    filter(if_any(everything(), ~!is.na(.))) 
  
  
  cat("Test 3 A: No NA values if dfrt_any\n")
  if(nrow(test_2_A_df)>0){
    cat("\tNot passed! 😱\n")
    print(test_2_A_df)
    if(stop_if_not_passed) do_stop <- TRUE
  } else {
    cat("\tPassed! 🎉\n")
  }
  
  cat("Test 3 B: if forest_initial =0 THEN NA dfrt_area_*\n")
  if(nrow(test_2_B_df)>0){
    cat("\tNot passed! 😱\n")
    print(test_2_B_df)
    if(stop_if_not_passed) do_stop <- TRUE
  } else {
    cat("\tPassed! 🎉\n")
  }
  
  
  ### Test 4: unique area_initial
  test_4_df <- df %>% 
    distinct(across(c({{.group_vars}}, starts_with("area")))) %>% 
    add_count(across({{.group_vars}})) %>% 
    filter(n>1) 
  
  cat("Test 4: area_* vars are unique at .group_vars level\n")
  if(nrow(test_4_df)>0){
    cat("\tNot passed! 😱\n")
    print(test_4_df)
    if(stop_if_not_passed) do_stop <- TRUE
  } else {
    cat("\tPassed! 🎉\n")
  }
  
  ### Test 5: dfrt_area_by is correct
  test_5_df <- df %>% 
    mutate(dfrt_area_by_tot_check= 100 * dfrt_area/{{area_var}},
           dfrt_area_by_forest_check= 100 * dfrt_area/area_forest_initial,
           diff_by_tot = dfrt_area_by_tot_check- dfrt_area_by_tot,
           diff_by_forest = dfrt_area_by_forest_check- dfrt_area_by_forest) %>% 
    filter(abs(diff_by_tot)>tol |abs(diff_by_forest)>tol) %>% 
    select({{.group_vars}}, starts_with(c("dfrt_area_by", "diff_by_")))
  
  cat("Test 5: dfrt_area_by_* is correct\n")
  if(nrow(test_5_df)>0){
    cat("\tNot passed! 😱\n")
    print(test_5_df)
    if(stop_if_not_passed) do_stop <- TRUE
  } else {
    cat("\tPassed! 🎉\n")
  }
  
  ## Test 6: forest cover is correct?
  if(any(str_detect("frst_area", colnames(df)))){
    cat("Test 6: final forest cover: using epl_add_forest_check()\n")
    test_6_df <- epl_add_forest_check(df, tol=tol, has_year_0 = has_forest_cover_0_year)
    if(nrow(test_6_df) > 0){
      cat("\tNot passed! 😱\n")
      print(test_6_df)
      if(stop_if_not_passed) do_stop <- TRUE
    } else {
      cat("\tPassed! 🎉\n")
    }
  }
  
  ## Test 7: forest cover is correct?
  if(all(c("area_forest_initial", "area_forest_final") %in% colnames(df))){
    cat("Test 7: dfrt_any==FALSE <=> area_forest_initial==area_forest_final  \n")
    test_7_df <- df %>% 
      filter((area_forest_initial==area_forest_final & dfrt_any)|(area_forest_initial!=area_forest_final & !dfrt_any))
    if(nrow(test_7_df) > 0){
      cat("\tNot passed! 😱\n")
      print(test_7_df)
      if(stop_if_not_passed) do_stop <- TRUE
    } else {
      cat("\tPassed! 🎉\n")
    }
  }
  
 
  if(do_stop) stop()
  
}



################################
#'## PRODES tests
################################

if(FALSE){
  
  ## PRODES
  df_test_PRD_raw <- tibble(PRODES_class_id = c(1,2,3, 1,15,16,
                                                1,2,16, 2,3),
                            sum_area = 10000,
                            cell_id=rep(c("No def", "Normal_full_forest", "Normal_half_forest", "No deforestable"), c(3,3,3, 2)),
                            dfrt_area_in_pixels=2L)
  
  
  ## NEW  
  df_test_PRD_raw %>% 
    epl_PRD_transfo_NEW(PRD_year = 2021, units_is = "m2", units_set = "km2")
  
  ## previous
  df_test_PRD_raw %>% 
    epl_PRD_add_categs(PRD_year=2021, meta_cols_keep = c("PRODES_class")) %>% 
    epl_PRD_transfo(.group_vars=cell_id, .PRD_class_var = PRODES_class) %>% 
    select(-dfrt_area_in_pixels) 
  
  
}


if(FALSE){
  
  # ## Case A: hansen style, only def year
  #  -> places with no def at start ignored
  #  -> no variable def_by_forest, forest_initla and forest final
  # 
  # ## Case B: def year and more
  #  -> rows for start 0 forest should have NA in dfrt_year, yet value in dfrt_area
  #  -> then either:
  #     - there is already a column area_forest_initial
  #     - other rows have also NA in dfrt_year
  
  
}

################################
#'## Complete
################################


#' Complete years with 0
#' 
#' @param df data-frame
#' @param year_var name of the year variable
#' @param nest_vars variable to nest for complete
#' @param years_all Years to complete
epl_dfrt_complete <- function(df, year_var= dfrt_year,
                              nest_vars=nesting(cell_id, cellsize_km, area_forest_initial, dfrt_any, area_total, area_forest_final),
                              years_all = NULL,
                              preserve_NA_dfrt = TRUE){
  
  ## get all years in data
  if(is.null(years_all)) {
    years_all <- unique(df %>% pull({{year_var}})) %>% discard(is.na) %>% 
      sort()
    
    ## check all years ?
    years_seq_full <- seq(min(years_all), max(years_all))
    if(!all(years_seq_full%in%years_all)) warning("Some years with 0 deforestation data? Use argument 'years_all'")
  }
  
  ##
  # vars_fill <- select(df, starts_with("dfrt_")) %>% colnames() %>% 
  #   discard(~. %in% c("dfrt_any", "dfrt_year"))
  
  ##
  
  ## complete all cases
  ## follow-up status on: https://github.com/tidyverse/tidyr/issues/1397
  df_comp <- df %>%
    complete({{nest_vars}},
             dfrt_year=years_all,
             fill = list(dfrt_area=0, dfrt_area_by_tot=0, dfrt_area_by_forest=0))
  
  ## complete "non-deforestable"cases
  df_not_deforestable <- df %>%
    filter(area_forest_initial==0) %>% 
    select(starts_with("dfrt_area"))
  if(anyNA(df_not_deforestable)){
    if(!all(is.na(df_not_deforestable))) warning("Only some 'No deforestable' with NA, not all?")
    if(preserve_NA_dfrt) {
      df_comp <- df_comp %>% 
        mutate(across(starts_with("dfrt_area"), 
                      ~if_else(!dfrt_any & area_forest_initial==0, NA_real_,.)))
    } else {
      warning("Overwriting dfrt_area_* NA with 0 for non deforestable units.")
    }
  }
  
  ## remove dfrt_year NA
  df_comp <- df_comp %>% 
    filter(!is.na(dfrt_year))
  
  ## return result
  df_comp
}

if(FALSE){
  ## data
  df_test_Han_2 <- tibble(cell_id=rep(c("Normal", "Zero def", "No deforestable", "some def"), c(3,1, 1,1)),
                          lossyear = as.integer(c(0,2,3,0,NA,5)),
                          area = 10000) %>%   
    group_by(cell_id) %>%
    mutate(area_total = sum(area)) %>%
    ungroup() %>% 
    mutate(area_forest_initial = if_else(is.na(lossyear), 0, area_total))
  
  ## run epl_han_process
  df <- df_test_Han_2 %>% 
    epl_han_process(area_mask_forest_var = area_forest_initial) 
  
  ## apply
  df%>% 
    epl_dfrt_complete(nest_vars = nesting(cell_id, dfrt_any, area_total, area_forest_initial, area_forest_final)) 
  
  df%>% 
    epl_dfrt_complete(nest_vars = nesting(cell_id, dfrt_any, area_total, area_forest_initial, area_forest_final),
                      years_all=2002:2005) 
  df%>% 
    epl_dfrt_complete(nest_vars = nesting(cell_id, dfrt_any, area_total, area_forest_initial, area_forest_final),
                      years_all=2002:2005, preserve_NA_dfrt = FALSE) 
}


#' Add forest
epl_add_forest <- function(df, .group_vars = cell_id,
                           area_mask_forest_var = area_forest_initial,
                           full_area_var=area_total,
                           first_year =2000L,
                           add_first_year = FALSE,
                           year_var= dfrt_year,
                           na_0_initial_to_0=TRUE){
  ## step 1: add for newer years
  df_out <- df %>% 
    group_by(across({{.group_vars}})) %>% 
    arrange({{year_var}}) %>% 
    mutate(frst_area = {{area_mask_forest_var}}-cumsum(dfrt_area),
           frst_area_by_tot = 100* frst_area/ {{full_area_var}}) %>% 
    ungroup()
  if(na_0_initial_to_0 && any(pull(df, {{area_mask_forest_var}})==0)){
    df_out <- df_out %>% 
      mutate(frst_area = if_else({{area_mask_forest_var}}==0,0, frst_area),
             frst_area_by_tot = if_else({{area_mask_forest_var}}==0,0, frst_area_by_tot))
    
  }
  
  
  ## step 2: add missing year
  if(add_first_year){
    if(first_year %in% pull(df_out, {{year_var}})) {
      warning(paste("First year", first_year, "already in data!?"))
    }
    df_out_add <- df_out %>% 
      slice_min({{year_var}}) %>% 
      mutate(across(starts_with("dfrt_area"), ~NA),
             frst_area = {{area_mask_forest_var}},
             frst_area_by_tot = 100* frst_area/ {{full_area_var}},
             {{year_var}} := first_year)
    df_out <- df_out %>% 
      rbind(df_out_add) %>% 
      arrange(across(c({{.group_vars}}, {{year_var}})))
  }
  df_out
}


#' @param has_year_0 Does the data contain the initial year?
epl_add_forest_check <- function(df, last_year = max(pull(df, {{year_var}}), na.rm=TRUE),
                                 area_forest_final_var=area_forest_final,
                                 area_forest_initial_var=area_forest_initial,
                                 year_var = dfrt_year,
                                 has_year_0=FALSE,
                                 tol = 1e-16){
  
  ## check variable final is in data
  area_forest_final_var_char <- rlang::as_name(rlang::enquo(area_forest_final_var))
  
  if(! area_forest_final_var_char%in% colnames(df)) {
    stop(paste("Need variable ", area_forest_final_var_char, "in data. Change arg 'area_forest_final_var'?"))
  }
  test_1 <- df %>% 
    filter({{year_var}}==last_year) %>% 
    filter(abs(frst_area-{{area_forest_final_var}})>tol)
  
  if(nrow(test_1)>0) {
    warning("Last forest cover not equal to forest_final_var?")
  }
  
  ## test 2: if has initial year
  if(has_year_0){
    test_2 <- df %>% 
      slice_min({{year_var}}) %>% 
      filter(abs(frst_area-{{area_forest_initial_var}})>tol)
    if(nrow(test_2)>0) {
      warning("First forest cover not equal to forest_initial_var?")
    }  
    test_1 <- rbind(test_1, test_2)
  }
  test_1
}


if(test_here){
  
  
  ## prep data
  df_test_Han_prep <- df_test_Han %>% 
    epl_han_process(recompute_forest=TRUE) %>% 
    epl_dfrt_complete(nest_vars=nesting(cell_id, area_forest_initial, dfrt_any, area_total, area_forest_final),
                      years_all=2000:2005) 
  df_test_Han_2_prep <- df_test_Han_2 %>% 
    epl_han_process(area_mask_forest_var = area_forest_initial) %>% 
    epl_dfrt_complete(nest_vars = nesting(cell_id, dfrt_any, area_total, area_forest_initial, area_forest_final),
                      years_all=2000:2005) 
  
  ## do 1  
  out_1_no2000 <- df_test_Han_prep%>% 
    epl_add_forest() 
  out_1_no2000_add <- df_test_Han_prep%>% 
    epl_add_forest(add_first_year=TRUE, first_year=1999) 
  
  out_2_no2000 <- df_test_Han_prep%>% 
    epl_add_forest() 
  out_2_no2000_add <- df_test_Han_prep%>% 
    epl_add_forest(add_first_year=TRUE, first_year=1999) 
  
  epl_han_check_final(out_1_no2000_add, has_forest_cover_0_year = TRUE)
  epl_han_check_final(out_2_no2000_add, has_forest_cover_0_year = TRUE)
  
  test_that("epl_add_forest: final cover is same as forest_final", 
            expect_equal(nrow(epl_add_forest_check(out_1_no2000)), 0))
  test_that("epl_add_forest: final cover is same as forest_final", 
            expect_equal(nrow(epl_add_forest_check(out_1_no2000_add)), 0))
  test_that("epl_add_forest: final cover is same as forest_final", 
            expect_equal(nrow(epl_add_forest_check(out_2_no2000)), 0))
  test_that("epl_add_forest: final cover is same as forest_final", 
            expect_equal(nrow(epl_add_forest_check(out_2_no2000_add)), 0))
  
}



################################
#'## Add annualised dfrt rate
################################

#' add annualised
internl_div <- function(x,y) {
  res <- x/y
  if(any(is.nan(res))) {
    res[is.nan(res) & y==0] <- 0
  } 
  res
}
epl_add_dfrt_annualized <- function(df, .group_vars = cell_id,
                                    frst_area_var=frst_area,
                                    year_var = dfrt_year, warn_unbalanced=TRUE){
  
  ## check is balanced
  if(warn_unbalanced){
    cnt <- df%>% 
      count(across({{.group_vars}}), name = "n_by_group") %>% 
      count(n_by_group)
    if(nrow(cnt)!=1) warning("Not a balanced dataset!?")
  }
  
  ## 
  df%>% 
    group_by(across({{.group_vars}})) %>% 
    arrange({{year_var}}, .by_group = TRUE) %>%
    mutate(dfrt_area_annualized = 100* internl_div(dfrt_area, dplyr::lag({{frst_area_var}}))) %>% 
    ungroup()
}

if(FALSE){
  df_test_Han_2_prep %>% 
    epl_add_forest() %>% 
    epl_add_dfrt_annualized() %>% 
    select(-area_forest_final, -dfrt_area_by_tot, -dfrt_area_by_forest)
}

#' ################################
#' #'## Hansen check
#' ################################
#' 
#' #' Check whether `area_mask_forest_var` is same as sum(dfrt_area)
#' epl_dfrt_check_initial_is_sum_all <- function(df, .group_vars = cell_id,
#'                                               area_mask_forest_var = area_forest_initial, tol_warn=1e-16){
#'   res <- df %>%
#'     replace_na(list(dfrt_area=0)) %>% ## here yes dfrt area NA is 0!
#'     group_by(across(c({{.group_vars}}, {{area_mask_forest_var}}))) %>%
#'     summarise(dfrt_area_check=sum(dfrt_area)) %>%
#'     mutate(diff = dfrt_area_check - {{area_mask_forest_var}}) %>%
#'     ungroup() %>%
#'     arrange(desc(abs(diff)))
#'   
#'   if(nrow(filter(res, diff>tol_warn))>0){
#'     warning("Small aggregation/rounding issue?")
#'   }
#' 
#'   res
#' }
#' 
#' if(FALSE){
#'   
#'   out_2A <- df_test_Han_2 %>% 
#'     epl_han_process(area_mask_forest_var = area_forest_initial) 
#'   
#'   out_2A
#'   
#'   out_2A %>% 
#'     epl_dfrt_check_initial_is_sum_all(area_mask_forest_var = area_forest_initial)
#' }
