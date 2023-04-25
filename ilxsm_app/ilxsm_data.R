### -- Functions for wrangling ILXSM data from BSM -- ### 

## Selecting weights
isolate_weights <- function(bsm_data, stat_area_shapefile) {
  dat <- read.csv(bsm_data)
  dat <- dat %>%
    mutate(PARAM_VALUE_NUM = case_when(PARAM_TYPE == 'ML' & UNIT_MEASURE == 'CM' ~ PARAM_VALUE_NUM * 10, 
                                       PARAM_TYPE == 'ML' & UNIT_MEASURE == 'MM'~ PARAM_VALUE_NUM, 
                                       PARAM_TYPE == 'WT' & UNIT_MEASURE == 'GM' ~ PARAM_VALUE_NUM))
  dat$LAND_DATE <-lubridate::dmy(dat$LAND_DATE)
  dat <- dat %>%
    mutate(year = lubridate::year(LAND_DATE),
           month = lubridate::month(LAND_DATE),
           week = lubridate::week(LAND_DATE), 
           day = lubridate::day(LAND_DATE))
  wt <- dat %>%
    filter(PARAM_TYPE == 'WT') %>%
    dplyr::rename(weight = 'PARAM_VALUE_NUM')
  ## Add coordinates
  wd = here::here('shapefiles')
  stat_areas_sp <- rgdal::readOGR(wd, stat_area_shapefile, verbose = FALSE) 
  proj4string(stat_areas_sp) <- CRS("+init=epsg:4326")
  # Subset the shape file for just the stat areas that match the ILXSM data
  stat_areas2 <- stat_areas_sp[na.omit(stat_areas_sp@data$Id %in% c('526','534',
                                                                    '537','541',
                                                                    '562',
                                                                    '615','616', 
                                                                    '621','622',
                                                                    '623', '624',
                                                                    '626', '631',
                                                                    '632', '633')),]
  shp_df <- broom::tidy(stat_areas2, region = 'Id')
  # Identify centroid of each stat area by name
  centroid.stat <- aggregate(cbind(long, lat) ~ id, data=shp_df, FUN=mean)
  # Adjust a couple of the stat area label locations
  centroid.stat[1,c(2,3)] <- c(-69.50, 40.50)
  centroid.stat[3,c(2,3)] <- c(-70.75, 40.50)
  centroid.stat[5,c(2,3)] <- c(-66.75, 41.25)
  centroid.stat[8,c(2,3)] <- c(-74.5, 38.5)
  centroid.stat[13,c(2,3)] <- c(-75.25, 36.5)
  
  # Add column to store coordinates
  wt <- wt %>%
    add_column(lat = NA, 
               lon = NA)
  # Match and record coords by stat area in exisiting data frame (df)
  for (i in 1:length(centroid.stat$id)){
    a <- subset(wt, AREA_CODE == centroid.stat$id[i])
    locs <- which(wt$AREA_CODE %in% a$AREA_CODE)
    b <- subset(centroid.stat, id == centroid.stat$id[i], select = 'lat')
    c <- subset(centroid.stat, id == centroid.stat$id[i], select = 'long')
    wt[locs,17] <- b$lat
    wt[locs,18] <- c$long
  }
  wt <- wt
}

## Selecting lengths and adding in centroid coordinates for stat areas
isolate_lengths <- function(bsm_data, stat_area_shapefile) {
  dat <- read.csv(bsm_data)
  dat <- dat %>%
    mutate(PARAM_VALUE_NUM = case_when(PARAM_TYPE == 'ML' & UNIT_MEASURE == 'CM' ~ PARAM_VALUE_NUM * 10, 
                                       PARAM_TYPE == 'ML' & UNIT_MEASURE == 'MM'~ PARAM_VALUE_NUM, 
                                       PARAM_TYPE == 'WT' & UNIT_MEASURE == 'GM' ~ PARAM_VALUE_NUM))
  dat$LAND_DATE <-lubridate::dmy(dat$LAND_DATE)
  dat <- dat %>%
    mutate(year = lubridate::year(LAND_DATE),
           month = lubridate::month(LAND_DATE),
           week = lubridate::week(LAND_DATE), 
           day = lubridate::day(LAND_DATE))
  ml <- dat %>%
    filter(PARAM_TYPE == 'ML') %>%
    dplyr::rename(length = PARAM_VALUE_NUM)
  ## Add coordinates
  wd = here::here('shapefiles')
  stat_areas_sp <- rgdal::readOGR(wd, stat_area_shapefile, verbose = FALSE) 
  proj4string(stat_areas_sp) <- CRS("+init=epsg:4326")
  # Subset the shape file for just the stat areas that match the ILXSM data
  stat_areas2 <- stat_areas_sp[na.omit(stat_areas_sp@data$Id %in% c('526','534',
                                                                    '537','541',
                                                                    '562',
                                                                    '615','616', 
                                                                    '621','622',
                                                                    '623', '624',
                                                                    '626', '631',
                                                                    '632', '633')),]
  shp_df <- broom::tidy(stat_areas2, region = 'Id')
  # Identify centroid of each stat area by name
  centroid.stat <- aggregate(cbind(long, lat) ~ id, data=shp_df, FUN=mean)
  # Adjust a couple of the stat area label locations
  centroid.stat[1,c(2,3)] <- c(-69.50, 40.50)
  centroid.stat[3,c(2,3)] <- c(-70.75, 40.50)
  centroid.stat[5,c(2,3)] <- c(-66.75, 41.25)
  centroid.stat[8,c(2,3)] <- c(-74.5, 38.5)
  centroid.stat[13,c(2,3)] <- c(-75.25, 36.5)
  
  # Add column to store coordinates
  ml <- ml %>%
    add_column(lat = NA, 
               lon = NA)
  # Match and record coords by stat area in exisiting data frame (df)
  for (i in 1:length(centroid.stat$id)){
    a <- subset(ml, AREA_CODE == centroid.stat$id[i])
    locs <- which(ml$AREA_CODE %in% a$AREA_CODE)
    b <- subset(centroid.stat, id == centroid.stat$id[i], select = 'lat')
    c <- subset(centroid.stat, id == centroid.stat$id[i], select = 'long')
    ml[locs,17] <- b$lat
    ml[locs,18] <- c$long
  }
  ml <- ml
  
  }

## Pairing lengths and weights 
# Consolidate the dataset to just samples that have paired weight/length values
pair_length_weight <- function(mantle_length_df, body_weight_df) {
  
  ml.c <- ml %>% dplyr::select(LAND_DATE,AREA_CODE,ORGANISM_ID, 
                               length,VESSEL_NAME, VTR_SERIAL_NUM,
                               year, month, week, day, lat, lon)
  
  wt.c <- wt %>% dplyr::select(LAND_DATE,AREA_CODE,ORGANISM_ID, 
                               weight,VESSEL_NAME, VTR_SERIAL_NUM,
                               year, month, week, day, lat, lon)
  
  lw_paired <- right_join(ml.c, wt.c, 
                          by = c('LAND_DATE','AREA_CODE','ORGANISM_ID', 
                                 'VESSEL_NAME', 'VTR_SERIAL_NUM',
                                 'year', 'month', 'week', 'day', 'lat', 'lon'))
  
  lw_paired <- na.omit(lw_paired)
  lw_paired <- lw_paired %>% relocate(weight, .after = length)
}


## Adding lat lons - this is a seperate function so that you can easily pair
## different locations if necessary
# add_locs <- function(stat_area_shapefile, df) {
#   wd = here::here('shapefiles')
#   stat_areas_sp <- rgdal::readOGR(wd, stat_area_shapefile, verbose = FALSE) 
#   proj4string(stat_areas_sp) <- CRS("+init=epsg:4326")
#   # Subset the shape file for just the stat areas that match the ILXSM data
#   stat_areas2 <- stat_areas_sp[na.omit(stat_areas_sp@data$Id %in% c('526','534',
#                                                                     '537','541',
#                                                                     '562',
#                                                                     '615','616', 
#                                                                     '621','622',
#                                                                     '623', '624',
#                                                                     '626', '631',
#                                                                     '632', '633')),]
#   shp_df <- broom::tidy(stat_areas2, region = 'Id')
#   # Identify centroid of each stat area by name
#   centroid.stat <- aggregate(cbind(long, lat) ~ id, data=shp_df, FUN=mean)
#   # Adjust a couple of the stat area label locations
#   centroid.stat[1,c(2,3)] <- c(-69.50, 40.50)
#   centroid.stat[3,c(2,3)] <- c(-70.75, 40.50)
#   centroid.stat[5,c(2,3)] <- c(-66.75, 41.25)
#   centroid.stat[8,c(2,3)] <- c(-74.5, 38.5)
#   centroid.stat[13,c(2,3)] <- c(-75.25, 36.5)
#   # Add column to store coordinates
#   df <- df %>%
#     add_column(lat = NA, 
#                lon = NA)
#   # Match and record coords by stat area in exisiting data frame (df)
#   for (i in 1:length(centroid.stat$id)){
#     a <- subset(df, AREA_CODE == centroid.stat$id[i])
#     locs <- which(df$AREA_CODE %in% a$AREA_CODE)
#     b <- subset(centroid.stat, id == centroid.stat$id[i], select = 'lat')
#     c <- subset(centroid.stat, id == centroid.stat$id[i], select = 'long')
#     df[locs,17] <- b$lat
#     df[locs,18] <- c$long
#   }
#   df <- df
# }


