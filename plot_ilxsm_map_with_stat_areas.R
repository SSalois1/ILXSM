# Bringing in shape files and bathymetry for maps
library(shadowtext)
library(marmap)
library(ggplot2)
library(tidyverse)
library(sf)
library(RColorBrewer)
library(ggnewscale)
## Fix data - what is what - this is not the most updated, the newer ones do 
## not include stat area - need from Thomas
lw_data_old <- read.csv('ilxsm_weights_lengths.csv')

# Newer data from Thomas
lw_data <- read.csv('ILXSM_Pull_01_25_23.csv')
lw_data$LAND_DATE <-lubridate::dmy(lw_data$LAND_DATE)
lw_data <- lw_data %>%
  mutate(year = lubridate::year(LAND_DATE),
         month = lubridate::month(LAND_DATE),
         week = lubridate::week(LAND_DATE), 
         day = lubridate::day(LAND_DATE))
# wt_full <- lw_data %>%
#   filter(PARAM_TYPE == 'WT') %>%
#   rename(weight = PARAM_VALUE_NUM)

# Compare if there are new stat areas in updated data set
# Note - If AREA_CODE == 0, I drop it
unique_areas <- sort(unique(lw_data$AREA_CODE))
unique_areas_old <- sort(unique(lw_data_old$AREA_CODE))
# I do not know area 600:  526 534 537 541 562 600* 615 616 621 622 623 624 626 631 632 633
stat_areas <- data.frame(area_code = sort(unique(lw_data$AREA_CODE)), 
                         lat = c(0, 40.50, 39.5, 40.5, 39.5,
                                 41.25, 0, 39.5, 39.5, 38.5,
                                 38.5, 38.5, 38.5, 37.5,
                                 36.5, 36.5, 36.5), 
                         lon = c(0, -69.5, -70.50, -70.75, -69.5,
                                 -66.75, 0, -73.5,-72.35,-74.5,
                                 -73.5, -72.5, -71.0,-74.5,
                                 -75.25, -74.5, -73.5))
lw_tally <- lw_data %>% 
  group_by(AREA_CODE) %>%
  summarise(n = length(ORGANISM_ID))

lw_tally$lat <- NA
lw_tally$lon <- NA
for (i in 1:length(stat_areas$area_code)){
  a <- subset(lw_tally, AREA_CODE == unique_areas[i])
  locs <- which(lw_tally$AREA_CODE %in% a$AREA_CODE)
  b <- subset(stat_areas, area_code == unique_areas[i], select = 'lat')
  c <- subset(stat_areas, area_code == unique_areas[i], select = 'lon')
  lw_tally[locs,3] <- b$lat
  lw_tally[locs,4] <- c$lon
}

# If you need to remove stat area 0 and 600, remove these rows 
# lw_tally <- lw_tally[-c(1,7),] 
# Mixed Area 000 are all stat areas
# the 600 series is only stat areas that are identified from 601 to 699
# (Stat area 639 is the largest at the moment). 
# Also there are micro level series, they end in tenths. 
# Example #1:  610, 620 630, ect. 
# Example #2: The 610 series is comprised of stat areas  611 ,612 , 613:619.

## Bring in NAFO shelfbreak shapefiles
wd = here::here("shapefiles")
stat_areas_sp <- rgdal::readOGR(wd,'groundfish_stat_areas') # wd = working directory
proj4string(stat_areas_sp) <- CRS("+init=epsg:4326")
plot(stat_areas_sp) # just looking
# 526 534 537 541 562 600* 615 616 621 622 623 624 626 631 632 633
stat_areas2 <- stat_areas_sp[na.omit(stat_areas_sp@data$Id %in% c('526','534',
                                                                  '537','541',
                                                                  '562', '600',
                                                                  '615','616', 
                                                                  '621','622',
                                                                  '623', '624',
                                                                  '626', '631',
                                                                  '632', '633')),]

plot(stat_areas2) # just looking 
ggplot()+
  geom_sf(data = stat_areas2 %>% st_as_sf() ,  
          fill = NA, color = 'gray20', lwd = 0.6) +
  geom_point(data = lw_tally,
             aes(x = lon, y = lat, size = n), 
             color = 'black', bg = 'deeppink3',
             pch = 21) +
  coord_sf(xlim = c(-76,-66.0), ylim = c(35.5,44),
           datum = sf::st_crs(4326)) +
  labs(size = 'Number of Samples') +
  theme_classic()
  

wd = ("C:/Users/sarah.salois/Documents/github/ssalois1/calc_wcr_metrics")
US.areas <- rgdal::readOGR(wd, 'USA')
proj4string(US.areas) <- CRS("+init=epsg:4326")
wd = ("~/github/ssalois1/NEFSC-illex_indicator_viewer/shapefiles")
canyons <- rgdal::readOGR(wd, 'major_canyons')
class(canyons)
proj4string(canyons)
proj4string(canyons) <- CRS("+init=epsg:4326")
canyons <- st_read("~/github/ssalois1/NEFSC-illex_indicator_viewer/shapefiles/major_canyons.shp") %>% st_transform(., "+proj=longlat +datum=NAD83 +no_defs")
ports <- st_read("~/github/ssalois1/NEFSC-illex_indicator_viewer/shapefiles/major_ports.shp") %>% st_transform(., "+proj=longlat +datum=NAD83 +no_defs")
mesh_exmp <- rgdal::readOGR(wd, 'Illex_Fishery_Mesh_Exemption_Area')
lobster <- rgdal::readOGR(wd, 'Lobster_Restricted_Gear_Areas')

canada.areas <-rgdal::readOGR(wd,'Canada')
proj4string(canada.areas) <- CRS("+init=epsg:4326")
## bring in bathymetry
atl <- marmap::getNOAA.bathy(-78,-64, 35, 45)
atl = fortify.bathy(atl)
blues = colorRampPalette(brewer.pal(9,'Blues'))(25)
depths <- c(0,50,100,200,300,500,Inf)
depths2 <- c('0','50','100','200','>300')
blues2 <- blues[c(5,7,9,11,13,24)]


interestingcanyons <- c("Norfolk Canyon", "Wilmington Canyon", 
                        "Spencer Canyon", "Hudson Canyon", 
                        "Atlantis Canyon")

extent(mesh_exmp)
class      : Extent 
xmin       : -74.88333 
xmax       : -65.69947 
ymin       : 35.46667 
ymax       : 43.96667 
# Major Ports  
# Hampton, VA; Cape May, NJ; Point Judith, RI; New Bedford, MA
# New Bedford, MA = 41.6362, -70.9342
# Point Judith, RI = 41.3731, -71.4856 
# Cape May, NJ = 38.9351, -74.9060
# Hampton, VA = 37.0299, -76.3452
ports <- data.frame(lon = c( -70.9342,-71.4856,-74.9060, -76.3452), 
                    lat = c(41.6362, 41.3731,38.9351, 37.0299),
                    port = c('New Bedford, MA', 'Narragansett, RI', 
                             'Cape May, NJ', 'Hampton, VA'))
locations <- data.frame(lon = c( -70.9342,-71.4856,-74.9060, -76.3452), 
                    lat = c(41.6362, 41.3731,38.9351, 37.0299),
                    port = c('Cape Hatteras, NC', 'Narragansett, RI', 
                             'Cape May, NJ', 'Hampton, VA'))

fishing <- marmap::getNOAA.bathy(-80,-66, 36, 41,resolution=1)
fishing = fortify.bathy(fishing)

m1 = ggplot() +
  geom_contour_filled(data = atl,
                      aes(x=x,y=y,z=-1*z),
                      breaks=c(0,50,100,250,500,Inf),
                      size=c(0.3)) + 
  scale_fill_manual(values = blues2, # 5:20
                    name = paste("Depth (m)"),
                    labels = depths2) +
  geom_contour(data = atl,
               aes(x=x,y=y,z=-1*z),
               breaks=c(0,50,100,250,500,Inf),
               size=c(0.3), 
               col = 'lightgrey') +
  new_scale_fill() +
  geom_sf(data = US.areas %>% st_as_sf() ,color = 'gray20', fill = 'wheat3') +
  geom_sf(data = canada.areas %>% st_as_sf() ,color = 'gray20', fill = 'wheat3') +
  geom_sf(data = stat_areas2 %>% st_as_sf() ,  
          fill = NA, color = 'gray20', lwd = 0.6) +
  geom_point(data = lw_tally,
             aes(x = lon, y = lat, size = n), 
             color = 'black', bg = 'deeppink3',
             pch = 21) +
  geom_point(data = ports,
             aes(x = lon, y = lat), 
             color = 'black', bg = 'cornsilk1',
             pch = 23, lwd = 5) +
  labs(size = 'Number of Samples') +
  geom_label(data = ports, aes(lon, lat, label = port),
             colour = 'black', fill = 'cornsilk1', alpha = 0.8,
             fontface = 'bold', nudge_x = c(0.03,-1.2,0.07,0.80),
             nudge_y = c(0.40,0.2,0.4,0.33), size = 3) +
  coord_sf(xlim = c(-76,-66.0), ylim = c(35.5,44), datum = sf::st_crs(4326)) +
  xlab('Longitude') + 
  ylab('Latitude') +
  theme_bw()

m1 + theme(axis.text = element_text(size = 13), 
        axis.title = element_text(size = 15),
        axis.ticks.length = unit(-1.4, "mm")) 
ggsave(path = here::here('figures'),'ilxsm_map_all_years_updated.jpg', 
       width = 7, height = 8, units = "in", dpi = 300)
