library(patchwork)
library(ggplot2)
library(dplyr)
library(lubridate)
library(ecodata)
## -- FIGURE 1 -- ##
# Maps 
lw_data <- read.csv('Entire_Database_Pull_ILXSM_2.14.23.csv')
lw_tally_full <- lw_data %>% 
  group_by(AREA_CODE) %>%
  summarise(n = length(ORGANISM_ID))

lw_tally_full$lat <- NA
lw_tally_full$lon <- NA
for (i in 1:length(stat_areas$area_code)){
  a <- subset(lw_tally_full, AREA_CODE == unique_areas[i])
  locs <- which(lw_tally_full$AREA_CODE %in% a$AREA_CODE)
  b <- subset(stat_areas, area_code == unique_areas[i], select = 'lat')
  c <- subset(stat_areas, area_code == unique_areas[i], select = 'lon')
  lw_tally_full[locs,3] <- b$lat
  lw_tally_full[locs,4] <- c$lon
}
lw_tally_full <- lw_tally_full[-c(1:2,8:9, 12),] # this is not an 
## Bring in shapefiles
wd = here::here("shapefiles")
stat_areas_sp <- rgdal::readOGR(wd,'groundfish_stat_areas', verbose = FALSE) 
proj4string(stat_areas_sp) <- CRS("+init=epsg:4326")
# 526 534 537 541 562 600* 615 616 621 622 623 624 626 631 632 633
# Subset the shape file for just the stat areas that match the ILXSM data
stat_areas2 <- stat_areas_sp[na.omit(stat_areas_sp@data$Id %in% c('526','534',
                                                                  '537','541',
                                                                  '562',
                                                                  '615','616', 
                                                                  '621','622',
                                                                  '623', '624',
                                                                  '626', '631',
                                                                  '632', '633')),]

## -- Set up quick simple map for future plots -- ## 
shp_df <- broom::tidy(stat_areas2, region = 'Id')
cnames <- aggregate(cbind(long, lat) ~ id, data=shp_df, FUN=mean)
# Adjust a couple of the stat area label locations
cnames[1,c(2,3)] <- c(-69.50, 40.50)
cnames[3,c(2,3)] <- c(-70.75, 40.50)
cnames[5,c(2,3)] <- c(-66.75, 41.25)
cnames[8,c(2,3)] <- c(-74.5, 38.5)
cnames[13,c(2,3)] <- c(-75.25, 36.5)

stat_map.v2 = ggplot() + 
  geom_polygon(data = stat_areas2, aes(x = long, y = lat, group = group), 
               colour = "black", fill = NA) +
  geom_text(data = cnames, 
            aes(x = long, y = lat, label = id), 
            size = 4) + 
  geom_sf(data = US.areas %>% st_as_sf() ,
          color = 'gray20', fill = 'lightgrey') +
  coord_sf(xlim = c(-76,-66.0), ylim = c(35.5,42.3), 
           datum = sf::st_crs(4326)) +
  xlab('Longitude') + 
  ylab('Latitude') +
  theme_classic2()  

stat_map.v2 + theme(text = element_text(size = 14), 
                    axis.text = element_text(size = 13), 
                    axis.title = element_text(size = 15))
# ggsave(path = here::here('figures'),'stat_areas_grey.jpeg',
#        width = 6, height = 3.75, units = "in", dpi = 300)

## -- Back to points of interest for the main detailed map below -- ##

# Major Ports  
ports <- data.frame(lon = c( -70.9342,-71.4856,-74.9060, -76.3452), 
                    lat = c(41.6362, 41.3731,38.9351, 37.0299),
                    port = c('New Bedford, MA', 'Narragansett, RI', 
                             'Cape May, NJ', 'Hampton, VA'))
# Geographical locations of interest
locations <- data.frame(lon = c( -70.9342), 
                        lat = c(41.6362),
                        port = c('Cape Hatteras, NC'))

## bring in bathymetry
atl <- marmap::getNOAA.bathy(-80,-58, 33, 46)
atl = fortify.bathy(atl)
# UPdated bathymetry
blues = colorRampPalette(brewer.pal(9,'Blues'))(28)
blues2 <- blues[c(5,7,9,11,13,15,16,17,18,19,20,22,24,26,27,28)]
depthslabel <- c('0','50','100','200', '300', '400','500','600',
                 '700', '800', '900', '1000', '2000',
                 '3000','>4000','>5000')
# Coast lines 
wd = ("C:/Users/sarah.salois/Documents/github/ssalois1/calc_wcr_metrics/shape_files")
US.areas <- rgdal::readOGR(wd, 'USA')
canada.areas <-rgdal::readOGR(wd,'Canada')
proj4string(US.areas) <- CRS("+init=epsg:4326")
proj4string(canada.areas) <- CRS("+init=epsg:4326")
US.areas <- US.areas %>% st_as_sf()
canada.areas <- canada.areas %>% st_as_sf()

## get coastline from mapdata 
coast = map_data("world2Hires")
coast = subset(coast, region %in% c('Canada', 'USA'))
coast$lon = (360 - coast$long)*-1 
## Points of interest
processors <- data.frame(lon = c(-74.87532557099381, -71.51081400184871 ), 
                         lat = c(38.962357229173776, 41.382384079398356),
                         processor = c('lunds', 'town_dock'))

ports <- data.frame(lon = c( -70.9342,-71.4856,-74.9060, -76.3452), 
                    lat = c(41.6362, 41.3731,38.9351, 37.0299),
                    port = c('New Bedford, MA', 'Point Judith, RI', 
                             'Cape May, NJ', 'Hampton, VA'))
geo_points <- data.frame(lon = c( -75.5393), 
                         lat = c(35.248),
                         port = c('Cape Hatteras'))
# ideal way to do this - will change year to a param in quarto, removes area
p1 = ggplot() + 
  geom_contour_filled(data = atl,
                      aes(x=x,y=y,z=-1*z), 
                      breaks=c(0,50,100,200,300,400,500,600,
                               700, 800, 900, 1000, 2000,
                               3000,4000,5766)) +
  scale_fill_manual(values = blues2,
                    name = paste("Depth (m)"),
                    labels = depths2,
                    drop = TRUE,
                    guide = 'none') +
  geom_contour(data = atl,
               aes(x=x,y=y,z=-1*z),
               breaks=c(0,50,100,200, Inf),
               size=c(0.3),
               col = 'darkgrey') +
  geom_sf(data = US.areas %>% st_as_sf(),color = 'gray20', fill = 'lightgrey') +
  geom_sf(data = canada.areas %>% st_as_sf(),color = 'gray20', fill = 'lightgrey') +
  # coord_sf(xlim = c(-76,-60.0), ylim = c(35,45), datum = sf::st_crs(4326)) +
  xlab('Longitude') + 
  ylab('Latitude') +
  geom_sf(data = stat_areas2 %>% st_as_sf() ,  
          fill = NA, color = 'gray20', lwd = 0.9) +
  geom_point(data = lw_tally_full,
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
  coord_sf(xlim = c(-76,-66.0), ylim = c(35.5,42.3), datum = sf::st_crs(4326)) + 
  theme_bw()  
p1 +  theme(text = element_text(size = 14), 
            axis.text = element_text(size = 13), 
            axis.title = element_text(size = 15),
            legend.position = c(0.85, 0.15),
            legend.background = element_rect(fill = "white", color = "black"), 
            legend.title = element_text(size = 11),
            legend.text = element_text(size = 11)) 
# -- FIGURE 2 -- #
# Landings and economic value
landings_23 <- read.csv(here::here('data/Illex_CAMS_LANDINGS_TOTALS.csv')) # updated from Andy
options(scipen=999)
vals = seq(from = 10000000, to= 60000000, by = 10000000)
val.labs = c('10,000,000','20,000,000',
          '30,000,000','40,000,000',
         '50,000,000','60,000,000')
val.labs = c('0','20,000,000',
             '40,000,000',
            '60,000,000')
val.breaks = seq(from = 10000000, to= 60000000, by = 10000000)
p2 = ggplot(landings_23) + 
  geom_line(aes(x = YEAR, y = LIVLB, linetype = 'Landings')) + 
  geom_line(aes(x = YEAR, y = VALUE, linetype = 'Value'), show.legend = TRUE) +
  xlim(1996,2023) +
  labs(x = 'Year') + 
  scale_y_continuous(name = 'Landings (Pounds) or Value (Dollars)',
                       breaks = seq(0, 60000000, by = 20000000), 
                       labels = val.labs) +
  #scale_color_manual(values = c('black', 'black'), linetype = c('solid', 'dashed')) +
  scale_linetype_manual('', values=c('Landings' = 1,'Value' = 2)) +
  #theme(legend.direction = "horizontal", legend.position = "bottom") + 
  ecodata::theme_ts()
p2 +  theme(text = element_text(size = 14), 
            axis.text = element_text(size = 13), 
            axis.title = element_text(size = 16),
            axis.ticks.length = unit(-1.4, "mm"),
            legend.position = c(0.50, 0.90),
            legend.text=element_text(size=14)) 
ggsave(path = here::here('figures'),'figure_2.jpg', 
       width = 9, height = 6, units = "in", dpi = 300)


# -- FIGURE 4 -- #
# Number of processors and stat areas 
# pulling in the data
ptab <- read.csv(here::here('data/Illex_Vessel_Pivot_Tables_Years_Combined.csv'))
lw_data <- read.csv(here::here('data/ilxsm/Entire_Database_Pull_ILXSM_2.14.23.csv'))
dat <- read.csv(here::here('data/ilxsm/ILXSM_EntirePull_Wdealerinfo_3_14_21.csv'))
# making sure all variables are in same units
dat <- dat %>%
  mutate(PARAM_VALUE_NUM = case_when(PARAM_TYPE == 'ML' & UNIT_MEASURE == 'CM' ~ PARAM_VALUE_NUM * 10, 
                                     PARAM_TYPE == 'ML' & UNIT_MEASURE == 'MM'~ PARAM_VALUE_NUM, 
                                     PARAM_TYPE == 'WT' & UNIT_MEASURE == 'GM' ~ PARAM_VALUE_NUM))
# putting date in correct format and adding in time points of interest
dat$LAND_DATE <-lubridate::dmy(dat$LAND_DATE)
dat <- dat %>%
  mutate(year = lubridate::year(LAND_DATE),
         month = lubridate::month(LAND_DATE),
         week = lubridate::week(LAND_DATE), 
         day = lubridate::day(LAND_DATE))
# Summarizing data sets 
ptab_tally <- ptab %>% 
  group_by(Year, Processor) %>%
  summarise(n_vessels = length(unique(Fishing.Vessel)))

ptab_stat <- ptab %>% 
  group_by(Year) %>%
  summarise(n_vessels = length(unique(Fishing.Vessel)))

dat_stat <- dat %>% 
  group_by(year) %>%
  summarise(n_vessels = length(unique(VESSEL_NAME)), 
            n_stat_areas = length(unique(AREA_CODE)))

ptab_stat$stat_area <- dat_stat %>% filter(year %in% c(2021, 2022)) %>% select(n_stat_areas)
ptab_stat <- ptab_stat %>% rename(stat_area = stat_area$n_stat_areas)
# ptab_stat
p1 = ggplot(data = ptab_stat, aes(x = factor(Year), y = n_vessels, 
                                  fill = factor(Year))) +
  geom_bar(stat ='identity', position = position_dodge(),
           col = 'black') +
  ylab('Number of Vessels') +
  xlab('Year') +
  #labs(fill = 'Year') +
  scale_fill_grey(start = 0, end = 0.8) + 
  ecodata::theme_ts()

p1 = p1 +  theme(legend.position = 'none', 
            text = element_text(size = 14), 
            axis.text = element_text(size = 13), 
            axis.title = element_text(size = 15),
            axis.ticks.length = unit(-1.4, "mm")) 


p2 = ggplot(data = ptab_tally, aes(x = factor(Year), y = n_vessels, 
                                   fill = factor(Processor))) +
  geom_bar(stat ='identity', position = position_dodge(),
           col = 'black') +
  ylab('Number of Vessels') +
  xlab('Year') +
  labs(fill = 'Processor/Dealer') +
  scale_fill_grey(start = 0, end = 0.8) + 
  annotate('text', x = 0.5, y = 16, label= '(a)', size = 5) +
  ecodata::theme_ts()

p2 = p2 +  theme(text = element_text(size = 14), 
            axis.text = element_text(size = 13), 
            axis.title = element_text(size = 15),
            axis.ticks.length = unit(-1.4, "mm"), 
            legend.position = c(0.79, 0.80),
            legend.background = element_rect(fill = "white", color = "black"), 
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 8))
  
  

p3 = ggplot(data = ptab_stat, aes(x = factor(Year), y = stat_area$n_stat_areas)) +
  geom_bar(stat ='identity', position = position_dodge(),
           col = 'black', fill = 'grey22') +
  ylab('Number of Statistical Areas Sampled') +
  xlab('Year') +
  labs(fill = 'Year') +
  #scale_fill_grey(start = 0, end = 0.8) + 
  annotate('text', x = 0.5, y = 18, label= '(b)', size = 5) +
  ecodata::theme_ts()

p3 = p3 +  theme(text = element_text(size = 14), 
            axis.text = element_text(size = 13), 
            axis.title = element_text(size = 15),
            axis.ticks.length = unit(-1.4, "mm")) 

p2 + p3
ggsave(path = here::here('figures'),'figure_4.jpeg',
       width = 10, height = 6, units = "in", dpi = 300)


## -- FIUGRE 5 -- ## 
# Density ridge plot weekly weights/lengths across stat areas
library(tidyverse)
# changed from dt to ml_wt_22 for app
wt <- dat %>%
  filter(PARAM_TYPE == 'WT') %>%
  rename(weight = PARAM_VALUE_NUM)
ml <- dat %>%
  filter(PARAM_TYPE == 'ML') %>%
  rename(length = PARAM_VALUE_NUM)

l1 = ggplot(wt %>% filter(year %in% c(2021, 2022)), 
            aes(x = weight, y = factor(week), fill =  factor(AREA_CODE))) +
  geom_density_ridges2(alpha = .4, color = 'black',
                      scale = 2.5, rel_min_height = .01) +
  labs(x = 'Weight (g)', 
       y = 'Week', fill = 'Statistical Area') + # title = 'ILXSM Weights', 
  xlim(0, 400) +
  theme_ridges() +
  theme_bw() + 
  facet_wrap(~year)
l2 = ggplot(ml %>% filter(year %in% c(2021, 2022)), 
            aes(x = length, y = factor(week), fill =  factor(AREA_CODE))) +
  geom_density_ridges2(alpha = .4, color = 'black',
                       scale = 2.5, rel_min_height = .01) +
  labs(x = 'Length (mm)', 
       y = 'Week', fill = 'Statistical Area') + # title = 'ILXSM Lengths',
  xlim(0, 400) + 
  theme_ridges() +
  theme_bw() + 
  facet_wrap(~year)

l1 + theme(text = element_text(size = 14), 
           axis.text = element_text(size = 13), 
           axis.title = element_text(size = 15))

l2 + theme(text = element_text(size = 14), 
         axis.text = element_text(size = 13), 
         axis.title = element_text(size = 15))


wt1 = ggplot(wt, 
            aes(x = weight, y = factor(week), fill =  factor(AREA_CODE))) +
  geom_density_ridges2(alpha = .4, color = 'black',
                       scale = 2.5, rel_min_height = .01) +
  labs(x = 'Weight (g)', 
       y = 'Week', fill = 'Statistical Area') + # title = 'ILXSM Weights', 
  annotate('text', x = 370, y = 30, label= '(a)', size = 5) +
  xlim(0, 400) +
  theme_ridges() +
  theme_bw() 
ml1 = ggplot(ml,
            aes(x = length, y = factor(week), fill =  factor(AREA_CODE))) +
  geom_density_ridges2(alpha = .4, color = 'black',
                       scale = 2.5, rel_min_height = .01) +
  labs(x = 'Length (mm)', 
       y = 'Week', fill = 'Statistical Area') + # title = 'ILXSM Lengths',
  annotate('text', x = 370, y = 30, label= '(b)', size = 5) +
  xlim(0, 400) + 
  theme_ridges() +
  theme_bw()

wt1 = wt1 + theme(text = element_text(size = 14), 
              axis.text = element_text(size = 13), 
              axis.title = element_text(size = 15), 
              legend.position = 'none')

ml1 = ml1 + theme(text = element_text(size = 14), 
                  axis.text = element_text(size = 13), 
                  axis.title = element_text(size = 15))


wt1 + ml1





## -- FIGURE 6 --  ##
# 2d Histogram
# Consolidate dataset to only samples with paired weight/length values
ml.c <- ml %>% select(LAND_DATE,AREA_CODE,ORGANISM_ID, 
                                length,VESSEL_NAME, VTR_SERIAL_NUM)

wt.c <- wt %>% select(LAND_DATE,AREA_CODE,ORGANISM_ID, 
                                weight,VESSEL_NAME, VTR_SERIAL_NUM)

lw_paired <- right_join(ml.c, wt.c, 
                        by = c('LAND_DATE','AREA_CODE','ORGANISM_ID', 
                               'VESSEL_NAME', 'VTR_SERIAL_NUM'))

lw_paired <- na.omit(lw_paired)
lw_paired <- lw_paired %>% relocate(weight, .after = length)

lw_paired <- lw_paired %>%
  mutate(year = lubridate::year(LAND_DATE),
         month = lubridate::month(LAND_DATE),
         week = lubridate::week(LAND_DATE), 
         day = lubridate::day(LAND_DATE))

ggplot(lw_paired, aes(x = length, y = weight)) +
  geom_point() + 
  geom_density_2d()
# figure 5a 
ga = ggplot(lw_paired %>% filter(year %in% c(2021,2022)), 
            aes(x=length, y=weight)) +
  #geom_point() + 
  #geom_density_2d() +
  geom_density_2d_filled(alpha = 0.4) +
  #scale_fill_gradientn(colours=pal) +
  geom_density_2d(colour = "grey45") +
  xlab('Mantle Length (mm)') +
  ylab('Body Weight (g)') +
  labs(fill = 'Count') +
  theme_bw() + 
  # ecodata::theme_facet() +
  facet_wrap(~year)
ga + theme(text = element_text(size = 14), 
           axis.text = element_text(size = 13), 
           axis.title = element_text(size = 15),
           legend.position = c(0.92, 0.73), # 73
           legend.background = element_rect(fill = "white", color = "black"), 
           legend.title = element_text(size = 9),
           legend.text = element_text(size = 8)) 
# figure 5b 
gb = ggplot(lw_paired, 
            aes(x=length, y=weight)) +
  geom_point(color = 'grey32') + 
  geom_density_2d() +
  #geom_density_2d_filled() +
  #geom_density_2d_filled(alpha = 0.4) +
  geom_density_2d(colour = 'dodgerblue1') +
  scale_fill_brewer() +
  xlab('Mantle Length (mm)') +
  ylab('Body Weight (g)') +
  labs(fill = 'Count') +
  theme_bw() 
gb + theme(text = element_text(size = 14), 
           axis.text = element_text(size = 13), 
           axis.title = element_text(size = 15))
           # legend.position = c(0.92, 0.73), # 73
           # legend.background = element_rect(fill = "white", color = "black"), 
           # legend.title = element_text(size = 9),
           # legend.text = element_text(size = 8)) 
# figure 5c 
gc = ggplot(lw_paired %>% filter(year %in% c(2021,2022)), 
            aes(x=length, y=weight)) +
  geom_point(color = 'grey32') + 
  geom_density_2d() +
  #geom_density_2d_filled() +
  #geom_density_2d_filled(alpha = 0.4) +
  geom_density_2d(colour = 'dodgerblue1') +
  scale_fill_brewer() +
  xlab('Mantle Length (mm)') +
  ylab('Body Weight (g)') +
  labs(fill = 'Count') +
  theme_bw() +
  facet_wrap(~year)

gc + theme(text = element_text(size = 14), 
           axis.text = element_text(size = 13), 
           axis.title = element_text(size = 15))

# COlor palettes
rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
r <- rf(32)
pal <- wes_palette("Zissou1", 21, type = "continuous")
# 2d histogram with default option
ggplot(lw_paired, aes(x=length, y=weight)) +
  geom_bin2d() +
  theme_bw()

# Bin size control + color palette
gd = ggplot(lw_paired, aes(x=length, y=weight)) +
  geom_bin2d(bins = 70) +
  #scale_fill_continuous(type = "viridis") +
  scale_fill_gradientn(colours=pal) +
  xlab('Mantle Length (mm)') +
  ylab('Body Weight (g)') +
  theme_bw()
gd + theme(text = element_text(size = 14), 
           axis.text = element_text(size = 13), 
           axis.title = element_text(size = 15))


ggplot(lw_paired, aes(x=length, y=weight)) +
  geom_hex() +
  scale_fill_gradientn(colours=r) +
  xlab('Mantle Length (mm)') +
  ylab('Body Weight (g)') +
  ecodata::theme_ts() 

g3 = ggplot(lw_paired %>% filter(year %in% c(2021,2022)), 
            aes(x=length, y=weight)) +
  geom_hex() +
  scale_fill_gradientn(colours=pal) +
  xlab('Mantle Length (mm)') +
  ylab('Body Weight (g)') +
  labs(fill = 'Count') +
  ecodata::theme_facet() +
  facet_wrap(~year)
g3 + theme(text = element_text(size = 14), 
           axis.text = element_text(size = 13), 
           axis.title = element_text(size = 15),
           legend.position = c(0.92, 0.73), # 73
           legend.background = element_rect(fill = "white", color = "black"), 
           legend.title = element_text(size = 9),
           legend.text = element_text(size = 8)) 


ggplot(lw_paired, aes(x=length, y=weight)) +
  geom_point() +
  geom_density_2d_filled(alpha = 0.4) +
  geom_density_2d(colour = "black")
