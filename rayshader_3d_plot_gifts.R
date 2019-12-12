options(rgl.useNULL = FALSE)
library(tidyverse)
library(ggplot2)
library(whitebox)
library(rayshader)
library(rayrender)
library(raster)
library(spatstat)
library(spatstat.utils)
library(suncalc)
library(sp)
library(lubridate)
library(rgdal)
library(leaflet)
library(geoviz)

# setwd(here::here())
setwd("C:/Users/Stephen/Desktop/R/rayshader")

# https://github.com/PennMUSA/MasterClass2019_3DMappingAndViz/blob/master/MusaMasterclass.md

# https://mail-wolf.de/?p=4439
library(leaflet)

# get bbox
# note that changing long impacts bbox width, chaning lat impacts bbox height
# reducing long shifted bbox west
# reducing lat shifted bbox south


################################################################################################


# devil's tower
# 44.591371, -104.714102

# tight box around devil's tower, but low res elevation map for 
# bbox <- list(
#         p1 = list(long = -104.714102, lat = 44.591391),
#         p2 = list(long = -104.716102, lat = 44.589371)
# )
# current_zscale <- 30

# slightly broader, but still tight on devils tower, confirms insufficient resolution to have tower-only plot
# bbox <- list(
#         p1 = list(long = -104.704102, lat = 44.593391),
#         p2 = list(long = -104.726102, lat = 44.582371)
# )
# current_zscale <- 30

# broader box around devil's tower - good for printing
# printed in cura with x = 101.7812; y = 75.6089; z = 40.3053
bbox <- list(
        p1 = list(long = -104.700102, lat = 44.601391),
        p2 = list(long = -104.730102, lat = 44.579371)
)
current_zscale <- 20



################################################################################################


# gettysburg
# https://maps.psiee.psu.edu/ImageryNavigator/
# 39.786831, -77.240230
# tight on little/big roundtop
bbox <- list(
        p1 = list(long = -77.225230, lat = 39.806831), # northeast point
        p2 = list(long = -77.260230, lat = 39.778831) # southwest point
)
current_zscale <- 20

# more obviously rectangular
# bbox <- list(
#         p1 = list(long = -77.232230, lat = 39.806831), # northeast point
#         p2 = list(long = -77.247230, lat = 39.778831) # southwest point
# )
# current_zscale <- 20


##########################


# labels
# render_label() x and y args are elevation_matrix indices, beginning from northwestern corner 
# x = longitude = elevation_matrix rows, y = latitude = elevation_matrix cols
# when displaying interactive 3d plot, north is the side initially facing northeast

# peach orchard
# 39.800911, -77.249907
peach_orchard_label_point_lat <- 39.800911
peach_orchard_label_point_lon <- -77.249907

# big roundtop
# 39.786071, -77.239276
big_roundtop_label_point_lat <- 39.786071
big_roundtop_label_point_lon <- -77.239276

# devil's den
# 39.7914091, -77.2423713
devils_den_label_point_lat <- 39.7914091
devils_den_label_point_lon <- -77.2423713


################################################################################################


# kauai
# 22.071800, -159.509567
bbox <- list(
        p1 = list(long = -159.259567, lat = 22.271800), # northeast point
        p2 = list(long = -159.819567, lat = 21.831800) # southwest point
)
current_zscale <- 15

# broader box including more ocean; confirms the elevation data is clipped at island edge
# bbox <- list(
#         p1 = list(long = -159.159567, lat = 22.371800), # northeast point
#         p2 = list(long = -159.919567, lat = 21.731800) # southwest point
# )


################################################################################################


# london
# https://twitter.com/SteinsZeit/status/1195840463917461505?s=19
# https://environment.data.gov.uk/dataset/6f51a299-351f-4e30-a5a3-2511da9688f7
# https://environment.data.gov.uk/DefraDataDownload/?Mode=survey
# https://ea.sharefile.com/share/view/s6c1e0bb7c8548948

setwd("C:/Users/Stephen/Desktop/R/rayshader/data/london/lidar_tiles_for_london_3d_print")

# read asc files as rasters
raster_layers <- tibble(filename = list.files(path = getwd(), "*.asc$")) %>%
        mutate(raster = map(filename, .f = ~ raster::raster(rgdal::readGDAL(.)))) %>%
        pull(raster)

# combine raster layers
raster_layers$fun <- mean
raster_mosaic <- do.call(raster::mosaic, raster_layers)
raster_mosaic
class(raster_mosaic)

# london tower bridge
# 51.5076734, -0.0879736
# bbox <- list(
#         p1 = list(long = -0.0779736, lat = 51.5096734), # northeast point
#         p2 = list(long = -0.0879736, lat = 51.5076734) # southwest point
# )

# crop raster
# note that xmin = eastern boundary, xmax = western boundary, ymin = southern boundary, ymax = northern boundary
raster_mosaic
# broader around buckingham and the river
# extent_bbox <- extent(528500, 531000, 179000, 180500)
extent_bbox <- extent(528800, 531000, 179300, 180500)
extent_bbox
elevation_raster <- raster::crop(x = raster_mosaic, y = extent_bbox)
elevation_raster

# convert raster to matrix
elevation_matrix <- matrix(raster::extract(raster_mosaic, raster::extent(elevation_raster), buffer = 1000), 
        nrow = ncol(elevation_raster), ncol = nrow(elevation_raster))
class(elevation_matrix)
dim(elevation_matrix)

# fill holes in raster with minimum elevation
elevation_matrix[is.na(elevation_matrix[])] <- min(elevation_matrix[], na.rm = TRUE)

# reduce matrix size further
elevation_matrix_small <- reduce_matrix_size(elevation_matrix, 0.4)
elevation_matrix_small %>% dim()

# set current_zscale
current_zscale <- 1


################################################################################################



# plot bbox on map
leaflet() %>%
        addTiles() %>% 
        addRectangles(
                lng1 = bbox$p1$long, lat1 = bbox$p1$lat,
                lng2 = bbox$p2$long, lat2 = bbox$p2$lat,
                fillColor = "transparent"
        ) %>%
        fitBounds(
                lng1 = bbox$p1$long, lat1 = bbox$p1$lat,
                lng2 = bbox$p2$long, lat2 = bbox$p2$lat
        ) %>%
        # addMarkers(lng = big_roundtop_label_point_lon, lat = big_roundtop_label_point_lat) %>%
        # addMarkers(lng = -77.259276, lat = 39.780071)
        identity()



##################################################################################


# get topo data from SRTM api
elevation_raster <- raster::getData("SRTM", lon = bbox$p1$long, lat = bbox$p2$lat)
elevation_raster
elevation_extent <- extent(bbox$p2$long, bbox$p1$long, bbox$p2$lat, bbox$p1$lat)
elevation_extent
elevation_raster <- raster::crop(x = elevation_raster, y = elevation_extent)
elevation_raster

elevation_matrix <- matrix(
        raster::extract(elevation_raster, raster::extent(elevation_raster), buffer = 1000), 
        nrow = ncol(elevation_raster), ncol = nrow(elevation_raster)
)
class(elevation_matrix)
dim(elevation_matrix)

# get elevation_matrix_small to reduce compute costs
# note it's recommended to keep smaller than 2000 x 2000
elevation_matrix_small <- elevation_matrix
# elevation_matrix_small <- reduce_matrix_size(elevation_matrix, 0.33)
# elevation_matrix_small <- reduce_matrix_size(elevation_matrix, 0.75)
dim(elevation_matrix_small)

# calculate rayshader layers
ambmat <- ambient_shade(elevation_matrix_small, zscale = current_zscale)
raymat <- ray_shade(elevation_matrix_small, zscale = current_zscale, lambert = TRUE)
watermap <- detect_water(elevation_matrix_small)

# plot 2D
elevation_matrix_small %>%
        sphere_shade(texture = "imhof4") %>%
        plot_map()

elevation_matrix_small %>%
        sphere_shade(texture = "imhof4") %>%
        add_water(watermap, color = "imhof4") %>%
        add_shadow(raymat, max_darken = 0.5) %>%
        add_shadow(ambmat, max_darken = 0.5) %>%
        plot_map()

# plot 3d
# when displaying interactive 3d plot, north is the side initially facing northeast
elevation_matrix_small %>%
        sphere_shade() %>%
        # add_water(watermap, color = "lightblue") %>%
        # add_shadow(raymat, max_darken = 0.5) %>%
        # add_shadow(ambmat, max_darken = 0.5) %>%
        plot_3d(elevation_matrix_small, zscale = current_zscale)

# note render_label x and y are matrix indices, not lat/long coordinates
render_label(elevation_matrix_small, "Peach Orchard", textcolor = "black", linecolor = "white", freetype = FALSE,
             x = get_label_point_lon_x_index_on_elevation_matrix(label_point_lon = peach_orchard_label_point_lon, 
                                                                 elevation_matrix = elevation_matrix_small), 
             y = get_label_point_lat_y_index_on_elevation_matrix(label_point_lat = peach_orchard_label_point_lat,
                                                                 elevation_matrix = elevation_matrix_small), 
             z = 100, textsize = 1, linewidth = 4, zscale = current_zscale, dashed = FALSE)

render_label(elevation_matrix_small, "Big Roundtop", textcolor = "black", linecolor = "white", freetype = FALSE,
             x = get_label_point_lon_x_index_on_elevation_matrix(label_point_lon = big_roundtop_label_point_lon,
                                                                 elevation_matrix = elevation_matrix_small),
             y = get_label_point_lat_y_index_on_elevation_matrix(label_point_lat = big_roundtop_label_point_lat,
                                                                 elevation_matrix = elevation_matrix_small),
             z = 100, textsize = 1, linewidth = 4, zscale = current_zscale, dashed = FALSE)

render_label(elevation_matrix_small, "Devil's Den", textcolor = "black", linecolor = "white", freetype = FALSE,
             x = get_label_point_lon_x_index_on_elevation_matrix(label_point_lon = devils_den_label_point_lon,
                                                                 elevation_matrix = elevation_matrix_small),
             y = get_label_point_lat_y_index_on_elevation_matrix(label_point_lat = devils_den_label_point_lat,
                                                                 elevation_matrix = elevation_matrix_small),
             z = 100, textsize = 1, linewidth = 4, zscale = current_zscale, dashed = FALSE)


# clear labels
render_label(clear_previous = TRUE)

# close rgl window
rgl::rgl.close()

# save 3d stl file
save_3dprint("kauai_v2.stl", maxwidth = 4, unit = "in")
