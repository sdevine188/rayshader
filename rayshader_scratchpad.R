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

# setwd(here::here())
setwd("C:/Users/Stephen/Desktop/R/rayshader")

# https://github.com/PennMUSA/MasterClass2019_3DMappingAndViz/blob/master/MusaMasterclass.md

# get hobart data
loadzip = tempfile() 
download.file("https://dl.dropboxusercontent.com/s/8ltz4j599z4njay/dem_01.tif.zip", loadzip)
## Alternate Link
#download.file("https://tylermw.com/data/dem_01.tif.zip", loadzip)
hobart_tif = raster::raster(unzip(loadzip, "dem_01.tif"))
unlink(loadzip)

hobart_mat = raster_to_matrix(hobart_tif)
unlink("dem_01.tif")

hobart_mat[1:10,1:10]

# visualize height
hobart_mat %>%
        height_shade() %>%
        plot_map()

# shade by direction-facing
hobart_mat %>%
        sphere_shade() %>%
        plot_map()

# add water
hobart_mat %>%
        sphere_shade() %>%
        add_water(detect_water(hobart_mat)) %>%
        plot_map()

# change color palette
hobart_mat %>%
        sphere_shade(texture = "desert") %>%
        add_water(detect_water(hobart_mat), color = "desert") %>%
        plot_map()

hobart_mat %>%
        sphere_shade(texture = "imhof4") %>%
        add_water(detect_water(hobart_mat), color = "imhof4") %>%
        plot_map()

# try traditional lambert shading that does not account for shadows
hobart_mat %>%
        lamb_shade(zscale = 33, sunangle = 135) %>%
        plot_map()

# add shadows to lambert shading
hobart_mat %>%
        lamb_shade(zscale = 33) %>%
        add_shadow(ray_shade(hobart_mat, zscale = 33, 
                             sunaltitude = 6, lambert = FALSE), 0.3) %>%
        plot_map()

# combine add_shadow with ray_shade and with lamb_shade to get best results
hobart_mat %>%
        sphere_shade() %>%
        add_water(detect_water(hobart_mat), color = "lightblue") %>%
        add_shadow(ray_shade(hobart_mat,zscale = 33, sunaltitude = 3,lambert = FALSE), max_darken = 0.5) %>%
        add_shadow(lamb_shade(hobart_mat,zscale = 33,sunaltitude = 3), max_darken = 0.5) %>%
        plot_map()

# change angle of sun
hobart_mat %>%
        sphere_shade(sunangle = 225) %>%
        add_water(detect_water(hobart_mat), color = "lightblue") %>%
        add_shadow(ray_shade(hobart_mat,sunangle = 225, zscale = 33, sunaltitude = 5,lambert = FALSE), 
                   max_darken = 0.5) %>%
        add_shadow(lamb_shade(hobart_mat,zscale = 33,sunaltitude = 5), max_darken = 0.8) %>%
        plot_map()

# add ambient occlusion (where sky particles reflect light, effectively making entire sky the light source)
# this means it's darker in a valley with less sky visible, than it is on a plain
#With ambient occlusion
hobart_mat %>%
        sphere_shade() %>%
        add_water(detect_water(hobart_mat), color = "lightblue") %>%
        add_shadow(ray_shade(hobart_mat, zscale = 33, sunaltitude = 5,lambert = FALSE), 
                   max_darken = 0.5) %>%
        add_shadow(lamb_shade(hobart_mat,zscale = 33, sunaltitude = 5), max_darken = 0.7) %>%
        add_shadow(ambient_shade(hobart_mat), max_darken = 0.1) %>%
        plot_map()


#########################


# 3d maps
ambientshadows = ambient_shade(hobart_mat)

# note i sometimes had issues where render_snapshot would get errors, even for code that had previouly worked
# i found i closed the rgl window, then i could start again "fresh" rebuilding a map and render_snapshot worked
# to avoid triggering the erroring out, i found if i just never manually manipulated the rgdal window, it worked
rgl::rgl.close()

hobart_mat %>%
        sphere_shade() %>%
        add_water(detect_water(hobart_mat), color = "lightblue") %>%
        add_shadow(ray_shade(hobart_mat, sunaltitude = 3, zscale = 33, lambert = FALSE), max_darken = 0.5) %>%
        add_shadow(lamb_shade(hobart_mat, sunaltitude = 3, zscale = 33), max_darken = 0.7) %>%
        add_shadow(ambientshadows, max_darken = 0.1) %>%
        plot_3d(hobart_mat, zscale = 10,windowsize = c(1000,1000))

render_snapshot()


#################


# rgl::rgl.clear()
# close rgl window
rgl::rgl.close()

hobart_mat %>%
        sphere_shade() %>%
        add_water(detect_water(hobart_mat), color = "lightblue") %>%
        add_shadow(ray_shade(hobart_mat, sunaltitude = 3, zscale = 33, lambert = FALSE), max_darken = 0.5) %>%
        add_shadow(lamb_shade(hobart_mat, sunaltitude = 3, zscale = 33), max_darken = 0.7) %>%
        add_shadow(ambientshadows, max_darken = 0) %>%
        plot_3d(hobart_mat, zscale = 10,windowsize = c(1000,1000), 
                phi = 40, theta = 135, zoom = 0.9, 
                background = "grey30", shadowcolor = "grey5", 
                soliddepth = -50, shadowdepth = -100)

# can save by passing filename arg; needs to have rgl window open
render_snapshot()
render_snapshot(title_text = "River Derwent, Tasmania", 
                title_font = "Helvetica", 
                title_size = 50,
                title_color = "grey90")

render_snapshot(filename = "hobart_example.png")


###############


# change camera angle with render_camera
render_camera(theta = 90, phi = 30, zoom = 0.7, fov = 0)
render_snapshot()


##############3


# windows doesn't have freetype font library
if(.Platform$OS.type == "windows") {
        freetype = FALSE
} else {
        freetype = TRUE
}


render_label(hobart_mat, "River Derwent", textcolor ="white", linecolor = "white", freetype = freetype,
             x = 450, y = 260, z = 1400, textsize = 2.5, linewidth = 4, zscale = 10)
# note for a bit, i was getting an error with render_snapshot() when passing title_text
# but the basic render_snapshot() with no args still worked and can save png
# but then it started working again with title_text args etc without error??
render_snapshot(title_text = "render_label() demo, part 1",
                title_bar_alpha = 0.8,
                title_bar_color = "white")
# render_snapshot()
render_snapshot(filename = "hobart_example_6.png")


render_label(hobart_mat, "Jordan River (not that one)", textcolor ="white", linecolor = "white", freetype = freetype,
             x = 450, y = 140, z = 1400, textsize = 2.5, linewidth = 4, zscale = 10, dashed = TRUE)
render_snapshot(title_text = "render_label() demo, part 2",
                title_bar_alpha = 0.8,
                title_bar_color = "grey")
render_snapshot(filename = "hobart_example_7.png")


# clear labels
render_label(clear_previous = TRUE)

# close rgl window
rgl::rgl.close()


##########################


# add depth of field
hobart_mat %>%
        sphere_shade(sunangle = 60) %>%
        add_water(detect_water(hobart_mat), color = "lightblue") %>%
        add_shadow(ray_shade(hobart_mat, sunangle = 60, sunaltitude = 3, zscale = 33, lambert = FALSE), max_darken = 0.5) %>%
        add_shadow(lamb_shade(hobart_mat, sunangle = 60, sunaltitude = 3, zscale = 33), max_darken = 0.7) %>%
        add_shadow(ambientshadows, max_darken = 0.1) %>%
        plot_3d(hobart_mat, zscale = 10,windowsize = c(1000,1000), 
                background = "#edfffc", shadowcolor = "#273633")

render_snapshot()
render_camera(theta = 120, phi = 20, zoom = 0.3, fov = 90)
render_depth(focus = 0.81, preview_focus = TRUE)

render_depth(focus = 0.81, focallength = 200, title_bar_color = "black", vignette = TRUE,
             title_text = "The River Derwent, Tasmania", title_color = "white", title_size = 50)

# i had an issue where render_depth displayed correct in the graphics device, but not in rgdal window
# and for some reason render_snapshot to save to png was copying rgdal window, not graphics device
# but i could still save using base png() method
render_snapshot(filename = "hobart_example_8.png")
png("hobart_example_9.png")
render_depth(focus = 0.81, focallength = 200, title_bar_color = "black", vignette = TRUE,
             title_text = "The River Derwent, Tasmania", title_color = "white", title_size = 50)
dev.off()

# close rgl window
rgl::rgl.close()


###############


# render_highquality
# note it wraps all the shadowing refinement etc, 
# render_highquality runs for a long time, and it errored out for me...
hobart_mat %>%
        sphere_shade(texture = "desert") %>%
        add_water(detect_water(hobart_mat), color = "desert") %>%
        plot_3d(hobart_mat, zscale = 10)
# render_highquality()

# turn off global light, and add custom light source
# render_highquality(light = FALSE, 
        # scene_elements = sphere(y = 150, radius = 30, 
        #                         material = diffuse(lightintensity = 40, implicit_sample = TRUE)))

# close rgl window
rgl::rgl.close()


######################


# save stl file for 3d printing
hobart_mat %>%
        sphere_shade(sunangle = 60) %>%
        add_water(detect_water(hobart_mat), color = "lightblue") %>%
        add_shadow(ray_shade(hobart_mat, sunangle = 60, sunaltitude = 3, zscale = 33, lambert = FALSE), max_darken = 0.5) %>%
        add_shadow(lamb_shade(hobart_mat, sunangle = 60, sunaltitude = 3, zscale = 33), max_darken = 0.7) %>%
        add_shadow(ambientshadows, max_darken = 0.1) %>%
        plot_3d(hobart_mat, zscale = 10,windowsize = c(1000,1000), 
                background = "#edfffc", shadowcolor = "#273633")

save_3dprint("hobart_3d.stl", maxwidth = 4, unit = "in")


#################################################################################


# defining bounding box w trial and error
# https://wcmbishop.github.io/rayshader-demo/

library(leaflet)

# define bounding box with longitude/latitude coordinates
bbox <- list(
        p1 = list(long = -122.522, lat = 37.707),
        p2 = list(long = -122.354, lat = 37.84)
)

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
        )


############


# create define_image_size function 
define_image_size <- function(bbox, major_dim = 400) {
        # calculate aspect ration (width/height) from lat/long bounding box
        aspect_ratio <- abs((bbox$p1$long - bbox$p2$long) / (bbox$p1$lat - bbox$p2$lat))
        # define dimensions
        img_width <- ifelse(aspect_ratio > 1, major_dim, major_dim*aspect_ratio) %>% round()
        img_height <- ifelse(aspect_ratio < 1, major_dim, major_dim/aspect_ratio) %>% round()
        size_str <- paste(img_width, img_height, sep = ",")
        list(height = img_height, width = img_width, size = size_str)
}

image_size <- define_image_size(bbox, major_dim = 600)
image_size


############


# get elevation data
# https://elevation.nationalmap.gov/arcgis/rest/services/3DEPElevation/ImageServer/exportImage?bbox=-122.522%2C37.707%2C-122.354%2C37.84&bboxSR=4326&size=600%2C480&imageSR=4326&time=&format=jpgpng&pixelType=F32&noData=&noDataInterpretation=esriNoDataMatchAny&interpolation=+RSP_BilinearInterpolation&compression=&compressionQuality=&bandIds=&mosaicRule=&renderingRule=&f=html

# create get_usgs_elevation_data
get_usgs_elevation_data <- function(bbox, size = "400,400", file = NULL, 
                                    sr_bbox = 4326, sr_image = 4326) {
        require(httr)
        
        # TODO - validate inputs
        
        url <- parse_url("https://elevation.nationalmap.gov/arcgis/rest/services/3DEPElevation/ImageServer/exportImage")
        res <- GET(
                url, 
                query = list(
                        bbox = paste(bbox$p1$long, bbox$p1$lat, bbox$p2$long, bbox$p2$lat,
                                     sep = ","),
                        bboxSR = sr_bbox,
                        imageSR = sr_image,
                        size = size,
                        format = "tiff",
                        pixelType = "F32",
                        noDataInterpretation = "esriNoDataMatchAny",
                        interpolation = "+RSP_BilinearInterpolation",
                        f = "json"
                )
        )
        
        if (status_code(res) == 200) {
                body <- content(res, type = "application/json")
                # TODO - check that bbox values are correct
                # message(jsonlite::toJSON(body, auto_unbox = TRUE, pretty = TRUE))
                
                img_res <- GET(body$href)
                img_bin <- content(img_res, "raw")
                if (is.null(file)) 
                        file <- tempfile("elev_matrix", fileext = ".tif")
                writeBin(img_bin, file)
                message(paste("image saved to file:", file))
        } else {
                warning(res)
        }
        invisible(file)
}


# download elevation data
# note that i got an error using elev_file as the filepath, and needed to specify full path for it to work
# elev_file <- file.path("data", "sf-elevation.tif")
get_usgs_elevation_data(bbox, size = image_size$size, 
                        file = "C:/Users/Stephen/Desktop/R/rayshader/data/sf-elevation.tif",
                        sr_bbox = 4326, sr_image = 4326)


################


# create 2d map
# load elevation data
# elev_img <- raster::raster("data/sf-elevation.tif")
# elev_matrix <- matrix(
#         raster::extract(elev_img, raster::extent(elev_img), buffer = 1000),
#         nrow = ncol(elev_img), ncol = nrow(elev_img)
# )
# 
# # calculate rayshader layers
# ambmat <- ambient_shade(elev_matrix, zscale = 30)
# raymat <- ray_shade(elev_matrix, zscale = 30, lambert = TRUE)
# watermap <- detect_water(elev_matrix)
# 
# # plot 2D
# elev_matrix %>%
#         sphere_shade(texture = "imhof4") %>%
#         add_water(watermap, color = "imhof4") %>%
#         add_shadow(raymat, max_darken = 0.5) %>%
#         add_shadow(ambmat, max_darken = 0.5) %>%
#         plot_map()


##################################################################################


# get swiss alps
# https://mail-wolf.de/?p=4439
library(leaflet)

# get bbox
# note that changing long impacts bbox width, chaning lat impacts bbox height
# reducing long shifted bbox west
# reducing lat shifted bbox south

# broad bbox around valley, including mountains
# bbox <- list(
#         p1 = list(long = 7.772108, lat = 46.502030),
#         p2 = list(long = 7.979801, lat = 46.63766)
# )

# narrow bbox around valley
# bbox <- list(
#         p1 = list(long = 7.882108, lat = 46.542030),
#         p2 = list(long = 7.930801, lat = 46.61766)
# )

# broadest bbox, including grindelwald
bbox <- list(
        p1 = list(long = 7.772108, lat = 46.502030),
        p2 = list(long = 8.120801, lat = 46.71766)
)

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
        addMarkers(lng = 7.9215, lat =  46.6054)


##################################################################################


# get topo data from SRTM api
elevation_raster <- raster::getData("SRTM", lon = bbox$p1$long, lat = bbox$p2$lat)
elevation_raster
elevation_extent <- extent(bbox$p1$long,  bbox$p2$long, bbox$p1$lat, bbox$p2$lat)
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
ambmat <- ambient_shade(elevation_matrix_small, zscale = 30)
raymat <- ray_shade(elevation_matrix_small, zscale = 30, lambert = TRUE)
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
elevation_matrix_small %>%
        sphere_shade() %>%
        add_water(watermap, color = "lightblue") %>%
        add_shadow(raymat, max_darken = 0.5) %>%
        add_shadow(ambmat, max_darken = 0.5) %>%
        plot_3d(elevation_matrix_small, zscale = 30)

render_snapshot()
# note render_label x and y are matrix indices, not lat/long coordinates
render_label(elevation_matrix_small, "Wengen", textcolor = "black", linecolor = "white", freetype = FALSE,
             x = 140.6054, y = 150.9215, z = 100, textsize = 1, linewidth = 4, zscale = 30, dashed = FALSE)
# note that theta orbits camera around image, phi changes angle of camera above horizon
# higher value for zoom actually zooms out making image smaller
# higher theta rotates counter-clockwise
# higher phi raises angle of camera above horizon
render_camera(theta = 170, phi = 20, zoom = .8, fov = 10)
render_depth(focus = .5, preview_focus = TRUE)
render_depth(focus = 0.5, focallength = 200, title_bar_color = "black", vignette = TRUE,
             title_text = "Lauterbrunnen Valley", title_color = "white", title_size = 50)
render_snapshot(title_text = "render_label() demo, part 2",
                title_bar_alpha = 0.8,
                title_bar_color = "grey")
render_snapshot(filename = "hobart_example_7.png")

# clear labels
render_label(clear_previous = TRUE)

# close rgl window
rgl::rgl.close()

# 
save_3dprint("alps_3d.stl", maxwidth = 4, unit = "in")


