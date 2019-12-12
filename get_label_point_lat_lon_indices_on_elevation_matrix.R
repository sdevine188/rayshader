library(tidyverse)

# create get_label_point_lat_y_index_on_elevation_matrix function
get_label_point_lat_y_index_on_elevation_matrix <- function(label_point_lat, elevation_matrix) {
        
        label_point_pct_lat_distance_from_nw_corner <- (bbox$p1$lat - label_point_lat) / 
                                                                    (bbox$p1$lat - bbox$p2$lat)
        label_point_lat_y_index_on_elevation_matrix <- round((elevation_matrix %>% ncol()) * 
                                                                     label_point_pct_lat_distance_from_nw_corner)
        return(label_point_lat_y_index_on_elevation_matrix)
}


#############################


# create get_label_point_lon_x_index_on_elevation_matrix function
get_label_point_lon_x_index_on_elevation_matrix <- function(label_point_lon, elevation_matrix) {
        
        label_point_pct_lon_distance_from_nw_corner <- 1 - (bbox$p1$long - label_point_lon) / 
                                                                    (bbox$p1$long - bbox$p2$long)
        label_point_lon_x_index_on_elevation_matrix <- round((elevation_matrix %>% nrow()) * 
                                                                     label_point_pct_lon_distance_from_nw_corner)
        return(label_point_lon_x_index_on_elevation_matrix)
}


#############################


# test
# get_label_point_lat_x_index_on_elevation_matrix(label_point_lat = 39.800911, 
#                                                 elevation_matrix = elevation_matrix_small)

# get_label_point_lon_y_index_on_elevation_matrix(label_point_lon = -77.249907,
#                                                 elevation_matrix = elevation_matrix_small)
