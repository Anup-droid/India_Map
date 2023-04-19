# Install and load required packages
library("pacman")
pacman::p_load(sf,
               sp,
               rgdal,
               tidyverse,
               ggplot2)

# Read district shapefile of India
ind_shape <- st_read("C:/Shape files 2020/state.shp")

# Generate some random data points
set.seed(123)
ind_data <- data.frame(
  OBJECTID = ind_shape$OBJECTID,
  STATE = ind_shape$STATE,
  BCG = runif(length(ind_shape$OBJECTID), 0, 100),
  ROTA = runif(length(ind_shape$OBJECTID), 0, 100)
)

#Merge
ind_map_data <- merge(ind_shape, ind_data, by.x = "OBJECTID", by.y = "OBJECTID")

#Map Plot
ind_map=ggplot(ind_map_data,aes(fill=BCG))+
  geom_sf(color ="black")+
  scale_fill_viridis_b()+
  labs(title="Coverage of Vaccination in India",
       subtitle = "BCG Coverage",
       caption = "Source: HMIS - Data upto Feb-2023")+
  theme(title = element_text(face = "bold"),
        legend.position = "left")+
  theme_void()
ind_map

#Plot using function
ind_mp=function(x){
  y=ggplot(ind_map_data,aes(fill=x))+
    geom_sf(color ="black")+
    scale_fill_viridis_b()+
    labs(title="Coverage of Vaccination in India",
         subtitle = "BCG Coverage",
         caption = "Source: HMIS - Data upto Feb-2023")+
    theme(title = element_text(face = "bold"),
          legend.position = "left")+
    theme_void()
  return(y)
  }
ind_mp(ind_map_data$ROTA)
ind_mp(ind_map_data$BCG)


