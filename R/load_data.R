###########################################
# This file loads bathymetry data for the #
# great lakes and sets certain properties #
# for plots used in the other files.      #
###########################################

library(tidyverse)
library(rayshader)
library(glue)
library(terra)
library(scico)
library(raster)
library(rayimage)
library(rayrender)

# Data source: https://www.ngdc.noaa.gov/mgg/greatlakes/
# Download the GeoTiff files

# Files assumed to be in 'data/'

files <- list.files("data")

lakes <- map_df(files, function(lake) {
  d_ <- rast(glue("data/{lake}/{lake}.tif"))
  
  # Uncomment below to aggregate the data for prototyping;
  # make the factor higher if it is still taking too long
  
  #d_ <- terra::aggregate(d_, fact = 5)
  
  df <- as.data.frame(d_, xy = TRUE) %>%
    mutate(lake = str_remove(lake, "_lld$"))
  
  names(df) <- c("long", "lat", "elevation", "lake")
  
  return(df)
})

# Remove all points at or above sea level, then 
# convert depths to positive elvations (i.e. invert)

rev <- lakes %>%
  filter(elevation < 0) %>%
  mutate(elevation = abs(elevation)) 

# Background and text color will be used throughout
bg <- "white"
davos_c <- scico(n = 10, palette = "davos")
text_color <- davos_c[2]

# This is a plot with labels I used for the rotating light source video.
# For the rotating video with static shadow direction, I preferred to add
# title, subtitle, caption, and legend using {magick} (see R/make_vid_magick.R)

e <- rev %>%
  filter(lake == "superior") %>%
  ggplot(aes(long, lat, fill = elevation)) +
  geom_tile() +
  geom_tile(data = rev %>% filter(lake == "michigan")) +
  geom_tile(data = rev %>% filter(lake == "huron")) +
  geom_tile(data = rev %>% filter(lake == "erie")) +
  geom_tile(data = rev %>% filter(lake == "ontario")) +
  coord_sf(crs = 3347, expand = 0) +
  scale_fill_scico(palette = "davos", direction = -1) +
  theme(legend.position = c(.5, .5),
        legend.background = element_rect(fill = NA),
        legend.title = element_text(family = "serif", color = text_color, size = 12),
        legend.text = element_text(family = "serif", color = text_color, size = 8),
        #legend.key =  element_rect(fill = NA),
        #legend.key.size = unit(.02, "npc"),
        panel.background = element_rect(fill = bg),
        panel.grid = element_line(color = bg),
        plot.background = element_rect(fill = bg, color = bg),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.margin = margin(),
        plot.title = element_text(hjust = .9, size = 20,
                                  color = text_color, family = "serif",
                                  margin = margin(b = -25)),
        plot.caption = element_text(family = "serif", color = alpha(text_color, .5),
                                    size = 6, hjust = .5)) +
  labs(x = "", y = "", 
       title= "Great Lake Depths", 
       fill = "Depth (m)", 
       caption = "Graphic by Spencer Schien (@MrPecners) | Data from NOAA"
       ) 
  #guides(fill = guide_legend(override.aes = list(size = 1)))

# This will extract the legend and save it as an image,
# which is used by certain files.

source("R/get_leg_image.R")