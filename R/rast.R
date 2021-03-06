library(MetBrewer)

lake <- "huron"

d_ <- raster(glue("data/{lake}_lld/{lake}_lld.tif"))
lake <- "erie"

de <- raster(glue("data/{lake}_lld/{lake}_lld.tif"))

ded <- raster_to_matrix(de)
dd <- raster_to_matrix(d_)

both <- raster::merge(ded, dd)

small_h <- resize_matrix(dd, 0.25)
small_e <- resize_matrix(ded, .25)


fix <- function(x) {
  for(row in 1:nrow(x)) {
    for(col in 1:ncol(x)) {
      if(!is.na(x[row, col])) {
        if (x[row, col] > 0) {
          x[row, col] <- NA
        } else {
          x[row, col] <- abs(x[row, col])
        }
      }
      
    }
  }
  return(x)
}

full_fixed <- fix(both)
small_fixed_h <- fix(small_h)

colors <- met.brewer("Tam")


hm <- small_fixed_h

hm %>%
  height_shade(texture = (grDevices::colorRampPalette(rev(colors)))(256)) %>%
  #add_shadow(lamb_shade(hm)) %>%
  #add_shadow(ambient_shade(full_fixed, multicore = TRUE)) %>%
  add_shadow(ray_shade(hm, multicore = TRUE, sunaltitude = 80)) %>%
  plot_3d(heightmap = hm, solid = FALSE,
          #windowsize = c(800, 400), 
          #shadow_darkness = .25, shadowcolor = "#F1C659",
          phi = 90, zoom = .75, theta = 0, background = "white")

render_camera(phi = 80, theta = 0, zoom = .75)


render_highquality("huron_hq1.png", parallel = TRUE, lightaltitude = 80)




rgl::rgl.close()


library(magick)

scico(n = 10, palette = "lajolla")

text_color <- "#462919"

img <- image_read("test_hq2.png")

# Title
img_ <- image_annotate(img, "Great Lake Depths", font = "Cinzel Decorative",
                       weight = 700, decoration = "underline",
                       color = text_color, size = 200, gravity = "East",
                       location = "+350-500")
# Subtitle
img_ <- image_annotate(img_, "Lake Michigan", 
                       font = "Cinzel Decorative", location = "+700-200",
                       color = text_color, size = 175, gravity = "east")

# Caption
img_ <- image_annotate(img_, "Graphic by Spencer Schien (@MrPecners) | Data from NOAA", 
                       font = "Cinzel Decorative", location = "+0+20",
                       color = alpha(text_color, .5), size = 100, gravity = "south")

magick::image_write(img_, "test_hq_titled.png")
