library(MetBrewer)
library(magick)

lake <- "erie"

d_ <- raster(glue("data/{lake}_lld/{lake}_lld.tif"))

dd <- raster_to_matrix(d_)

small <- resize_matrix(dd, 0.25)


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

full_fixed <- fix(dd)
small_fixed <- fix(small)

colors <- met.brewer("Tam")


hm <- full_fixed

hm %>%
  height_shade(texture = (grDevices::colorRampPalette(rev(colors)))(256)) %>%
  #add_shadow(lamb_shade(hm)) %>%
  #add_shadow(ambient_shade(full_fixed, multicore = TRUE)) %>%
  add_shadow(ray_shade(hm, multicore = TRUE, sunaltitude = 80)) %>%
  plot_3d(heightmap = hm, solid = FALSE,
          windowsize = c(800, 400), zscale = .25,
          #shadow_darkness = .25, shadowcolor = "#F1C659",
          phi = 80, zoom = .5, theta = 0, background = "white")

render_camera(phi = 80, theta = 0, zoom = .4)


render_highquality("erie_hq.png", parallel = TRUE, lightaltitude = 90, 
                   width = 6000, height = 3000)




rgl::rgl.close()

text_color <- colors[8]

img <- image_read("erie_hq.png")

# Title
img_ <- image_annotate(img, "Great Lake Depths", font = "Cinzel Decorative",
                       weight = 700, decoration = "underline",
                       color = text_color, size = 200, gravity = "southeast",
                       location = "+300+1000")
# Subtitle
img_ <- image_annotate(img_, "Lake Erie", 
                       font = "Cinzel Decorative", location = "+900+700",
                       color = text_color, size = 175, gravity = "southeast")

# Caption
img_ <- image_annotate(img_, "Graphic by Spencer Schien (@MrPecners) | Data from NOAA", 
                       font = "Cinzel Decorative", location = "+0+20",
                       color = alpha(text_color, .5), size = 75, gravity = "south")

magick::image_write(img_, "erie_hq_titled.png")
