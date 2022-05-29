library(MetBrewer)
library(magick)

lake <- "huron"

d_ <- raster(glue("data/{lake}_lld/{lake}_lld.tif"))

dd <- raster_to_matrix(d_)

small <- resize_matrix(dd, 0.1)


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

remove <- function(x) {
  rx <- .8 * nrow(x)
  ry <- .8 * ncol(x)
  for(row in 1:nrow(x)) {
    for(col in 1:ncol(x)) {
      if(row > rx & col > ry) {
        x[row, col] <- NA
      }
    }
  }
  return(x)
}

ff_r <- remove(full_fixed)
sf_r <- remove(small_fixed)

#colors <- met.brewer("Tam")
colors <- rev(met.brewer("Paquin"))
colors <- scico(n = 10, palette = "batlow")


hm <- ff_r

w <- nrow(hm)
h <- ncol(hm)

wr <- w / max(c(w,h))
hr <- h / max(c(w,h))

hm %>%
  height_shade(texture = grDevices::colorRampPalette(colors)(256)) %>%
  #height_shade(texture = (grDevices::colorRampPalette(colors))(256)) %>%
  #add_shadow(lamb_shade(hm)) %>%
  #add_shadow() %>%
  #add_shadow(ray_shade(hm, multicore = TRUE, sunaltitude = 80)) %>%
  plot_3d(heightmap = hm, solid = FALSE, 
          windowsize = c(800*wr, 800*hr), shadowdepth = -100,
          shadowwidth = 100, shadowcolor = colors[1],
          phi = 90, zoom = .7, theta = 0, background = "white")

render_camera(phi = 90, theta = 0, zoom = .75, fov = 0)


render_highquality("huron_hq_env_deeper_batlow.png", parallel = TRUE, light = FALSE, interactive = FALSE,
                   #ambient_light = TRUE, backgroundhigh = colors[1],
                   environment_light = "env/phalzer_forest_01_4k.hdr",
                   intensity_env = 2,
                   rotate_env = -100, 
                   width = round(6000 * wr), height = round(6000 * hr)
                   )




rgl::rgl.close()

# Paquin palette

colors <- rev(met.brewer("Paquin"))
text_color <- colors[length(colors)]

img <- image_read("huron_hq_env_deeper.png")

# Title
img_ <- image_annotate(img, "Great Lake Depths", font = "Cinzel Decorative",
                       weight = 700, decoration = "underline",
                       color = text_color, size = 200, gravity = "southeast",
                       location = "+150+1400")
# Subtitle
img_ <- image_annotate(img_, "Lake Huron", 
                       font = "Cinzel Decorative", location = "+750+1100",
                       color = text_color, size = 150, gravity = "southeast")

# Caption
img_ <- image_annotate(img_, "Graphic by Spencer Schien (@MrPecners) | Data from NOAA", 
                       font = "Cinzel Decorative", location = "+0+50",
                       color = alpha(text_color, .5), size = 75, gravity = "south")

magick::image_write(img_, "huron_hq_env_panquin_titled.png")

# Batlow palette

colors <- scico(n = 10, palette = "batlow")
text_color <- colors[1]

img <- image_read("huron_hq_env_deeper_batlow.png")

# Title
img_ <- image_annotate(img, "Great Lake Depths", font = "Cinzel Decorative",
                       weight = 700, decoration = "underline",
                       color = text_color, size = 200, gravity = "southeast",
                       location = "+150+1400")
# Subtitle
img_ <- image_annotate(img_, "Lake Huron", 
                       font = "Cinzel Decorative", location = "+750+1100",
                       color = text_color, size = 150, gravity = "southeast")

# Caption
img_ <- image_annotate(img_, "Graphic by Spencer Schien (@MrPecners) | Data from NOAA", 
                       font = "Cinzel Decorative", location = "+0+50",
                       color = alpha(text_color, .5), size = 75, gravity = "south")

magick::image_write(img_, "huron_hq_env_batlow_titled.png")

