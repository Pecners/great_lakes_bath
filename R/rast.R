lake <- "michigan"

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

shadowcolor <- "#3569AF"

small %>%
  height_shade(texture = (grDevices::colorRampPalette(scico(n = 5, palette = "davos")[2:5]))(256)) %>%
  add_shadow(ambient_shade(small, multicore = TRUE)) %>%
  add_shadow(ray_shade(small, multicore = TRUE, sunaltitude = 80)) %>%
  plot_3d(heightmap = small, solid = FALSE, shadow = FALSE,
          phi = 90, theta = 0)

full_fixed %>%
  height_shade(texture = (grDevices::colorRampPalette(scico(n = 5, palette = "davos")[2:5]))(256)) %>%
  add_shadow(ambient_shade(full_fixed, multicore = TRUE)) %>%
  add_shadow(ray_shade(full_fixed, multicore = TRUE, sunaltitude = 80)) %>%
  plot_3d(heightmap = full_fixed, solid = FALSE, shadow = FALSE,
          phi = 90, theta = 0)

save_obj("test_rast.png")


render_snapshot("test_rast_map_full.png", webshot = TRUE)


rgl::rgl.close()
