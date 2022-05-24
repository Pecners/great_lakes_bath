meas <- rev %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) 

bb <- st_bbox(meas)

p1 <- st_sfc(st_point(c(bb["xmin"], bb["ymin"])), crs = 4326)
p2 <- st_sfc(st_point(c(bb["xmax"], bb["ymin"])), crs = 4326)

d1 <- st_distance(p1, p2)

p3 <- st_sfc(st_point(c(bb["xmin"], bb["ymax"])), crs = 4326)

d2 <- st_distance(p1, p3)

ppm1 <- d1 / abs((bb["xmin"] - bb["xmax"]))
ppm2 <- d2 / abs((bb["ymin"] - bb["ymax"]))

avg_dist <- mean(c(ppm1, ppm2))
avg_dist
