library(tidyverse)
library(rayshader)
library(glue)
library(magick)
library(MetBrewer)
library(terra)
library(scico)
library(raster)
library(rayimage)
library(rayrender)
# Data source: https://www.ngdc.noaa.gov/mgg/greatlakes/
# Download the GeoTiff files

files <- list.files("data")

lakes <- map_df(files, function(lake) {
  d_ <- rast(glue("data/{lake}/{lake}.tif"))
  d_ <- terra::aggregate(d_, fact = 5)
  df <- as.data.frame(d_, xy = TRUE) %>%
    mutate(lake = str_remove(lake, "_lld$"))
  names(df) <- c("long", "lat", "elevation", "lake")
  return(df)
})

rev <- lakes %>%
  filter(elevation < 0) %>%
  mutate(elevation = abs(elevation)) 

bg <- "white"

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
  theme(legend.position = "none",
        panel.background = element_rect(fill = bg),
        panel.grid = element_line(color = bg),
        plot.background = element_rect(fill = bg, color = bg),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin()) +
  labs(x = "", y = "")

saveRDS(e, "plot.rda")

#ggsave("normal_plot.png", w = 9, h = 6, bg = "grey90")
plot_gg(e, multicore = TRUE, width = 5, height = 5, sunangle = 180, raytrace = TRUE, 
        windowsize = c(1400, 866), solidcolor = bg, solidlinecolor = bg) 

save_obj("lakes.obj")

ggsave("test.png", width = 9, height = 9)

render_snapshot(camera_location = c(.095, 10, 1.58), camera_lookat = c(.095, 10, 1.58))

save_obj("lakes.obj")
rgl::rgl.close()


zoomvec <- c(
  seq(.4, .2, length.out = 25),
  seq(.2, .1, length.out = 25),
  rep(.1, 20),
  seq(.1, .35, length.out = 50)
  
)
# North-south direction of viewing
thetavec <- c(
  rep(80, 50),
  seq(80, 50, length.out = 20),
  seq(50, 0, length.out = 50)
)

# Sun
phivec <- c(
  seq(30, 20, length.out = 25),
  seq(20, 5, length.out = 25),
  rep(5, 20),
  seq(5, 30, length.out = 50)
)

frames <- length(zoomvec)

# taken from help

phivechalf = 5 + 60 * 1/(1 + exp(seq(-7, 20, length.out = 180)/2))
p2 <- 5 + 60 * 1/(1 + exp(seq(-7, 2, length.out = 90)/2))
p3 <- rep(5 + 60 * 1/(1 + exp(2/2)), 180)
phivecfull <- c(phivechalf, rev(phivechalf), rep(p2[1], 3), p2, p3, rev(p2))
theta <- c(-1:-360, rep(-360, 3), -360:-1)

render_movie(filename = "erie.mp4", type = "custom", 
             theta = theta, 
             phi = phivecfull,
             frames = length(theta))


phivechalf = 30 + 60 * 1/(1 + exp(seq(-7, 20, length.out = 180)/2))
phivecfull = c(phivechalf, rev(phivechalf))
thetavec = -90 + 45 * sin(seq(0,359,length.out = 360) * pi/180)
zoomvec = 0.45 + 0.2 * 1/(1 + exp(seq(-5, 20, length.out = 180)))
zoomvecfull = c(zoomvec, rev(zoomvec))

# Angle of sun, so to speak (not shadows)
phivecfull <- c(
  seq(80, 50, length.out = 10),
  seq(from = 80, to = 30, length.out = 10),
  rep(30, 340)
)

zoomvecfull <- c(
  seq(.6, .3, length.out = 20),
  seq(.3, .4, length.out = 10),
  rep(.4, 330)
)
render_movie(filename = "erie.mp4", type = "custom", frames = 360,
             phi = phivecfull, zoom = .5, theta = c(1:360) * -1)

save_obj("ggplot.obj")
rgl::rgl.close()

disk(radius=1000,y=-1, 
     material=diffuse(checkerperiod = 6,checkercolor="#0d401b", color="#496651")) %>% 
  add_object(obj_model("ggplot.obj", y=-0.02, texture=TRUE, scale_obj = 1/100)) %>%
  add_object(sphere(y=30,z=10,radius=5,material = light(intensity=40))) %>% 
  render_scene(lookfrom=c(20,20,20),fov=0,ortho_dimensions=c(30,30), width=800,height=800)

ggsave("raysurvace.png")



plot_image("raysurvace.png")
vals = locator(n=5)
vals$z = vals$y
vals$y = rep(5,length(vals$y))
selected_points = do.call(cbind,vals)
selected_points

loaded_texture = png::readPNG("raysurvace.png")
dim(loaded_texture)

#Center at origin, flip, and scale by 1/100 to match our rayrender model
selected_points[,1] = (selected_points[,1] - 2666/2)/100
selected_points[,3] = -(selected_points[,3] - 2162/2)/100

keylist = list()
for(i in 1:nrow(selected_points)) {
  keylist[[i]] = sphere(x=selected_points[i,1],
                        y=selected_points[i,2],
                        z=selected_points[i,3],radius=0.3,
                        material=diffuse(color="purple"))
}
keyscene = do.call(rbind,keylist)

disk(radius=1000,y=-1, 
     material=diffuse(checkerperiod = 6,checkercolor="#0d401b", color="#496651")) %>% 
  add_object(obj_model("ggplot.obj", y=-0.02, texture=TRUE, scale_obj = 1/100)) %>%
  add_object(sphere(y=30,z=-10,radius=5,material = light(intensity=40))) %>% 
  add_object(path(points=selected_points,width=0.1, closed = TRUE, 
                  material=diffuse(color="red"))) %>% 
  add_object(keyscene) %>% 
  render_scene(lookfrom = c(0, 10, 0), fov = 0, ortho_dimensions = c(30, 30), camera_up = c(0, 0, -1),
               width = 800, height = 800, samples = 256)


###############################
# Adjust height of coaster    #
###############################
selected_points_new <- selected_points
selected_points_new[,2] = c(0.3, 0.8, 4, 0.8, 0.5)

keylist_adjusted = list()
for(i in 1:nrow(selected_points_new)) {
  keylist_adjusted[[i]] = sphere(x=selected_points_new[i,1],
                                 y=selected_points_new[i,2],
                                 z=selected_points_new[i,3],
                                 radius=0.3, material=diffuse(color="purple"))
}
keyscene_adj = do.call(rbind,keylist_adjusted)

disk(radius=1000,y=-1, 
     material=diffuse(checkerperiod = 6,checkercolor="#0d401b", color="#496651")) %>% 
  add_object(obj_model("ggplot.obj", y=-0.02, texture=TRUE, scale_obj = 1/100)) %>%
  add_object(sphere(y=30,z=10,radius=5,material = light(intensity=40))) %>% 
  add_object(path(points=selected_points_new,width=0.1, closed = TRUE, 
                  material=diffuse(color="red"))) %>% 
  add_object(keyscene_adj) %>%
  render_scene(lookfrom = c(20, 20, 20),fov = 0,ortho_dimensions = c(30, 30), 
               width = 800, height = 800, samples = 256)

camera_motion = generate_camera_motion(selected_points_new, closed=TRUE, 
                                       frames = 360, constant_step = TRUE)


#Offset above track
selected_points_offset = selected_points_new
selected_points_offset[,2] = selected_points_offset[,2] + 0.15

strutlist = list()
for(i in 1:nrow(camera_motion)) {
  strutlist[[i]] = segment(start = as.numeric(camera_motion[i,1:3])-c(0,0.05,0),
                           end =as.numeric(camera_motion[i,1:3])-c(0,20,0),
                           radius=0.02, material=diffuse(color="grey10"))
  
}
strutscene = do.call(rbind,strutlist)

lookats <- selected_points_offset 
lookats[,1] <- 20
lookats[,2] <- 20
lookats[,3] <- 20

camera_motion_real = generate_camera_motion(selected_points_offset)

disk(radius=1000,y=-1, 
     material=diffuse(checkerperiod = 6,checkercolor="#0d401b", color="#496651")) %>% 
  add_object(obj_model("ggplot.obj", y=-0.02, texture=TRUE, scale_obj = 1/100)) %>%
  render_scene(lookfrom = selected_points_offset[1,], fov = 90, 
               width = 800, height = 800, samples = 256)

render_snapshot(camera_location = selected_points_offset[1,])




