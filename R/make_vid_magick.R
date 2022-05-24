###########################################
# This file creates the rotating map with #
# title, subtitle, caption, and legend    #
# superimposed in the foreground (i.e.    #
# not rotating).                          #
###########################################

library(magick)

# Plot without annotation -- needs the 'rev' object
# from R/load_data.R

e_noleg <- rev %>%
  filter(lake == "superior") %>%
  ggplot(aes(long, lat, fill = elevation)) +
  geom_tile() +
  geom_tile(data = rev %>% filter(lake == "michigan")) +
  geom_tile(data = rev %>% filter(lake == "huron")) +
  geom_tile(data = rev %>% filter(lake == "erie")) +
  geom_tile(data = rev %>% filter(lake == "ontario")) +
  coord_sf(crs = 3174, expand = 0 ) +
  scale_fill_scico(palette = "davos", direction = -1) +
  theme(legend.position = "none",
        #legend.key =  element_rect(fill = NA),
        #legend.key.size = unit(.02, "npc"),
        panel.background = element_rect(fill = bg),
        panel.grid = element_line(color = bg),
        plot.background = element_rect(fill = bg, color = bg),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.margin = margin()) +
  labs(x = "", y = "") 

# Set up the legend -- you'll have to have run R/get_leg_image.R first

leg <- image_read("legend.png") 

# get width and height of image with print(leg)

w <- 2662
h <- 2187

# You might need to dial this in, but here we're cropping the image
# to make it easier when we add it to the plots below. See here for
# {magick} documentation: https://cran.r-project.org/web/packages/magick/vignettes/intro.html

ww <- w/2 - 150
hh <- h/2 - 250

cleg <- image_crop(leg, glue("400x500+{ww}+{hh}")) %>%
  image_scale("x300")

print(cleg)

# As opposed to the rotating light video, this video can use a single rgl object
# so we just need to call this once.

rayshader::plot_gg(e_noleg, multicore = TRUE, width = 5, height = 5, sunangle = 1, 
                   raytrace = TRUE, scale = 50,
                   windowsize = c(1400, 866), solidcolor = bg, solidlinecolor = bg) 
#render_camera(zoom = .4)

# Set the number of frames -- I want 2 rotations, which is 720 degrees,
# but I also want to be able to slow down the video, which requires more
# frames to keep it from being chopy. Therefore, I'll iterate by .5 degrees
# and calculate vectors of 1440 length.

frames <- 720 * 2

# Set up temp dir for png files, or set up your own dir if you want to keep
# the frame png files.

png_files = file.path(tempdir(), sprintf("image%d.png", 
                                         seq_len(frames)))
on.exit(unlink(png_files))

#####################################
# Below we set phi, theta, and zoom #
# which determine the camera view.  #
# If you change what I have here,   #
# plot the values to makes sure     #
# everything is smooth.             #
# i.e. plot(phi)                    #
#####################################

# Set vector of phi values for the camera -- phi is the angle at which
# the camera is looking at the ground, e.g. 0 would be parallel to the ground.

phivechalf = 15 + 15 * 1/(1 + exp(seq(-15, 15, length.out = 120)/2))
phivecfull = c(phivechalf, rep(phivechalf[length(phivechalf)], 520), rev(phivechalf))
phi <- c(
  rep(30, 680),
  phivecfull
)
plot(phi)
# Set vector of theta, which is the angle around the plot. Theta of 0
# would be the orientation of how ggplot creates the plot.

theta <- rep(seq(from = 0, to = 359.5, by = .5), 2)


# Set a vector of zoom
zoomvec = 0.2 + 0.2 * 1/(1 + exp(seq(-15, 15, length.out = 120)/2))
zoom_vec_full <- c(zoomvec, rep(zoomvec[length(zoomvec)], 520),
                   rev(zoomvec))
zoom <- c(
  rep(.4, 680),
  zoom_vec_full
)


# This loops for the number of frames we want,
# changing the camera view of our rgl object to the respective values
# for phi, theta, and zoom

for (i in 1:frames) {
    
  
  render_camera(phi = phi[i], zoom = zoom[i], theta = theta[i])
  
  # Capture the snapshot of the new camera view, and save it to our 
  # png files.
  
  render_snapshot(filename = png_files[i], top = FALSE, 
                  webshot = FALSE)
  
  # Once saved, now we used {magick} to add our title, subtitle, and legend.
  # I'm using the Cinzel Decorative font, which I downloaded from Google.
  # (https://fonts.google.com/specimen/Cinzel+Decorative?query=Cinzel)
  # I manually added this font to my computer so {magick} could find it,
  # I'm not sure if there's a way to do it from R (e.g. with {showtext})
  
  img <- image_read(png_files[i])
  
  # Title
  img_ <- image_annotate(img, "Great Lake Depths", font = "Cinzel Decorative",
                         color = text_color, size = 80, gravity = "north")
  # Subtitle
  img_ <- image_annotate(img_, "(3-D height exaggerated)", 
                         font = "Cinzel Decorative", location = "+0+100",
                         color = text_color, size = 30, gravity = "north")
  
  # Caption
  img_ <- image_annotate(img_, "Graphic by Spencer Schien (@MrPecners) | Data from NOAA", 
                         font = "Cinzel Decorative", location = "+0+20",
                         color = alpha(text_color, .5), size = 20, gravity = "south")
  
  # Legend
  img_mosaic <- image_composite(img_, cleg, offset = "+40+500")
  
  magick::image_write(img_mosaic, png_files[i])
  
  #print(img_mosaic)
}

# Since it can take a while to render the rgl object to begin with, I wouldn't
# close the window until you're sure you're ready. You could also just x it out.

# rgl::rgl.close()

# Now, with our frames complete, we create the video!

av::av_encode_video(png_files, output = "full_size_slow.mp4", framerate = 25)
