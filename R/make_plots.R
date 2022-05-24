###########################################
# This file creates the rotating shadow   #
# video. This felt a little hacky to me,  #
# but it's the way I figured out how to   #
# create the effect I wanted.             #
###########################################

# This is an intensive script, so I'm using parallel processing
# to speed things up.

library(parallel)
library(doParallel)

#e <- readRDS("plot.rda")

# This function will create a 3D version of plot 'e' for the specified angle.
# We'll use this function to iterate over all angles we want.
# First you need to create the rgl object with the specified sun angle.
# I might be missing something, but as I understand it, you can adjust the 
# camera view and zoom of the scene, but to adjust the physics of it -- 
# i.e. the scale, the shadow angle, etc. -- you need to rerender the 
# rgl object. 

do_it <- function(angle = 1, scale = 50) {
  # render rgl object given the angle
  rayshader::plot_gg(e, multicore = TRUE, width = 5, height = 5, sunangle = angle, raytrace = TRUE, 
          windowsize = c(1400, 866), solidcolor = bg, solidlinecolor = bg, scale = scale) 
  
  # save the object -- this creates a png of the plot as well, which is what we want.
  # I couldn't figure out how to get this png without saving the whole object, so we'll
  # want to remove the other object files that are created.
  
  rayshader::save_obj(glue::glue("lakes_{angle}.obj"))
  rgl::rgl.close()
  file.remove(glue::glue("lakes_{angle}.mtl"))
  file.remove(glue::glue("lakes_{angle}.obj"))
}

# I have 10 cores on my machine, use parallel::detectCores() if you're unsure

cl <- makeCluster(10)
registerDoParallel(cl)

# This line of code will pass the angles we want to the do_it(), to be done
# in parallel.

foreach(i = 1:360) %dopar% do_it(i)

# This is the same as above, but instead of angles we're scaling the height


d <- seq(from = 0, to = 145, by = 5)

cl <- makeCluster(10)
registerDoParallel(cl)

foreach(i = d) %dopar% do_it(i)

# Next, see R/make_vid.R for creating the movie of rotating shadow
