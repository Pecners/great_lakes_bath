library(parallel)
library(doParallel)

e <- readRDS("plot.rda")

do_it <- function(angle) {
  cat(glue::glue("Starting angle {angle}"), "\n")
  rayshader::plot_gg(e, multicore = TRUE, width = 5, height = 5, sunangle = angle, raytrace = TRUE, 
          windowsize = c(1400, 866), solidcolor = bg, solidlinecolor = bg) 
  
  rayshader::save_obj(glue::glue("lakes_{angle}.obj"))
  rgl::rgl.close()
  file.remove(glue::glue("lakes_{angle}.mtl"))
  file.remove(glue::glue("lakes_{angle}.obj"))
  cat(glue::glue("Finished angle {angle}"), "\n")
}

cl <- makeCluster(10)
registerDoParallel(cl)


for (x in 1:36) {
  cat(glue("Working on angles {x}:{x*10}"), "\n")
  foreach(i = c(1:360)) %dopar% do_it(i)
}

do_it2 <- function(s) {
  cat(glue::glue("Starting angle {s}"), "\n")
  rayshader::plot_gg(e, multicore = TRUE, width = 5, height = 5, sunangle = 1, 
                     raytrace = TRUE, scale = s,
                     windowsize = c(1400, 866), solidcolor = bg, solidlinecolor = bg) 
  
  rayshader::save_obj(glue::glue("lakes_scale_{s}.obj"))
  rgl::rgl.close()
  file.remove(glue::glue("lakes_scale_{s}.mtl"))
  file.remove(glue::glue("lakes_scale_{s}.obj"))
  cat(glue::glue("Finished angle {s}"), "\n")
}

cl <- makeCluster(10)
registerDoParallel(cl)

d <- seq(from = 0, to = 145, by = 5)
foreach(i = 95) %dopar% do_it2(i)

