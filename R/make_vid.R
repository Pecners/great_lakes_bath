###########################################
# Run this file after you've created the  #
# plots in R/make_plots.R. This file will #
# order the files and create the video.   #
###########################################

# This is pulling all png files in the current working dir;
# Be sure there aren't other png files, or redirect this.

f <- list.files()
imgs <- f[str_detect(f, "\\.png")]

# Make sure they're in order because this is how they'll be put
# together to make the movie.

# Order of the growing files. This is the same as the vector of
# scales used in R/make_plots.R

r <- sort(rep(seq(from = 0, to = 145, by = 5), 1))

growing <- sprintf("lakes_scale_%d.png", r)

# Order of the sun angle files.

rotating <- sprintf("lakes_%d.png",seq(1,360,by=1))

# Combine both orders so that the video starts with the plot growing,
# then the light source does one rotation, then the plot shrinks.

input <- c(
  growing,
  rotating,
  rev(growing)
)

# Check if any files are missing -- the parallel process threw the 
# occassional error, and I wasn't always sure which plots were missed.
# I ended up having to use this step to make sure I had all the files.

missing <- input[!file.exists(input)]

# This will create the video. This using ffmpeg (a CLI tool), and it's
# how rayshader creates movies. I'm using this instead of render_movie()
# because we have two separate vectors of plots (rotating light and growing).
# render_movie() using a single rgl object, so it can't handle the two.

av::av_encode_video(input, framerate = 30, codec = "libx264", 
 output = "great_lakes_titled.mp4")
