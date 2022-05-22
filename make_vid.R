f <- list.files()
imgs <- f[str_detect(f, "\\.png")]


r <- sort(rep(seq(from = 0, to = 145, by = 5), 1))

growing <- sprintf("lakes_scale_%d.png", r)

rotating <- sprintf("lakes_%d.png",seq(1,360,by=1))

input <- c(
  growing,
  rotating,
  rev(growing)
)

av::av_encode_video(input, framerate = 30,
 output = "great_lakes.mp4")
