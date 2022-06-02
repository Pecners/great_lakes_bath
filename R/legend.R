add_legend <- function(data) {
  x_start <- round(.8 * nrow(data))
  y_start <- round(.5 * ncol(data))
  x_end <- x_start + .05 * nrow(data)
  y_end <- y_start + .25 * ncol(data)

  z <- abs(y_start - y_end)
  print(z)
  print(y_end)
  
  for (row in x_start:x_end) {
    for (col in y_start:y_end) {
      t <- (col - y_start) * z / 50
      if (t < max(data, na.rm = TRUE)) {
        data[row, col] <- t
      }
    }
  }
  
  return(data)
}

w_leg <- add_legend(small_fixed)
