#' Circle crop
#'
#' @param x Magick images
#'
#' @return Magick image
f_circle <- function(x) {
  # crop to square
  if(!"magick-image" %in% class(x)) x <- image_read(x)
  dat <- image_data(x, "rgba")
  dims <- dim(dat)
  center <- floor(dims[2:3]/2)
  r <- floor(min(dims[2:3])/2)
  start_point <- round(center-r)
  depth <- 2*r
  geom <- glue::glue("{depth}x{depth}+{start_point[1]}+{start_point[2]}")
  x <- image_crop(x, geom)

  # crop to a circle
  dat <- image_data(x, "rgba")
  dims <- dim(dat)
  center <- floor(dims[2:3]/2)

  x_vals <- 1:dims[2]
  y_vals <- 1:dims[3]

  for(x in x_vals) {
    d <- sqrt((x - center[1])^2 + (y_vals - center[2])^2)
    outside <- which(d > r)
    dat[4, x, outside] <- as.raw(00)
  }
  image_read(dat)
}


#' Title
#'
#' @param x Magick image
#'
#' @return Magick image
f_hex <- function(x) {
  # crop to right dimensions
  if(!"magick-image" %in% class(x)) x <- image_read(x)
  dat <- image_data(x, "rgba")
  dims <- dim(dat)
  center <- floor(dims[2:3]/2)
  r <- floor(min(dims[2:3])/2)
  start_point <- round(center-r)
  depth <- 2*r
  geom <- glue::glue("{depth*0.8662}x{depth}+{start_point[1]+0.1338*center[1]}+{start_point[2]}")
  x <- image_crop(x, geom)

  # crop to a hex
  dat <- image_data(x, "rgba")
  dims <- dim(dat)[-1]
  center <- round(dims/2)
  x1 <- round(center[1]-cos(pi/6)*center[2])
  x2 <- dims[1]-x1
  y1 <- (dims[2]-center[2])/2 + center[2]
  y2 <- (dims[2]-center[2])/2

  line1 <- function(x) (dims[2]-y1)/(center[1]-x1)*(x-x1) + y1
  line2 <- function(x) (y1-dims[2])/(x2-center[1])*(x-center[1]) + dims[2]

  x_vals <- 1:dims[1]
  y_vals <- 1:dims[2]

  for(x in 1:center[1]) {
    pos <- line1(x)
    outside <- which(y_vals < dims[2] - pos | y_vals > pos)
    dat[4, x, outside] <- as.raw(00)
  }

  for(x in (center[1]+1):dims[1]) {
    pos <- line2(x)
    outside <- which(y_vals < dims[2] - pos | y_vals > pos)
    dat[4, x, outside] <- as.raw(00)
  }
  image_read(dat)
}

#' heart cropping
#'
#' @param x Magick image
#'
#' @return Magick images
f_heart <- function(x) {

  if(!"magick-image" %in% class(x)) x <- image_read(x)
  dat <- image_data(x, "rgba")
  dims <- dim(dat)
  center <- floor(dims[2:3]/2)
  r <- floor(min(dims[2:3])/2)
  start_point <- round(center-r)
  depth <- 2*r
  geom <- glue::glue("{depth}x{depth}+{start_point[1]}+{start_point[2]}")
  x <- image_crop(x, geom)

  # crop to a heart
  dat <- image_data(x, "rgba")
  dims <- dim(dat)
  heart <- image_read(file.path(system.file(package = "cropcircles"), "masks", "heart.png"))
  heart <- image_resize(heart, glue::glue("{dims[2]}x{dims[3]}"))
  dat_heart <- image_data(heart, "rgba")
  dat[4,,] <- dat_heart[4,,]

  image_read(dat)
}

#' Add border helper
#'
#' @param x magick image
#' @param geom Geometric shape e.g. circle, hex, heart.
#' @param border_size Border size in pixels.
#' @param border_colour Border colour
#'
#' @return Magick image
add_border <- function(x, geom, border_size, border_colour) {

  info <- image_info(x)
  ht <- info$height
  wd <- info$width
  d <- max(ht, wd)+2*border_size
  bg <- image_blank(d, d, color = border_colour)

  if(geom == "hex") {
    x_adj <- round(border_size/ht*wd)
    offset <- glue("+{x_adj}+{border_size}")
    bg <- f_hex(bg)
  } else if(geom == "heart") {
    offset <- glue("+{border_size}+{border_size}")
    bg <- f_heart(bg)
  } else {
    offset <- glue("+{border_size}+{border_size}")
    bg <- f_circle(bg)
  }

  image_composite(bg, x, offset = offset)
}
