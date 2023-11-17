
#' Square crop helper
#'
#' @param x Magick images
#' @param just Where to justify the image prior to
#'
#' @return Magick image
cut_square <- function(x, just = "center") {
  if(!"magick-image" %in% class(x)) x <- image_read(x)
  dat <- image_data(x, "rgba")
  dims <- dim(dat)
  center <- floor(dims[2:3]/2)
  r <- floor(min(dims[2:3])/2)
  start_point <- round(center-r)
  depth <- 2*r
  if(just == "left") {
    start_point[1] <- 0
  } else if(just == "right") {
    start_point[1] <- 2*start_point[1]
  } else if(just == "top") {
    start_point[2] <- 0
  } else if(just == "bottom") {
    start_point[2] <- 2*start_point[2]
  }
  geom <- glue::glue("{depth}x{depth}+{start_point[1]}+{start_point[2]}")
  image_crop(x, geom)
}

#' Circle crop helper
#'
#' @param x Magick images
#' @param just Where to justify the image prior to cropping
#'
#' @return Magick image
cut_circle <- function(x, just = "center") {

    # crop to square
  x <- cut_square(x, just)

  # crop to a circle
  dat <- image_data(x, "rgba")
  dims <- dim(dat)
  center <- floor(dims[2:3]/2)
  r <- floor(min(dims[2:3])/2)

  x_vals <- 1:dims[2]
  y_vals <- 1:dims[3]

  for(x in x_vals) {
    d <- sqrt((x - center[1])^2 + (y_vals - center[2])^2)
    outside <- which(d > r)
    dat[4, x, outside] <- as.raw(00)
  }

  image_read(dat)

}


#' Hex crop helper
#'
#' @param x Magick image
#' @param just Where to justify the image prior to cropping
#'
#' @return Magick image
cut_hex <- function(x, just = "center") {

  # crop to right dimensions
  if(!"magick-image" %in% class(x)) x <- image_read(x)
  dat <- image_data(x, "rgba")
  dims <- dim(dat)
  center <- floor(dims[2:3]/2)
  r <- floor(min(dims[2:3])/2)
  start_point <- round(center-r)
  depth <- 2*r
  if(just == "left") {
    start_point[1] <- 0
  } else if(just == "right") {
    start_point[1] <- 2*start_point[1]
  } else if(just == "top") {
    start_point[2] <- 0
  } else if(just == "bottom") {
    start_point[2] <- 2*start_point[2]
  }
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

#' Parallelogram crop helper
#'
#' @param x Magick image
#' @param just Where to justify the image prior to cropping
#'
#' @return Magick image
cut_parallelogram <- function(x, just = "center") {

  # crop to square
  img <- cut_square(x, just)

  # crop to a paralleogram
  dat <- image_data(img, "rgba")
  dims <- dim(dat)[-1]
  center <- round(dims/2)
  theta <- 15

  y0 <- dims[1]*sin(pi*(90-theta)/180)
  x0 <- dims[1]*cos(pi*(90-theta)/180)

  dat_sq <- img |>
    image_flip() |>
    image_data("rgba")

  dims_sq <- dim(dat_sq)[-1]

  slope <- (dims_sq[2])/x0
  c <- -slope*(dims_sq[1]-x0)
  line_l <- function(x) slope*x
  line_r <- function(x) slope*x + c

  y_vals <- 1:dims_sq[2]

  for(x in 1:x0) {
    pos <- line_l(x)
    outside <- which(y_vals > pos)
    dat_sq[4, x, outside] <- as.raw(00)
  }

  for(x in floor(dims_sq[1]-x0):dims_sq[1]) {
    pos <- line_r(x)
    outside <- which(y_vals <= pos)
    dat_sq[4, x, outside] <- as.raw(00)
  }

  h0 <- dims[1]-x0
  dy <- (dims[1]-sqrt(h0^2-x0^2))/2
  geom <- glue("{dims[1]}x{dims[2]-2*dy}+0+{dy}")
  image_read(dat_sq) |>
    image_flip() |>
    image_crop(geom)

}

#' heart crop helper
#'
#' @param x Magick image
#' @param just Where to justify the image prior to cropping
#'
#' @return Magick images
cut_heart <- function(x, just = "center") {

  # crop to square
  x <- cut_square(x, just)

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
#' @param orig List of original dimensions e.g. `list(wd = 100, ht = 200)`
#' @param bg_fill Background fill
#'
#' @return Magick image
add_border <- function(x, geom, border_size, border_colour, bg_fill, orig) {

  info <- image_info(x)
  ht <- info$height
  wd <- info$width
  d0 <- max(ht, wd)
  d <- d0+2*border_size
  bg <- image_blank(d, d, color = border_colour)
  if(!is.null(bg_fill)) bg_fill <- image_blank(d0, d0, color = bg_fill)

  if(geom == "hex") {

    x_adj <- round(border_size/ht*wd)
    offset <- glue("+{x_adj}+{border_size}")
    bg <- cut_hex(bg)
    if(!is.null(bg_fill)) {
      bg_fill <- cut_hex(bg_fill)
      bg <- image_composite(bg, bg_fill, offset = offset)
    }

  } else if(geom == "heart") {

    offset <- glue("+{border_size}+{border_size}")
    bg <- cut_heart(bg)
    if(!is.null(bg_fill)) {
      bg_fill <- cut_heart(bg_fill)
      bg <- image_composite(bg, bg_fill, offset = offset)
    }

  } else if(geom == "circle"){

    offset <- glue("+{border_size}+{border_size}")
    bg <- cut_circle(bg)
    if(!is.null(bg_fill)) {
      bg_fill <- cut_circle(bg_fill)
      bg <- image_composite(bg, bg_fill, offset = offset)
    }

  } else if(geom == "square") {

    offset <- glue("+{border_size}+{border_size}")
    bg <- cut_square(bg)
    if(!is.null(bg_fill)) {
      bg_fill <- cut_square(bg_fill)
      bg <- image_composite(bg, bg_fill, offset = offset)
    }

  } else if(geom == "parallelogram") {

    offset <- glue("+{border_size}+{0.8*border_size}")
    d <- min(orig$wd, orig$ht)
    bg <- image_blank(width = d, height = d, color = border_colour)
    bg <- cut_parallelogram(bg)
    bg <- image_resize(bg, glue("{wd+2*border_size}x{ht+2*border_size}"))
    if(!is.null(bg_fill)) {
      bg_fill <- cut_parallelogram(bg_fill)
      bg_fill <- image_resize(bg_fill, glue("{wd}x{ht}"))
      bg <- image_composite(bg, bg_fill, offset = offset)
    }

  }

  xout <- image_composite(bg, x, offset = offset)
}
