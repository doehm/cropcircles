#' Circle crop
#'
#' Reads in an image and circle crops it with a transparent
#' background. If a new path is given it will save the cropped images to
#' the new location. If no path is given it will save to a temporary location
#' which will be cleared when the session is closed
#'
#' @param images Vector of image paths, either local or urls. If urls the images
#' will be downloaded first.
#' @param to Path to new location
#'
#' @importFrom magick image_read image_data image_write image_crop image_resize
#'
#' @return Path to cropped images
#' @export
#'
#' @examples
#' library(magick)
#'
#' x <- c(1, 3, 9, 8)
#' path <- "https://openpsychometrics.org/tests/characters/test-resources/pics/BB/"
#' img_paths <- paste0(path, x, ".jpg")
#'
#' img_paths_cropped <- circle_crop(img_paths)
#'
#' imgs <- image_read(img_paths_cropped)
#' image_montage(imgs)
circle_crop <- function(images, to = NULL) {

  if(is.null(to)) {
    to <- purrr::map_chr(1:length(images), ~tempfile(pattern = "cropped", tmpdir = tempdir(), fileext = ".png"))
  }

  purrr::map2_chr(images, to, function(images, to) {

    # crop to square
    img <- image_read(images)
    dat <- image_data(img, "rgba")
    dims <- dim(dat)
    center <- floor(dims[2:3]/2)
    r <- floor(min(dims[2:3])/2)
    start_point <- round(center-r)
    depth <- 2*r
    geom <- glue::glue("{depth}x{depth}+{start_point[1]}+{start_point[2]}")
    img <- image_crop(img, geom)

    # crop to a circle
    dat <- image_data(img, "rgba")
    dims <- dim(dat)
    center <- floor(dims[2:3]/2)

    x_vals <- 1:dims[2]
    y_vals <- 1:dims[3]

    for(x in x_vals) {
      d <- sqrt((x - center[1])^2 + (y_vals - center[2])^2)
      outside <- which(d > r)
      dat[4, x, outside] <- as.raw(00)
    }

    # write and return path
    image_write(image_read(dat), to)

    to

  })

}


#' Square crop
#'
#' Reads in an image and square crops. If a new path is given it will save the cropped images to
#' the new location. If no path is given it will save to a temporary location
#' which will be cleared when the session is closed
#'
#' @param images Vector of image paths, either local or urls. If urls the images
#' will be downloaded first.
#' @param to Path to new location
#'
#' @return Path to cropped images
#' @export
#'
#' @examples
#' library(magick)
#'
#' x <- c(1, 3, 9, 8)
#' path <- "https://openpsychometrics.org/tests/characters/test-resources/pics/BB/"
#' img_paths <- paste0(path, x, ".jpg")
#'
#' img_paths_cropped <- square_crop(img_paths)
#'
#' imgs <- image_read(img_paths_cropped)
#' image_montage(imgs)
square_crop <- function(images, to = NULL) {

  if(is.null(to)) {
    to <- purrr::map_chr(1:length(images), ~tempfile(pattern = "cropped", tmpdir = tempdir(), fileext = ".png"))
  }

  purrr::map2_chr(images, to, function(images, to) {

    # crop to square
    img <- image_read(images)
    dat <- image_data(img, "rgba")
    dims <- dim(dat)
    center <- floor(dims[2:3]/2)
    r <- floor(min(dims[2:3])/2)
    start_point <- round(center-r)
    depth <- 2*r
    geom <- glue::glue("{depth}x{depth}+{start_point[1]}+{start_point[2]}")
    img <- image_crop(img, geom)

    # write and return path
    image_write(img, to)

    to

  })

}

#' Hex crop
#'
#' Reads in an image and crops to a hexagon. If a new path is given it will save the cropped images to
#' the new location. If no path is given it will save to a temporary location
#' which will be cleared when the session is closed
#'
#' @param images Vector of image paths, either local or urls. If urls the images
#' will be downloaded first.
#' @param to Path to new location
#'
#' @return Path to cropped images
#' @export
#'
#' @examples
#' library(magick)
#'
#' x <- c(1, 3, 9, 8)
#' path <- "https://openpsychometrics.org/tests/characters/test-resources/pics/BB/"
#' img_paths <- paste0(path, x, ".jpg")
#'
#' img_paths_cropped <- hex_crop(img_paths)
#'
#' imgs <- image_read(img_paths_cropped)
#' image_montage(imgs)
hex_crop <- function(images, to = NULL) {

  if(is.null(to)) {
    to <- purrr::map_chr(1:length(images), ~tempfile(pattern = "cropped", tmpdir = tempdir(), fileext = ".png"))
  }

  purrr::map2_chr(images, to, function(images, to) {

    # crop to right dimensions
    img <- image_read(images[1])
    dat <- image_data(img, "rgba")
    dims <- dim(dat)
    center <- floor(dims[2:3]/2)
    r <- floor(min(dims[2:3])/2)
    start_point <- round(center-r)
    depth <- 2*r
    geom <- glue::glue("{depth*0.8662}x{depth}+{start_point[1]+0.1338*center[1]}+{start_point[2]}")
    img <- image_crop(img, geom)

    # crop to a hex
    dat <- image_data(img, "rgba")
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

    # write and return path
    image_write(image_read(dat), to)

    to

  })

}

#' Heart crop
#'
#' Reads in an image and crops to a heart shape If a new path is given it will save the cropped images to
#' the new location. If no path is given it will save to a temporary location
#' which will be cleared when the session is closed
#'
#' @param images Vector of image paths, either local or urls. If urls the images
#' will be downloaded first.
#' @param to Path to new location
#'
#' @return Path to cropped images
#' @export
#'
#' @examples
#' library(magick)
#'
#' x <- c(1, 3, 9, 8)
#' path <- "https://openpsychometrics.org/tests/characters/test-resources/pics/BB/"
#' img_paths <- paste0(path, x, ".jpg")
#'
#' img_paths_cropped <- heart_crop(img_paths)
#'
#' imgs <- image_read(img_paths_cropped)
#' image_montage(imgs)
heart_crop <- function(images, to = NULL) {

  if(is.null(to)) {
    to <- purrr::map_chr(1:length(images), ~tempfile(pattern = "cropped", tmpdir = tempdir(), fileext = ".png"))
  }

  purrr::map2_chr(images, to, function(image, to) {

    # crop to right dimensions
    img <- image_read(image)
    dat <- image_data(img, "rgba")
    dims <- dim(dat)
    center <- floor(dims[2:3]/2)
    r <- floor(min(dims[2:3])/2)
    start_point <- round(center-r)
    depth <- 2*r
    geom <- glue::glue("{depth}x{depth}+{start_point[1]}+{start_point[2]}")
    img <- image_crop(img, geom)

    # crop to a hex
    dat <- image_data(img, "rgba")
    dims <- dim(dat)
    heart <- image_read(file.path(system.file(package = "cropcircles"), "masks", "heart.png"))
    heart <- image_resize(heart, glue::glue("{dims[2]}x{dims[3]}"))
    dat_heart <- image_data(heart, "rgba")
    dat[4,,] <- dat_heart[4,,]

    # write and return path
    image_write(image_read(dat), to)

    to

  })

}

