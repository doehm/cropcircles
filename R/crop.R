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
#' @param border_size Border size in pixels.
#' @param border_colour Border colour.
#' @param just Where to justify the image prior to cropping. Accepted values: `left`, `right`, `top`, `bottom`
#'
#' @importFrom magick image_read image_data image_write image_crop image_resize image_blank image_info image_composite
#' @importFrom glue glue
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
#' img_paths_cropped <- circle_crop(img_paths, border_size = 6)
#'
#' imgs <- image_read(img_paths_cropped)
#' image_montage(imgs)
#'
#' # justification example
#' img_path <- file.path(system.file(package = "cropcircles"), "images", "walter-jesse.png")
#' image_read(img_path)
#'
#' # center (default)
#' image_read(circle_crop(img_path, border_size = 10))
#'
#' # left
#' image_read(circle_crop(img_path, border_size = 10, just = "left"))
#'
#' # right
#' image_read(circle_crop(img_path, border_size = 10, just = "right"))
circle_crop <- function(images, to = NULL, border_size = NULL, border_colour = "black", just = "center") {

  if(is.null(to)) {
    to <- purrr::map_chr(1:length(images), ~tempfile(pattern = "cropped", tmpdir = tempdir(), fileext = ".png"))
  }

  n <- length(images)
  if(length(border_colour) == 1) border_colour <- rep(border_colour, n)

  for(j in 1:n) {
    # crop image
    imgc <- cut_circle(images[j], just)

    # add border
    if(!is.null(border_size)) {
      imgc <- add_border(imgc, geom = "circle", border_size, border_colour[j])
    }

    # write and return path
    image_write(imgc, to[j])
  }

  to
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
#' @param border_size Border size in pixels
#' @param border_colour Border colours
#' @param just Where to justify image prior to cropping. Accepted values: `left`, `right`, `top`, `bottom`
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
square_crop <- function(images, to = NULL, border_size = NULL, border_colour = "black", just = "center") {

  if(is.null(to)) {
    to <- purrr::map_chr(1:length(images), ~tempfile(pattern = "cropped", tmpdir = tempdir(), fileext = ".png"))
  }

  n <- length(images)
  if(length(border_colour) == 1) border_colour <- rep(border_colour, n)

  for(j in 1:n) {
    # crop image
    imgc <- cut_square(images[j], just)

    # add border
    if(!is.null(border_size)) {
      imgc <- add_border(imgc, geom = "square", border_size, border_colour[j])
    }

    # write and return path
    image_write(imgc, to[j])
  }

  to

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
#' @param border_size Border size in pixels.
#' @param border_colour Border colour.
#' @param just Where to justify image prior to cropping. Accepted values: `left`, `right`, `top`, `bottom`
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
hex_crop <- function(images, to = NULL, border_size = NULL, border_colour = "black", just = "center") {

  if(is.null(to)) {
    to <- purrr::map_chr(1:length(images), ~tempfile(pattern = "cropped", tmpdir = tempdir(), fileext = ".png"))
  }

  n <- length(images)
  if(length(border_colour) == 1) border_colour <- rep(border_colour, n)

  for(j in 1:n) {
    # crop image
    imgc <- cut_hex(images[j], just)

    # add border
    if(!is.null(border_size)) {
      imgc <- add_border(imgc, geom = "hex", border_size, border_colour[j])
    }

    # write and return path
    image_write(imgc, to[j])
  }

  to

}

#' Heart crop
#'
#' Reads in an image and crops to a heart shape If a new path is given it will save the cropped images to
#' the new location. If no path is given it will save to a temporary location
#' which will be cleared when the session is closed
#'
#' @param images Vector of image paths, either local or urls. If urls the images
#' will be downloaded first.
#' @param to Path to new location.
#' @param border_size Border size in pixels.
#' @param border_colour Border colour.
#' @param just Where to justify image prior to cropping. Accepted values: `left`, `right`, `top`, `bottom`
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
heart_crop <- function(images, to = NULL, border_size = NULL, border_colour = "black", just = "center") {

  if(is.null(to)) {
    to <- purrr::map_chr(1:length(images), ~tempfile(pattern = "cropped", tmpdir = tempdir(), fileext = ".png"))
  }

  n <- length(images)
  if(length(border_colour) == 1) border_colour <- rep(border_colour, n)

  for(j in 1:n) {
    # crop image
    imgc <- cut_heart(images[j], just)

    # add border
    if(!is.null(border_size)) {
      imgc <- add_border(imgc, geom = "heart", border_size, border_colour[j])
    }

    # write and return path
    image_write(imgc, to[j])
  }

  to
}

