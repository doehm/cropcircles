#' Cropping functions
#'
#' Reads in an image and crops to the specified geometry with a transparent
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
#' @param bg_fill Background fill. Allows a different colour as background to the border colour for
#' images with a transparent background.
#'
#' @importFrom magick image_read image_data image_write image_crop image_resize image_blank image_info image_composite image_flip
#' @importFrom glue glue
#'
#' @name crop
#'
#' @return Path to cropped images
#' @export
#'
#' @examples
#' library(cropcircles)
#' library(magick)
#'
#' img_path <- file.path(system.file(package = "cropcircles"), "images", "walter-jesse.png")
#' img_cropped <- circle_crop(img_path, border_size = 6)
#' image_read(img_cropped)
#'
#' # other geometries
#'
#' image_read(hex_crop(img_path, border_size = 6))
#' image_read(heart_crop(img_path, border_size = 6))
#'
#' # justification example
#'
#' # center (default)
#' image_read(circle_crop(img_path, border_size = 6))
#'
#' # left
#' image_read(circle_crop(img_path, border_size = 6, just = "left"))
#'
#' # right
#' image_read(circle_crop(img_path, border_size = 6, just = "right"))
circle_crop <- function(images, to = NULL, border_size = NULL, border_colour = "black", bg_fill = NULL, just = "center") {

  if(is.null(to)) {
    to <- purrr::map_chr(1:length(images), ~tempfile(pattern = "cropped", tmpdir = tempdir(), fileext = ".png"))
  }

  n <- length(images)
  if(length(border_colour) != n) border_colour <- rep(border_colour, n)
  if(length(border_size) != n) border_size <- rep(border_size, n)
  if(length(bg_fill) != n & !is.null(bg_fill)) bg_fill <- rep(bg_fill, n)
  if(length(just) != n) just <- rep(just, n)

  for(j in 1:n) {
    # crop image
    imgc <- cut_circle(images[j], just[j])

    # add border
    if(!is.null(border_size)) {
      imgc <- add_border(imgc, geom = "circle", border_size[j], border_colour[j], bg_fill[j])
    }

    # write and return path
    image_write(imgc, to[j])
  }

  to
}


#' @rdname crop
#' @export
square_crop <- function(images, to = NULL, border_size = NULL, border_colour = "black", bg_fill = NULL, just = "center") {

  if(is.null(to)) {
    to <- purrr::map_chr(1:length(images), ~tempfile(pattern = "cropped", tmpdir = tempdir(), fileext = ".png"))
  }

  n <- length(images)
  if(length(border_colour) != n) border_colour <- rep(border_colour, n)
  if(length(border_size) != n) border_size <- rep(border_size, n)
  if(length(bg_fill) != n & !is.null(bg_fill)) bg_fill <- rep(bg_fill, n)
  if(length(just) != n) just <- rep(just, n)

  for(j in 1:n) {
    # crop image
    imgc <- cut_square(images[j], just[j])

    # add border
    if(!is.null(border_size)) {
      imgc <- add_border(imgc, geom = "square", border_size[j], border_colour[j], bg_fill = bg_fill[j])
    }

    # write and return path
    image_write(imgc, to[j])
  }

  to

}

#' @rdname crop
#' @export
hex_crop <- function(images, to = NULL, border_size = NULL, border_colour = "black", bg_fill = NULL, just = "center") {

  if(is.null(to)) {
    to <- purrr::map_chr(1:length(images), ~tempfile(pattern = "cropped", tmpdir = tempdir(), fileext = ".png"))
  }

  n <- length(images)
  if(length(border_colour) != n) border_colour <- rep(border_colour, n)
  if(length(border_size) != n) border_size <- rep(border_size, n)
  if(length(bg_fill) != n & !is.null(bg_fill)) bg_fill <- rep(bg_fill, n)
  if(length(just) != n) just <- rep(just, n)

  for(j in 1:n) {
    # crop image
    imgc <- cut_hex(images[j], just[j])

    # add border
    if(!is.null(border_size)) {
      imgc <- add_border(imgc, geom = "hex", border_size[j], border_colour[j], bg_fill = bg_fill[j])
    }

    # write and return path
    image_write(imgc, to[j])
  }

  to

}

#' @rdname crop
#' @export
heart_crop <- function(images, to = NULL, border_size = NULL, border_colour = "black", bg_fill = NULL, just = "center") {

  if(is.null(to)) {
    to <- purrr::map_chr(1:length(images), ~tempfile(pattern = "cropped", tmpdir = tempdir(), fileext = ".png"))
  }

  n <- length(images)
  if(length(border_colour) != n) border_colour <- rep(border_colour, n)
  if(length(border_size) != n) border_size <- rep(border_size, n)
  if(length(bg_fill) != n & !is.null(bg_fill)) bg_fill <- rep(bg_fill, n)
  if(length(just) != n) just <- rep(just, n)

  for(j in 1:n) {
    # crop image
    imgc <- cut_heart(images[j], just[j])

    # add border
    if(!is.null(border_size)) {
      imgc <- add_border(imgc, geom = "heart", border_size[j], border_colour[j], bg_fill = bg_fill[j])
    }

    # write and return path
    image_write(imgc, to[j])
  }

  to
}

#' @rdname crop
#' @export
parallelogram_crop <- function(images, to = NULL, border_size = NULL, border_colour = "black", bg_fill = NULL, just = "center") {

  if(is.null(to)) {
    to <- purrr::map_chr(1:length(images), ~tempfile(pattern = "cropped", tmpdir = tempdir(), fileext = ".png"))
  }

  n <- length(images)
  if(length(border_colour) != n) border_colour <- rep(border_colour, n)
  if(length(border_size) != n) border_size <- rep(border_size, n)
  if(length(bg_fill) != n & !is.null(bg_fill)) bg_fill <- rep(bg_fill, n)
  if(length(just) != n) just <- rep(just, n)

  for(j in 1:n) {
    # crop image
    imgc <- cut_parallelogram(images[j], just[j])

    # add border
    if(!is.null(border_size)) {
      info <- image_read(images[j]) |>
        image_info()
      imgc <- add_border(imgc, geom = "parallelogram", border_size[j], border_colour[j], bg_fill = bg_fill[j], orig = list(wd = info$width, ht = info$height))
    }

    # write and return path
    image_write(imgc, to[j])
  }

  to

}



#' @rdname crop
#' @export
crop_circle <- circle_crop

#' @rdname crop
#' @export
crop_square <- square_crop

#' @rdname crop
#' @export
crop_hex <- hex_crop

#' @rdname crop
#' @export
crop_heart <- heart_crop

#' @rdname crop
#' @export
crop_parallelogram <- parallelogram_crop
