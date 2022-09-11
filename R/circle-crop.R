#' Circle crop
#'
#' This function reads in an image and circle crops it with a transparent
#' background. If a new path is given it will save the cropped images to
#' the new location. If no path is given it will save to a temporary location
#' which will be cleared when the session is closed
#'
#' @param images Vector of image paths, either local or urls. If urls the images
#' will be downloaded first.
#' @param to Path to new location
#'
#' @importFrom magick image_read image_data image_write image_crop
#'
#' @return Path to cropped images
#' @export
#'
#' @examples \dontrun{
#'
#' images <- c(
#'   'https://openpsychometrics.org/tests/characters/test-resources/pics/BB/1.jpg',
#'   'https://openpsychometrics.org/tests/characters/test-resources/pics/BB/3.jpg',
#'   'https://openpsychometrics.org/tests/characters/test-resources/pics/BB/9.jpg'
#'   )
#'
#'  img_cropped <- circle_crop(images)
#'
#'  magick::image_read(img_cropped[1])
#'
#' }
circle_crop <- function(images, to = NULL) {

  # looks like image_read handsles the downloads so don't need this
  # if(any(is_url(images))) {
  #   id <- which(is_url(images))
  #   images_urls <- images[id]
  #   images[id] <- download_images(images_urls)
  # }

  if(is.null(to)) {
    to <- purrr::map_chr(1:length(images), ~tempfile(pattern = "cropped", tmpdir = tempdir(), fileext = ".png"))
  }

  purrr::map2_chr(images, to, function(images, to) {

    img <- image_read(images)
    dat <- image_data(img, "rgba")
    dims <- dim(dat)

    center <- floor(dims[2:3]/2)
    r <- floor(min(dims[2:3])/2)

    # crop to square
    start_point <- round(center-r)
    depth <- 2*r
    geom <- glue::glue("{depth}x{depth}+{start_point[1]}+{start_point[2]}")

    img <- image_crop(img, geom)
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

    image_write(image_read(dat), to)

    to

  })

}



#' Download images
#'
#' If the images are a URL they will be downloaded and saved in a temporary location.
#' The images are cleared when the session ends
#'
#' @param images A vector of URLs to image location
#'
#' @importFrom utils download.file
#'
#' @return The paths to the downloaded images
#' @export
#'
#' @examples \dontrun{
#' images <- c(
#'   'https://openpsychometrics.org/tests/characters/test-resources/pics/BB/1.jpg',
#'   'https://openpsychometrics.org/tests/characters/test-resources/pics/BB/3.jpg',
#'   'https://openpsychometrics.org/tests/characters/test-resources/pics/BB/9.jpg'
#'   )
#'
#' download_images(images)
#' }
download_images <- function(images) {
  dest <- purrr::map_chr(1:length(images), ~tempfile(pattern = "cropped", tmpdir = tempdir(), fileext = ".png"))
  download.file(images, dest, mode = "wb", quiet = TRUE)
  dest
}


#' Is URL
#'
#' Checks if the given address is a URL or not. Returns a logical vector.
#'
#' @param path Path to image
#'
#' @return Logical vector. `TRUE` if path is a url. `FALSE` otherwise
#' @export
#'
#' @examples
#'  is_url('https://openpsychometrics.org/tests/characters/test-resources/pics/BB/1.jpg')
#'
#'  is_url("C:/path_to_not_url/image.png")
is_url <- function(path) {
  base::substr(path, 1, 4) == "http"
}
