#' Circle crop
#'
#' This functions reads in an image  and circle crops it with a transparent
#' background. If a new path is given it will save the cropped images to
#' the new location. If no path is given it will save to a temporary location
#' which will be cleared when the session is closed
#'
#' @param images Vector of image paths, either local or urls. If urls the images
#' will be downloaded first.
#' @param to Path to new location
#'
#' @importFrom magick image_read image_data image_write
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

  if(any(is_url(images))) {
    images_urls <- images[is_url(images)]
    image_paths <- download_images(images_urls)
    images <- c(images[!is_url(images)], image_paths)
  }

  if(is.null(to)) {
    to <- purrr::map_chr(1:length(images), ~tempfile(pattern = "cropped", tmpdir = tempdir(), fileext = ".png"))
  }

  purrr::map2_chr(images, to, function(images, to) {

    # read in the image and do the stuff
    img <- image_read(images)
    dat <- image_data(img, "rgba")
    dims <- dim(dat)

    center <- dims[2:3]/2
    r <- min(dims[2:3])/2
    dx <- (center[1]-r)
    dy <- (center[2]-r)
    x_vals <- 1:(2*r)
    y_vals <- x_vals

    for(x in x_vals) {
      y <- round(sqrt(r^2 - (x - r)^2) + r)
      inside <- (2*r-y):y
      outside <- y_vals[-inside]
      dat[4, x+dx, outside+dy] <- as.raw(00)
    }

    if(dx > 0) {
      x_pos <- ceiling(c(1:dx, (dims[2]-dx):dims[2]))
      dat[4, x_pos, ] <- as.raw(00)
    }
    if(dy > 0) {
      y_pos <- ceiling(c(1:dy, (dims[3]-dy):dims[3]))
      dat[4, ,y_pos] <- as.raw(00)
    }

    # browser()

    image_read(dat) |>
      image_write(to)

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
#' download_images()
#' }
download_images <- function(images) {
  dest <- purrr::map_chr(1:length(images), ~tempfile(pattern = "cropped", tmpdir = tempdir(), fileext = ".png"))
  # purrr::map2_chr(images, dest, ~download.file(.x, .y, mode = "wb"))
  download.file(images, dest, mode = "wb")
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



