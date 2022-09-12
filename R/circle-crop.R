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
#' library(magick)
#'
#' img_paths <- c(
#'   "https://openpsychometrics.org/tests/characters/test-resources/pics/BB/1.jpg",
#'   "https://openpsychometrics.org/tests/characters/test-resources/pics/BB/3.jpg",
#'   "https://openpsychometrics.org/tests/characters/test-resources/pics/BB/9.jpg",
#'   "https://openpsychometrics.org/tests/characters/test-resources/pics/BB/8.jpg")
#'
#'  img_paths_cropped <- circle_crop(img_paths)
#'
#'  imgs <- image_read(img_paths_cropped)
#'  image_montage(imgs)
#' }
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
