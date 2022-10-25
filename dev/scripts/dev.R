

# make hexsticker ---------------------------------------------------------

library(showtext)
font_add_google("Antonio", "ant")
showtext_auto()

image_read("dev/images/crop-circle-2.jpg") |>
  image_modulate(brightness = 40) |>
  image_write("dev/images/crop-circle-3.jpg")

sticker(
  "dev/images/crop-circle-3.jpg",
  package = "cropcircles",
  p_size=20,
  s_x=1,
  s_y=1,
  s_width=2,
  s_height=2,
  p_y = 0.55,
  p_color = "#d6ccc2",
  p_fontface = "bold",
  p_family = "ant",
  h_color = "#d6ccc2",
  h_size = 2,
  filename="dev/images/hex-staging.png")

# test function -----------------------------------------------------------

images <- c(
  # 'https://openpsychometrics.org/tests/characters/test-resources/pics/BB/9.jpg',
  "dev/images/moss-beast.jpg",
  "dev/images/tolosh.jpg",
  "dev/images/baal.jpg",
  "dev/images/HarryBigfoot.webp"
)

a <- Sys.time()
x <- circle_crop(images)
b <- Sys.time()
b - a

image_read(x) |>
  magick::image_montage()


# square crop -------------------------------------------------------------

a <- Sys.time()
x <- square_crop(images)
b <- Sys.time()
b - a

image_read(x) |>
  magick::image_montage()


# hex crop ----------------------------------------------------------------

a <- Sys.time()
x <- hex_crop(images[1])
b <- Sys.time()
b - a

image_read(x) |>
  magick::image_montage()


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

x_vals <- 1:dims[2]
y_vals <- 1:dims[3]

for(x in x_vals) {
  d <- sqrt((x - center[1])^2 + (y_vals - center[2])^2)
  outside <- which(d > r)
  dat[4, x, outside] <- as.raw(00)
}



x <- rep(1:dims[2], dims[3])
y <- rep(1:dims[3], each = dims[2])
d <- sqrt((x - center[1])^2 + (y - center[2])^2)
outside <- which(d > r)
dat[4,,][outside] <- as.raw(00)
