
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


# purrr::map2_chr(images, to, function(images, to) {

to <- "zibella-cropped.png"
images <- "../tidyTuesday/zibella.jpg"

# read in the image and do the stuff
img <- image_read(images)
dat <- image_data(img, "rgba")
dims <- dim(dat)

center <- floor(dims[2:3]/2)
r <- floor(min(dims[2:3])/2)

# crop to square
start_point <- round(center-r)
depth <- 2*r
geom <- glue::glue("{depth}x{depth}+{start_point[1]}+{start_point[2]}")

img <- magick::image_crop(img, geom)
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

# })

library(ggpath)
ggplot() +
  geom_from_path(aes(0, 0, path = zib))

ancestral_trail <- c(
  "dev/images/moss-beast.jpg",
  "dev/images/tolosh.jpg",
  "dev/images/baal.jpg"
)

x <- circle_crop(ancestral_trail)
image_read(x)


x <- circle_crop("https://github.com/doehm/cropcircles/blob/main/dev/images/moss-beast.jpg")
