
# cropcircles <img src='dev/images/hex.png' align="right" height="240" />

Crop an image into a circle with a transparent background.

The purpose of this package is to provide a simple and straight forward
way to circle crop an image, with a transparent background and plot it
with `ggimage`, `ggpath`, or include in tables using e.g. `gt`,
`reactable`, etc. There are a few ways to do this, but this package
intends to make it as simplified as possible.

## Installation

``` r
devtools::install_github("doehm/cropcircles")
```

## Usage

The main function `circle_crop` takes a vector of image paths, either
local or URL links, crops the image and returns the path. The path of
the cropped images can be provided or if left blank it will save them to
a temp location which is cleared when the session ends.

``` r
library(cropcircles)
library(dplyr)
library(ggimage)

# breaking bad images
images <- c(
  "https://openpsychometrics.org/tests/characters/test-resources/pics/BB/1.jpg",
  "https://openpsychometrics.org/tests/characters/test-resources/pics/BB/3.jpg", 
  "https://openpsychometrics.org/tests/characters/test-resources/pics/BB/9.jpg", 
  "https://openpsychometrics.org/tests/characters/test-resources/pics/BB/8.jpg")

df <- tibble(
  y = 1:4,
  images = images
  ) |> 
  mutate(images_cropped = circle_crop(images))

ggplot() +
  geom_image(aes(1.5, y, image = images), df, size = 0.15) +
  geom_image(aes(3.5, y, image = images_cropped), df, size = 0.15) +
  xlim(0, 5) +
  ylim(0, 5) +
  coord_fixed()
```

<img src='dev/images/bb.png' align="center"/>

The function can take an image with any dimensions. It will circle crop
the image from the center with a diameter of the smallest dimension.
