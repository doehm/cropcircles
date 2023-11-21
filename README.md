
<img src='https://cranlogs.r-pkg.org/badges/cropcircles'/><img src='https://cranlogs.r-pkg.org/badges/grand-total/cropcircles'/><img src='https://www.r-pkg.org/badges/version/cropcircles'/>

# cropcircles <img src='dev/images/hex.png' align="right" height="240" />

Crop an image into a circle with a transparent background.

The purpose of this package is to provide a simple and straight forward
way to circle crop an image, with a transparent background and plot it
with `ggimage`, `ggpath`, or include in tables using e.g. `gt`,
`reactable`, etc. There are a few ways to do this, but this package
intends to make it as simplified as possible.

## Release notes

Version 0.2.4 has changed the function naming convention to `crop_*`.
The old functions are still available and work the same way but will be
deprecated at some stage. You are encouraged to use `crop_*` from now
on.

## Installation

From CRAN

``` r
install.packages("cropcircles")
```

Or Git

``` r
devtools::install_github("doehm/cropcircles")
```

## Usage

The main function `crop_circle` takes a vector of image paths, either
local or URL links, crops the image and returns the path. The path of
the cropped images can be provided or if left blank it will save them to
a temp location which is cleared when the session ends.

A border can be added by specifying the size (in pixels) and colour.

``` r
library(cropcircles)
library(magick)
```

    ## Linking to ImageMagick 6.9.12.93
    ## Enabled features: cairo, freetype, fftw, ghostscript, heic, lcms, pango, raw, rsvg, webp
    ## Disabled features: fontconfig, x11

``` r
img_path <- file.path(system.file(package = "cropcircles"), "images", "walter-jesse.png")

# saves to a temporary path
img_cropped <- crop_circle(img_path, border_size = 4)

# plot image with magic
# can be used with ggimage or ggpath
image_read(img_cropped)
```

<img src="README_files/figure-gfm/unnamed-chunk-4-1.png" width="232" />

``` r
# other geometries

image_read(crop_hex(img_path, border_size = 4))
```

<img src="README_files/figure-gfm/unnamed-chunk-4-2.png" width="201" />

``` r
image_read(crop_heart(img_path, border_size = 4))
```

<img src="README_files/figure-gfm/unnamed-chunk-4-3.png" width="232" />

<!-- <img src='dev/images/bb.png' align="center"/> -->

The function can take an image with any dimensions. It will circle crop
the image from the center with a diameter of the smallest dimension.

## Justify

With rectangular images the subject for focus may not be centered. The
`crop_*` functions include a `just` argument which can take values
`left`, `right`, `top` and `bottom`. It simply shifts the initial
cropping window to the desired side.

``` r
library(magick)

# justification example
img_path <- file.path(system.file(package = "cropcircles"), "images", "walter-jesse.png")
orig <- image_read(img_path)

# center (default)
center <- image_read(crop_circle(img_path, border_size = 4))

# left
left <- image_read(crop_circle(img_path, border_size = 4, just = "left"))

# right
right <- image_read(crop_circle(img_path, border_size = 4, just = "right"))

image_montage(c(orig, center, left, right))
```

<img src="README_files/figure-gfm/unnamed-chunk-5-1.png" width="768" />
