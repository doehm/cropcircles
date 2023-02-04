
# Remove background

Note: This is under development and not part of the `cropcircles`
package. Yet.

The background from an image can be remove through an API call to
[remove.bg](https://www.remove.bg/).

# Steps

## 1. Create an account

Create an account at [remove.bg](https://www.remove.bg/). I’ve found
this site to be very simple, albeit pricey. However, the first 50 images
are free each month.

Log in, head to [here](https://www.remove.bg/tools-api/api-commandline)
and follow the FAQs to generate an API key.

## 2. Install Python and `reticulate`

This requires Python and `reticulate` to access the API.

Install `reticulate`.

``` r
install.packages("reticulate")
```

Install [python](https://www.python.org/downloads/). I won’t go into the
details here. Installing Python and configuring it is a personal journey
that one must experience alone.

Test that python has been installed correctly and can be accessed using
`reticulate` with the following code.

``` r
library(reticulate)

py_run_string("x = 2+2")

py$x
```

If `py$x` returns 4 it worked.

Just some basics on `reticulate`. R and Python can work together
seamlessly. Both languages can access the same data in the same
location. In other words, any object created in R e.g. `dat`, can be
accessed in the Python instanced by using `r.dat`. Any object created in
Python can be accessed in using `py$` e.g. `py$dat`. In the one workflow
you can switch between R and Python which is incredible!

For more head [here](https://rstudio.github.io/reticulate/)

## 3. Call the API

Save `removebg.py` to your local directory. Copy the code below, make
the appropriate changes and run the code. Once run the images should be
saved to the `to` location.

``` r
library(reticulate)

# only takes png or jpg
# can be a vector of images paths
# must be stored in 'images'
images <- "/path/to/the/images.png"

# the output path of the new image with background removed
# must be same length as 'images'
# must be stored in 'to'
to <- "/path/to/write/the/new/image.png"

# your api key from https://www.remove.bg/
api_key <- "your_api_key"

# run the python script
# this will call the removebg api for each image
py_run_file("path/to/removebg.py")
```

Enjoy backgroundless images!
