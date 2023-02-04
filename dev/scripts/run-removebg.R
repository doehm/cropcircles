
# remove bg ---------------------------------------------------------------

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
py_run_file("dev/scripts/removebg.py")
