x_imgs <- c("dev/images/moss-beast.jpg", "dev/images/baal.jpg", "dev/images/tolosh.jpg")
border_cols <- sample(colours(), 3)

x <- circle_crop(x_imgs, border_size = 16, border_colour = border_cols)
image_montage(reduce(map(x, image_read), c))

x <- hex_crop(x_imgs, border_size = 16, border_colour = border_cols)
image_montage(reduce(map(x, image_read), c))

x <- heart_crop(x_imgs, border_size = 16, border_colour = border_cols)
image_montage(reduce(map(x, image_read), c))
