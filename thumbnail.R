library(magick)

thumbnail <- image_read(here::here("method_1.png"))

width_px <- image_info(thumbnail)$width
thumbnail <- image_crop(thumbnail, paste0(width_px * .8, "x", 
                                          width_px * .8, "+", 
                                          width_px * .1, "+",
                                          width_px * .1))

thumbnail <- image_resize(thumbnail, 250)
thumbnail <- image_convert(thumbnail, format = "jpeg")

image_write(thumbnail, 
            path = here::here("output", "thumbnail.jpeg"), 
            format = "jpeg",
            compression = "JPEG")