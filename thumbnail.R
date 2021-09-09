library(magick)

thumbnail <- image_read(here::here("method_1.png"))

width_px <- 2592
thumbnail <- image_crop(thumbnail, paste0(width_px * .8, "x", 
                                          width_px * .8, "+", 
                                          width_px * .1, "+",
                                          width_px * .1))

image_write(thumbnail, path = here::here("output", "thumbnail.png"))
