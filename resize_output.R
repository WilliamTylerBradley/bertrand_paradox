library(magick)

for(i in 1:3) {
  pic <- image_read(here::here(paste0("method_", i, ".png")))
  
  pic <- image_resize(pic, 750)
  pic <- image_convert(pic, format = "jpeg")
  
  image_write(pic, 
              path = here::here("output", paste0("method_", i, "_small.jpeg")), 
              format = "jpeg",
              compression = "JPEG")
}