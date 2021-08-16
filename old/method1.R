library(ggplot2)
library(tidyverse)
library(here)

n_lines <- 10000
x_limits <- c(0, 18)
y_limits <- c(0, 24)

# (x - h)^2 + (y - k)^2 = r^2
circle_h = (x_limits[1] * .5 + x_limits[2] * .5)
circle_k = (y_limits[1] * .33 + y_limits[2] * .66)
circle_r = (x_limits[2] - x_limits[1]) * .75 * .5

lines <- data.frame(line = seq(1, n_lines),
                    point_1 = runif(n_lines, 0, 2*pi),
                    point_2 = runif(n_lines, 0, 2*pi))
lines <- lines %>%
  mutate(point_1_x = circle_h + circle_r * cos(point_1),
         point_1_y = circle_k + circle_r * sin(point_1),
         point_2_x = circle_h + circle_r * cos(point_2),
         point_2_y = circle_k + circle_r * sin(point_2)) %>%
  mutate(midpoint_x = (point_1_x + point_2_x) / 2,
         midpoint_y = (point_1_y + point_2_y) / 2)

# Y max is circle_k + circle_r  
# Y min is circle_k - circle_r  
# map to 230 to 250
map_bounds <- function(x, circle_k, circle_r){
  286.933 + -2.963 * x
}  

# lm(y ~ x, data = data.frame(x = c(9.09, 22.59),
#                             y = c(260, 220)))

lines$color = hcl(h = runif(n_lines,
                            map_bounds(lines$midpoint_y, circle_k = circle_k, circle_r) - 10,
                            map_bounds(lines$midpoint_y, circle_k = circle_k, circle_r) + 10),
                  c = 50,
                  l = 50)

lines$color = hcl(h = map_bounds(lines$midpoint_y, circle_k = circle_k, circle_r),
                  c = 50,
                  l = 50)

plot1 <- ggplot() +
  geom_segment(data = lines, aes(x = point_1_x,
                                 y = point_1_y,
                                 xend = point_2_x,
                                 yend = point_2_y),
                                 color = lines$color) + 
  geom_point(data = lines, aes(x = midpoint_x,
                               y = midpoint_y),
             color = lines$color) +
  xlim(c(0, 18)) +
  ylim(c(0, 24)) +
  theme_void()

ggsave(filename = file.path(here::here(), "method1.pdf"),
       plot = plot1,
       device = "pdf",
       width = 18,
       height = 24,
       units = "in",
       dpi = "print")

