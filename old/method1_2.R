library(ggplot2)
library(tidyverse)
library(here)

# set.seed(1)

n_lines <- 5000
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
         midpoint_y = (point_1_y + point_2_y) / 2) %>%
  mutate(mu = (midpoint_y - (circle_k - circle_r)) / (2 * circle_r),
         var = .005) %>%
  mutate(mu = ifelse(mu < .01, .01, ifelse(mu > .99, .99, mu)), # keeps a, b > 0
         alpha = ((1 - mu) / var - 1 / mu) * mu ^ 2,
         beta = alpha * (1 / mu - 1)) %>%
  mutate(h = runif(n(), 240, 260),
         c = rbeta(n(), shape1 = alpha, shape2 = beta) * 33 + 50,
         l = rbeta(n(), shape1 = alpha, shape2 = beta) * 33 + 50,
         line_color = hcl(h, c, l),
         point_color = hcl(h - 40))

plot1 <- ggplot() + 
  geom_blank() +
  xlim(c(0, 18)) +
  ylim(c(0, 24)) +
  theme_void()

for(i in seq(1, 5)){
  plot1 <- plot1 +
    geom_segment(data = lines[i, ], aes(x = point_1_x,
                                        y = point_1_y,
                                        xend = point_2_x,
                                        yend = point_2_y),
                 color = lines$line_color[i]) + 
    geom_point(data = lines[i, ], aes(x = midpoint_x,
                                      y = midpoint_y),
               color = lines$point_color[i])
  print(i)
}

ggsave(filename = file.path(here::here(), "method1.pdf"),
       plot = plot1,
       device = "pdf",
       width = 18,
       height = 24,
       units = "in",
       dpi = "print")

