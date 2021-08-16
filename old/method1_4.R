library(ggplot2)
library(tidyverse)
library(here)

# set.seed(1)

n_lines <- 50
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

GeomSimplePoint <- ggproto("GeomSimplePoint", Geom,
                           required_aes = c("x", "y"),
                           default_aes = aes(shape = 19, colour = "black"),
                           draw_key = draw_key_point,
                           
                           draw_panel = function(data, panel_params, coord) {
                             coords <- coord$transform(data, panel_params)
                             
                             str(coords)
                             
                             grid::gTree(children = grid::gList(grid::pointsGrob(
                               (coords$x + coords$xend)/2, (coords$y + coords$yend)/2,
                               pch = coords$shape,
                               gp = grid::gpar(col = coords$colour)
                             ),
                             grid::segmentsGrob(
                               coords$x, coords$y, coords$xend, coords$yend,
                               gp = grid::gpar(col = coords$colour)
                             )))
                           }
)


geom_simple_point <- function(mapping = NULL, data = NULL, stat = "identity",
                              position = "identity", na.rm = FALSE, show.legend = NA, 
                              inherit.aes = TRUE, ...) {
  layer(
    geom = GeomSimplePoint, mapping = mapping,  data = data, stat = stat, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

ggplot(data = lines, aes(x = point_1_x,
                         y = point_1_y,
                         xend = point_2_x,
                         yend = point_2_y,
                         xmid = midpoint_x,
                         ymid = midpoint_y)) +
  geom_simple_point(color = lines$line_color) + 
  geom_blank() +
  xlim(c(0, 18)) +
  ylim(c(0, 24)) +
  theme_void()

pg <- ggplot_build(p)
pg$data
