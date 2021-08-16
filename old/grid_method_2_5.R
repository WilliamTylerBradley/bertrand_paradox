library(grid)
library(tidyverse)
library(here)
#  http://sape.inf.usi.ch/quick-reference/ggplot2/colour

##----

set.seed(2)

## Parameters
# circle_x is x value of center
# circle_y is y value of center
# circle_radius is radius
# radius is radian
# chord is fraction along the radius 
## Returns
# dataframe with
# point_1_x, point_1_y, point_2_x, point_2_y, midpoint_x, midpoint_y
line_points <- function(circle_x, circle_y, circle_radius, radius, chord) {
  # move out to radius from origin
  x_1 = circle_radius * cos(radius)
  y_1 = circle_radius * sin(radius)
  x_2 = circle_radius * cos(radius)
  y_2 = circle_radius * sin(radius)
  
  # https://www.mathsisfun.com/algebra/trig-solving-ssa-triangles.html
  # we know one angle is 90*
  # one side is circle_radius
  # one side is chord
  new_angle = pi - pi/2 - asin(((chord * circle_radius) * sin(pi/2)) / circle_radius)
  
  # now rotate these points around to the radius
  x_1_moved = cos(new_angle) * x_1 - sin(new_angle) * y_1
  y_1_moved = sin(new_angle) * x_1 + cos(new_angle) * y_1
  x_2_moved = cos(-new_angle) * x_2 - sin(-new_angle) * y_2
  y_2_moved = sin(-new_angle) * x_2 + cos(-new_angle) * y_2
  
  # move back to where the circle is
  x_1_moved = x_1_moved + circle_x
  y_1_moved = y_1_moved + circle_y
  x_2_moved = x_2_moved + circle_x
  y_2_moved = y_2_moved + circle_y
  
  return(data.frame(point_1_x = x_1_moved,
                    point_1_y = y_1_moved,
                    point_2_x = x_2_moved, 
                    point_2_y = y_2_moved,
                    midpoint_x = (x_1_moved + x_2_moved) / 2,
                    midpoint_y = (y_1_moved + y_2_moved) / 2))
}

hue_center <- 65

n_lines <- 5000
x_limits <- c(0, 18)
y_limits <- c(0, 24)

# (x - h)^2 + (y - k)^2 = r^2
circle_h = (x_limits[1] * .5 + x_limits[2] * .5) # halfway on x
# circle_k = (y_limits[1] * .33 + y_limits[2] * .66) # two-thirds up on y
circle_r = (x_limits[2] - x_limits[1]) * .75 * .5 # diameter is 3/4 of x
circle_k = y_limits[2] - circle_r - ((x_limits[2] - x_limits[1]) * .25 * .5)
# sets circle with even margins

lines <- data.frame(line = seq(1, n_lines),
                    radius = runif(n_lines, 0, 2*pi),
                    chord = runif(n_lines, 0, 1))
lines <- lines %>%
  bind_cols(pmap_dfr(list(circle_x = circle_h,
                          circle_y = circle_k,
                          circle_radius = circle_r, 
                          radius = .$radius,
                          chord = .$chord), line_points))

lines <- lines %>% 
  mutate(mu = line/n_lines,
         var = .005) %>%
  mutate(mu = ifelse(mu < .01, .01, ifelse(mu > .99, .99, mu)), # keeps a, b > 0
         alpha = ((1 - mu) / var - 1 / mu) * mu ^ 2,
         beta = alpha * (1 / mu - 1)) %>%
  mutate(h = runif(n(), hue_center - 5, hue_center + 5),
         c = rbeta(n(), shape1 = alpha, shape2 = beta) * 20 + 30,
         l = rbeta(n(), shape1 = alpha, shape2 = beta) * 20 + 30,
         line_color = hcl(h, c, l),
         point_color = hcl(h, c + 40, l + 40))

pdf(file = file.path(here::here(), "method2.pdf"),
    width = 18,
    height = 24,
    useDingbats = FALSE) # Or else points will be digbats and can't convert to PNG

grid.circle(x = circle_h,
            y = circle_k,
            r = circle_r,
            default.units = "in",
            gp = gpar(col = hcl(h = hue_center, c = 50, l = 50),
                      lwd=10))

for(i in 1:nrow(lines)) {
  grid.lines(x = c(lines$point_1_x[i], lines$point_2_x[i]),
             y = c(lines$point_1_y[i], lines$point_2_y[i]),
             gp = gpar(col = lines$line_color[i],
                       alpha = 1 - (i/n_lines)),
             default.units = "in")
  grid.points(x = lines$midpoint_x[i],
              y = lines$midpoint_y[i],
              pch = 16,
              gp = gpar(col = lines$point_color[i],
                        alpha = .95 - (i/n_lines)*.9),
              default.units = "in")
}

## Add process

## box location
# center of center
box_center_y <- (circle_k - circle_r) / 2

# box sizes
box_width <- (circle_k - circle_r) / 4

# space between boxes
box_space <- box_width / 2

## center of other boxes
box_center_x <- (c(-1, 0, 1) * (box_width + box_space)) + circle_h

## Add boxes

##----

# box 1
grid.rect(x = box_center_x[1],
          y = box_center_y,
          width = box_width,
          height = box_width,
          default.units = "in",
          just = "center",
          gp = gpar(lwd=5))

grid.circle(x = box_center_x[1],
            y = box_center_y,
            r = box_width * .75 * .5,
            gp = gpar(col = hcl(h = hue_center, c = 50, l = 50)),
            default.units = "in")

n_opts <- 9
opts <- data.frame(opt = seq(1, n_opts),
                   radius = seq(0, n_opts-1) * 
                     (2*pi / n_opts))
opts <- opts %>%
  mutate(
    x1 = box_center_x[1],
    y1 = box_center_y,
    x2 = box_center_x[1] + 
      (box_width * .75 * .5) * 
      cos(radius),
    y2 = box_center_y + 
      (box_width * .75 * .5) * 
      sin(radius )) 

opts <- opts %>% 
  mutate(col1 = ifelse(opt == sample(opt, 1), 
                       hcl(h = hue_center, c = 50, l = 50), 
                       hcl(h = hue_center, c = 0, l = 50 + 40)))

for(i in seq(1, nrow(opts))) {
  grid.lines(x = c(opts$x1[i], opts$x2[i]),
             y = c(opts$y1[i], opts$y2[i]),
             default.units = "in",
             gp = gpar(col = opts$col1[i]))
}

##----

# box 2
grid.rect(x = box_center_x[2],
          y = box_center_y,
          width = box_width,
          height = box_width,
          default.units = "in",
          just = "center",
          gp = gpar(lwd=5))

grid.circle(x = box_center_x[2],
            y = box_center_y,
            r = box_width * .75 * .5,
            gp = gpar(col = hcl(h = hue_center, c = 50, l = 50)),
            default.units = "in")

opts <- opts %>%
  filter(col1 == hcl(h = hue_center, c = 50, l = 50)) %>%
  mutate(x1 = box_center_x[2],
         x2 = box_center_x[2] + 
           (box_width * .75 * .5) * 
           cos(radius))

opts_2 <- data.frame(opt = seq(1, n_opts),
                     radius = opts$radius,
                     chord = seq(0, n_opts-1) * (1/n_opts))
opts_2 <- opts_2 %>%
  bind_cols(pmap_dfr(list(circle_x = box_center_x[2],
                          circle_y = box_center_y,
                          circle_radius = (box_width * .75 * .5), 
                          radius = .$radius,
                          chord = .$chord), line_points))

opts_2 <- opts_2 %>% 
  mutate(col1 = ifelse(opt == sample(opt, 1), 
                       hcl(h = hue_center, c = 50, l = 50), 
                       hcl(h = hue_center, c = 0, l = 50 + 40)))

grid.lines(x = c(opts$x1, opts$x2),
           y = c(opts$y1, opts$y2),
           default.units = "in",
           gp = gpar(col = opts$col1))

for(i in seq(1, nrow(opts_2))) {
  grid.lines(x = c(opts_2$point_1_x[i], opts_2$point_2_x[i]),
             y = c(opts_2$point_1_y[i], opts_2$point_2_y[i]),
             default.units = "in",
             gp = gpar(col = opts_2$col1[i]))
}

# box 3
grid.rect(x = box_center_x[3],
          y = box_center_y,
          width = box_width,
          height = box_width,
          default.units = "in",
          just = "center",
          gp = gpar(lwd=5))

grid.circle(x = box_center_x[3],
            y = box_center_y,
            r = box_width * .75 * .5,
            gp = gpar(col = hcl(h = hue_center, c = 50, l = 50)),
            default.units = "in")

opts_2 <- opts_2 %>%
  filter(col1 == hcl(h = hue_center, c = 50, l = 50)) %>%
  select(c(opt, radius, chord, col1)) %>%
  bind_cols(pmap_dfr(list(circle_x = box_center_x[3],
                          circle_y = box_center_y,
                          circle_radius = (box_width * .75 * .5), 
                          radius = .$radius,
                          chord = .$chord), line_points))

grid.lines(x = c(opts_2$point_1_x, opts_2$point_2_x),
           y = c(opts_2$point_1_y, opts_2$point_2_y),
           default.units = "in",
           gp = gpar(col = opts_2$col1,
                     lwd = 5))

dev.off()

# library(pdftools)
# library(png)
# 
# bitmap <- pdf_render_page(file.path(here::here(), "method2.pdf"),
#                           dpi = 300)
# 
# png::writePNG(bitmap, file.path(here::here(), "method2.png"))

