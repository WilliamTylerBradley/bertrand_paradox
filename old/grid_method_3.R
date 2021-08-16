##------------------
# Libraries and seed
##------------------

set.seed(3)

library(grid)
library(tidyverse)
library(here)

#set.seed(3)

##-----------------------
# Transformation Function
##-----------------------

## Parameters
# circle_x is x value of center
# circle_y is y value of center
# circle_radius is radius
# point_x is the x value of the random point
# point_y is the y value of the random point

## Returns
# dataframe with
# point_1_x, point_1_y, point_2_x, point_2_y, midpoint_x, midpoint_y
method_3 <- function(circle_x, circle_y, circle_radius, point_x, point_y) {
  # move out to radius from origin
  point_x = circle_radius * point_x
  point_y = circle_radius * point_y
  
  # rotate so just dealing with y
  flatten_by = atan2(point_y, point_x)
  
  # now rotate these points around to the radius
  x_rotated = cos(-flatten_by) * point_x - sin(-flatten_by) * point_y
  y_rotated = sin(-flatten_by) * point_x + cos(-flatten_by) * point_y # Should be zero
  
  # find y values 
  # x^2 + y^2 = r^2
  # y^2 = r^2 - x^2
  # y = +/- sqrt(r^2 - x^2)
  y1 = sqrt(circle_radius^2 - x_rotated^2)
  y2 = -sqrt(circle_radius^2 - x_rotated^2)
  
  # rotate back
  point_1_x = cos(flatten_by) * x_rotated - sin(flatten_by) * y1
  point_1_y = sin(flatten_by) * x_rotated + cos(flatten_by) * y1
  point_2_x = cos(flatten_by) * x_rotated - sin(flatten_by) * y2
  point_2_y = sin(flatten_by) * x_rotated + cos(flatten_by) * y2

  # move back to where the circle is
  point_1_x = point_1_x + circle_x
  point_1_y = point_1_y + circle_y
  point_2_x = point_2_x + circle_x
  point_2_y = point_2_y + circle_y
  
  return(data.frame(point_1_x = point_1_x,
                    point_1_y = point_1_y,
                    point_2_x = point_2_x, 
                    point_2_y = point_2_y,
                    midpoint_x = (point_1_x + point_2_x) / 2,
                    midpoint_y = (point_1_y + point_2_y) / 2))
}

##-----------
# Image setup
##-----------

# Circle center based on 18 x 24 image
x_limits <- c(0, 18)
y_limits <- c(0, 24)

# Number of lines/points
n_lines <- 5000

## Variables are based on this formula:
# (x - h)^2 + (y - k)^2 = r^2

circle_h = (x_limits[1] * .5 + x_limits[2] * .5) # halfway on x
circle_r = (x_limits[2] - x_limits[1]) * .75 * .5 # diameter is 3/4 of x
# sets circle with even margins from the top
circle_k = y_limits[2] - circle_r - ((x_limits[2] - x_limits[1]) * .25 * .5)

##----------------
# Get lines/points
##----------------

## Get random points:
# https://stackoverflow.com/questions/5837572/
#     generate-a-random-point-within-a-circle-uniformly/50746409#50746409
# r = R * sqrt(random())
# theta = random() * 2 * PI
# x = centerX + r * cos(theta)
# y = centerY + r * sin(theta)
lines <- data.frame(line = seq(1, n_lines),
                    r = sqrt(runif(n_lines, 0, 1)),
                    theta = runif(n_lines, 0, 2*pi))
lines <- lines %>%
  mutate(point_x = r * cos(theta),
         point_y = r * sin(theta)) %>%
  select(line, point_x, point_y)

# Apply method_3 to the points
lines <- lines %>%
  bind_cols(pmap_dfr(list(circle_x = circle_h,
                          circle_y = circle_k,
                          circle_radius = circle_r, 
                          point_x = .$point_x,
                          point_y = .$point_y), method_3))

##----------
# Set colors
##----------

# Color center
hue_center <- 15

# Random colors
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

##---------------------
# Create and save image
##---------------------

pdf(file = file.path(here::here(), "method3.pdf"),
    width = 18,
    height = 24,
    useDingbats = FALSE) # Or else points will be digbats and can't convert to PNG

## Create main circle
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

## Add process instructions

## box locations
# center of center
box_center_y <- (circle_k - circle_r) / 2

# box sizes
box_width <- (circle_k - circle_r) / 4

# space between boxes
box_space <- box_width / 2

## center of other boxes
box_center_x <- (c(-1, 0, 1) * (box_width + box_space)) + circle_h

## Add boxes

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
            gp = gpar(col = hcl(h = 245, c = 50, l = 50)),
            default.units = "in")

n_opts <- 9

# Square in a circle
top_left_corner_x <- ((box_width * .75 * .5 * .75) * cos(3/4*pi)
                        + box_center_x[1])
top_left_corner_y <- ((box_width * .75 * .5 * .75) * sin(3/4*pi)
                      + box_center_y)
movement <- (box_width * .75 * .5 * .75) / sqrt(2)

opts <- data.frame(opt = seq(1, n_opts),
                   x = top_left_corner_x + 
                     (movement * rep(seq(0, 2), times = 3)),
                   y = top_left_corner_y + 
                     (-movement * rep(seq(0, 2), each = 3)))

opts <- opts %>% 
  mutate(col = ifelse(opt == sample(opt, 1), 
                       hcl(h = 245, c = 50, l = 50), 
                       hcl(h = 245, c = 0, l = 50 + 40)))

grid.points(x = opts$x,
            y = opts$y,
            default.units = "in",
            pch = 16,
            gp = gpar(col = opts$col))


## box 2 ----
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
            gp = gpar(col = hcl(h = 245, c = 50, l = 50)),
            default.units = "in")

opts <- opts %>%
  filter(col == hcl(h = 245, c = 50, l = 50)) %>%
  mutate(x = (x - box_center_x[1]) + box_center_x[2])

opts <- opts %>%
  bind_cols(pmap_dfr(list(circle_x = box_center_x[2],
                          circle_y = box_center_y,
                          circle_radius = (box_width * .75 * .5), 
                          point_x = (.$x - box_center_x[2]) / (box_width * .75 * .5),
                          point_y = (.$y - box_center_y) / (box_width * .75 * .5))
                     , method_3))

grid.lines(x = c(box_center_x[2], opts$x),
           y = c(box_center_y, opts$y),
           gp = gpar(col = hcl(h = hue_center, c = 50, l = 50 + 40)),
           default.units = "in")

grid.lines(x = c(opts$x, opts$point_1_x),
           y = c(opts$y, opts$point_1_y),
           gp = gpar(col = opts$col),
           default.units = "in")

## box 3 ----
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
            gp = gpar(col = hcl(h = 245, c = 50, l = 50)),
            default.units = "in")

opts <- opts %>%
  filter(col == hcl(h = 245, c = 50, l = 50)) %>%
  mutate(x = (x - box_center_x[2]) + box_center_x[3],
         point_1_x = (point_1_x - box_center_x[2]) + box_center_x[3],
         point_2_x = (point_2_x - box_center_x[2]) + box_center_x[3])

grid.lines(x = c(opts$point_1_x, opts$point_2_x),
           y = c(opts$point_1_y, opts$point_2_y),
           gp = gpar(col = opts$col,
                     lwd = 5),
           default.units = "in")

dev.off()

# lines <- lines %>%
#   mutate(x1 = (midpoint_x - circle_h) / circle_r,
#          y1 = (midpoint_y - circle_k) / circle_r)



# library(pdftools)
# library(png)
# 
# bitmap <- pdf_render_page(file.path(here::here(), "method3.pdf"),
#                           dpi = 300)
# 
# png::writePNG(bitmap, file.path(here::here(), "method3.png"))

