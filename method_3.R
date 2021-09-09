##------------------
# Libraries and Seed
##------------------
library(grid)
library(tidyverse)
library(colorspace)
library(here)
library(ragg)

set.seed(3)

##------------------
# Variables
##------------------
n_lines <- 4000
x_limits <- c(0, 18)
y_limits <- c(0, 24)
line_color <- as(hex2RGB("#579A89"), "polarLUV") # Dark green - blue
point_color <- as(hex2RGB("#94F3E7"), "polarLUV") # Bright blue - green

## (x - h)^2 + (y - k)^2 = r^2
# Set circle halfway on x
circle_h = (x_limits[1] * .5 + x_limits[2] * .5)
# Set diameter of 3/4 of x
circle_r = (x_limits[2] - x_limits[1]) * .75 * .5 
# Set circle with even margins on x and top of y
circle_k = y_limits[2] - circle_r - ((x_limits[2] - x_limits[1]) * .25 * .5)

##-----------------------
# Functions
##-----------------------
### Transformation
## Parameters
# circle_x is x value of center
# circle_y is y value of center
# circle_radius is radius
# point_x is the x value of the random point
# point_y is the y value of the random point
## Returns
# dataframe with
# point_1_x, point_1_y, point_2_x, point_2_y, midpoint_x, midpoint_y
line_points <- function(circle_x, circle_y, circle_radius, point_x, point_y) {
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

### Get colors
## Parameters
# n_points is the number of colors
# oversample is the factor to oversample in case of dropping colors
# H_point is the base H value
# C_point is the base C value
# L_point is the base L value
# theta_radius is the radius for direction for theta, on the C-L plane
# perpendicular_C_L_radius is the radius perpendicular to other ones, roughly goes with H
# tilt_theta is the angle to tilt on C-L plane
# other_C_L_radius is the radius for other C-L vector
# H_bound is the limit on H
get_color_points <- function(n_points, oversample,
                             H_point, C_point, L_point,
                             theta_radius, other_C_L_radius, 
                             perpendicular_C_L_radius,
                             tilt_theta, H_bound) {
  data.frame(x = rnorm(n = n_points * oversample), # over sample in case some points fail
             y = rnorm(n = n_points * oversample),
             z = rnorm(n = n_points * oversample),
             U = runif(n = n_points * oversample)^(1/3)) %>%
    mutate(normalize = sqrt(x^2 + y^2 + z^2)) %>%
    mutate(x = x * U / normalize,
           y = y * U / normalize,
           z = z * U / normalize) %>%
    select(-U, -normalize) %>% # have random points in a sphere here
    mutate(x = x * theta_radius, # stretch
           y = y * other_C_L_radius,
           z = z * perpendicular_C_L_radius) %>%
    mutate(z_tilt = z * cos(tilt_theta) + x * sin(tilt_theta), # tilt
           x_tilt = z * -sin(tilt_theta) + x * cos(tilt_theta)) %>%
    mutate(x = x_tilt,
           z = z_tilt) %>%
    select(-x_tilt, -z_tilt) %>%
    mutate(x_turn = x * cos(H_point * pi/180) - y * sin(H_point * pi/180), # rotate
           y_turn = x * sin(H_point * pi/180) + y * cos(H_point * pi/180)) %>%
    mutate(x = x_turn,
           y = y_turn) %>%
    select(-x_turn, -y_turn) %>%
    mutate(x = x + C_point * cos(H_point * pi/180), # move
           y = y + C_point * sin(H_point * pi/180),
           z = z + L_point) %>%
    mutate(H = (atan2(y, x) * 180/pi) %% 360,
           C = sqrt(x^2 + y^2),
           L = z) %>%
    filter(L >= 0 & L <= 100 & C >= 0) %>%
    mutate(color_value = hcl(H, C, L, fixup = FALSE)) %>%
    filter(!is.na(color_value)) %>% # check if exists
    mutate(H_diff = (180 - abs(abs(H - H_point) - 180)) * 
             sign(180 - abs(H - H_point)) * sign(H - H_point)) %>% # H diff, check if crosses 360
    filter(abs(H_diff) <= H_bound) %>% # check in H bound
    filter(!is.na(hcl(H_point - H_diff, C, L, fixup = FALSE))) %>% # symmetric
    select(!H_diff) %>%
    sample_n(n_points) # sample down to desired amount
}

##-------------
# Dataset setup
##-------------

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
lines <- lines %>%
  bind_cols(pmap_dfr(list(circle_x = circle_h,
                          circle_y = circle_k,
                          circle_radius = circle_r, 
                          point_x = .$point_x,
                          point_y = .$point_y), line_points))

##----------
# Set colors
##----------
## Create an "ellipse" around a color
theta_radius <- 30
perpendicular_C_L_radius <- 3
other_C_L_radius <- 10
H_bound <- 3

## tilt the ellipse towards L = max_chroma
max_chromas <- max_chroma(h = coords(line_color)[, 'H'], l = seq(1, 100, .5))
tilt_theta <- atan2(seq(1, 100, .5)[max(max_chromas) == max_chromas] - 
                      coords(line_color)[, 'L'],
                    max(max_chromas) - coords(line_color)[, 'C'])

line_colors <- get_color_points(n_lines,
                                10,
                                coords(line_color)[, 'H'],
                                coords(line_color)[, 'C'],
                                coords(line_color)[, 'L'],
                                theta_radius, 
                                other_C_L_radius, 
                                perpendicular_C_L_radius,
                                tilt_theta, 
                                H_bound)

lines$line_color <- line_colors$color_value

# Repeat for points
## tilt the ellipse towards L = 25
max_chromas <- max_chroma(h = coords(point_color)[, 'H'], l = seq(1, 100, .5))
tilt_theta <- atan2(seq(1, 100, .5)[max(max_chromas) == max_chromas] - 
                      coords(point_color)[, 'L'],
                    max(max_chromas) - coords(point_color)[, 'C'])

point_colors <- get_color_points(n_lines,
                                 10,
                                 coords(point_color)[, 'H'],
                                 coords(point_color)[, 'C'],
                                 coords(point_color)[, 'L'],
                                 theta_radius, 
                                 other_C_L_radius, 
                                 perpendicular_C_L_radius,
                                 tilt_theta, 
                                 H_bound)

lines$point_color <- point_colors$color_value

##-----
# Image
##-----
agg_png(file = file.path(here::here(), "method_3.png"),
        width = 18,
        height = 24,
        units = "in",
        res = 144)

grid.circle(x = circle_h,
            y = circle_k,
            r = circle_r,
            default.units = "in",
            gp = gpar(col = hex(line_color),
                      lwd = 10))

for(i in 1:nrow(lines)) {
  grid.lines(x = c(lines$point_1_x[i], lines$point_2_x[i]),
             y = c(lines$point_1_y[i], lines$point_2_y[i]),
             gp = gpar(col = lines$line_color[i]),
             default.units = "in")
}

for(i in 1:nrow(lines)) {
  grid.points(x = lines$midpoint_x[i],
              y = lines$midpoint_y[i],
              pch = 16,
              size = unit(.125, units = "inches"),
              gp = gpar(col = lines$point_color[i]),
              default.units = "in")
}

##-------
# Process
##-------
## Variables

# Box Location
# Center of center
box_center_y <- (circle_k - circle_r) / 2

# Box sizes
box_width <- (circle_k - circle_r) / 4

# Space between boxes
box_space <- box_width / 2

# Centers of other boxes
box_center_x <- (c(-1, 0, 1) * (box_width + box_space)) + circle_h

# Circle radius
box_radius <- (box_width * .75 * .5)

# Number of options
n_opts <- 9

## Box 1
grid.rect(x = box_center_x[1],
          y = box_center_y,
          width = box_width,
          height = box_width,
          default.units = "in",
          just = "center",
          gp = gpar(col = hex(line_color),
                    lwd=5))

grid.circle(x = box_center_x[1],
            y = box_center_y,
            r = box_radius,
            gp = gpar(col = hex(line_color)),
            default.units = "in")

# Options
# Circle in a circle
opts <- data.frame(opt = seq(1, n_opts),
                   x = box_radius * sqrt(1/2) * 
                     c(0, cos(seq(0, 
                                  360 * (n_opts - 2)/(n_opts - 1),
                                  length.out = (n_opts - 1)) * pi/180)) +
                     box_center_x[1],
                   y = box_radius * sqrt(1/2) * 
                     c(0, sin(seq(0, 
                                  360 * (n_opts - 2)/(n_opts - 1),
                                  length.out = (n_opts - 1)) * pi/180))+
                     box_center_y)

opts <- opts %>% 
  mutate(col = ifelse(opt == sample(opt, 1), 
                       hex(line_color), 
                       'white'))

grid.points(x = opts$x,
            y = opts$y,
            default.units = "in",
            pch = 21,
            gp = gpar(col = hex(line_color),
                      fill = opts$col))

## Box 2
grid.rect(x = box_center_x[2],
          y = box_center_y,
          width = box_width,
          height = box_width,
          default.units = "in",
          just = "center",
          gp = gpar(col = hex(line_color),
                    lwd=5))

grid.circle(x = box_center_x[2],
            y = box_center_y,
            r = box_radius,
            gp = gpar(col = hex(line_color)),
            default.units = "in")

opts <- opts %>%
  filter(col == hex(line_color)) %>%
  mutate(x = (x - box_center_x[1]) + box_center_x[2])

opts <- opts %>%
  bind_cols(pmap_dfr(list(circle_x = box_center_x[2],
                          circle_y = box_center_y,
                          circle_radius = (box_radius), 
                          point_x = (.$x - box_center_x[2]) / (box_radius),
                          point_y = (.$y - box_center_y) / (box_radius)),
                     line_points))

grid.lines(x = c(box_center_x[2], opts$x),
           y = c(box_center_y, opts$y),
           gp = gpar(col = hex(line_color)),
           default.units = "in")

grid.lines(x = c(opts$x, opts$point_1_x),
           y = c(opts$y, opts$point_1_y),
           gp = gpar(col = hex(line_color)),
           default.units = "in")

# Right angle box
right_angle_length <- box_radius / 10

x_vector_1 <- (box_center_x[2] - opts$x)
y_vector_1 <- (box_center_y - opts$y)
unit_1 <- sqrt(x_vector_1^2 + y_vector_1^2)
x_vector_1 <- x_vector_1 / unit_1 * right_angle_length
y_vector_1 <- y_vector_1 / unit_1 * right_angle_length

x_vector_2 <- (opts$point_1_x - opts$x)
y_vector_2 <- (opts$point_1_y - opts$y)
unit_2 <- sqrt(x_vector_2^2 + y_vector_2^2)
x_vector_2 <- x_vector_2 / unit_2 * right_angle_length
y_vector_2 <- y_vector_2 / unit_2 * right_angle_length

grid.lines(x = c(opts$x + x_vector_1 + x_vector_2, 
                 opts$x + x_vector_2),
           y = c(opts$y + y_vector_1 + y_vector_2,
                 opts$y + y_vector_2),
           gp = gpar(col = hex(line_color)),
           default.units = "in")

grid.lines(x = c(opts$x + x_vector_1 + x_vector_2, 
                 opts$x + x_vector_1),
           y = c(opts$y + y_vector_1 + y_vector_2,
                 opts$y + y_vector_1),
           gp = gpar(col = hex(line_color)),
           default.units = "in")

## Box 3
grid.rect(x = box_center_x[3],
          y = box_center_y,
          width = box_width,
          height = box_width,
          default.units = "in",
          just = "center",
          gp = gpar(col = hex(line_color),
                    lwd=5))

grid.circle(x = box_center_x[3],
            y = box_center_y,
            r = box_radius,
            gp = gpar(col = hex(line_color)),
            default.units = "in")

opts <- opts %>%
  filter(col == hex(line_color)) %>%
  mutate(x = (x - box_center_x[2]) + box_center_x[3],
         point_1_x = (point_1_x - box_center_x[2]) + box_center_x[3],
         point_2_x = (point_2_x - box_center_x[2]) + box_center_x[3])

grid.lines(x = c(opts$point_1_x, opts$point_2_x),
           y = c(opts$point_1_y, opts$point_2_y),
           gp = gpar(col = opts$col,
                     lwd = 5),
           default.units = "in")

grid.points(x = mean(c(opts$point_1_x, opts$point_2_x)),
            y = mean(c(opts$point_1_y, opts$point_2_y)),
            pch = 16,
            size = unit(.125, units = "inches"),
            gp = gpar(col = hex(point_color)),
            default.units = "in")

##------
# Border
##------
grid.rect(x = x_limits[1],
          y = y_limits[1],
          width = .25 / 2,
          height = y_limits[2],
          gp = gpar(col = hex(line_color),
                    fill = hex(line_color)),
          just = c("left", "bottom"),
          default.units = "in")
grid.rect(x = x_limits[1],
          y = y_limits[2],
          width = x_limits[2],
          height = -.25 / 2,
          gp = gpar(col = hex(line_color),
                    fill = hex(line_color)),
          just = c("left", "bottom"),
          default.units = "in")
grid.rect(x = x_limits[2],
          y = y_limits[2],
          width = -.25 / 2,
          height = -y_limits[2],
          gp = gpar(col = hex(line_color),
                    fill = hex(line_color)),
          just = c("left", "bottom"),
          default.units = "in")
grid.rect(x = x_limits[1],
          y = y_limits[1],
          width = x_limits[2],
          height = .25 / 2,
          gp = gpar(col = hex(line_color),
                    fill = hex(line_color)),
          just = c("left", "bottom"),
          default.units = "in")

grid.circle(x = x_limits[1],
            y = y_limits[1],
            r = .25,
            gp = gpar(col = hex(point_color),
                      fill = hex(point_color)),
            default.units = "in")
grid.circle(x = x_limits[2],
            y = y_limits[1],
            r = .25,
            gp = gpar(col = hex(point_color),
                      fill = hex(point_color)),
            default.units = "in")
grid.circle(x = x_limits[2],
            y = y_limits[2],
            r = .25,
            gp = gpar(col = hex(point_color),
                      fill = hex(point_color)),
            default.units = "in")
grid.circle(x = x_limits[1],
            y = y_limits[2],
            r = .25,
            gp = gpar(col = hex(point_color),
                      fill = hex(point_color)),
            default.units = "in")

dev.off()