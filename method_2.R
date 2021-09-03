##------------------
# Libraries and Seed
##------------------
library(grid)
library(tidyverse)
library(colorspace)
library(here)
library(ragg)

set.seed(200)

##------------------
# Variables
##------------------
n_lines <- 5000
x_limits <- c(0, 18)
y_limits <- c(0, 24)
line_color <- as(hex2RGB("#8D3882"), "polarLUV") # Dark purple - red
point_color <- as(hex2RGB("#F084C8"), "polarLUV") # Bright red - purple

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
  new_angle = pi - pi/2 - asin(((chord * circle_radius) * 
                                  sin(pi/2)) / circle_radius)
  
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
lines <- data.frame(line = seq(1, n_lines),
                    radius = runif(n_lines, 0, 2*pi),
                    chord = runif(n_lines, 0, 1))
lines <- lines %>%
  bind_cols(pmap_dfr(list(circle_x = circle_h,
                          circle_y = circle_k,
                          circle_radius = circle_r, 
                          radius = .$radius,
                          chord = .$chord), line_points))

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

## Set alpha
lines <- lines %>% 
  mutate(alpha_value = .95 - (row_number()/n_lines)*.5)

##-----
# Image
##-----
agg_png(file = file.path(here::here(), "method_2.png"),
        width = 18,
        height = 24,
        units = "in",
        res = 144)

grid.circle(x = circle_h,
            y = circle_k,
            r = circle_r,
            default.units = "in",
            gp = gpar(col = hex(line_color),
                      lwd=10))

for(i in 1:nrow(lines)) {
  grid.lines(x = c(lines$point_1_x[i], lines$point_2_x[i]),
             y = c(lines$point_1_y[i], lines$point_2_y[i]),
             gp = gpar(col = lines$line_color[i],
                       alpha = lines$alpha_value[i]),
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
opts <- data.frame(opt = seq(1, n_opts),
                   radius = seq(0, n_opts-1) * 
                     (2*pi / n_opts))
opts <- opts %>%
  mutate(
    x1 = box_center_x[1],
    y1 = box_center_y,
    x2 = box_center_x[1] + 
      box_radius * 
      cos(radius),
    y2 = box_center_y + 
      box_radius * 
      sin(radius )) 

opts <- opts %>% 
  mutate(lty1 = ifelse(opt == sample(opt, 1), 1, 2))

for(i in seq(1, nrow(opts))) {
  grid.lines(x = c(opts$x1[i], opts$x2[i]),
             y = c(opts$y1[i], opts$y2[i]),
             default.units = "in",
             gp = gpar(col = hex(line_color),
                       lty = opts$lty1[i]))
}

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
  filter(lty1 == 1) %>%
  mutate(x1 = box_center_x[2],
         x2 = box_center_x[2] + 
           box_radius * 
           cos(radius))

opts_2 <- data.frame(opt = seq(1, ceiling(n_opts / 2)),
                     radius = opts$radius,
                     chord = seq(0, ceiling(n_opts / 2)-1) *
                       (1/ceiling(n_opts / 2)))
opts_2 <- opts_2 %>%
  bind_cols(pmap_dfr(list(circle_x = box_center_x[2],
                          circle_y = box_center_y,
                          circle_radius = box_radius, 
                          radius = .$radius,
                          chord = .$chord), line_points))

opts_2 <- opts_2 %>% 
  mutate(lty1 = ifelse(opt == sample(opt, 1), 1, 2))

grid.lines(x = c(opts$x1, opts$x2),
           y = c(opts$y1, opts$y2),
           default.units = "in",
           gp = gpar(col = hex(line_color),
                     lty = opts$lty1))

for(i in seq(1, nrow(opts_2))) {
  grid.lines(x = c(opts_2$point_1_x[i], opts_2$point_2_x[i]),
             y = c(opts_2$point_1_y[i], opts_2$point_2_y[i]),
             default.units = "in",
             gp = gpar(col = hex(line_color),
                       lty = opts_2$lty1[i]))
}

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

opts_2 <- opts_2 %>%
  filter(lty1 == 1) %>%
  select(c(opt, radius, chord, lty1)) %>%
  bind_cols(pmap_dfr(list(circle_x = box_center_x[3],
                          circle_y = box_center_y,
                          circle_radius = box_radius, 
                          radius = .$radius,
                          chord = .$chord), line_points))

grid.lines(x = c(opts_2$point_1_x, opts_2$point_2_x),
           y = c(opts_2$point_1_y, opts_2$point_2_y),
           default.units = "in",
           gp = gpar(col = hex(line_color),
                     lwd = 5))

grid.points(x = mean(c(opts_2$point_1_x, opts_2$point_2_x)),
            y = mean(c(opts_2$point_1_y, opts_2$point_2_y)),
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