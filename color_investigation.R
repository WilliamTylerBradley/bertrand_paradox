##---------
# Libraries
##---------
library(tidyverse)
library(patchwork)
library(colorspace)

##------------
# Pick a color
##------------
H_point <- 112
C_point <- 60
L_point <- 68

color_hex <- hcl(H_point, 
                 C_point,
                 L_point,
                 fixup = FALSE)

color_points <- data.frame(x = C_point * cos(H_point * pi/180),
                           y = C_point * sin(H_point * pi/180),
                           z = L_point,
                           H = H_point,
                           C = C_point,
                           L = L_point,
                           color_value = color_hex,
                           perpendicular_from_C_L = 0,
                           parallel_along_C_L = 0,
                           row_value = 0,
                           col_value = 0)

##--------------------------------
# See color in H, C, L color space 
##--------------------------------
# C-L Plane ----
get_C_L_plane <- function(H_point) {
  expand_grid(H = H_point,
              C = seq(0, 180, .5),
              L = seq(1, 100, .5)) %>%
    mutate(color_value = hcl(H, C, L, fixup = FALSE)) %>%
    filter(!is.na(color_value))
}
C_L_plane <- get_C_L_plane(H_point)

graph_C_L_plane <- function(C_L_plane, color_points, color_hex) {
  ggplot() +
    geom_point(data = C_L_plane,
               aes(C, L, color = color_value, fill = color_value)) +
    geom_point(data = color_points,
               aes(C, L, color = "white", fill = "white")) +
    scale_x_continuous(labels = abs) +
    scale_color_identity() +
    scale_fill_identity() +
    geom_point(aes(x = C_point,
                   y = L_point),
               color = 'black',
               fill = color_hex,
               shape = 21,
               size = 2) +
    coord_equal()
}
graph_C_L_plane(C_L_plane, color_points, color_hex)
ggsave(here::here("output", "color_investigation", "c_l_plane.png"), 
       width = 5, height = max(C_L_plane$L) / max(C_L_plane$C) * 5)

# H-L Curve ----
get_H_L_curve <- function(C_point) {
  expand_grid(H = seq(1, 360, 1),
              C = C_point,
              L = seq(1, 100, .5)) %>%
    mutate(color_value = hcl(H, C, L, fixup = FALSE)) %>%
    filter(!is.na(color_value))
}
H_L_curve <- get_H_L_curve(C_point)

label_H_center <- function(H_point, ...) {
  function(x) {(x + (180 - H_point)) %% 360}
}

graph_H_L_curve <- function(H_L_curve, color_points, color_hex, H_point) {
  ggplot() +
    geom_point(data = H_L_curve,
               aes((H + (180 - H_point)) %% 360, L, 
                   color = color_value, fill = color_value)) +
    geom_point(data = color_points,
               aes((H + (180 - H_point)) %% 360, L, 
                   color = "white", fill = "white")) +
    scale_color_identity() +
    scale_fill_identity() +
    geom_point(aes(x = 180, # Because we rotated points to not drop over edge
                   y = L_point),
               color = 'black',
               fill = color_hex,
               shape = 21,
               size = 2) +
    scale_x_reverse('H', # Like you're standing on the inside
                    labels = label_H_center(H_point = H_point),
                    limits = c(360, 0)) +
    coord_equal()
}
graph_H_L_curve(H_L_curve, color_points, color_hex, H_point)
ggsave(here::here("output", "color_investigation", "h_l_curve.png"), 
       width = 5, height = max(H_L_curve$L) / max(H_L_curve$H) * 5)

# H-C plane ----
get_H_C_plane <- function(L_point){
  expand_grid(H = seq(1, 360, 1),
              C = seq(0, 180, .5),
              L = L_point) %>%
    mutate(color_value = hcl(H, C, L, fixup = FALSE)) %>%
    filter(!is.na(color_value))
}
H_C_plane <- get_H_C_plane(L_point)

graph_H_C_plane <- function(H_C_plane, color_points, color_hex) {
  ggplot() +
    geom_point(data = H_C_plane,
               aes(H, C, color = color_value, fill = color_value)) +
    geom_point(data = color_points,
               aes(H, C, color = "white", fill = "white")) +
    scale_color_identity() +
    scale_fill_identity() +
    scale_x_continuous(breaks = seq(45, 360, 45),
                       minor_breaks = seq(0, 315, 45) + 45/2,
                       labels = c('45', '90', '135', '180', 
                                  '225', '270', '315', '0|360')) +
    scale_y_continuous(limits = c(0, 180)) +
    geom_point(aes(x = H_point,
                   y = C_point),
               color = 'black',
               fill = color_hex,
               shape = 21,
               size = 2) +
    coord_polar(start = 270 * pi / 180,
                direction = -1)
}
graph_H_C_plane(H_C_plane, color_points, color_hex)
ggsave(here::here("output", "color_investigation", "h_c_plane.png"), 
       width = 5, height = 5)

# C tangent plane ----
C_circle <- data.frame(H = seq(1, 360),
                       C = C_point,
                       color_value = "white")
C_tangent_plane <- expand_grid(x = C_point, # Plane perpendicular to H at C
                       perpendicular_from_C_L = 
                         seq(-sqrt(180^2 - C_point^2), sqrt(180^2 - C_point^2)),
                       L = seq(1, 100, 1)) %>%
  mutate(x_rotate = x * cos(H_point * pi/180) -  # rotate
           perpendicular_from_C_L * sin(H_point * pi/180),
         y_rotate = x * sin(H_point * pi/180) + 
           perpendicular_from_C_L * cos(H_point * pi/180)) %>%
  mutate(x = x_rotate,
         y = y_rotate) %>%
  select(-x_rotate, -y_rotate)  %>%
  mutate(H = (atan2(y, x) * 180/pi) %% 360,
         C = sqrt(x^2 + y^2)) %>%
  mutate(color_value = "white")
ggplot(data = H_C_plane,
       aes(H, C, color = color_value, fill = color_value)) +
  geom_point() +
  scale_color_identity() +
  scale_fill_identity() +
  scale_x_continuous(breaks = seq(45, 360, 45),
                     minor_breaks = seq(0, 315, 45) + 45/2,
                     labels = c('45', '90', '135', '180', 
                                '225', '270', '315', '0|360')) +
  scale_y_continuous(limits = c(0, 180)) +
  geom_path(data = C_circle) +
  geom_segment(x = H_point,
               y = 0,
               xend = H_point,
               yend = C_point,
               col = "white") +
  geom_point(data = C_tangent_plane, col = "black") +
  geom_point(x = H_point,
             y = C_point,
             color = 'black',
             fill = color_hex,
             shape = 21) +
  coord_polar(start = 270 * pi / 180,
              direction = -1)
ggsave(here::here("output", "color_investigation", "c_tangent_plane_setup.png"), 
       width = 5, height = 5)

get_C_tangent_plane <- function(H_point, C_point) {
  expand_grid(x = C_point, # Plane perpendicular to H at C
              perpendicular_from_C_L = seq(-180, 180, .5),
              L = seq(1, 100, .5)) %>%
    mutate(x_rotate = x * cos(H_point * pi/180) -  # rotate
             perpendicular_from_C_L * sin(H_point * pi/180),
           y_rotate = x * sin(H_point * pi/180) + 
             perpendicular_from_C_L * cos(H_point * pi/180)) %>%
    mutate(x = x_rotate,
           y = y_rotate) %>%
    select(-x_rotate, -y_rotate)  %>%
    mutate(H = (atan2(y, x) * 180/pi) %% 360,
           C = sqrt(x^2 + y^2)) %>%
    mutate(color_value = hcl(H, C, L, fixup = FALSE)) %>%
    filter(!is.na(color_value))
}
C_tangent_plane <- get_C_tangent_plane(H_point, C_point)

graph_C_tangent_plane <- function(C_tangent_plane, color_points, color_hex) {
  ggplot() +
    geom_point(data = C_tangent_plane,
               aes(perpendicular_from_C_L, L, 
                   color = color_value, fill = color_value)) +
    geom_point(data = color_points,
               aes(perpendicular_from_C_L, L, 
                   color = "white", fill = "white")) +
    scale_color_identity() +
    scale_fill_identity() +
    scale_x_reverse("Distance perpendicular to C-L plane",
                    labels = abs) +
    geom_point(aes(x = 0,
                   y = L_point),
               color = 'black',
               fill = color_hex,
               shape = 21,
               size = 2) +
    coord_equal()
}
graph_C_tangent_plane(C_tangent_plane, color_points, color_hex)
ggsave(here::here("output", "color_investigation", "c_tangent_plane.png"), 
       width = 5, height = max(C_tangent_plane$perpendicular_from_C_L) /
         max(C_tangent_plane$L) * 5)

##------
# Sphere
##------
radius <- 5
n_points <- 250^2

color_points <- data.frame(x = rnorm(n = n_points),
                           y = rnorm(n = n_points),
                           z = rnorm(n = n_points),
                           U = runif(n = n_points)^(1/3)) %>%
  mutate(normalize = sqrt(x^2 + y^2 + z^2)) %>%
  mutate(x = x * U / normalize,
         y = y * U / normalize,
         z = z * U / normalize) %>%
  select(-U, -normalize) %>% # have random points in a sphere here
  mutate(x = x * radius, # stretch
         y = y * radius,
         z = z * radius) %>%
  mutate(x = x + C_point * cos(H_point * pi/180), # move
         y = y + C_point * sin(H_point * pi/180),
         z = z + L_point) %>%
  mutate(H = (atan2(y, x) * 180/pi) %% 360,
         C = sqrt(x^2 + y^2),
         L = z) %>%
  mutate(color_value = hcl(H, C, L, fixup = FALSE)) %>%
  mutate(perpendicular_from_C_L = x * sin(-H_point * pi/180) + 
           y * cos(-H_point * pi/180),
         parallel_along_C_L = x * cos(-H_point * pi/180) - 
           y * sin(-H_point * pi/180)) %>%
  mutate(row_value = sample(row_number(), n()),
         col_value = ceiling(row_value / sqrt(n_points))) %>%
  mutate(row_value = (row_value %% sqrt(n_points)) + 1)

graph_info <- function(H_point, C_point, L_point) {
  color_hex <- hcl(H_point, 
                   C_point,
                   L_point,
                   fixup = FALSE)
  
  ggplot() +
    geom_rect(aes(xmin = 0, xmax = 1,
                  ymin = 0, ymax = .5), col = color_hex, fill = color_hex) +
    geom_text(data = data.frame(x = 0,
                                y = seq(1.5, .75, -.25),
                                label = c(paste("HEX Value:", color_hex), 
                                          paste("H Value:", H_point),
                                          paste("C Value:", C_point),
                                          paste("L Value:", L_point))),
              aes(x, y, label = label), hjust = 0, size = 4) +
    coord_equal() +
    theme_void()
}

graph_sample <- function(color_points) {
  ggplot(data = color_points,
         aes(x = row_value,
             y = col_value,
             fill = color_value)) +
    geom_tile() +
    coord_equal() +
    scale_fill_identity() +
    theme_void()
}

p1 <- graph_info(H_point, C_point, L_point)
p2 <- graph_sample(color_points)
p1 + p2
ggsave(here::here("output", "color_investigation", "sphere_info.png"), 
       width = 5, height = 3)


graph_C_L <- function(color_points) {
  ggplot(data = color_points, aes(C, L, 
                                  col = color_value, fill = color_value)) +
    geom_point() +
    scale_color_identity() +
    scale_fill_identity() +
    coord_equal() +
    theme(axis.line=element_blank(), axis.text.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks=element_blank(),
          axis.title.x=element_blank(), axis.title.y=element_blank())
}

p1 <- graph_C_L_plane(C_L_plane, color_points, color_hex)
p2 <- graph_C_L(color_points)
p1 + p2
ggsave(here::here("output", "color_investigation", "sphere_c_l.png"), 
       width = 5, height = 2.5)

graph_H_L <- function(color_points, H_point) {
  ggplot(data = color_points, aes((H + (180 - H_point)) %% 360, L, 
                                  col = color_value, fill = color_value)) +
    geom_point() +
    scale_color_identity() +
    scale_fill_identity() +
    scale_x_reverse() +
    coord_equal() +
    theme(axis.line=element_blank(), axis.text.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks=element_blank(),
          axis.title.x=element_blank(), axis.title.y=element_blank())
}

p1 <- graph_H_L_curve(H_L_curve, color_points, color_hex, H_point)
p2 <- graph_H_L(color_points, H_point)
p1 + p2
ggsave(here::here("output", "color_investigation", "sphere_h_l.png"), 
       width = 5, height = 1.5)

graph_perpendicular_from_C_L <- function(color_points) {
  ggplot(data = color_points, aes(perpendicular_from_C_L, L, 
                                  color = color_value, fill = color_value)) +
    geom_point() +
    scale_color_identity() +
    scale_fill_identity()  +
    scale_x_reverse() +
    coord_equal() +
    theme(axis.line=element_blank(), axis.text.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks=element_blank(),
          axis.title.x=element_blank(), axis.title.y=element_blank())
}

p1 <- graph_C_tangent_plane(C_tangent_plane, color_points, color_hex)
p2 <- graph_perpendicular_from_C_L(color_points)
p1 + p2
ggsave(here::here("output", "color_investigation", "sphere_p_c_l.png"), 
       width = 5, height = 2)

graph_x_y <- function(color_points, H_point) {
  ggplot(data = color_points, aes(x, y, 
                                  col = color_value, fill = color_value)) +
    geom_abline(slope = c(tan(-67.5 * pi/180), 
                          tan(-45 * pi/180), 
                          tan(-22.5 * pi/180),
                          0, 
                          100000,
                          tan(22.5 * pi/180), 
                          tan(45 * pi/180), 
                          tan(67.5 * pi/180)), 
                intercept = 0,
                color = "white") +
    geom_abline(slope = tan(H_point * pi/180), 
                intercept = 0,
                color = "black") +
    geom_point() +
    scale_color_identity() +
    scale_fill_identity() +
    coord_equal() +
    theme(axis.line=element_blank(), axis.text.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks=element_blank(),
          axis.title.x=element_blank(), axis.title.y=element_blank(),
          panel.grid.major=element_blank(), panel.grid.minor=element_blank())
}

p1 <- graph_H_C_plane(H_C_plane, color_points, color_hex)
p2 <- graph_x_y(color_points, H_point)
p1 + p2
ggsave(here::here("output", "color_investigation", "sphere_h_c.png"), 
       width = 5, height = 2.5)

##-------
# Ellipse
##-------
H_radius <- 2.5 # Actually C plane
C_radius <- 5 #
L_radius <- 10 #

color_points <- data.frame(x = rnorm(n = n_points),
                           y = rnorm(n = n_points),
                           z = rnorm(n = n_points),
                           U = runif(n = n_points)^(1/3)) %>%
  mutate(normalize = sqrt(x^2 + y^2 + z^2)) %>%
  mutate(x = x * U / normalize,
         y = y * U / normalize,
         z = z * U / normalize) %>%
  select(-U, -normalize) %>% # have random points in a sphere here
  mutate(x = x * C_radius, # stretch
         y = y * H_radius,
         z = z * L_radius) %>%
  mutate(x_turn = x * cos(H_point * pi/180) - 
           y * sin(H_point * pi/180), # rotate
         y_turn = x * sin(H_point * pi/180) + 
           y * cos(H_point * pi/180)) %>%
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
  mutate(perpendicular_from_C_L = x * sin(-H_point * pi/180) + 
           y * cos(-H_point * pi/180),
         parallel_along_C_L = x * cos(-H_point * pi/180) - 
           y * sin(-H_point * pi/180)) %>%
  mutate(row_value = sample(row_number(), n()),
         col_value = ceiling(row_value / sqrt(n_points))) %>%
  mutate(row_value = (row_value %% sqrt(n_points)) + 1)

p1 <- graph_info(H_point, C_point, L_point)
p2 <- graph_sample(color_points)
p1 + p2
ggsave(here::here("output", "color_investigation", "ellipse_info.png"), 
       width = 5, height = 3)

p1 <- graph_C_L_plane(C_L_plane, color_points, color_hex)
p2 <- graph_C_L(color_points)
p1 + p2
ggsave(here::here("output", "color_investigation", "ellipse_c_l.png"), 
       width = 5, height = 3)

p1 <- graph_H_L_curve(H_L_curve, color_points, color_hex, H_point)
p2 <- graph_H_L(color_points, H_point)
p1 + p2
ggsave(here::here("output", "color_investigation", "ellipse_h_l.png"), 
       width = 5, height = 1.5)

p1 <- graph_C_tangent_plane(C_tangent_plane, color_points, color_hex)
p2 <- graph_perpendicular_from_C_L(color_points)
p1 + p2
ggsave(here::here("output", "color_investigation", "ellipse_p_c_l.png"), 
       width = 5, height = 2)

p1 <- graph_H_C_plane(H_C_plane, color_points, color_hex)
p2 <- graph_x_y(color_points, H_point)
p1 + p2
ggsave(here::here("output", "color_investigation", "ellipse_h_c.png"), 
       width = 5, height = 3)

##--------------
# Tilted Ellipse
##--------------
theta_radius <- 10
other_C_L_radius <- 3
perpendicular_C_L_radius <- 5

# Try rotating to point major axis to max chroma value
max_chromas <- max_chroma(h = H_point, l = seq(1, 100, .5))
tilt_theta <- atan2(seq(1, 100, .5)[max(max_chromas) == max_chromas] - L_point,
                     max(max_chromas) - C_point)

color_points <- data.frame(x = rnorm(n = n_points),
           y = rnorm(n = n_points),
           z = rnorm(n = n_points),
           U = runif(n = n_points)^(1/3)) %>%
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
  mutate(x_turn = x * cos(H_point * pi/180) - 
           y * sin(H_point * pi/180), # rotate
         y_turn = x * sin(H_point * pi/180) + 
           y * cos(H_point * pi/180)) %>%
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
  mutate(perpendicular_from_C_L = x * sin(-H_point * pi/180) + 
           y * cos(-H_point * pi/180),
         parallel_along_C_L = x * cos(-H_point * pi/180) - 
           y * sin(-H_point * pi/180)) %>%
  mutate(row_value = sample(row_number(), n()),
         col_value = ceiling(row_value / sqrt(n_points))) %>%
  mutate(row_value = (row_value %% sqrt(n_points)) + 1)

p1 <- graph_info(H_point, C_point, L_point)
p2 <- graph_sample(color_points)
p1 + p2
ggsave(here::here("output", "color_investigation", "tilted_ellipse_info.png"), 
       width = 5, height = 3)

p1 <- graph_C_L_plane(C_L_plane, color_points, color_hex)
p2 <- graph_C_L(color_points)
p1 + p2
ggsave(here::here("output", "color_investigation", "tilted_ellipse_c_l.png"), 
       width = 5, height = 2)

p1 <- graph_H_L_curve(H_L_curve, color_points, color_hex, H_point)
p2 <- graph_H_L(color_points, H_point)
p1 + p2 # Not quite an ellipse
ggsave(here::here("output", "color_investigation", "tilted_ellipse_h_l.png"), 
       width = 5, height = 1.5)

p1 <- graph_C_tangent_plane(C_tangent_plane, color_points, color_hex)
p2 <- graph_perpendicular_from_C_L(color_points)
p1 + p2
ggsave(here::here("output", "color_investigation", "tilted_ellipse_p_c_l.png"), 
       width = 5, height = 2)

p1 <- graph_H_C_plane(H_C_plane, color_points, color_hex)
p2 <- graph_x_y(color_points, H_point)
p1 + p2
ggsave(here::here("output", "color_investigation", "tilted_ellipse_h_c.png"), 
       width = 5, height = 3)

##--------
# Clean up
##--------
# sampling, hitting edges
# within h bounds, this also catches C on the other side
# symmetric on H

H_bound <- 3 # up to 90
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
    sample_n(n_points) %>% # sample down to desired amount
    mutate(perpendicular_from_C_L = x * sin(-H_point * pi/180) + 
             y * cos(-H_point * pi/180),
           parallel_along_C_L = x * cos(-H_point * pi/180) - 
             y * sin(-H_point * pi/180)) %>%
    mutate(row_value = sample(row_number(), n()),
           col_value = ceiling(row_value / sqrt(n_points))) %>%
    mutate(row_value = (row_value %% sqrt(n_points)) + 1)
}

color_points <- get_color_points(250^2, 10,
                                 H_point, C_point, L_point,
                                 theta_radius, other_C_L_radius, 
                                 perpendicular_C_L_radius,
                                 tilt_theta, H_bound)

##-------
# Compare
##-------
H_bound <- 90
color_points_sphere <- get_color_points(250^2, 10,
                                        H_point, C_point, L_point,
                                        theta_radius = radius, 
                                        other_C_L_radius = radius, 
                                        perpendicular_C_L_radius = radius,
                                        tilt_theta = 0, H_bound)
color_points_ellipse <- get_color_points(250^2, 10,
                                         H_point, C_point, L_point,
                                         theta_radius = C_radius, 
                                         other_C_L_radius = H_radius, 
                                         perpendicular_C_L_radius = L_radius,
                                         tilt_theta = 0, H_bound)
color_points_tilted_ellipse <- get_color_points(250^2, 10,
                                                H_point, C_point, L_point,
                                                theta_radius, 
                                                other_C_L_radius, 
                                                perpendicular_C_L_radius,
                                                tilt_theta, H_bound)

p1 <- graph_sample(color_points_sphere)
p2 <- graph_sample(color_points_ellipse)
p3 <- graph_sample(color_points_tilted_ellipse)
p1 + p2 + p3
ggsave(here::here("output", "color_investigation", "compare_info.png"), 
       width = 5, height = 1.5)

p1 <- graph_C_L(color_points_sphere)
p2 <- graph_C_L(color_points_ellipse)
p3 <- graph_C_L(color_points_tilted_ellipse)
p1 + p2 + p3
ggsave(here::here("output", "color_investigation", "compare_c_l.png"), 
       width = 5, height = 1.5)

p1 <- graph_H_L(color_points_sphere, H_point)
p2 <- graph_H_L(color_points_ellipse, H_point)
p3 <- graph_H_L(color_points_tilted_ellipse, H_point)
p1 + p2 + p3
ggsave(here::here("output", "color_investigation", "compare_h_l.png"), 
       width = 5, height = 3)

p1 <- graph_perpendicular_from_C_L(color_points_sphere)
p2 <- graph_perpendicular_from_C_L(color_points_ellipse)
p3 <- graph_perpendicular_from_C_L(color_points_tilted_ellipse)
p1 + p2 + p3
ggsave(here::here("output", "color_investigation", "compare_c_l.png"), 
       width = 5, height = 3)

p1 <- graph_x_y(color_points_sphere, H_point)
p2 <- graph_x_y(color_points_ellipse, H_point)
p3 <- graph_x_y(color_points_tilted_ellipse, H_point)
p1 + p2 + p3
ggsave(here::here("output", "color_investigation", "compare_h_c.png"), 
       width = 5, height = 2)

#--------------------
# Try some other ones
#--------------------

# Outside Edge ----
H_point <- 63
C_point <- 93
L_point <- 81

color_hex <- hcl(H_point, 
                 C_point,
                 L_point,
                 fixup = FALSE)

C_L_plane <- get_C_L_plane(H_point)
H_L_curve <- get_H_L_curve(C_point)
C_tangent_plane <- get_C_tangent_plane(H_point, C_point)
H_C_plane <- get_H_C_plane(L_point)

theta_radius <- 40
other_C_L_radius <- 3
perpendicular_C_L_radius <- 5

tilt_theta <- 0
H_bound <- 90

color_points <- get_color_points(250^2, 10,
                                 H_point, C_point, L_point,
                                 theta_radius, other_C_L_radius, 
                                 perpendicular_C_L_radius,
                                 tilt_theta, H_bound)

p1 <- graph_info(H_point, C_point, L_point)
p2 <- graph_sample(color_points)
p1 + p2
ggsave(here::here("output", "color_investigation", "outside_edge_info.png"), 
       width = 5, height = 3)

p1 <- graph_C_L_plane(C_L_plane, color_points, color_hex)
p2 <- graph_C_L(color_points)
p1 + p2
ggsave(here::here("output", "color_investigation", "outside_edge_c_l.png"), 
       width = 5, height = 1.5)

p1 <- graph_H_L_curve(H_L_curve, color_points, color_hex, H_point)
p2 <- graph_H_L(color_points, H_point)
p1 + p2
ggsave(here::here("output", "color_investigation", "outside_edge_h_l.png"), 
       width = 5, height = 1.5)

p1 <- graph_C_tangent_plane(C_tangent_plane, color_points, color_hex)
p2 <- graph_perpendicular_from_C_L(color_points)
p1 + p2
ggsave(here::here("output", "color_investigation", "outside_edge_p_c_l.png"), 
       width = 5, height = 1.5)

p1 <- graph_H_C_plane(H_C_plane, color_points, color_hex)
p2 <- graph_x_y(color_points, H_point)
p1 + p2
ggsave(here::here("output", "color_investigation", "outside_edge_h_c.png"), 
       width = 5, height = 3)

# Inside Edge ----
H_point <- 319
C_point <- 10
L_point <- 50

color_hex <- hcl(H_point, 
                 C_point,
                 L_point,
                 fixup = FALSE)

C_L_plane <- get_C_L_plane(H_point)
H_L_curve <- get_H_L_curve(C_point)
C_tangent_plane <- get_C_tangent_plane(H_point, C_point)
H_C_plane <- get_H_C_plane(L_point)

theta_radius <- 40
other_C_L_radius <- 5
perpendicular_C_L_radius <- 20

tilt_theta <- 90 * pi/180
H_bound <- 45

color_points <- get_color_points(250^2, 10,
                                 H_point, C_point, L_point,
                                 theta_radius, other_C_L_radius, 
                                 perpendicular_C_L_radius,
                                 tilt_theta, H_bound)

p1 <- graph_info(H_point, C_point, L_point)
p2 <- graph_sample(color_points)
p1 + p2
ggsave(here::here("output", "color_investigation", "inside_edge_info.png"), 
       width = 5, height = 3)

p1 <- graph_C_L_plane(C_L_plane, color_points, color_hex)
p2 <- graph_C_L(color_points)
p1 + p2
ggsave(here::here("output", "color_investigation", "inside_edge_c_l.png"), 
       width = 5, height = 3)

p1 <- graph_H_L_curve(H_L_curve, color_points, color_hex, H_point)
p2 <- graph_H_L(color_points, H_point)
p1 + p2
ggsave(here::here("output", "color_investigation", "inside_edge_h_l.png"), 
       width = 5, height = 1.5)

p1 <- graph_C_tangent_plane(C_tangent_plane, color_points, color_hex)
p2 <- graph_perpendicular_from_C_L(color_points)
p1 + p2
ggsave(here::here("output", "color_investigation", "inside_edge_p_c_l.png"), 
       width = 5, height = 2.5)

p1 <- graph_H_C_plane(H_C_plane, color_points, color_hex)
p2 <- graph_x_y(color_points, H_point)
p1 + p2
ggsave(here::here("output", "color_investigation", "inside_edge_h_c.png"), 
       width = 5, height = 2.5)
