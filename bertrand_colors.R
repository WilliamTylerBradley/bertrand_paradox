##---------
# Libraries
##---------
library(tidyverse)
library(patchwork)
library(colorspace)

##---------
# Functions
##---------

# Basics ----
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

# C L plane ----
get_C_L_plane <- function(H_point) {
  expand_grid(H = H_point,
              C = seq(0, 180, .5),
              L = seq(1, 100, .5)) %>%
    mutate(color_value = hcl(H, C, L, fixup = FALSE)) %>%
    filter(!is.na(color_value))
}

graph_C_L_plane <- function(C_L_plane, color_points) {
  ggplot() +
    geom_point(data = C_L_plane,
               aes(C, L, color = color_value, fill = color_value)) +
    geom_point(data = color_points,
               aes(C, L, color = "white", fill = "white")) +
    scale_x_continuous(labels = abs) +
    scale_color_identity() +
    scale_fill_identity() +
    coord_equal()
}

graph_C_L <- function(color_points) {
  ggplot(data = color_points, aes(C, L, 
                                  col = color_value, fill = color_value)) +
    geom_point() +
    scale_color_identity() +
    scale_fill_identity() +
    coord_equal() 
}

# H C plane ----
get_H_C_plane <- function(L_point){
  expand_grid(H = seq(1, 360, 1),
              C = seq(0, 180, .5),
              L = L_point) %>%
    mutate(color_value = hcl(H, C, L, fixup = FALSE)) %>%
    filter(!is.na(color_value))
}

graph_H_C_plane <- function(H_C_plane, color_points) {
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
    coord_polar(start = 270 * pi / 180,
                direction = -1)
}

graph_x_y <- function(color_points, H_point) {
  ggplot(data = color_points, aes(x, y, 
                                  col = color_value, fill = color_value)) +
    geom_abline(slope = c(tan(-67.5 * pi/180), 
                          tan(-45 * pi/180), 
                          tan(-22.5 * pi/180),
                          0, 100000,
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

# Sample points ----
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
    mutate(row_value = sample(row_number(), n()),
           col_value = ceiling(row_value / sqrt(n_points))) %>%
    mutate(row_value = (row_value %% sqrt(n_points)) + 1)
}

##---------
# Variables
##---------

n_points <- 250^2

# Radii are in relation to the H C plane
theta_radius <- 30 # Radius for direction for theta, on the C-L plane
perpendicular_C_L_radius <- 3 # Radius perpendicular to other ones, roughly goes with H
other_C_L_radius <- 10 # Radius for other C-L vector

H_bound <- 3 # Bound on H

H_points <- c(35, # Dark Orange - Yellow
              60, # Bright Yellow - Orange
              166, # Dark Green - Blue
              179, # Bright Blue - Green
              316, # Dark Purple - Red
              331) # Bright Red - Purple

C_points <- c(81, # Dark Orange - Yellow
              77, # Bright Yellow - Orange
              32, # Dark Green - Blue # A little darker
              44, # Bright Blue - Green
              57, # Dark Purple - Red
              70) # Bright Red - Purple

L_points <- c(56, # Dark Orange - Yellow
              85, # Bright Yellow - Orange
              59, # Dark Green - Blue
              90, # Bright Blue - Green
              38, # Dark Purple - Red
              69) # Bright Red - Purple

max_chromas <- map(H_points, function(h) max_chroma(h, l = seq(1, 100, .5)))

tilt_thetas <- pmap(list(max_chromas, C_points, L_points),
                    function(mc, c, l) {
                      atan2(seq(1, 100, .5)[max(mc) == mc] - l,
                            max(mc) - c) })

color_points <- pmap(list(rep(250^2, 6),
                          rep(10, 6),
                          H_points, 
                          C_points,
                          L_points,
                          rep(theta_radius, 6),
                          rep(other_C_L_radius, 6),
                          rep(perpendicular_C_L_radius, 6),
                          tilt_thetas,
                          rep(H_bound, 6)),
                     get_color_points )

color_hexes <- hcl(H_points, 
                 C_points,
                 L_points,
                 fixup = FALSE)

C_L_planes <- map(H_points, get_C_L_plane)
H_C_planes <- map(L_points, get_H_C_plane)



## 1, 2
p1 <- graph_info(H_points[1], C_points[1], L_points[1])
p2 <- graph_sample(color_points[[1]])
p3 <- graph_info(H_points[2], C_points[2], L_points[2])
p4 <- graph_sample(color_points[[2]])
(p1 + p2) / (p3 + p4)
ggsave(here::here("output", "bertrand_colors", "1_2_info.png"), 
       width = 5, height = 7)

p1 <- graph_C_L_plane(C_L_planes[[1]], color_points[[1]])
p2 <- graph_C_L(color_points[[1]])
p3 <- graph_C_L_plane(C_L_planes[[2]], color_points[[2]])
p4 <- graph_C_L(color_points[[2]])
p1 + p2 + plot_layout(widths = c(1, 1))
ggsave(here::here("output", "bertrand_colors", "1_c_l.png"), 
       width = 5, height = 2)
p3 + p4 + plot_layout(widths = c(1, 1))
ggsave(here::here("output", "bertrand_colors", "2_c_l.png"), 
       width = 5, height = 2)

p1 <- graph_H_C_plane(H_C_planes[[1]], color_points[[1]])
p2 <- graph_x_y(color_points[[1]], H_points[[1]])
p3 <- graph_H_C_plane(H_C_planes[[2]], color_points[[2]])
p4 <- graph_x_y(color_points[[2]], H_points[[2]])
p1 + p2 + plot_layout(widths = c(1, 1))
ggsave(here::here("output", "bertrand_colors", "1_h_c.png"), 
       width = 5, height = 2.5)
p3 + p4 + plot_layout(widths = c(1, 1))
ggsave(here::here("output", "bertrand_colors", "2_h_c.png"), 
       width = 5, height = 2.5)

## 3, 4
p1 <- graph_info(H_points[3], C_points[3], L_points[3])
p2 <- graph_sample(color_points[[3]])
p3 <- graph_info(H_points[4], C_points[4], L_points[4])
p4 <- graph_sample(color_points[[4]])
(p1 + p2) / (p3 + p4)
ggsave(here::here("output", "bertrand_colors", "3_4_info.png"), 
       width = 5, height = 7)

p1 <- graph_C_L_plane(C_L_planes[[3]], color_points[[3]])
p2 <- graph_C_L(color_points[[3]])
p3 <- graph_C_L_plane(C_L_planes[[4]], color_points[[4]])
p4 <- graph_C_L(color_points[[4]])
p1 + p2 + plot_layout(widths = c(1, 1))
ggsave(here::here("output", "bertrand_colors", "3_c_l.png"), 
       width = 5, height = 2)
p3 + p4 + plot_layout(widths = c(1, 1))
ggsave(here::here("output", "bertrand_colors", "4_c_l.png"), 
       width = 5, height = 2)

p1 <- graph_H_C_plane(H_C_planes[[3]], color_points[[3]])
p2 <- graph_x_y(color_points[[3]], H_points[[3]])
p3 <- graph_H_C_plane(H_C_planes[[4]], color_points[[4]])
p4 <- graph_x_y(color_points[[4]], H_points[[4]])
p1 + p2 + plot_layout(widths = c(1, 1))
ggsave(here::here("output", "bertrand_colors", "3_h_c.png"), 
       width = 5, height = 2.5)
p3 + p4 + plot_layout(widths = c(1, 1))
ggsave(here::here("output", "bertrand_colors", "4_h_c.png"), 
       width = 5, height = 2.5)

## 5, 6
p1 <- graph_info(H_points[5], C_points[5], L_points[5])
p2 <- graph_sample(color_points[[5]])
p3 <- graph_info(H_points[6], C_points[6], L_points[6])
p4 <- graph_sample(color_points[[6]])
(p1 + p2) / (p3 + p4)
ggsave(here::here("output", "bertrand_colors", "5_6_info.png"), 
       width = 5, height = 7)

p1 <- graph_C_L_plane(C_L_planes[[5]], color_points[[5]])
p2 <- graph_C_L(color_points[[5]])
p3 <- graph_C_L_plane(C_L_planes[[6]], color_points[[6]])
p4 <- graph_C_L(color_points[[6]])
p1 + p2 + plot_layout(widths = c(1, 1))
ggsave(here::here("output", "bertrand_colors", "5_c_l.png"), 
       width = 5, height = 2)
p3 + p4 + plot_layout(widths = c(1, 1))
ggsave(here::here("output", "bertrand_colors", "6_c_l.png"), 
       width = 5, height = 2)

p1 <- graph_H_C_plane(H_C_planes[[5]], color_points[[5]])
p2 <- graph_x_y(color_points[[5]], H_points[[5]])
p3 <- graph_H_C_plane(H_C_planes[[6]], color_points[[6]])
p4 <- graph_x_y(color_points[[6]], H_points[[6]])
p1 + p2 + plot_layout(widths = c(1, 1))
ggsave(here::here("output", "bertrand_colors", "5_h_c.png"), 
       width = 5, height = 2.5)
p3 + p4 + plot_layout(widths = c(1, 1))
ggsave(here::here("output", "bertrand_colors", "6_h_c.png"), 
       width = 5, height = 2.5)
