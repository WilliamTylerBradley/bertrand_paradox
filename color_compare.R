##---------
# Libraries
##---------
library(tidyverse)
library(patchwork)
library(colorspace)

##------------
# Pick a color
##------------
H_point <- 322
C_point <- 26
L_point <- 69

color_hex <- hcl(H_point, 
                 C_point, 
                 L_point,
                 fixup = FALSE)

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

graph_C_L_plane <- function(C_L_plane, color_hex) {
  ggplot() +
    geom_point(data = C_L_plane,
               aes(C, L, color = color_value, fill = color_value)) +
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
graph_C_L_plane(C_L_plane, color_hex)
ggsave(here::here("output", "color_compare", "c_l_plane.png"), 
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

graph_H_L_curve <- function(H_L_curve, color_hex, H_point) {
  ggplot() +
    geom_point(data = H_L_curve,
               aes((H + (180 - H_point)) %% 360, L, 
                   color = color_value, fill = color_value)) +
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
graph_H_L_curve(H_L_curve, color_hex, H_point)
ggsave(here::here("output", "color_compare", "h_l_curve.png"), 
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

graph_H_C_plane <- function(H_C_plane, color_hex) {
  ggplot() +
    geom_point(data = H_C_plane,
               aes(H, C, color = color_value, fill = color_value)) +
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
graph_H_C_plane(H_C_plane, color_hex)
ggsave(here::here("output", "color_compare", "h_c_plane.png"), 
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
ggsave(here::here("output", "color_compare", "c_tangent_plane_setup.png"), 
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

graph_C_tangent_plane <- function(C_tangent_plane, color_hex) {
  ggplot() +
    geom_point(data = C_tangent_plane,
               aes(perpendicular_from_C_L, L, 
                   color = color_value, fill = color_value)) +
    scale_color_identity() +
    scale_fill_identity() +
    scale_x_reverse("Distance Perpendicular to C-L Plane",
                    labels = abs) +
    geom_point(aes(x = 0,
                   y = L_point),
               color = 'black',
               fill = color_hex,
               shape = 21,
               size = 2) +
    coord_equal()
}
graph_C_tangent_plane(C_tangent_plane, color_hex)
ggsave(here::here("output", "color_compare", "c_tangent_plane.png"), 
       width = 5, height = max(C_tangent_plane$L) /
         (max(C_tangent_plane$perpendicular_from_C_L) * 2) * 5)

##------------------
# x, y, L perimeter
##------------------
width <- 15
n_color <- 50 ^ 2

# http://extremelearning.com.au/how-to-evenly-distribute-points-on-a-sphere-more-effectively-than-the-canonical-fibonacci-lattice/#more-3069
get_xyl_data <- function(H_point, C_point, L_point, width, n_color) {
  data.frame(theta = 2 * pi * seq(0, n_color - 1) / ((1 + sqrt(5)) / 2),
             phi = acos(1 - 2 * (seq(0, n_color - 1) + .5) / n_color)) %>%
    mutate(x = cos(theta) * sin(phi),
           y = sin(theta) * sin(phi),
           L = cos(phi)) %>%
    mutate(x = x * width + C_point * cos(H_point * pi/180),
           y = y * width + C_point * sin(H_point * pi/180),
           L = L * width + L_point) %>%
    mutate(H = (atan2(y, x) * 180/pi) %% 360,
           C = sqrt(x^2 + y^2)) %>%
    filter(L >= 0 & L <= 100 & C >= 0) %>%
    mutate(color_value = hcl(h = H,
                             c = C,
                             l = L)) %>%
    mutate(x = C * cos(H * pi/180),
           y = C * sin(H * pi/180)) %>%
    mutate(perpendicular_from_C_L = x * sin(-H_point * pi/180) + y * cos(-H_point * pi/180),
           parallel_along_C_L = x * cos(-H_point * pi/180) - y * sin(-H_point * pi/180)) %>%
    mutate(row_value = sample(row_number(), n()),
           col_value = ceiling(row_value / sqrt(n_color))) %>%
    mutate(row_value = (row_value %% sqrt(n_color)) + 1) %>%
    mutate(L_cut = as.character(
      cut(L, breaks = c(-Inf, seq(L_point - width, 
                                  L_point + width, 
                                  length.out = 7), 
                        Inf),
          labels = c(L_point - width, 
                     seq(L_point - (width * .8), 
                         L_point + (width * .8), 
                         length.out = 6), 
                     L_point + width))),
      C_cut = as.character(
        cut(C,
            breaks = c(-Inf, 
                       seq(C_point - width, 
                           C_point + width, 
                           length.out = 7), 
                       Inf),
            labels = c(C_point - width, 
                       seq(C_point - (width * .8), 
                           C_point + (width * .8), 
                           length.out = 6), 
                       C_point + width))),
      H_cut = as.character(
        cut((180 - abs(abs(H - H_point) - 180)) * 
              sign(180 - abs(H - H_point)) * 
              sign(H - H_point),
            breaks = c(-Inf, 
                       seq(0 - width, 
                           0 + width, 
                           length.out = 7), 
                       Inf),
            labels = c(H_point - width, 
                       seq(H_point - (width * .8), 
                           H_point + (width * .8), 
                           length.out = 6), 
                       H_point + width) %% 360)),
      perpendicular_from_C_L_cut = as.character(
        cut(perpendicular_from_C_L,
            breaks = c(-Inf, seq(0 - width, 
                                 0 + width, 
                                 length.out = 7), 
                       Inf),
            labels = c(0 - width, 
                       seq(0 - (width * .8), 
                           0 + (width * .8), 
                           length.out = 6), 
                       0 + width))),
      parallel_along_C_L_cut = as.character(
        cut(parallel_along_C_L,
            breaks = c(-Inf, 
                       seq(C_point - width, 
                           C_point + width, 
                           length.out = 7), 
                       Inf),
            labels = c(C_point - width, 
                       seq(C_point - (width * .8), 
                           C_point + (width * .8), 
                           length.out = 6), 
                       C_point + width)))) %>%
    mutate(L_cut = as.factor(as.numeric(L_cut)),
           C_cut = as.factor(as.numeric(C_cut)),
           H_cut = as.factor(as.numeric(H_cut)),
           perpendicular_from_C_L_cut = 
             as.factor(as.numeric(perpendicular_from_C_L_cut)),
           parallel_along_C_L_cut = 
             as.factor(as.numeric(parallel_along_C_L_cut))) %>%
    mutate(H_cut = fct_expand(H_cut, 
                              as.character(c(H_point - width, 
                                             seq(H_point - (width * .8), 
                                                 H_point + (width * .8), 
                                                 length.out = 6), 
                                             H_point + width) %% 360)),
           perpendicular_from_C_L_cut = 
             fct_expand(perpendicular_from_C_L_cut,
                        as.character(c(0 - width, 
                                       seq(0 - (width * .8), 
                                           0 + (width * .8), 
                                           length.out = 6), 
                                       0 + width)))) %>%
    mutate(H_cut = fct_relevel(H_cut, 
                               as.character(c(H_point - width, 
                                              seq(H_point - (width * .8), 
                                                  H_point + (width * .8), 
                                                  length.out = 6), 
                                              H_point + width) %% 360)),
           perpendicular_from_C_L_cut = 
             fct_relevel(perpendicular_from_C_L_cut,
                         as.character(c(0 - width, 
                                        seq(0 - (width * .8), 
                                            0 + (width * .8), 
                                            length.out = 6), 
                                        0 + width)))) %>%
    mutate(H_cut = fct_rev(H_cut),
           perpendicular_from_C_L_cut = fct_rev(perpendicular_from_C_L_cut))
}

xyl <- get_xyl_data(H_point, C_point, L_point, width, n_color)

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
p2 <- graph_sample(xyl)
p1 + p2
ggsave(here::here("output", "color_compare", "xyl_info.png"), 
       width = 5, height = 3)

# C L plane by H
graph_C_L_by_H <- function(color_points) {
  ggplot(data = color_points, aes(C, L, 
                                  col = color_value, fill = color_value)) +
    geom_point() +
    scale_color_identity() +
    scale_fill_identity() +
    facet_wrap(~ H_cut, nrow = 2) +
    coord_equal() 
}

get_C_L_plane_by_H <- function(color_points, H_point, width) {
  map_dfr(as.numeric(as.character(unique(color_points$H_cut))), 
          get_C_L_plane) %>%
    mutate(H_cut = as.factor(H)) %>%
    mutate(H_cut = fct_expand(H_cut, 
                              as.character(c(H_point - width, 
                                             seq(H_point - (width * .8), 
                                                 H_point + (width * .8), 
                                                 length.out = 6), 
                                             H_point + width) %% 360))) %>%
    mutate(H_cut = fct_relevel(H_cut, 
                               as.character(c(H_point - width, 
                                              seq(H_point - (width * .8), 
                                                  H_point + (width * .8), 
                                                  length.out = 6), 
                                              H_point + width) %% 360))) %>%
    mutate(H_cut = fct_rev(H_cut))
}
C_L_plane_by_H <- get_C_L_plane_by_H(xyl, H_point, width)

graph_C_L_plane_by_H <- function(C_L_plane, color_points) {
  ggplot() +
    geom_point(data = C_L_plane,
               aes(C, L, color = color_value, fill = color_value)) +
    geom_point(data = color_points,
               aes(C, L, color = "white", fill = "white")) +
    scale_x_continuous(labels = abs) +
    scale_color_identity() +
    scale_fill_identity() +
    facet_wrap(~ H_cut, nrow = 2) +
    coord_equal()
}
graph_C_L_plane_by_H(C_L_plane_by_H, xyl)
ggsave(here::here("output", "color_compare", "xyl_c_l_plane.png"), 
       width = 5, height = 2.5)
graph_C_L_by_H(xyl)
ggsave(here::here("output", "color_compare", "xyl_c_l.png"), 
       width = 5, height = 3)

# H L curve
graph_H_L_by_C <- function(color_points, H_point) {
  ggplot(data = color_points, aes((H + (180 - H_point)) %% 360, L, 
                                  color = color_value, fill = color_value)) +
    geom_point() +
    scale_color_identity() +
    scale_fill_identity() +
    scale_x_reverse('H', # Like you're standing on the inside
                    labels = label_H_center(H_point = H_point)) +
    facet_wrap(~ C_cut, nrow = 2) +
    coord_equal()
}

get_H_L_curve_by_C <- function(color_points) {
  map_dfr(as.numeric(as.character(unique(color_points$C_cut))), get_H_L_curve) %>%
    mutate(C_cut = as.factor(C))
}
H_L_curve_by_C <- get_H_L_curve_by_C(xyl)

graph_H_L_curve_by_C <- function(H_L_curve_by_C, color_points, H_point) {
  ggplot() +
    geom_point(data = H_L_curve_by_C,
               aes((H + (180 - H_point)) %% 360, L, 
                   color = color_value, fill = color_value)) +
    geom_point(data = color_points,
               aes((H + (180 - H_point)) %% 360, L, 
                   color = "white", fill = "white")) +
    scale_color_identity() +
    scale_fill_identity() +
    scale_x_reverse('H', # Like you're standing on the inside
                    labels = label_H_center(H_point = H_point),
                    limits = c(360, 0)) +
    facet_wrap(~ C_cut, nrow = 2) +
    coord_equal()
}
graph_H_L_curve_by_C(H_L_curve_by_C, xyl, H_point)
ggsave(here::here("output", "color_compare", "xyl_h_l_curve.png"), 
       width = 5, height = 2)
graph_H_L_by_C(xyl, H_point)
ggsave(here::here("output", "color_compare", "xyl_h_l.png"), 
       width = 5, height = 2.5)

# H C plane by L
graph_x_y_by_L <- function(color_points, H_point) {
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
    facet_wrap(~ L_cut, nrow = 2) +
    coord_equal() +
    theme(axis.line=element_blank(), axis.text.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks=element_blank(),
          axis.title.x=element_blank(), axis.title.y=element_blank(),
          panel.grid.major=element_blank(), panel.grid.minor=element_blank())
}

get_H_C_plane_by_L <- function(color_points) {
  map_dfr(as.numeric(
    as.character(unique(color_points$L_cut))), get_H_C_plane) %>%
    mutate(L_cut = as.factor(L))
}
H_C_plane_by_L <- get_H_C_plane_by_L(xyl)

graph_H_C_plane_by_L <- function(H_C_plane_by_L, color_points) {
  ggplot() +
    geom_point(data = H_C_plane_by_L,
               aes(H, C, color = color_value, fill = color_value)) +
    geom_point(data = color_points,
               aes(H , C, color = "white", fill = "white")) +
    scale_color_identity() +
    scale_fill_identity() +
    scale_x_continuous(breaks = seq(45, 360, 45),
                       minor_breaks = seq(0, 315, 45) + 45/2,
                       labels = c('45', '90', '135', '180', 
                                  '225', '270', '315', '0|360')) +
    scale_y_continuous(limits = c(0, 180)) +
    facet_wrap(~ L_cut, nrow = 2) +
    coord_polar(start = 270 * pi / 180,
                direction = -1)
}
graph_H_C_plane_by_L(H_C_plane_by_L, xyl)
ggsave(here::here("output", "color_compare", "xyl_h_c_plane.png"), 
       width = 5, height = 4)
graph_x_y_by_L(xyl, H_point)
ggsave(here::here("output", "color_compare", "xyl_h_c.png"), 
       width = 5, height = 3)


# C tangent plane
label_perpendicular_from_C_L_cut<- function(x) {
  as.character(abs(as.numeric(x)))
}

graph_parallel_perpendicular_by_L <- function(color_points) {
  ggplot(data = color_points, 
         aes(x = parallel_along_C_L,
             y = perpendicular_from_C_L,
             color = color_value)) +
    geom_point() +
    scale_color_identity() +
    scale_y_continuous("Distance Perpendicular to C-L Plane", # similar to H
                       labels = abs) +
    labs(x = "Distance Parallel to C-L Plane") + # similar to C
    facet_wrap(~ L_cut, nrow = 2) +
    coord_equal()
}
graph_parallel_perpendicular_by_L(xyl)
ggsave(here::here("output", "color_compare", "xyl_par_per.png"), 
       width = 5, height = 4)

graph_perpendicular_L_by_parallel <- function(color_points) {
  ggplot(data = color_points, 
         aes(x = perpendicular_from_C_L,
             y = L,
             color = color_value)) +
    geom_point() +
    scale_color_identity() +
    scale_x_continuous("Distance Perpendicular to C-L Plane",
                       labels = abs) +
    scale_y_continuous(labels = abs) +
    facet_wrap(~ parallel_along_C_L_cut, nrow = 2) +
    coord_equal()
}
graph_perpendicular_L_by_parallel(xyl)
ggsave(here::here("output", "color_compare", "xyl_per_l.png"), 
       width = 5, height = 4)

graph_parallel_L_by_perpendicular <- function(color_points) {
  ggplot(data = color_points, 
         aes(x = parallel_along_C_L,
             y = L,
             color = color_value)) +
    geom_point() +
    scale_color_identity() +
    labs(x = "Distance Parallel to C-L Plane") +
    facet_wrap(~ perpendicular_from_C_L_cut, nrow = 2,
               labeller = as_labeller(label_perpendicular_from_C_L_cut)) +
    coord_equal()
}
graph_parallel_L_by_perpendicular(xyl)
ggsave(here::here("output", "color_compare", "xyl_par_l.png"), 
       width = 5, height = 4)

##------------------
# H, C, L, perimeter
##------------------
get_hcl_data <- function(H_point, C_point, L_point, width, n_color) {
 data.frame(theta = 2 * pi * seq(0, n_color - 1) / ((1 + sqrt(5)) / 2),
                  phi = acos(1 - 2 * (seq(0, n_color - 1) + .5) / n_color)) %>%
  mutate(H = cos(theta) * sin(phi),
         C = sin(theta) * sin(phi),
         L = cos(phi)) %>%
  mutate(H = H * width + H_point,
         C = C * width + C_point,
         L = L * width + L_point) %>%
  mutate(H = H %% 360) %>% # not really needed except for graphs
  filter(L >= 0 & L <= 100 & C >= 0) %>%
  mutate(color_value = hcl(h = H,
                           c = C,
                           l = L)) %>%
  mutate(x = C * cos(H * pi/180),
         y = C * sin(H * pi/180)) %>%
  mutate(perpendicular_from_C_L = x * sin(-H_point * pi/180) + 
           y * cos(-H_point * pi/180), # not a C value since we didn't rotate by H
         parallel_along_C_L = x * cos(-H_point * pi/180) - 
           y * sin(-H_point * pi/180)) %>%
  mutate(row_value = sample(row_number(), n()),
         col_value = ceiling(row_value / sqrt(n_color))) %>%
  mutate(row_value = (row_value %% sqrt(n_color)) + 1) %>%
  mutate(L_cut = as.character(
    cut(L, breaks = c(-Inf, seq(L_point - width, 
                                L_point + width, 
                                length.out = 7), 
                      Inf),
        labels = c(L_point - width, 
                   seq(L_point - (width * .8), 
                       L_point + (width * .8), 
                       length.out = 6), 
                   L_point + width))),
         C_cut = as.character(
           cut(C,
               breaks = c(-Inf, 
                          seq(C_point - width, 
                              C_point + width, 
                              length.out = 7), 
                          Inf),
               labels = c(C_point - width, 
                          seq(C_point - (width * .8), 
                              C_point + (width * .8), 
                              length.out = 6), 
                          C_point + width))),
         H_cut = as.character(
           cut((180 - abs(abs(H - H_point) - 180)) * 
                 sign(180 - abs(H - H_point)) * 
                 sign(H - H_point),
               breaks = c(-Inf, 
                          seq(0 - width, 
                              0 + width, 
                              length.out = 7), 
                          Inf),
               labels = c(H_point - width, 
                          seq(H_point - (width * .8), 
                              H_point + (width * .8), 
                              length.out = 6), 
                          H_point + width) %% 360)),
         perpendicular_from_C_L_cut = as.character(
           cut(perpendicular_from_C_L,
               breaks = c(-Inf, seq(0 - width, 
                                    0 + width, 
                                    length.out = 7), 
                          Inf),
               labels = c(0 - width, 
                          seq(0 - (width * .8), 
                              0 + (width * .8), 
                              length.out = 6), 
                          0 + width))),
         parallel_along_C_L_cut = as.character(
           cut(parallel_along_C_L,
               breaks = c(-Inf, 
                          seq(C_point - width, 
                              C_point + width, 
                              length.out = 7), 
                          Inf),
               labels = c(C_point - width, 
                          seq(C_point - (width * .8), 
                              C_point + (width * .8), 
                              length.out = 6), 
                          C_point + width)))) %>%
  mutate(L_cut = as.factor(as.numeric(L_cut)),
         C_cut = as.factor(as.numeric(C_cut)),
         H_cut = as.factor(as.numeric(H_cut)),
         perpendicular_from_C_L_cut = 
           as.factor(as.numeric(perpendicular_from_C_L_cut)),
         parallel_along_C_L_cut = 
           as.factor(as.numeric(parallel_along_C_L_cut))) %>%
  mutate(H_cut = fct_expand(H_cut, 
                            as.character(c(H_point - width, 
                                           seq(H_point - (width * .8), 
                                               H_point + (width * .8), 
                                               length.out = 6), 
                                           H_point + width) %% 360)),
         perpendicular_from_C_L_cut = 
           fct_expand(perpendicular_from_C_L_cut,
                      as.character(c(0 - width, 
                                     seq(0 - (width * .8), 
                                         0 + (width * .8), 
                                         length.out = 6), 
                                     0 + width)))) %>%
  mutate(H_cut = fct_relevel(H_cut, 
                             as.character(c(H_point - width, 
                                            seq(H_point - (width * .8), 
                                                H_point + (width * .8), 
                                                length.out = 6), 
                                            H_point + width) %% 360)),
         perpendicular_from_C_L_cut = 
           fct_relevel(perpendicular_from_C_L_cut,
                      as.character(c(0 - width, 
                                     seq(0 - (width * .8), 
                                         0 + (width * .8), 
                                         length.out = 6), 
                                     0 + width)))) %>%
  mutate(H_cut = fct_rev(H_cut),
         perpendicular_from_C_L_cut = fct_rev(perpendicular_from_C_L_cut))
}

hcl <- get_hcl_data(H_point, C_point, L_point, width, n_color)

p1 <- graph_info(H_point, C_point, L_point)
p2 <- graph_sample(hcl)
p1 + p2
ggsave(here::here("output", "color_compare", "hcl_info.png"), 
       width = 5, height = 3)

# C L plane by H
C_L_plane_by_H <- get_C_L_plane_by_H(hcl, H_point, width)
graph_C_L_by_H(hcl)
ggsave(here::here("output", "color_compare", "hcl_c_l.png"), 
       width = 5, height = 4)
graph_C_L_plane_by_H(C_L_plane_by_H, hcl)
ggsave(here::here("output", "color_compare", "hcl_c_l_plane.png"), 
       width = 5, height = 3)

# H L curve
H_L_curve_by_C <- get_H_L_curve_by_C(hcl)
graph_H_L_by_C(hcl, H_point)
ggsave(here::here("output", "color_compare", "hcl_h_l.png"), 
       width = 5, height = 4)
graph_H_L_curve_by_C(H_L_curve_by_C, hcl, H_point)
ggsave(here::here("output", "color_compare", "hcl_h_l_curve.png"), 
       width = 5, height = 2)

# H C plane by L
H_C_plane_by_L <- get_H_C_plane_by_L(hcl)
graph_x_y_by_L(hcl, H_point)
ggsave(here::here("output", "color_compare", "hcl_h_c.png"), 
       width = 5, height = 3)
graph_H_C_plane_by_L(H_C_plane_by_L, hcl)
ggsave(here::here("output", "color_compare", "hcl_h_c_plane.png"), 
       width = 5, height = 4)

# C tangent plane
graph_parallel_perpendicular_by_L(hcl)
ggsave(here::here("output", "color_compare", "hcl_par_per.png"), 
       width = 5, height = 3)

graph_perpendicular_L_by_parallel(hcl)
ggsave(here::here("output", "color_compare", "hcl_per_l.png"), 
       width = 5, height = 6)

graph_parallel_L_by_perpendicular(hcl)
ggsave(here::here("output", "color_compare", "hcl_par_l.png"), 
       width = 5, height = 5)

##-------
# Compare
##-------
compare <- rbind(xyl[, c('H', 'C', 'L', 'x', 'y', 
                         'H_cut', 'C_cut', 'L_cut', 
                         'perpendicular_from_C_L', 'parallel_along_C_L', 
                         'perpendicular_from_C_L_cut', 'parallel_along_C_L_cut',
                         'row_value', 'col_value', 'color_value')],
                 hcl[, c('H', 'C', 'L', 'x', 'y', 
                         'H_cut', 'C_cut', 'L_cut', 
                         'perpendicular_from_C_L', 'parallel_along_C_L', 
                         'perpendicular_from_C_L_cut', 'parallel_along_C_L_cut',
                         'row_value', 'col_value', 'color_value')]) %>%
  mutate(setting = c(rep('XYL', n_color), rep('HCL', n_color))) 

# C L plane
graph_C_L_by_H_comparison <- function(color_points) {
  ggplot(data = color_points, aes(C, L, col = setting)) +
    geom_point(alpha = .5) +
    facet_wrap(~ H_cut, nrow = 2) +
    coord_equal() +
    scale_color_discrete("") +
    theme(legend.position = "bottom")
}
graph_C_L_by_H_comparison(compare)
ggsave(here::here("output", "color_compare", "compare_c_l.png"), 
       width = 5, height = 4)

# H L curve
graph_H_L_by_C_comparison <- function(color_points, H_point) {
  ggplot(data = color_points, 
         aes((H + (180 - H_point)) %% 360, L, color = setting)) +
    geom_point(alpha = .5) +
    scale_x_reverse('H',
                    labels = label_H_center(H_point = H_point)) +
    facet_wrap(~ C_cut, nrow = 2) +
    coord_equal() +
    scale_color_discrete("") +
    theme(legend.position = "bottom")
}
graph_H_L_by_C_comparison(compare, H_point)
ggsave(here::here("output", "color_compare", "compare_h_l.png"), 
       width = 5, height = 3)

# H C plane
graph_x_y_by_L_comparison <- function(color_points, H_point) {
  ggplot(data = color_points, aes(x, y, col = setting)) +
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
    geom_point(alpha = .5) +
    facet_wrap(~ L_cut, nrow = 2) +
    coord_equal() +
    scale_color_discrete("") +
    theme(legend.position = "bottom",
          axis.line=element_blank(), axis.text.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks=element_blank(),
          axis.title.x=element_blank(), axis.title.y=element_blank(),
          panel.grid.major=element_blank(), panel.grid.minor=element_blank())
}
graph_x_y_by_L_comparison(compare, H_point)
ggsave(here::here("output", "color_compare", "compare_h_c.png"), 
       width = 5, height = 4)

# C tangent plane
graph_parallel_perpendicular_by_L_comparison <- function(color_points) {
  ggplot(data = color_points, 
         aes(x = parallel_along_C_L,
             y = perpendicular_from_C_L,
             color = setting)) +
    geom_point(alpha = .5) +
    scale_y_continuous("Distance Perpendicular to C-L Plane",
                       labels = abs) +
    labs(x = "Distance Parallel to C-L Plane") +
    facet_wrap(~ L_cut, nrow = 2) +
    coord_equal() +
    scale_color_discrete("") +
    theme(legend.position = "bottom")
}
graph_parallel_perpendicular_by_L_comparison(compare)
ggsave(here::here("output", "color_compare", "compare_par_per.png"), 
       width = 5, height = 4)

graph_perpendicular_L_by_parallel_comparison <- function(color_points) {
  ggplot(data = color_points, 
         aes(x = perpendicular_from_C_L,
             y = L,
             color = setting)) +
    geom_point(alpha = .5) +
    scale_x_continuous("Distance Perpendicular to C-L Plane", 
            labels = abs) +
    scale_y_continuous(labels = abs) +
    facet_wrap(~ parallel_along_C_L_cut, nrow = 2) +
    coord_equal() +
    scale_color_discrete("") +
    theme(legend.position = "bottom")
}
graph_perpendicular_L_by_parallel_comparison(compare)
ggsave(here::here("output", "color_compare", "compare_per_l.png"), 
       width = 5, height = 4)

graph_parallel_L_by_perpendicular_comparison <- function(color_points) {
  ggplot(data = color_points, 
         aes(x = parallel_along_C_L,
             y = L,
             color = setting)) +
    geom_point(alpha = .5) +
    labs(x = "Distance Parallel to C-L Plane") +
    facet_wrap(~ perpendicular_from_C_L_cut, nrow = 2,
               labeller = as_labeller(label_perpendicular_from_C_L_cut)) +
    coord_equal() +
    scale_color_discrete("") +
    theme(legend.position = "bottom")
}
graph_parallel_L_by_perpendicular_comparison(compare)
ggsave(here::here("output", "color_compare", "compare_par_l.png"), 
       width = 5, height = 5)

compare <- compare %>%
  group_by(setting) %>%
  arrange((180 - abs(abs(H - H_point) - 180)) * 
            sign(180 - abs(H - H_point)) * sign(H - H_point)) %>%
  mutate(col_value = ceiling(row_number() / sqrt(n_color))) %>%
  arrange(col_value, L) %>%
  group_by(setting, col_value) %>%
  mutate(row_value = row_number())

ggplot(data = compare,
       aes(x = col_value,
           y = row_value,
           fill = color_value)) +
  geom_tile() +
  coord_equal() +
  scale_fill_identity() +
  theme_void() +
  facet_grid(~setting)
ggsave(here::here("output", "color_compare", "compare_samples.png"), 
       width = 5, height = 2.5)

compare <- compare %>%
  group_by(setting) %>%
  arrange((180 - abs(abs(H - H_point) - 180)) * 
            sign(180 - abs(H - H_point)) * sign(H - H_point)) %>%
  mutate(col_value = ceiling(row_number() / sqrt(n_color))) %>%
  arrange(col_value, C) %>%
  group_by(setting, col_value) %>%
  mutate(row_value = row_number())

ggplot(data = compare,
       aes(x = col_value,
           y = row_value,
           fill = color_value)) +
  geom_tile() +
  coord_equal() +
  scale_fill_identity() +
  theme_void() +
  facet_grid(~setting)
ggsave(here::here("output", "color_compare", "compare_samples2.png"), 
       width = 5, height = 2.5)

## Try lower value of C ----
C_point <- 16

hcl <- get_hcl_data(H_point, C_point, L_point, width, n_color)

xyl <- get_xyl_data(H_point, C_point, L_point, width, n_color)

compare <- rbind(xyl[, c('H', 'C', 'L', 'x', 'y', 
                         'H_cut', 'C_cut', 'L_cut', 
                         'perpendicular_from_C_L', 'parallel_along_C_L', 
                         'perpendicular_from_C_L_cut', 'parallel_along_C_L_cut',
                         'row_value', 'col_value', 'color_value')],
                 hcl[, c('H', 'C', 'L', 'x', 'y', 
                         'H_cut', 'C_cut', 'L_cut', 
                         'perpendicular_from_C_L', 'parallel_along_C_L', 
                         'perpendicular_from_C_L_cut', 'parallel_along_C_L_cut',
                         'row_value', 'col_value', 'color_value')]) %>%
  mutate(setting = c(rep('XYL', n_color), rep('HCL', n_color))) 

# C L plane
graph_C_L_by_H_comparison(compare)
ggsave(here::here("output", "color_compare", "C_lower_c_l.png"), 
       width = 5, height = 4)

# H L curve
graph_H_L_by_C_comparison(compare, H_point)
ggsave(here::here("output", "color_compare", "C_lower_h_l.png"), 
       width = 5, height = 2.5)

# H C plane
graph_x_y_by_L_comparison(compare, H_point)
ggsave(here::here("output", "color_compare", "C_lower_h_c.png"), 
       width = 5, height = 4)

# C tangent plane
graph_parallel_perpendicular_by_L_comparison(compare)
ggsave(here::here("output", "color_compare", "C_lower_par_per.png"), 
       width = 5, height = 4)

graph_perpendicular_L_by_parallel_comparison(compare)
ggsave(here::here("output", "color_compare", "C_lower_per_l.png"), 
       width = 5, height = 4)

graph_parallel_L_by_perpendicular_comparison(compare)
ggsave(here::here("output", "color_compare", "C_lower_par_l.png"), 
       width = 5, height = 4)

compare <- compare %>%
  group_by(setting) %>%
  arrange((180 - abs(abs(H - H_point) - 180)) * 
            sign(180 - abs(H - H_point)) * sign(H - H_point)) %>%
  mutate(col_value = ceiling(row_number() / sqrt(n_color))) %>%
  arrange(col_value, L) %>%
  group_by(setting, col_value) %>%
  mutate(row_value = row_number())

ggplot(data = compare,
       aes(x = col_value,
           y = row_value,
           fill = color_value)) +
  geom_tile() +
  coord_equal() +
  scale_fill_identity() +
  theme_void() +
  facet_grid(~setting)
ggsave(here::here("output", "color_compare", "C_lower_samples.png"), 
       width = 5, height = 2.5)

compare <- compare %>%
  group_by(setting) %>%
  arrange((180 - abs(abs(H - H_point) - 180)) * 
            sign(180 - abs(H - H_point)) * sign(H - H_point)) %>%
  mutate(col_value = ceiling(row_number() / sqrt(n_color))) %>%
  arrange(col_value, C) %>%
  group_by(setting, col_value) %>%
  mutate(row_value = row_number())

ggplot(data = compare,
       aes(x = col_value,
           y = row_value,
           fill = color_value)) +
  geom_tile() +
  coord_equal() +
  scale_fill_identity() +
  theme_void() +
  facet_grid(~setting)
ggsave(here::here("output", "color_compare", "C_lower_samples2.png"), 
       width = 5, height = 2.5)

## Try higher value of C ----
C_point <- 75
 
hcl <- get_hcl_data(H_point, C_point, L_point, width, n_color)

xyl <- get_xyl_data(H_point, C_point, L_point, width, n_color)

compare <- rbind(xyl[, c('H', 'C', 'L', 'x', 'y', 
                         'H_cut', 'C_cut', 'L_cut', 
                         'perpendicular_from_C_L', 'parallel_along_C_L', 
                         'perpendicular_from_C_L_cut', 'parallel_along_C_L_cut',
                         'row_value', 'col_value', 'color_value')],
                 hcl[, c('H', 'C', 'L', 'x', 'y', 
                         'H_cut', 'C_cut', 'L_cut', 
                         'perpendicular_from_C_L', 'parallel_along_C_L', 
                         'perpendicular_from_C_L_cut', 'parallel_along_C_L_cut',
                         'row_value', 'col_value', 'color_value')]) %>%
  mutate(setting = c(rep('XYL', n_color), rep('HCL', n_color)))

# C L plane
graph_C_L_by_H_comparison(compare)
ggsave(here::here("output", "color_compare", "C_higher_c_l.png"), 
       width = 5, height = 4)

# H L curve
graph_H_L_by_C_comparison(compare, H_point)
ggsave(here::here("output", "color_compare", "C_higher_h_l.png"), 
       width = 5, height = 4)

# H C plane
graph_x_y_by_L_comparison(compare, H_point)
ggsave(here::here("output", "color_compare", "C_higher_h_c.png"), 
       width = 5, height = 4)

# C tangent plane
graph_parallel_perpendicular_by_L_comparison(compare)
ggsave(here::here("output", "color_compare", "C_higher_par_per.png"), 
       width = 5, height = 5)

graph_perpendicular_L_by_parallel_comparison(compare)
ggsave(here::here("output", "color_compare", "C_higher_per_l.png"), 
       width = 5, height = 4)

graph_parallel_L_by_perpendicular_comparison(compare)
ggsave(here::here("output", "color_compare", "C_higher_par_l.png"), 
       width = 5, height = 4)

compare <- compare %>%
  group_by(setting) %>%
  arrange((180 - abs(abs(H - H_point) - 180)) * 
            sign(180 - abs(H - H_point)) * sign(H - H_point)) %>%
  mutate(col_value = ceiling(row_number() / sqrt(n_color))) %>%
  arrange(col_value, L) %>%
  group_by(setting, col_value) %>%
  mutate(row_value = row_number())

ggplot(data = compare,
       aes(x = col_value,
           y = row_value,
           fill = color_value)) +
  geom_tile() +
  coord_equal() +
  scale_fill_identity() +
  theme_void() +
  facet_grid(~setting)
ggsave(here::here("output", "color_compare", "C_higher_samples.png"), 
       width = 5, height = 2.5)

compare <- compare %>%
  group_by(setting) %>%
  arrange((180 - abs(abs(H - H_point) - 180)) * 
            sign(180 - abs(H - H_point)) * sign(H - H_point)) %>%
  mutate(col_value = ceiling(row_number() / sqrt(n_color))) %>%
  arrange(col_value, C) %>%
  group_by(setting, col_value) %>%
  mutate(row_value = row_number())

ggplot(data = compare,
       aes(x = col_value,
           y = row_value,
           fill = color_value)) +
  geom_tile() +
  coord_equal() +
  scale_fill_identity() +
  theme_void() +
  facet_grid(~setting)
ggsave(here::here("output", "color_compare", "C_higher_samples2.png"), 
       width = 5, height = 2.5)
