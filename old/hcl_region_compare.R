library(colorspace)
library(tidyverse)
# http://hclwizard.org/hclcolorpicker/

 color <- '#D4AF37'
 color <- '#9B111E'
# color <- '#50C878'
# color <- '#0F52BA'

#test
#color <- '#AC9665'
color <- as(hex2RGB(color), "polarLUV")

L_target <- 25
## transformation angle = angle between coords(color)[, 'L'] and L_target
L_target_theta = -atan(coords(color)[, 'C'] / (coords(color)[, 'L'] - 25))

## Choose some colors at random
n_random = 10000

l_range <- 20
c_range <- 12
h_range <- 3
## need to figure out h_range for value of color
# needs to be cord at that color
h_range_xy <- coords(color)[, 'C'] * (h_range * (pi/180) * 2)
h_range_xy <- coords(color)[, 'C'] * sin(h_range * (pi/180))


# https://math.stackexchange.com/questions/87230/picking-random-points-in-the-volume-of-sphere-with-uniform-probability/87238#87238
df_random <- data.frame(L = rnorm(n = n_random * 10), # * 10 because some will
                        C = rnorm(n = n_random * 10),   #   get dropped
                        H = rnorm(n = n_random * 10),
                        U = runif(n = n_random * 10)^(1/3)) %>%
  mutate(normalize = sqrt(L^2 + C^2 + H^2)) %>%
  mutate(L = L * U / normalize,
         C = C * U / normalize,
         H = H * U / normalize) %>%
  select(-U, -normalize) %>% # have random points in an ellipse here
  mutate(L = L * l_range, # stretch
         C = C * c_range,
         H = H * h_range) %>%
  mutate(L_trans = L * cos(L_target_theta) + C * sin(L_target_theta), # tilt
         C_trans = L * -sin(L_target_theta) + C * cos(L_target_theta)) %>%
  mutate(L = L_trans,
         C = C_trans) %>%
  select(-L_trans, -C_trans) %>%
  mutate(L = L + coords(color)[, 'L'], # move
         C = C + coords(color)[, 'C'],
         H = H + coords(color)[, 'H']) %>%
  mutate(hex_color = hex(polarLUV(L, C, H))) %>%
  filter(!is.na(hex_color)) %>% # check
  sample_n(n_random)

df_random <- df_random %>%
  mutate(x_tile = (row_number() - 1) %/% round(sqrt(n()))) %>%
  group_by(x_tile) %>%
  mutate(y_tile = row_number()) %>%
  ungroup()

ggplot(data = df_random, aes(x_tile, y_tile)) +
  geom_tile(fill = df_random$hex_color) +
  coord_equal() +
  theme_void()

df_random <- df_random %>%
  mutate(L_cut = as.numeric(as.character(cut(L, 16, 
                                             labels = round(quantile(L, 
                                                                     seq(0.1, 1, length.out = 16)), 2)))),
         C_cut = as.numeric(as.character(cut(C, 16, 
                                             labels = round(quantile(C, 
                                                                     seq(0.1, 1, length.out = 16)), 2)))),
         H_cut = as.numeric(as.character(cut(H, 16, 
                                             labels = round(quantile(H, 
                                                                     seq(0.1, 1, length.out = 16)), 2)))))

ggplot(data = df_random, aes(C, L)) +
  geom_point(col = df_random$hex_color) +
  facet_wrap(~ H_cut) +
  coord_equal() 

ggplot(data = df_random, aes(x, y)) +
  geom_point(col = df_random$hex_color) +
  facet_wrap(~ L_cut) +
  coord_equal() 

ggplot(data = df_random, aes(H, L)) +
  geom_point(col = df_random$hex_color) +
  facet_wrap(~ C_cut) +
  coord_equal() 

## Compare against hcl_region7

## check ----
# df_random <- df_random %>%
#   mutate(L_cut = as.numeric(as.character(cut(L, 16,
#                                              labels = round(quantile(L,
#                                                                      seq(0.1, 1, length.out = 16)), 2)))),
#          y_cut = as.numeric(as.character(cut(y, 16,
#                                              labels = round(quantile(y,
#                                                                      seq(0.1, 1, length.out = 16)), 2)))),
#          x_cut = as.numeric(as.character(cut(x, 16,
#                                              labels = round(quantile(x,
#                                                                      seq(0.1, 1, length.out = 16)), 2)))))
# 
# ggplot(data = df_random[1:n_random, ], aes(y, L)) +
#   geom_point() +
#   facet_wrap(~ x_cut) +
#   coord_equal()
# 
# ggplot(data = df_random[1:n_random, ], aes(x, y)) +
#   geom_point() +
#   facet_wrap(~ L_cut) +
#   coord_equal()
# 
# ggplot(data = df_random[1:n_random, ], aes(x, L)) +
#   geom_point() +
#   facet_wrap(~ y_cut) +
#   coord_equal()
# 
# df_random <- df_random %>%
#   select(-L_cut, -y_cut, -x_cut)
## ----
