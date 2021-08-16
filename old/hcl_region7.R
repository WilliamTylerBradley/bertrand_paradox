library(colorspace)
library(tidyverse)
# http://hclwizard.org/hclcolorpicker/

color <- '#FFD700'
# color <- '#D4AF37'
# color <- '#9B111E'
# color <- '#50C878'
# color <- '#0F52BA'

#test
color <- '#AC9665'
color <- as(hex2RGB(color), "polarLUV")

L_target <- 25
## transformation angle = angle between coords(color)[, 'L'] and L_target
L_target_theta = -atan(coords(color)[, 'C'] / (coords(color)[, 'L'] - 25))

## Choose some colors at random
n_random = 10000

l_range <- 20
c_range <- 12
h_range <- 3

# https://math.stackexchange.com/questions/87230/picking-random-points-in-the-volume-of-sphere-with-uniform-probability/87238#87238
df_random <- data.frame(L = rnorm(n = n_random * 10),
                        C = rnorm(n = n_random * 10),
                        H = rnorm(n = n_random * 10),
                        U = runif(n = n_random * 10)^(1/3))

df_random <- df_random %>%
  mutate(normalize = sqrt(L^2 + C^2 + H^2)) %>%
  mutate(L = L * U / normalize,
         C = C * U / normalize,
         H = H * U / normalize) %>%
  select(-U, -normalize)

# stretch
df_random <- df_random %>%
  mutate(L = L * l_range,
         C = C * c_range,
         H = H * h_range)

# rotate
df_random <- df_random %>%
  mutate(L_trans = L * cos(L_target_theta) + C * sin(L_target_theta),
         C_trans = L * -sin(L_target_theta) + C * cos(L_target_theta)) %>%
  mutate(L = L_trans,
         C = C_trans) %>%
  select(-L_trans, -C_trans) 

# move
df_random <- df_random %>%
  mutate(L = L + coords(color)[, 'L'],
         C = C + coords(color)[, 'C'],
         H = H + coords(color)[, 'H'])

# color and check
df_random <- df_random %>%
  mutate(hex_color = hex(polarLUV(L, C, H))) %>%
  filter(!is.na(hex_color))

df_random <- df_random[1:n_random, ]

df_random <- df_random %>%
  mutate(x = (row_number() - 1) %/% round(sqrt(n()))) %>%
  group_by(x) %>%
  mutate(y = row_number()) %>%
  ungroup()

ggplot(data = df_random, aes(x, y)) +
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
                                                                     seq(0.1, 1, length.out = 16)), 2))))) %>%
  mutate(x_graph = C * cos(H * (pi / 180)),
         y_graph = C * sin(H * (pi / 180)) )

ggplot(data = df_random, aes(C, L)) +
  geom_point(col = df_random$hex_color) +
  facet_wrap(~ H_cut) +
  coord_equal() 

ggplot(data = df_random, aes(x_graph, y_graph)) +
  geom_point(col = df_random$hex_color) +
  facet_wrap(~ L_cut) +
  coord_equal() 

ggplot(data = df_random, aes(H, L)) +
  geom_point(col = df_random$hex_color) +
  facet_wrap(~ C_cut) +
  coord_equal() 
