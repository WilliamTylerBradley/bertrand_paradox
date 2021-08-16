library(colorspace)
library(tidyverse)
# http://hclwizard.org/hclcolorpicker/

color <- '#9B111E'
color <- '#843C40'
color <- as(hex2RGB(color), "polarLUV")

target_point <- max_chroma(h = coords(color)[, 'H'], l = seq(1, 100))
# ggplot(data = data.frame(C = L_target,
#                          L = seq(0, 100))) +
#   geom_line(aes(x = L, y = C))
target_point <- c(max(target_point), match(max(target_point), target_point))

## transformation angle = angle between coords(color)[, 'L'] and L_target
target_theta = atan2(target_point[1] - coords(color)[, 'C'], # subtract because we'll tilt before we move
                     target_point[2] - coords(color)[, 'L']) # flipped because we're moving from top of z down

## Choose some colors at random
n_random = 10000

l_range <- 20
c_range <- 12
h_range <- 3
## Get x, y distance from sides of h
h_range_x <- coords(color)[, 'C'] * cos((coords(color)[, 'H'] + c(-h_range, h_range)) * (pi/180))
h_range_y <- coords(color)[, 'C'] * sin((coords(color)[, 'H'] + c(-h_range, h_range)) * (pi/180))
h_range_xy <- sqrt((h_range_x[1] - h_range_x[2])^2 + (h_range_y[1] - h_range_y[2])^2) / 2


# https://math.stackexchange.com/questions/87230/picking-random-points-in-the-volume-of-sphere-with-uniform-probability/87238#87238
df_random <- data.frame(L = rnorm(n = n_random * 10),
                        y = rnorm(n = n_random * 10), # y apprx C
                        x = rnorm(n = n_random * 10), # x apprx H
                        U = runif(n = n_random * 10)^(1/3)) %>%
  mutate(normalize = sqrt(L^2 + y^2 + x^2)) %>%
  mutate(L = L * U / normalize,
         y = y * U / normalize,
         x = x * U / normalize) %>%
  select(-U, -normalize)
  
# ggplot(data = df_random,
#        aes(x = x,
#            y = y)) +
#   geom_text(aes(label = id)) +
#   coord_equal()
# ggplot(data = df_random,
#        aes(x = x,
#            y = L)) +
#   geom_point() +
#   coord_equal()
# ggplot(data = df_random,
#        aes(x = y,
#            y = L)) +
#   geom_point() +
#   coord_equal()
# df_random <- data.frame(L = c(1, 0, 0, 0, 0, 0, -1),
#                          y = c(0, 1, 0, 0, 0, -1, 0),
#                          x = c(0, 0, 1, 0, -1, 0, 0),
#                         id = seq(1, 7)) %>%
#   mutate(L_original = L,
#          y_original = y,
#          x_original = x)

# stretch 
df_random <- df_random %>%
  mutate(L = L * l_range,
         y = y * h_range_xy,
         x = x * c_range)

# tilt around y axis
df_random <- df_random %>%
  mutate(x_tilt = x * cos(target_theta) + L * sin(target_theta),
         L_tilt = x * -sin(target_theta) + L * cos(target_theta)) %>%
  mutate(x = x_tilt,
         L = L_tilt) %>%
  select(-x_tilt, -L_tilt)

# turn around z axis
df_random <- df_random %>%
  mutate(x_turn = x * cos(coords(color)[, 'H'] * (pi / 180)) - y * sin(coords(color)[, 'H'] * (pi / 180)),
         y_turn = x * sin(coords(color)[, 'H'] * (pi / 180)) + y * cos(coords(color)[, 'H'] * (pi / 180))) %>%
  mutate(x = x_turn,
         y = y_turn) %>%
  select(-x_turn, -y_turn)

# move
df_random <- df_random %>%
  mutate(x = x + coords(color)[, 'C'] * cos(coords(color)[, 'H'] * (pi / 180)),
         y = y + coords(color)[, 'C'] * sin(coords(color)[, 'H'] * (pi / 180)),
         L = L + coords(color)[, 'L']) 

# Convert to polar
df_random <- df_random %>%
  mutate(H = atan2(y, x) * 180/pi,
         C = sqrt(x^2 + y^2))

###################That is not right

# color and check
df_random <- df_random %>%
  mutate(hex_color = hex(polarLUV(L, C, H))) %>%
  filter(!is.na(hex_color)) %>%
  filter(H <= coords(color)[, 'H'] + h_range &
           H >= coords(color)[, 'H'] - h_range)
  # mutate(hex_color = if_else(H <= coords(color)[, 'H'] + h_range &
  #        H >= coords(color)[, 'H'] - h_range, hex_color, '#000000'))

df_random <- df_random[1:n_random, ]

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

# See if equal all around
ggplot(data = df_random, aes(C, L)) +
  geom_density_2d_filled() +
  facet_wrap(~ H_cut) +
  coord_equal() # not actually equal
ggplot(data = df_random, aes(C)) +
  geom_density() # but this is equal
ggplot(data = df_random, aes(C)) +
  geom_density() +
  facet_wrap(~ H_cut)
# because because the farther away h are small C, has to balance out
# so for the base line h, values need to have higher C
# grayer for farther away in H
# Try this with just a circle


ggplot(data = df_random, aes(x, y)) +
  geom_density_2d_filled() +
  facet_wrap(~ L_cut)
ggplot(data = df_random, aes(H, L)) +
  geom_density_2d_filled() +
  facet_wrap(~ C_cut)


## Fill in whole color space
full_colors <- expand.grid(H_cut = unique(df_random$H_cut), C = seq(1, 200, by = 5), L = seq(1, 100, by = 5)) %>%
  mutate(hex_color = hex(polarLUV(L, C, H_cut))) %>%
  filter(!is.na(hex_color))
ggplot(data = full_colors, aes(C, L, col = hex_color, fill = hex_color)) +
  geom_tile() +
  geom_point(data = df_random, color = "white") +
  scale_color_identity() +
  scale_fill_identity() +
  facet_wrap(~ H_cut) +
  coord_equal() 

full_colors <- expand.grid(H = seq(1, 360, by = 10), 
                           C_cut = unique(df_random$C_cut),
                           L = seq(1, 100, by = 5)) %>%
  mutate(hex_color = hex(polarLUV(L, C_cut, H))) %>%
  filter(!is.na(hex_color))
ggplot(data = full_colors, aes(H, L, col = hex_color, fill = hex_color)) +
  geom_tile() +
  geom_point(data = df_random, color = "white") +
  scale_color_identity() +
  scale_fill_identity() +
  facet_wrap(~ C_cut) +
  coord_equal() 

full_colors <- expand.grid(x = seq(-200, 200, by = 2), 
                           y = seq(-200, 200, by = 2),
                           L_cut = unique(df_random$L_cut)) %>%
  mutate(C = (sqrt(y^2 + x^2)), # convert to polar
         H = atan2(y, x) * (180/pi)) %>%
  mutate(hex_color = hex(polarLUV(L_cut, C, H))) %>%
  filter(!is.na(hex_color))
ggplot(data = full_colors, aes(x, y, col = hex_color, fill = hex_color)) +
  geom_tile() +
  geom_point(data = df_random, color = "white") +
  scale_color_identity() +
  scale_fill_identity() +
  facet_wrap(~ L_cut) +
  coord_equal() 
