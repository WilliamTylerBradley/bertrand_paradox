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

L_target <- max_chroma(h = coords(color)[, 'H'], l = seq(1, 100))
# ggplot(data = data.frame(C = L_target,
#                          L = seq(0, 100))) +
#   geom_line(aes(x = L, y = C))
L_target <- match(max(L_target), L_target)

## transformation angle = angle between coords(color)[, 'L'] and L_target
L_target_theta = -atan(coords(color)[, 'C'] / (L_target - coords(color)[, 'L']))

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
df_random <- data.frame(L = rnorm(n = n_random * 10),
                        y = rnorm(n = n_random * 10), # y apprx C
                        x = rnorm(n = n_random * 10), # x apprx H
                        U = runif(n = n_random * 10)^(1/3)) %>%
  mutate(normalize = sqrt(L^2 + y^2 + x^2)) %>%
  mutate(L = L * U / normalize,
         y = y * U / normalize,
         x = x * U / normalize) %>%
  select(-U, -normalize) %>%
  mutate(L = L * l_range,
         y = y * c_range,
         x = x * h_range_xy)

# move
df_random <- df_random %>%
  mutate(y = y + coords(color)[, 'C']) %>% # move out to C
  mutate(C = (sqrt(y^2 + x^2)), # convert to polar
         H = atan2(y, x) * (180/pi) - 90) %>% ##################################### Why is there a minus 90 here?
  mutate(L = L + coords(color)[, 'L'], 
         H = H + coords(color)[, 'H']) %>%
  mutate(x = C * cos(H * (pi / 180)),
         y = C * sin(H * (pi / 180)) ) 
# %>%
#   filter(H <= coords(color)[, 'H'] + h_range & 
#          H >= coords(color)[, 'H'] - h_range)

# rotate
df_random <- df_random %>%
  mutate(L = L - coords(color)[, 'L'],
         C = C - coords(color)[, 'C']) %>%
  mutate(L_trans = L * cos(L_target_theta) + C * sin(L_target_theta),
         C_trans = L * -sin(L_target_theta) + C * cos(L_target_theta)) %>%
  mutate(L = L_trans + coords(color)[, 'L'],
         C = C_trans + coords(color)[, 'C']) %>%
  select(-L_trans, -C_trans)

########## Move last?


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

## Fill in whole color space
full_colors <- expand.grid(H = unique(df_random$H_cut), C = seq(1, 200, by = 5), L = seq(1, 100, by = 5))
full_colors <- rbind(full_colors, 
                     expand.grid(H = seq(1, 360, by = 10), 
                                 C = unique(df_random$C_cut),
                                 L = seq(1, 100, by = 5),
                                 facet = 'C'))
full_colors <- rbind(full_colors, 
                     expand.grid(H = seq(1, 360, by = 10), 
                                 C = seq(1, 100, by = 5),
                                 L = unique(df_random$L_cut),
                                 facet = 'L'))
full_colors <- full_colors %>%
  mutate(H_cut = H,
         C_cut = C,
         L_cut = L,
         x = C * cos(H * (pi / 180)),
         y = C * sin(H * (pi / 180)),
         hex_color = hex(polarLUV(L, C, H))) %>%
  filter(!is.na(hex_color))

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
         H = atan2(y, x) * (180/pi) - 90) %>%
  mutate(hex_color = hex(polarLUV(L_cut, C, H))) %>%
  filter(!is.na(hex_color))
ggplot(data = full_colors, aes(x, y, col = hex_color, fill = hex_color)) +
  geom_tile() +
  geom_point(data = df_random, color = "white") +
  scale_color_identity() +
  scale_fill_identity() +
  facet_wrap(~ L_cut) +
  coord_equal() 
## something's not right, even after getting H right (+90)
