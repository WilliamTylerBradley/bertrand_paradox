library(colorspace)
library(tidyverse)
# http://hclwizard.org/hclcolorpicker/

color <- '#FFD700'
color <- '#D4AF37'
color <- '#9B111E'
#color <- '#50C878'
color <- '#0F52BA'

#test
color <- '#AC9665'
color <- as(hex2RGB(color), "polarLUV")

## Equally spaced points of a cylinder around a point ----
# proportion of max to min circumference
l_range <- 7
c_range <- 7
h_range <- 7

df <- data.frame(expand.grid(L = seq(-l_range * 1.5, l_range * 1.5) +
                               coords(color)[, 'L'],
                             C = seq(-c_range * 1.5, c_range * 1.5) + 
                               coords(color)[, 'C'],
                             H = seq(-h_range * 1.5, h_range * 1.5) + 
                               coords(color)[, 'H']))

# graph this
df <- df %>%
  mutate(x_graph = C * cos(H * (pi / 180)),
         y_graph = C * sin(H * (pi / 180)) )

# ggplot(data = df, aes(x_graph, y_graph)) +
#   geom_point() +
#   facet_wrap(~ L) +
#   coord_equal()

# Add color
df$hex_color <- hex(polarLUV(L = df$L,
                             C = df$C,
                             H = df$H))
df$hex_color <- ifelse(is.na(df$hex_color), '#000000', df$hex_color)

# ggplot(data = df, aes(x_graph, y_graph)) +
#   geom_point(col = df$hex_color) +
#   facet_wrap(~ df$L) +
#   coord_equal()

## Add beta distibution
# #pick a random color around the center
n_size <- 100000
df <- data.frame(id = seq(1, n_size))

df$l_random <- rbeta(n = n_size,
                       shape1 = 1.25,
                       shape2 = 1)
df$l_random <- (df$l_random - .5) * l_range + coords(color)[, 'L']
df$c_random <- rbeta(n = n_size,
                       shape1 = 1,
                       shape2 = 1)
df$c_random <- (df$c_random - .5) * c_range + coords(color)[, 'C']
df$h_random <- rbeta(n = n_size,
                       shape1 = 1,
                       shape2 = 1)
df$h_random <- (df$h_random - .5) * h_range + coords(color)[, 'H']

df$l_graph <- cut(df$l_random, 25,
                    labels = round(quantile(df$l_random,
                                            seq(0.1, 1, length.out = 25)), 2))

df <- df %>%
  mutate(x_graph = c_random * cos(h_random * (pi / 180)),
         y_graph = c_random * sin(h_random * (pi / 180)) )

df$hex_color <- hex(polarLUV(L = df$l_random,
                             C = df$c_random,
                             H = df$h_random))
df$hex_color <- ifelse(is.na(df$hex_color), '#000000', df$hex_color)

ggplot(data = df, aes(x_graph, y_graph)) +
  geom_point(col = df$hex_color) +
  facet_wrap(~ df$l_graph) +
  coord_equal()
