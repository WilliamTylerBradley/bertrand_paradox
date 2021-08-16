## reqs:
# region near color
# deal with weird shape of hcl color space
# deal with any possible region, distributions
# deal with distance increases as C increases (don't like this)
# can generate at the start, not on the fly

library(colorspace)
library(tidyverse)
# http://hclwizard.org/hclcolorpicker/

color <- '#FFD700'
#color <- '#D4AF37'
color <- '#9B111E'
color <- '#50C878'
#color <- '#0F52BA'
color <- as(hex2RGB(color), "polarLUV")

## Equally spaced points of a cylinder around a point ----
# proportion of max to min circumference
l_range <- 7
c_range <- 7
h_range <- 7

df <- data.frame(L = seq(-l_range, l_range) + coords(color)[, 'L'],
                 C = seq(-c_range, c_range) + coords(color)[, 'C'],
                 H = seq(-h_range, h_range) + coords(color)[, 'H']) %>%
  expand(L, C, H)

# graph this
df <- df %>%
  mutate(x_graph = C * cos(H * (pi / 180)),
         y_graph = C * sin(H * (pi / 180)) )

ggplot(data = df, aes(x_graph, y_graph)) +
  geom_point() +
  facet_wrap(~ L) +
  coord_equal()

# Add color
df$hex_color <- hex(polarLUV(L = df$L,
                               C = df$C,
                               H = df$H))
df$hex_color <- ifelse(is.na(df$hex_color), '#000000', df$hex_color)

ggplot(data = df, aes(x_graph, y_graph)) +
  geom_point(col = df$hex_color) +
  facet_wrap(~ df$L) +
  coord_equal()

# add in circle
df$L_base <- coords(color)[, 'L']
df$C_base <- coords(color)[, 'C']
df$H_base <- coords(color)[, 'H'] 
df <- df %>%
  mutate(distance = sqrt((L - L_base)^2 + 
                         (C - C_base)^2 +
                         (H - H_base)^2))
# doesn't factor in curve
df$hex_color <- ifelse(df$distance > l_range, '#000000', df$hex_color)

ggplot(data = df, aes(x_graph, y_graph)) +
  geom_point(col = df$hex_color) +
  facet_wrap(~ L) +
  coord_equal()

#### circle but different ranges ----
l_range <- 20
c_range <- 10
h_range <- 5

df <- data.frame(expand.grid(L = seq(-l_range, l_range) + coords(color)[, 'L'],
               C = seq(-c_range, c_range) + coords(color)[, 'C'],
               H = seq(-h_range, h_range) + coords(color)[, 'H']))


# graph this
df <- df %>%
  mutate(x_graph = C * cos(H * (pi / 180)),
         y_graph = C * sin(H * (pi / 180)) )

ggplot(data = df, aes(x_graph, y_graph)) +
  geom_point() +
  facet_wrap(~ L) +
  coord_equal()

# Add color
df$hex_color <- hex(polarLUV(L = df$L,
                             C = df$C,
                             H = df$H))
df$hex_color <- ifelse(is.na(df$hex_color), '#000000', df$hex_color)

ggplot(data = df, aes(x_graph, y_graph)) +
  geom_point(col = df$hex_color) +
  facet_wrap(~ df$L) +
  coord_equal()

# add in circle
df$L_base <- coords(color)[, 'L']
df$C_base <- coords(color)[, 'C']
df$H_base <- coords(color)[, 'H'] 
df <- df %>%
  mutate(lr = l_range,
         cr = c_range,
         hr = h_range) %>%
  mutate(distance2 = ((L - L_base)/lr)^2 + 
                            ((C - C_base)/cr)^2 +
                            ((H - H_base)/hr)^2)
df$hex_color <- ifelse(df$distance2 > 1, '#000000', df$hex_color)
# doesn't factor in curve

ggplot(data = df, aes(x_graph, y_graph)) +
  geom_point(col = df$hex_color) +
  facet_wrap(~ L) +
  coord_equal()

#### area for weights ----
# redo with just outside to get area for weights?
# redo with just outside for examples (just use grid)
# weight based on missing chucks

## retry
# line through an ellipse
# http://www.ambrsoft.com/TrigoCalc/Circles2/Ellipse/EllipseLine.htm
# (x - h)^2 / a^2 + (d - k)^2 / b^2 = 1
# x = h +/- (a / b)*sqrt(b^2 - d^2 - k^2 + 2*d*k)
## x = h1, h2
## h = H_base
## a = hr
## d = L
## k = L_base
## b = lr

df_check <- df %>%
  mutate(lr = l_range,
         hr = h_range) %>%
  mutate(h1 = H_base - (hr/lr) * sqrt(lr^2 - L^2 - L_base^2 + 2*L*L_base),
         h2 = H_base + (hr/lr) * sqrt(lr^2 - L^2 - L_base^2 + 2*L*L_base)) %>%
  mutate(diff = h2 - h1) %>%
  distinct(L, L_base, H_base, lr, hr, h1, h2, diff)

## check
df_check <- df %>%
  mutate(lr = l_range) %>%
  mutate(hr = (H - H_base) / sqrt(1 - ((L - L_base)/lr)^2)) %>%
  distinct(L, hr)



#pick a random color around the center
df <- df.frame(id = seq(1, n_size))

df$l_random <- rbeta(n = n_size, 
                       shape1 = 1, 
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