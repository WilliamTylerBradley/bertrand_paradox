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

# ggplot(data = df, aes(x_graph, y_graph)) +
#   geom_point(col = df$hex_color) +
#   facet_wrap(~ L) +
#   coord_equal()

#### circle but different ranges ----
l_range <- 20
c_range <- 10
h_range <- 5

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

# ggplot(data = df, aes(x_graph, y_graph)) +
#   geom_point(col = df$hex_color) +
#   facet_wrap(~ L) +
#   coord_equal()
# 
# ggplot(data = df, aes(C, L)) +
#   geom_point(col = df$hex_color) +
#   facet_wrap(~ H) +
#   coord_equal()

## running into issues because of shapes, want to get darker colors
# what if we tilt the ellipse towards L = 25, C = 0, H = 0
# https://math.stackexchange.com/questions/426150/what-is-the-general-equation-of-the-ellipse-that-is-not-in-the-origin-and-rotate
# ((x - h) * cos(A) + (y - k) * sin(A)^2) / a^2
# https://math.stackexchange.com/questions/3513289/matrix-form-for-rotate-ellipse?noredirect=1&lq=1
# 

L_target <- 25
## transformation angle = angle between coords(color)[, 'L'] and L_target
L_target_theta = -atan(coords(color)[, 'C'] / (coords(color)[, 'L'] - 25))
# L_target_theta * (180/pi)

# transformation matrix
# trans_mat <- matrix(c(1, 0, 0, 
#                       0, cos(L_target_theta), -sin(L_target_theta), 
#                       0, sin(L_target_theta), cos(L_target_theta)),
#                        nrow = 3, ncol = 3, byrow = TRUE)
# trans_mat <- matrix(c(cos(L_target_theta), 0, -sin(L_target_theta), 
#                       0, 1, 0, 
#                       sin(L_target_theta), 0, cos(L_target_theta)),
#                     nrow = 3, ncol = 3, byrow = TRUE)
trans_mat <- matrix(c(cos(L_target_theta), -sin(L_target_theta), 0,
                      sin(L_target_theta), cos(L_target_theta), 0,
                      0, 0, 1),
                    nrow = 3, ncol = 3, byrow = TRUE)
trans_mat <- t(trans_mat) %*% 
  diag(c(l_range^-1, c_range^-1, h_range^-1)) %*% 
  trans_mat

color_matrix <- as.matrix(df[, c("L", "C", "H")], ncol = 3) -
  as.matrix(df[, c("L_base", "C_base", "H_base")], ncol = 3)

df$distance_new <- rowSums( (color_matrix %*% trans_mat)^2)

# Add color
df$hex_color <- hex(polarLUV(L = df$L,
                             C = df$C,
                             H = df$H))
df$hex_color <- ifelse(is.na(df$hex_color), '#000000', df$hex_color)
df$hex_color <- ifelse(df$distance_new > 1, '#000000', df$hex_color)
# doesn't factor in curve

# ggplot(data = df, aes(x_graph, y_graph)) +
#   geom_point(col = df$hex_color) +
#   facet_wrap(~ L) +
#   coord_equal()
# 
# ggplot(data = df, aes(C, L)) +
#   geom_point(col = df$hex_color) +
#   facet_wrap(~ H) +
#   coord_equal() 

## random points in x, y, z
# see if in balloon

## balloon is here:
((L * cos(L_target_theta) + C * sin(L_target_theta)) / l_range)^2 + 
  ((L * -sin(L_target_theta) + C * cos(L_target_theta)) / c_range)^2 +
  (H / h_range)^2 = 1

## propose steps MCMC style - then thin and multiple steps
# Start at center 

small_step <- function(L, C, H, 
                       l_base, c_base, h_base,
                       l_step, c_step, h_step,
                       l_range, c_range, h_range,
                       L_target_theta) {
  
  L <- L + l_step - l_base
  C <- C + c_step - 
  H <- H + h_step
  
  if((( * cos(L_target_theta) + C * sin(L_target_theta)) / l_range)^2 + 
    ((L * -sin(L_target_theta) + C * cos(L_target_theta)) / c_range)^2 +
    (H / h_range)^2 <= 1) {
    
    hex_color = hex(polarLUV(L, C, H))
    
    if(!is.na(hex_color)){
      return(data.frame(L = L, H = H, C = C, hex_color = hex_color))
    }
  }
  
  return(data.frame(L = NA, H = NA, C = NA, hex_color = NA))
  
}

small_step(coords(color)[, 'L'], coords(color)[, 'C'], coords(color)[, 'H'],
           0, 0, 0,
           l_range, c_range, h_range,
           L_target_theta
           )

## check ballon function passed as parameter?
L <- coords(color)[, 'L']
C <- coords(color)[, 'C']
H <- coords(color)[, 'H']
((L * cos(L_target_theta) + C * sin(L_target_theta)) / l_range)^2 + 
  ((L * -sin(L_target_theta) + C * cos(L_target_theta)) / c_range)^2 +
  (H / h_range)^2 <= 1


## Choose some colors at random
n_random = 50000

l_range <- 25
c_range <- 15
h_range <- 5
df_random <- data.frame(L = rnorm(n = n_random),
                        C = rnorm(n = n_random),
                        H = rnorm(n = n_random))


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

# df_random <- df_random %>%
#   mutate(x = (row_number() - 1) %/% round(sqrt(n()))) %>%
#   group_by(x) %>%
#   mutate(y = row_number()) %>%
#   ungroup()
# 
# ggplot(data = df_random, aes(x, y)) +
#   geom_tile(fill = df_random$hex_color) +
#   coord_equal() +
#   theme_void()

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

test <- df_random %>%
  group_by(C_cut) %>%
  summarise(cnt = n())

## graph beta distribution that's been tilted by using dbeta

#pick a random color around the center
# df <- df.frame(id = seq(1, n_size))
# 
# df$l_random <- rbeta(n = n_size, 
#                        shape1 = 1, 
#                        shape2 = 1)
# df$l_random <- (df$l_random - .5) * l_range + coords(color)[, 'L'] 
# df$c_random <- rbeta(n = n_size, 
#                        shape1 = 1, 
#                        shape2 = 1)
# df$c_random <- (df$c_random - .5) * c_range + coords(color)[, 'C'] 
# df$h_random <- rbeta(n = n_size, 
#                        shape1 = 1, 
#                        shape2 = 1)
# df$h_random <- (df$h_random - .5) * h_range + coords(color)[, 'H']
# 
# df$l_graph <- cut(df$l_random, 25, 
#                     labels = round(quantile(df$l_random, 
#                                             seq(0.1, 1, length.out = 25)), 2))

## What if just beta instead of ellipse
x <- seq(0, 1, .001)
y <- dbeta(x, shape1 = 2, shape2 = 1)
plot(x, y)






