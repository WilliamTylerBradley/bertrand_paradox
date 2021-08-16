library(colorspace)
library(tidyverse)
# http://hclwizard.org/hclcolorpicker/

color <- '#FFD700'
#color <- '#D4AF37'
color <- '#9B111E'
color <- '#50C878'
 color <- '#0F52BA'
color <- as(hex2RGB(color), "polarLUV")

## Equally spaced points of a cylinder around a point
# proportion of max to min circumference
l_range <- 15
c_range <- 10
h_range <- 3
num_points <- 50

# For each level of l, get the values of h, and then c
# l ranges from 
data <- data.frame(L_seq = seq(-l_range, l_range)) %>%
  mutate(L = L_seq + 
           coords(color)[, 'L'])

# now h_range need to change according level of l
# at L min and max, H needs to be at the center
# L center, H needs to get to H min and max
# just change proportionally to difference in L
data <- data %>%
  mutate(H = coords(color)[, 'H'],
         H_range = (l_range - abs(L_seq)) / l_range * h_range)

# Same for C and points
data <- data %>%
  mutate(C_range = (l_range - abs(L_seq)) / l_range * c_range,
         C = coords(color)[, 'C'],
         num_point = ceiling((l_range - abs(L_seq)) / l_range * num_points)) %>%
  mutate(num_point = if_else(num_point < 1, 1, num_point))


# Now got center, radius of each circle
# points around a circle, but outside needs to stretch?
central_angle <- (pi / 180) 
mid_arc_length <- (coords(color)[, 'C']     ) * (central_angle)
arc_lengths <- (coords(color)[, 'C'] + seq(-c_range, c_range, length.out = (num_points/2 - 1))) * 
  (central_angle)
arc_lengths <- mid_arc_length / arc_lengths 
arc_lengths <- c(0, cumsum(arc_lengths / (sum(arc_lengths))))
C_values <- sin((arc_lengths * pi) - (pi/2)) # y values
C_values <- c(C_values, rev(C_values))[1:num_points]
C_values <- (C_values * c_range) + coords(color)[, 'C'] 

H_values <- cos((arc_lengths * pi) - (pi/2)) # x values
H_values <- c(H_values, -rev(H_values))[1:num_points]
H_values <- (H_values * h_range) + coords(color)[, 'H'] 

test <- data.frame(x = H_values,
                   y = C_values,
                   lab = seq(1, length(C_values)))

ggplot(data = test, aes(x, y)) +
  geom_text(aes(label = lab)) +
  coord_equal()


B()H_min = data[15, "H_min"]
H_max = data[15, "H_max"]

H_seq <- seq(H_min, H_max, length.out = num_points)

max_chroma(h = H_seq,
           l = data[15, "L"])


get_points <- function(L, H, H_range, C, C_range, num_point) {
  central_angle <- (pi / 180) 
  mid_arc_length <- C * (central_angle)
  arc_lengths <- (C + seq(-C_range, C_range, length.out = (num_point/2 - 1))) * 
    (central_angle)
  arc_lengths <- mid_arc_length / arc_lengths 
  arc_lengths <- c(0, cumsum(arc_lengths / (sum(arc_lengths))))
  C_values <- sin((arc_lengths * pi) - (pi/2)) # y values
  C_values <- c(C_values, rev(C_values))[1:num_point]
  C_values <- (C_values * C_range) + C 
  
  H_values <- cos((arc_lengths * pi) - (pi/2)) # x values
  H_values <- c(H_values, -rev(H_values))[1:num_point]
  H_values <- (H_values * H_range) + H
  
  data.frame(L = L,
             H = H_values,
             C = C_values)
}

get_points(data[15, "L"],
           data[15, "H"],
           data[15, "H_range"],
           data[15, "C"],
           data[15, "C_range"],
           data[15, "num_point"])

color_region <- pmap_dfr(list(L = data$L,
              H = data$H,
              H_range = data$H_range,
              C = data$C,
              C_range = data$C_range,
              num_point = data$num_point), get_points)

color_region$max_C <- max_chroma(color_region$H,
                                 color_region$L,
                                 floor = TRUE)
color_region <- color_region %>%
  mutate(C = if_else(C < max_C, C, max_C))

color_region$hex <- hex(polarLUV(L = color_region$L,
                         C = color_region$C,
                         H = color_region$H),
                        fixup = TRUE)

color_region <- color_region %>%
  mutate(x = C * cos(H* (pi / 180)),
         y = C * sin(H* (pi / 180)))

ggplot(data = color_region, 
       aes(x = x, y = y)) +
  geom_point(color = color_region$hex) +
  coord_equal() +
  facet_wrap(~ L)

# set.seed(1)
# # When picking random colors, weight based on num in L
# color_region <- color_region[sample(nrow(color_region)), ]
# color_count <- ceiling(sqrt(nrow(color_region)))
# color_region$x <- rep(seq(1, color_count),
#                       color_count)[1:nrow(color_region)]
# color_region$y <- rep(seq(1, color_count),
#                       each = color_count)[1:nrow(color_region)]
# 
# ggplot(data = color_region,
#        aes(x = x, y = y)) +
#   geom_tile(fill = color_region$hex) +
#   coord_equal() +
#   theme_void()

# # Convex hull
# library(geometry)
# color_hull <- convhulln(as.matrix(color_region[, c('L', 'C', 'H')]), "FA")
# color_region2 <- as.matrix(color_region[, c('L', 'C', 'H')])[color_hull]
# 

# just get max around a sphere
# then weight average

# get maxes on each level - no, use max chroma

summary <- color_region %>%
  mutate(side = if_else(C < coords(color)[, 'C'], 'In', 'Out')) %>%
  group_by(L, side) %>% 
  summarise(cnt = n())
  



