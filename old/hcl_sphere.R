library(colorspace)
library(tidyverse)
# http://hclwizard.org/hclcolorpicker/

color <- '#FFD700'
color <- '#D4AF37'
#color <- '#9B111E'
#color <- '#50C878'
color <- '#0F52BA'
color <- as(hex2RGB(color), "polarLUV")

## Equally spaced points of a cylinder around a point
# proportion of max to min circumference
l_range <- 15
c_range <- 10
h_range <- 3

central_angle <- 25 * (pi / 180)
mid_arc_length <- (coords(color)[, 'C']     ) * (central_angle)
arc_lengths <- (coords(color)[, 'C'] + seq(-c_range, c_range)) * 
  (central_angle)

# mid_arc_length needs 25 points
num_points <- data.frame(C = seq(-c_range, c_range) + 
                           coords(color)[, 'C'],
                         num_point = round(25 * arc_lengths / mid_arc_length)) # num points for each value of C

num_points <- merge(data.frame(L = seq(-l_range, l_range) + 
                            coords(color)[, 'L']),
                      num_points) %>%
  mutate(H = coords(color)[, 'H'])


num_points <- num_points %>%
  mutate(x = C * cos(H),
         y = C * sin(H))

get_points <- function(L, C, H, num_point) {
  data.frame(L = L,
             C = C,
             H = H + seq(-3, 3, length.out = num_point))
}

# get_points(L = num_points[3, 'L'],
#            C = num_points[3, 'C'],
#            H = num_points[3, 'H'],
#            num_point = num_points[3, 'num_point'])

color_region <- pmap_dfr(list(L = num_points$L,
              C = num_points$C,
              H = num_points$H,
              num_point = num_points$num_point), get_points)

color_region$hex <- hex(polarLUV(L = color_region$L,
                         C = color_region$C,
                         H = color_region$H))

color_region <- color_region %>%
  filter(!is.na(hex))

color_region <- color_region %>%
  mutate(x = C * cos(H* (pi / 180)),
         y = C * sin(H* (pi / 180)))

# ggplot(data = color_region, 
#        aes(x = x, y = y)) +
#   geom_point(color = color_region$hex) +
#   coord_equal() +
#   xlim(-100, 180) +
#   ylim(-100, 100)

ggplot(data = color_region, 
       aes(x = x, y = y)) +
  geom_point(color = color_region$hex) +
  coord_equal() +
  facet_wrap(~ L)

set.seed(1)
# When picking random colors, weight based on num in L
color_region <- color_region[sample(nrow(color_region)), ]
color_count <- ceiling(sqrt(nrow(color_region)))
color_region$x <- rep(seq(1, color_count),
                      color_count)[1:nrow(color_region)]
color_region$y <- rep(seq(1, color_count),
                      each = color_count)[1:nrow(color_region)]

ggplot(data = color_region,
       aes(x = x, y = y)) +
  geom_tile(fill = color_region$hex) +
  coord_equal() +
  theme_void()

# # Convex hull
# library(geometry)
# color_hull <- convhulln(as.matrix(color_region[, c('L', 'C', 'H')]), "FA")
# color_region2 <- as.matrix(color_region[, c('L', 'C', 'H')])[color_hull]
# 

# just get max around a sphere
# then weight average





