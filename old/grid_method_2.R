library(grid)
library(tidyverse)
library(here)
#  http://sape.inf.usi.ch/quick-reference/ggplot2/colour

x^2 + y^2 = 1
y = 



cos(pi/2)
sin(pi/2)
# circle_x is x value of center
# circle_y is y value of center
# circle_radius is radius
# radius is radian
# chord is fraction along the radius 
end_points <- function(circle_x, circle_y, circle_radius, radius, chord) {
  
  ## Assume at the origin, then move at the end
  # Find first point on the chord
  midpoint_x = chord * cos(radius)
  midpoint_y = chord * sin(radius)
  
  # Find second point by rotating midpoint of circle 90*
  # 1) moving to origin, substract midpoint_x and midpoint_y
  # 2) rotating around, cos/sin
  # 3) move back, add back 
  new_point_x = midpoint_y + midpoint_x
  new_point_y = (-midpoint_x) + midpoint_y
  
  # Find intersections
  # http://mathworld.wolfram.com/Circle-LineIntersection.html
  dx = new_point_x - midpoint_x
  dy = new_point_y - midpoint_y
  dr = sqrt(dx^2 + dy^2)
  D = midpoint_x*new_point_y - new_point_x*midpoint_y
  
  point_x1 = (D*dy + sign(dy)*dx*sqrt(circle_radius^2*dr^2-D^2)) / dr^2
  point_x2 = (D*dy - sign(dy)*dx*sqrt(circle_radius^2*dr^2-D^2)) / dr^2
  point_y1 = (-D*dx + abs(dy)*sqrt(circle_radius^2*dr^2-D^2)) / dr^2
  point_y2 = (-D*dx - abs(dy)*sqrt(circle_radius^2*dr^2-D^2)) / dr^2

  return(data.frame(midpoint_y = midpoint_y,
                    midpoint_x = midpoint_x,
                    point_x1 = point_x1, 
                    point_x2 = point_x2,
                    point_y1 = point_y1, 
                    point_y2 = point_y2))
}

n_opts <- 12
opts <- data.frame(opt = seq(1, n_opts),
                   radius = seq(0, n_opts-1) * 
                     (2*pi / n_opts))
opts <- opts %>%
  mutate(chord = .5) %>%
  mutate(midpoint_x = 0 + chord * cos(radius),
         midpoint_y = 0 + chord * sin(radius)) %>%
  mutate(chord_m = -(0 - midpoint_x)/(0 - midpoint_y)) %>%
  mutate(chord_b = midpoint_y - (chord_m * midpoint_x)) 



#----

opts <- opts %>%
  bind_cols(pmap_dfr(list(circle_x = 0,
                          circle_y = 0,
                          circle_radius = 1, 
                          radius = .$radius,
                          chord = .5), end_points))
midpoints <- opts[, c(1, 2, 3, 4)]
opts <- opts[, c(1, 2, 5, 6, 7, 8)]
names(opts) <- c("opt", "radius", "x", "y", "x", "y")
opts <- rbind(opts[, c(1, 2, 3, 4)], opts[, c(1, 2, 5, 6)])
ggplot(data = opts, aes(x = x, y = y, col = opt, group = opt)) +
  geom_line() +
  geom_point(data = midpoints, aes(x = midpoint_x, y = midpoint_y))

#----

#?light blue background?

hue_center <- 65

#set.seed(2)

n_lines <- 50
x_limits <- c(0, 18)
y_limits <- c(0, 24)

# (x - h)^2 + (y - k)^2 = r^2
circle_h = (x_limits[1] * .5 + x_limits[2] * .5) # halfway on x
# circle_k = (y_limits[1] * .33 + y_limits[2] * .66) # two-thirds up on y
circle_r = (x_limits[2] - x_limits[1]) * .75 * .5 # diameter is 3/4 of x
circle_k = y_limits[2] - circle_r - ((x_limits[2] - x_limits[1]) * .25 * .5)
# sets circle with even margins

lines <- data.frame(line = seq(1, n_lines),
                    radius = runif(n_lines, 0, 2*pi),
                    chord = runif(n_lines, 0, circle_r))
# http://www.ambrsoft.com/TrigoCalc/Circles2/circlrLine_.htm
lines <- lines %>%
  mutate(midpoint_x = circle_h + chord * cos(radius),
         midpoint_y = circle_k + chord * sin(radius)) %>%
  mutate(chord_m = -(circle_h - midpoint_x)/(circle_k - midpoint_y)) %>%
  mutate(chord_b = midpoint_y - (chord_m * midpoint_x)) %>%
  mutate(delta = (circle_r^2)*(1 + chord_m^2) - (circle_k - chord_m*circle_h - chord_b)^2) %>%
  mutate(point_1_x = (circle_h + circle_k*chord_m - chord_b*chord_m + sqrt(delta)) / (1 + chord_m^2),
         point_2_x = (circle_h + circle_k*chord_m - chord_b*chord_m - sqrt(delta)) / (1 + chord_m^2),
         point_1_y = (chord_b + circle_h*chord_m + circle_k*(chord_m^2) + chord_m*sqrt(delta)) / (1 + chord_m^2),
         point_2_y = (chord_b + circle_h*chord_m + circle_k*(chord_m^2) - chord_m*sqrt(delta)) / (1 + chord_m^2)) %>%
  mutate(check1 = (point_1_x - circle_h)^2 + (point_1_y - circle_k)^2,
         check2 = (point_1_x - circle_h)^2 + (point_2_y - circle_k)^2)

lines <- lines %>%
  mutate(mu = line/n_lines,
         var = .005) %>%
  mutate(mu = ifelse(mu < .01, .01, ifelse(mu > .99, .99, mu)), # keeps a, b > 0
         alpha = ((1 - mu) / var - 1 / mu) * mu ^ 2,
         beta = alpha * (1 / mu - 1)) %>%
  mutate(h = runif(n(), hue_center - 5, hue_center + 5),
         c = rbeta(n(), shape1 = alpha, shape2 = beta) * 20 + 30,
         l = rbeta(n(), shape1 = alpha, shape2 = beta) * 20 + 30,
         line_color = hcl(h, c, l),
         point_color = hcl(h, c + 40, l + 40))

pdf(file = file.path(here::here(), "method2.pdf"),
    width = 18,
    height = 24,
    useDingbats = FALSE) # Or else points will be digbats and can't convert to PNG

grid.circle(x = circle_h,
            y = circle_k,
            r = circle_r,
            default.units = "in",
            gp = gpar(col = hcl(h = hue_center, c = 50, l = 50),
                      lwd=10))

for(i in 1:nrow(lines)) {
  grid.lines(x = c(lines$point_1_x[i], lines$point_2_x[i]),
             y = c(lines$point_1_y[i], lines$point_2_y[i]),
             gp = gpar(col = lines$line_color[i],
                       alpha = 1 - (i/n_lines)),
             default.units = "in")
  grid.points(x = lines$midpoint_x[i],
              y = lines$midpoint_y[i],
              pch = 16,
              gp = gpar(col = lines$point_color[i],
                        alpha = .95 - (i/n_lines)*.9),
              default.units = "in")
}

## Add process

## box location
# center of center
box_center_y <- (circle_k - circle_r) / 2

# box sizes
box_width <- (circle_k - circle_r) / 4

# space between boxes
box_space <- box_width / 2

## center of other boxes
box_center_x <- (c(-1, 0, 1) * (box_width + box_space)) + circle_h

## Add boxes

# box 1
grid.rect(x = box_center_x[1],
          y = box_center_y,
          width = box_width,
          height = box_width,
          default.units = "in",
          just = "center",
          gp = gpar(lwd=5))

grid.circle(x = box_center_x[1],
            y = box_center_y,
            r = box_width * .75 * .5,
            gp = gpar(col = hcl(h = hue_center, c = 50, l = 50)),
            default.units = "in")

n_opts <- 10
opts <- data.frame(opt = seq(1, n_opts),
                   radius = seq(0, n_opts-1) * 
                     (2*pi / n_opts))
opts <- opts %>%
  mutate(
                   x1 = box_center_x[1],
                   y1 = box_center_y,
                   x2 = box_center_x[1] + 
                     (box_width * .75 * .5) * 
                     cos(radius),
                   y2 = box_center_y + 
                     (box_width * .75 * .5) * 
                     sin(radius )) 

opts <- opts %>% 
  mutate(col1 = ifelse(opt == sample(opt, 1), 
                       hcl(h = hue_center, c = 50, l = 50), 
                       hcl(h = hue_center, c = 0, l = 50 + 40)))

for(i in seq(1, nrow(opts))) {
  grid.lines(x = c(opts$x1[i], opts$x2[i]),
              y = c(opts$y1[i], opts$y2[i]),
              default.units = "in",
              gp = gpar(col = opts$col1[i]))
}

# box 2
grid.rect(x = box_center_x[2],
          y = box_center_y,
          width = box_width,
          height = box_width,
          default.units = "in",
          just = "center",
          gp = gpar(lwd=5))

grid.circle(x = box_center_x[2],
            y = box_center_y,
            r = box_width * .75 * .5,
            gp = gpar(col = hcl(h = hue_center, c = 50, l = 50)),
            default.units = "in")

opts <- opts %>%
  mutate(x = box_center_x[2] + 
           (box_width * .75 * .5) * 
           cos(seq(0, n_opts-1) * 
                 (2*pi / n_opts))) %>%
  filter(col1 == hcl(h = hue_center, c = 50, l = 50)) 

opts_2 <- data.frame(opt = seq(1, n_opts),
                     radius = opts$radius,
                     chord = seq(0, n_opts-1) * (1/n_opts) * (box_width * .75 * .5))
opts_2 <- opts_2 %>%
  mutate(x1 = box_center_x[2] + 
           chord * 
                      cos(radius),
                    y1 = box_center_y + chord *
                      sin(radius )) 

opts_2 <- opts_2 %>%
  mutate(midpoint_x = box_center_x[2] + chord * cos(radius),
         midpoint_y = box_center_y + chord * sin(radius)) %>%
  mutate(chord_m = ifelse(box_center_y != midpoint_y, -(box_center_x[2] - midpoint_x)/(box_center_y - midpoint_y), Inf)) %>%
  mutate(chord_b = ifelse(chord_m != Inf, midpoint_y - (chord_m * midpoint_x), Inf)) %>%
  mutate(delta = ((box_width * .75 * .5)^2)*(1 + chord_m^2) - (box_center_y - chord_m*box_center_x[2] - chord_b)^2) %>%
  mutate(point_1_x = (box_center_x[2] + box_center_y*chord_m - chord_b*chord_m + sqrt(delta)) / (1 + chord_m^2),
         point_2_x = (box_center_x[2] + box_center_y*chord_m - chord_b*chord_m - sqrt(delta)) / (1 + chord_m^2),
         point_1_y = (chord_b + box_center_x[2]*chord_m + box_center_y*(chord_m^2) + chord_m*sqrt(delta)) / (1 + chord_m^2),
         point_2_y = (chord_b + box_center_x[2]*chord_m + box_center_y*(chord_m^2) - chord_m*sqrt(delta)) / (1 + chord_m^2)) %>%
  mutate(check1 = (point_1_x - box_center_x[2])^2 + (point_1_y - box_center_y)^2,
         check2 = (point_1_x - box_center_x[2])^2 + (point_2_y - box_center_y)^2)
  

grid.points(x = opts$x,
            y = opts$y,
            default.units = "in",
            pch = 16,
            gp = gpar(col = opts$col2))

# box 3
grid.rect(x = box_center_x[3],
          y = box_center_y,
          width = box_width,
          height = box_width,
          default.units = "in",
          just = "center",
          gp = gpar(lwd=5))

grid.circle(x = box_center_x[3],
            y = box_center_y,
            r = box_width * .75 * .5,
            gp = gpar(col = hcl(h = hue_center, c = 50, l = 50)),
            default.units = "in")

opts <- opts %>%
  mutate(x = box_center_x[3] + 
           (box_width * .75 * .5) * 
           cos(seq(0, n_opts-1) * 
                 (2*pi / n_opts))) %>%
  filter(col1 == hcl(h = hue_center, c = 50, l = 50)) 

grid.lines(x = opts$x,
           y = opts$y,
           default.units = "in",
           gp = gpar(col = hcl(h = hue_center, c = 50, l = 50),
                     lwd = 5))

dev.off()

# library(pdftools)
# library(png)
# 
# bitmap <- pdf_render_page(file.path(here::here(), "method2.pdf"),
#                           dpi = 300)
# 
# png::writePNG(bitmap, file.path(here::here(), "method2.png"))

