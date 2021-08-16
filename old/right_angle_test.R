library(grid)
library(tidyverse)
library(here)
#  http://sape.inf.usi.ch/quick-reference/ggplot2/colour

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
  
  return(data.frame(midpoint_x = midpoint_x, 
                    midpoint_y = midpoint_y,
                    new_point_x = new_point_x, 
                    new_point_y = new_point_y))
}

n_opts <- 12
opts <- data.frame(opt = seq(1, n_opts),
                   radius = seq(0, n_opts-1) * 
                     (2*pi / n_opts))
opts <- opts %>%
  bind_cols(pmap_dfr(list(circle_x = 0,
                          circle_y = 0,
                          circle_radius = 1, 
                          radius = .$radius,
                          chord = 1), end_points))
names(opts) <- c("opt", "radius", "x", "y", "x", "y")
opts <- rbind(opts[, c(1, 2, 3, 4)], opts[, c(1, 2, 5, 6)])
ggplot(data = opts, aes(x = x, y = y, col = opt, group = opt)) +
  geom_line()
