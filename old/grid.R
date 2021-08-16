library(grid)
library(tidyverse)
library(here)

#?light blue background?

set.seed(1)

n_lines <- 5000
x_limits <- c(0, 18)
y_limits <- c(0, 24)

# (x - h)^2 + (y - k)^2 = r^2
circle_h = (x_limits[1] * .5 + x_limits[2] * .5) # halfway on x
# circle_k = (y_limits[1] * .33 + y_limits[2] * .66) # two-thirds up on y
circle_r = (x_limits[2] - x_limits[1]) * .75 * .5 # diameter is 3/4 of x
circle_k = y_limits[2] - circle_r - ((x_limits[2] - x_limits[1]) * .25 * .5)
# sets circle with even margins

lines <- data.frame(line = seq(1, n_lines),
                    point_1 = runif(n_lines, 0, 2*pi),
                    point_2 = runif(n_lines, 0, 2*pi))
lines <- lines %>%
  mutate(point_1_x = circle_h + circle_r * cos(point_1),
         point_1_y = circle_k + circle_r * sin(point_1),
         point_2_x = circle_h + circle_r * cos(point_2),
         point_2_y = circle_k + circle_r * sin(point_2)) %>%
  mutate(midpoint_x = (point_1_x + point_2_x) / 2,
         midpoint_y = (point_1_y + point_2_y) / 2) %>%
  mutate(mu = line/n_lines,
         var = .005) %>%
  mutate(mu = ifelse(mu < .01, .01, ifelse(mu > .99, .99, mu)), # keeps a, b > 0
         alpha = ((1 - mu) / var - 1 / mu) * mu ^ 2,
         beta = alpha * (1 / mu - 1)) %>%
  mutate(h = runif(n(), 240, 250),
         c = rbeta(n(), shape1 = alpha, shape2 = beta) * 20 + 30,
         l = rbeta(n(), shape1 = alpha, shape2 = beta) * 20 + 30,
         line_color = hcl(h, c, l),
         point_color = hcl(h, c + 40, l + 40))

pdf(file = file.path(here::here(), "method1.pdf"),
    width = 18,
    height = 24,
    useDingbats = FALSE) # Or else points will be digbats and can't convert to PNG

grid.circle(x = circle_h,
            y = circle_k,
            r = circle_r,
            default.units = "in",
            gp = gpar(col = hcl(h = 245, c = 50, l = 50),
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
            gp = gpar(col = hcl(h = 245, c = 50, l = 50)),
            default.units = "in")

n_opts <- 10
opts <- data.frame(opt = seq(1, n_opts),
                   x = box_center_x[1] + 
                     (box_width * .75 * .5) * 
                     cos(seq(0, n_opts-1) * 
                           (2*pi / n_opts) ),
                   y = box_center_y + 
                     (box_width * .75 * .5) * 
                     sin(seq(0, n_opts-1) * 
                           (2*pi / n_opts) ))

opts <- opts %>% 
  mutate(col1 = ifelse(opt == sample(opt, 1), 
                       hcl(h = 245, c = 50, l = 50), 
                       hcl(h = 245, c = 0, l = 50 + 40))) %>%
  group_by(col1) %>%
  mutate(col2 = ifelse(opt == sample(opt, 1) | col1 == hcl(h = 245, c = 50, l = 50), 
                       hcl(h = 245, c = 50, l = 50), 
                       hcl(h = 245, c = 0, l = 50 + 40))) %>%
  ungroup()
# have to have 'col1 == hcl(h = 245, c = 50, l = 50)', not sure why

grid.points(x = opts$x,
            y = opts$y,
            default.units = "in",
            pch = 16,
            gp = gpar(col = opts$col1))

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
            gp = gpar(col = hcl(h = 245, c = 50, l = 50)),
            default.units = "in")

opts <- opts %>%
  mutate(x = box_center_x[2] + 
           (box_width * .75 * .5) * 
           cos(seq(0, n_opts-1) * 
                 (2*pi / n_opts)))

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
            gp = gpar(col = hcl(h = 245, c = 50, l = 50)),
            default.units = "in")

opts <- opts %>%
  mutate(x = box_center_x[3] + 
           (box_width * .75 * .5) * 
           cos(seq(0, n_opts-1) * 
                 (2*pi / n_opts))) %>%
  filter(col2 == hcl(h = 245, c = 50, l = 50)) 

grid.lines(x = opts$x,
           y = opts$y,
           default.units = "in",
           gp = gpar(col = hcl(h = 245, c = 50, l = 50),
                     lwd = 5))

dev.off()

library(pdftools)
library(png)

bitmap <- pdf_render_page(file.path(here::here(), "method1.pdf"),
                          dpi = 300)

png::writePNG(bitmap, file.path(here::here(), "method1.png"))

