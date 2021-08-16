library(tidyverse)
library(colorspace)

line_color <- as(hex2RGB("#B77C3B"), "polarLUV")
line_color <- as(hex2RGB("#E3C59B"), "polarLUV")


max_chromas <- max_chroma(h = coords(line_color)[, 'H'], l = seq(1, 100, .5))
l_target_theta <- -atan2(max(max_chromas) - coords(line_color)[, 'C'],
                       (seq(1, 100, .5)[(max(max_chromas) == max_chromas)]) - coords(line_color)[, 'L'])

line_colors <- data.frame(L = rnorm(n = n_lines * 10), # * 10 because some will
                          C = rnorm(n = n_lines * 10),   #   get dropped
                          H = rnorm(n = n_lines * 10),
                          U = runif(n = n_lines * 10)^(1/3)) %>%
  mutate(normalize = sqrt(L^2 + C^2 + H^2)) %>%
  mutate(L = L * U / normalize,
         C = C * U / normalize,
         H = H * U / normalize) %>%
  select(-U, -normalize) %>% # have random points in an ellipse here
  mutate(L = L * 100, # stretch
         C = C * 1,
         H = H * 1) %>%
  mutate(L_trans = L * cos(l_target_theta) + C * sin(l_target_theta), # tilt
         C_trans = L * -sin(l_target_theta) + C * cos(l_target_theta)) %>%
  mutate(L = L_trans,
         C = C_trans) %>%
  select(-L_trans, -C_trans) %>%
  mutate(L = L + coords(line_color)[, 'L'], # move
         C = C + coords(line_color)[, 'C'],
         H = H + coords(line_color)[, 'H']) %>%
  mutate(hex_color = hex(polarLUV(L, C, H))) %>%
  filter(!is.na(hex_color)) %>% # check
  sample_n(n_lines)

# ggplot(data = line_colors) +
#   geom_point(aes(x = C, y = L))

full_colors <- expand.grid(H = coords(line_color)[, 'H'], 
                           C = seq(1, 180, .5), 
                           L = seq(1, 100, .5)) %>%
  mutate(hex_color = hex(polarLUV(L, C, H))) %>%
  filter(!is.na(hex_color))
ggplot(data = full_colors, aes(C, L, col = hex_color, fill = hex_color)) +
  geom_point() +
  geom_point(data = line_colors, color = "white") +
  scale_color_identity() +
  scale_fill_identity() +
  geom_point(x = coords(line_color)[, 'C'],
             y = coords(line_color)[, 'L'],
             color = 'black') +
  coord_equal() 


