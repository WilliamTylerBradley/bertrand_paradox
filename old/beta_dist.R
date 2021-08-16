library(ggplot2)
library(tidyverse)
library(here)

mu <- seq(.1, .9, .1)
beta_data <- data.frame(mu = rep(mu, each = 99),
                        var = .005,
                        x = rep(seq(.01, .99, .01), length(mu)))
beta_data <- beta_data %>%
  mutate(alpha = ((1 - mu) / var - 1 / mu) * mu ^ 2,
         beta = alpha * (1 / mu - 1))
beta_data <- beta_data %>%
  mutate(y = dbeta(x, alpha, beta))

ggplot(data = beta_data,
       aes(x, y, color = mu, group = mu)) +
  geom_line()
